[@@@ocaml.warning "+a-40-41-42"]
(* Coverage instrumentation compatible with LLVM's SanitizerCoverage
   [-fsanitize-coverage=trace-pc-guard].

   We allocate a per-compilation-unit array of uint32 guard slots in
   the [ocaml_trace_pc_guards] section and insert a call to

     void __sanitizer_cov_trace_pc_guard(uint32_t *guard);

   at every branch target, passing a pointer to the corresponding slot.
   The linker concatenates the arrays of all units into one section
   delimited by [__start_ocaml_trace_pc_guards] and
   [__stop_ocaml_trace_pc_guards]; whoever provides the callback is
   expected to register that range once per executable or shared object
   (typically by calling [__sanitizer_cov_trace_pc_guard_init] from a
   C constructor), just as clang does for its [__sancov_guards] section.

   The tree walk closely mirrors [Afl_instrument]. *)

open! Int_replace_polymorphic_compare
open Cmm

(* ---- state --------------------------------------------------------- *)

(* Number of guard slots allocated so far in the current compilation
   unit.  Set to [Some r] by [with_instrumentation_state] and reset
   to [None] at the end. *)
let guard_count : int ref option ref = ref None

let next_guard () =
  match !guard_count with
  | None ->
    Misc.fatal_error
      "Trace_pc_guard_instrument: not inside with_instrumentation_state"
  | Some r ->
    let n = !r in
    r := n + 1;
    n

(* ---- symbol helpers ------------------------------------------------ *)

let guard_sym_name () =
  Cmm_helpers.make_symbol "trace_pc_guards"

let global_sym name =
  { sym_name = name; sym_global = Global }

(* ---- call helpers -------------------------------------------------- *)

(* Emit: if (caml_trace_pc_guard_enabled) __sanitizer_cov_trace_pc_guard(&guards[n])
   where each guard is a uint32 (4 bytes).

   The byte-sized flag is defined by whatever provides the callbacks at link
   time. Checking it inline keeps the cost of instrumented code down to a
   well-predicted branch when coverage collection is not active (e.g. in
   build tools, or when running outside the coverage environment), which an
   unconditional call cannot achieve even with an empty callee because of the
   register spills the call forces. *)
let emit_trace_pc_guard dbg =
  let n = next_guard () in
  let guard_base = Cconst_symbol (global_sym (guard_sym_name ()), dbg) in
  let guard_ptr =
    if n = 0
    then guard_base
    else Cop (Cadda, [guard_base; Cconst_int (n * 4, dbg)], dbg)
  in
  let call =
    Cop
      ( Cextcall
          { func = "__sanitizer_cov_trace_pc_guard";
            builtin = false;
            returns = true;
            effects = Arbitrary_effects;
            coeffects = Has_coeffects;
            ty = typ_void;
            alloc = false;
            ty_args = [XInt]
          },
        [guard_ptr],
        dbg )
  in
  let enabled =
    Cop
      ( Cload
          { memory_chunk = Byte_unsigned;
            mutability = Asttypes.Mutable;
            is_atomic = false
          },
        [Cconst_symbol (global_sym "caml_trace_pc_guard_enabled", dbg)],
        dbg )
  in
  Cifthenelse
    ( Cop (Ccmpi Cne, [enabled; Cconst_int (0, dbg)], dbg),
      dbg, call,
      dbg, Ctuple [],
      dbg )

(* ---- tree walk (mirrors Afl_instrument) ----------------------------- *)

let rec with_trace_pc_guard b dbg =
  let call = emit_trace_pc_guard dbg in
  Csequence (call, instrument b)

and instrument = function
  (* Branch targets get logging *)
  | Cifthenelse (cond, t_dbg, t, f_dbg, f, dbg) ->
    Cifthenelse
      ( instrument cond,
        t_dbg, with_trace_pc_guard t t_dbg,
        f_dbg, with_trace_pc_guard f f_dbg,
        dbg )
  | Ccatch (Exn_handler, cases, body) ->
    let cases =
      List.map
        (fun Cmm.{ label = nfail; params = ids;
                   body = e; dbg; is_cold } ->
          Cmm.{ label = nfail; params = ids;
                 body = with_trace_pc_guard e dbg;
                 dbg; is_cold })
        cases
    in
    Ccatch (Exn_handler, cases, instrument body)
  | Cswitch (e, cases, handlers, dbg) ->
    let handlers =
      Array.map
        (fun (handler, handler_dbg) ->
          (with_trace_pc_guard handler handler_dbg, handler_dbg))
        handlers
    in
    Cswitch (instrument e, cases, handlers, dbg)
  (* Recurse without logging *)
  | Clet (v, e, body) ->
    Clet (v, instrument e, instrument body)
  | Cphantom_let (v, defining_expr, body) ->
    Cphantom_let (v, defining_expr, instrument body)
  | Cname_for_debugger (var, body) -> Cname_for_debugger (var, instrument body)
  | Ctuple es ->
    Ctuple (List.map instrument es)
  | Cop (op, es, dbg) ->
    Cop (op, List.map instrument es, dbg)
  | Csequence (e1, e2) ->
    Csequence (instrument e1, instrument e2)
  | Ccatch ((Normal | Recursive as flag), cases, body) ->
    let cases =
      List.map
        (fun Cmm.{ label = nfail; params = ids;
                   body = e; dbg; is_cold } ->
          Cmm.{ label = nfail; params = ids;
                 body = instrument e; dbg; is_cold })
        cases
    in
    Ccatch (flag, cases, instrument body)
  | Cexit (ex, args, traps) ->
    Cexit (ex, List.map instrument args, traps)
  (* Leaves *)
  | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
  | Cconst_vec128 _ | Cconst_vec256 _ | Cconst_vec512 _ | Cconst_mask _
  | Cconst_symbol _ | Cvar _ | Cinvalid _ as c ->
    c

(* ---- public API ----------------------------------------------------- *)

let instrument_function c dbg =
  with_trace_pc_guard c dbg

let instrument_initialiser c dbg =
  with_trace_pc_guard c (dbg ())

let guards_section = "ocaml_trace_pc_guards"

let with_instrumentation_state f =
  let r = ref 0 in
  guard_count := Some r;
  let phrases = f () in
  let count = !r in
  guard_count := None;
  if count = 0
  then phrases
  else
    (* Emit the guard array: [count] uint32 slots, all zero-initialised,
       in the dedicated section so that the linker concatenates the arrays
       of every unit and provides [__start_ocaml_trace_pc_guards] and
       [__stop_ocaml_trace_pc_guards] delimiting the whole. *)
    let data =
      Cdata_in_section
        { section = guards_section;
          items =
            Cdefine_symbol (global_sym (guard_sym_name ()))
            :: List.init count (fun _i -> Cint32 0n)
        }
    in
    phrases @ [data]
