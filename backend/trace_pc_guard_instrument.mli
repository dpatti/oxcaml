[@@@ocaml.warning "+a-40-41-42"]

(** Coverage instrumentation compatible with LLVM's SanitizerCoverage
    [-fsanitize-coverage=trace-pc-guard].

    Emits calls to [__sanitizer_cov_trace_pc_guard] at branch targets,
    and places each unit's guard array in the [ocaml_trace_pc_guards]
    section so that the linker-provided [__start_ocaml_trace_pc_guards]
    and [__stop_ocaml_trace_pc_guards] delimit all guards of a linked
    executable or shared object.  The callback is not defined by the
    compiler or the runtime: like clang, we only emit references, and
    the final link must supply it, along with a constructor that passes
    the section bounds to [__sanitizer_cov_trace_pc_guard_init] once.

    Unlike clang, each branch-target call is guarded by a test of the
    byte-sized global [caml_trace_pc_guard_enabled], which the link
    must also define.  This lets the provider of the callbacks turn
    the instrumentation into a predictable branch when coverage is
    not being collected. *)

(** Instrument a function body. Must be called within the scope of
    {!with_instrumentation_state}. *)
val instrument_function : Cmm.expression -> Debuginfo.t -> Cmm.expression

(** Instrument a module initialiser. Must be called within the scope of
    {!with_instrumentation_state}. *)
val instrument_initialiser
  :  Cmm.expression
  -> (unit -> Debuginfo.t)
  -> Cmm.expression

(** [with_instrumentation_state f] runs [f ()], which may call
    {!instrument_function} and {!instrument_initialiser} any number of
    times, and returns the list of Cmm phrases produced by [f]
    together with any additional data phrases needed for the guard
    array. *)
val with_instrumentation_state
  :  (unit -> Cmm.phrase list)
  -> Cmm.phrase list
