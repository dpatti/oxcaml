(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module DLL = Doubly_linked_list

let gate_symbol : Cmm.symbol =
  { sym_name = "__sancov_should_track"; sym_global = Global }

let callback = "__sanitizer_cov_trace_pc_guard"

(* Number of guards allocated so far in the current unit. Guard [n] is the local
   symbol [guard_symbol n]; [emit_guards] defines all of them, in order. *)
let guard_count = ref 0

let reset () = guard_count := 0

let guard_symbol n : Cmm.symbol =
  { sym_name = Cmm_helpers.make_symbol (Printf.sprintf "trace_pc_guard_%d" n);
    sym_global = Local
  }

let new_guard () =
  let n = !guard_count in
  incr guard_count;
  guard_symbol n

(* An edge is critical if its source has several successors and its destination
   several predecessors. A probe can only record such an edge if a block is
   inserted on it. Exceptional edges cannot be split, but they still count
   towards the number of successors. *)
let split_critical_edges cfg_with_layout =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let critical_edges =
    Cfg.fold_blocks cfg ~init:[] ~f:(fun _label block acc ->
        let num_successors =
          Label.Set.cardinal (Cfg.successor_labels ~normal:true ~exn:true block)
        in
        if num_successors <= 1
        then acc
        else
          Label.Set.fold
            (fun successor acc ->
              let successor_block = Cfg.get_block_exn cfg successor in
              if Label.Set.cardinal successor_block.predecessors > 1
              then (block.start, successor) :: acc
              else acc)
            (Cfg.successor_labels ~normal:true ~exn:false block)
            acc)
  in
  List.iter
    (fun (source, destination) ->
      let (_ : Cfg.basic_block list) =
        Cfg_with_layout.insert_block cfg_with_layout (DLL.make_empty ())
          ~after:(Cfg.get_block_exn cfg source)
          ~before:(Some (Cfg.get_block_exn cfg destination))
      in
      ())
    critical_edges;
  not (Misc.Stdlib.List.is_empty critical_edges)

module Post_dominators : sig
  type t

  val build : Cfg.t -> t

  (** [is_post_dominating t x y] is [true] iff all paths from [y] to the exit of
      the function go through [x]. *)
  val is_post_dominating : t -> Label.t -> Label.t -> bool
end = struct
  (* The algorithm of Cooper, Harvey and Kennedy ("A Simple, Fast Dominance
     Algorithm") applied to the reversed graph, whose root is a virtual exit
     node succeeding every block without successors. Nodes are numbered in
     postorder of the reversed graph, so the root has the highest number. Blocks
     from which no exit is reachable (infinite loops) are not numbered: they
     neither post-dominate nor are post-dominated by any other block. *)
  type t =
    { number : int Label.Tbl.t;
      ipdom : int array
    }

  let successors cfg label =
    Cfg.successor_labels ~normal:true ~exn:true (Cfg.get_block_exn cfg label)

  let build cfg =
    let exits =
      Cfg.fold_blocks cfg ~init:[] ~f:(fun label block acc ->
          if
            Label.Set.is_empty
              (Cfg.successor_labels ~normal:true ~exn:true block)
          then label :: acc
          else acc)
    in
    let number = Label.Tbl.create 32 in
    let postorder = ref [] in
    let counter = ref 0 in
    (* Iterative depth-first search over predecessors, to avoid deep recursion
       on large functions. *)
    let visited = Label.Tbl.create 32 in
    let rec visit stack =
      match stack with
      | [] -> ()
      | (label, children) :: rest -> (
        match children with
        | [] ->
          Label.Tbl.replace number label !counter;
          incr counter;
          postorder := label :: !postorder;
          visit rest
        | child :: children ->
          let stack = (label, children) :: rest in
          if Label.Tbl.mem visited child
          then visit stack
          else (
            Label.Tbl.replace visited child ();
            let block = Cfg.get_block_exn cfg child in
            visit ((child, Label.Set.elements block.predecessors) :: stack)))
    in
    List.iter
      (fun exit ->
        if not (Label.Tbl.mem visited exit)
        then (
          Label.Tbl.replace visited exit ();
          let block = Cfg.get_block_exn cfg exit in
          visit [exit, Label.Set.elements block.predecessors]))
      exits;
    let root = !counter in
    let ipdom = Array.make (root + 1) (-1) in
    ipdom.(root) <- root;
    let is_exit = Label.Tbl.create 32 in
    List.iter (fun exit -> Label.Tbl.replace is_exit exit ()) exits;
    let rec intersect b1 b2 =
      if b1 = b2
      then b1
      else if b1 < b2
      then intersect ipdom.(b1) b2
      else intersect b1 ipdom.(b2)
    in
    (* [!postorder] lists the blocks in reverse postorder, i.e. from the one
       closest to the root. *)
    let changed = ref true in
    while !changed do
      changed := false;
      List.iter
        (fun label ->
          let b = Label.Tbl.find number label in
          let candidates =
            Label.Set.fold
              (fun successor acc ->
                match Label.Tbl.find_opt number successor with
                | Some n when ipdom.(n) >= 0 -> n :: acc
                | Some _ | None -> acc)
              (successors cfg label)
              (if Label.Tbl.mem is_exit label then [root] else [])
          in
          match candidates with
          | [] -> ()
          | first :: rest ->
            let new_ipdom = List.fold_left intersect first rest in
            if ipdom.(b) <> new_ipdom
            then (
              ipdom.(b) <- new_ipdom;
              changed := true))
        !postorder
    done;
    { number; ipdom }

  let is_post_dominating t x y =
    match Label.Tbl.find_opt t.number x, Label.Tbl.find_opt t.number y with
    | None, _ | _, None -> Label.equal x y
    | Some x, Some y ->
      let root = Array.length t.ipdom - 1 in
      let rec walk n =
        if n = x then true else if n = root then false else walk t.ipdom.(n)
      in
      walk y
end

(* The pruning of LLVM's [shouldInstrumentBlock]: a block that dominates all of
   its successors is covered by them, and so is a block with several
   predecessors that post-dominates all of them. The block executed on function
   entry is always instrumented. *)
let should_instrument ~dominators ~post_dominators ~is_entry
    (block : Cfg.basic_block) =
  is_entry
  ||
  let successors = Cfg.successor_labels ~normal:true ~exn:true block in
  let is_full_dominator =
    (not (Label.Set.is_empty successors))
    && Label.Set.for_all
         (fun successor ->
           Cfg_dominators.is_dominating dominators block.start successor)
         successors
  in
  let is_full_post_dominator =
    (not (Label.Set.is_empty block.predecessors))
    && Label.Set.for_all
         (fun predecessor ->
           Post_dominators.is_post_dominating post_dominators block.start
             predecessor)
         block.predecessors
  in
  let has_single_predecessor = Label.Set.cardinal block.predecessors = 1 in
  (not is_full_dominator)
  && not (is_full_post_dominator && not has_single_predecessor)

(* A block containing nothing but an unreachable terminator is never executed:
   instrumenting it would only add a guard that is never hit. *)
let is_unreachable (block : Cfg.basic_block) =
  DLL.is_empty block.body
  &&
  match block.terminator.desc with
  | Invalid { label_after = None; _ } -> true
  | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Switch _ | Return | Raise _ | Tailcall_self _ | Tailcall_func _
  | Call_no_return _
  | Invalid { label_after = Some _; _ }
  | Call _ | Prim _ ->
    false

(* The number of instructions at the start of [block] that must precede its
   probe, or [None] if the block cannot be instrumented.

   The probe is a C call, which clobbers physical registers, so it can only be
   placed where no physical register is live. Blocks typically start by moving
   values out of physical registers: function parameters at the start of the
   function, results at the return point of a call, and the exception bucket at
   the start of a trap handler. *)
let insertion_point cfg_with_infos (block : Cfg.basic_block) =
  let no_physical_register_live (id : InstructionId.t) =
    let live = Cfg_with_infos.liveness_find cfg_with_infos id in
    not (Reg.Set.exists Reg.is_preassigned live.before)
  in
  let rec find n = function
    | [] ->
      if no_physical_register_live block.terminator.id then Some n else None
    | (instr : Cfg.basic Cfg.instruction) :: rest ->
      if no_physical_register_live instr.id then Some n else find (n + 1) rest
  in
  find 0 (DLL.to_list block.body)

let split_list l n =
  let rec loop n rev_prefix l =
    match l with
    | x :: rest when n > 0 -> loop (n - 1) (x :: rev_prefix) rest
    | _ -> List.rev rev_prefix, l
  in
  loop n [] l

(* Split [block] after its first [prefix_length] instructions into:

   {v block: prefix gate := load __sancov_should_track if gate goto call_block
   else goto rest_block rest_block: (rest of block, original terminator)
   call_block: __sanitizer_cov_trace_pc_guard(&guard); goto rest_block v}

   [block] keeps its label, so that branches to it, including self tail calls,
   are unaffected. [rest_block] directly follows [block] in the layout so that
   the common case falls through, and [call_block] is placed at the end. *)
let instrument_block cfg_with_layout (block : Cfg.basic_block) ~prefix_length =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let prefix, suffix = split_list (DLL.to_list block.body) prefix_length in
  let stack_offset, dbg =
    match suffix with
    | (instr : Cfg.basic Cfg.instruction) :: _ -> instr.stack_offset, instr.dbg
    | [] -> block.terminator.stack_offset, block.terminator.dbg
  in
  let make desc ~arg ~res =
    Cfg.make_instruction ~desc ~arg ~res ~dbg ~stack_offset
      ~id:(InstructionId.get_and_incr cfg.next_instruction_id)
      ()
  in
  let basic (desc : Cfg.basic) ~arg ~res = make desc ~arg ~res in
  let terminator (desc : Cfg.terminator) ~arg ~res = make desc ~arg ~res in
  let call_label = Cmm.new_label () in
  let rest_label = Cmm.new_label () in
  (* The successors of [block] become the successors of [rest_block]. This must
     be computed before the terminator of [block] is replaced. *)
  Label.Set.iter
    (fun successor ->
      let successor_block = Cfg.get_block_exn cfg successor in
      successor_block.predecessors
        <- successor_block.predecessors
           |> Label.Set.remove block.start
           |> Label.Set.add rest_label)
    (Cfg.successor_labels ~normal:true ~exn:true block);
  let rest_block : Cfg.basic_block =
    { start = rest_label;
      body = DLL.of_list suffix;
      terminator = block.terminator;
      predecessors = Label.Set.of_list [block.start; call_label];
      stack_offset;
      exn = block.exn;
      can_raise = block.can_raise;
      is_trap_handler = false;
      cold = block.cold
    }
  in
  let gate_address = Reg.create Cmm.Int in
  let gate = Reg.create Cmm.Int in
  DLL.clear block.body;
  DLL.add_list block.body prefix;
  DLL.add_end block.body
    (basic (Op (Const_symbol gate_symbol)) ~arg:[||] ~res:[| gate_address |]);
  DLL.add_end block.body
    (basic
       (Op
          (Load
             { memory_chunk = Word_int;
               addressing_mode = Arch.identity_addressing;
               mutability = Mutable;
               is_atomic = false
             }))
       ~arg:[| gate_address |] ~res:[| gate |]);
  block.terminator
    <- terminator
         (Truth_test { ifso = call_label; ifnot = rest_label })
         ~arg:[| gate |] ~res:[||];
  block.exn <- None;
  block.can_raise <- false;
  let guard = Reg.create Cmm.Int in
  let loc_args, stack_ofs, stack_align =
    Proc.loc_external_arguments [Cmm.XInt]
  in
  let loc_arg = loc_args.(0) in
  let call_block : Cfg.basic_block =
    { start = call_label;
      body =
        DLL.of_list
          [ basic (Op (Const_symbol (new_guard ()))) ~arg:[||] ~res:[| guard |];
            basic (Op Move) ~arg:[| guard |] ~res:loc_arg ];
      terminator =
        terminator
          (Prim
             { op =
                 External
                   { func_symbol = callback;
                     alloc = false;
                     effects = Arbitrary_effects;
                     ty_res = Cmm.typ_void;
                     ty_args = [Cmm.XInt];
                     stack_ofs;
                     stack_align
                   };
               label_after = rest_label
             })
          ~arg:loc_arg ~res:[||];
      predecessors = Label.Set.singleton block.start;
      stack_offset;
      exn = None;
      can_raise = false;
      is_trap_handler = false;
      cold = false
    }
  in
  Cfg_with_layout.add_block cfg_with_layout rest_block ~after:block.start;
  match DLL.last (Cfg_with_layout.layout cfg_with_layout) with
  | None -> Misc.fatal_error "Cfg_trace_pc_guard: empty layout"
  | Some last ->
    Cfg_with_layout.add_block cfg_with_layout call_block ~after:last

let run cfg_with_infos =
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  if split_critical_edges cfg_with_layout
  then (
    Cfg_with_infos.invalidate_liveness cfg_with_infos;
    Cfg_with_infos.invalidate_dominators_and_loop_infos cfg_with_infos);
  (* All insertion points are computed before the graph is modified, since they
     depend on liveness. Blocks are visited in layout order so that guards are
     numbered deterministically. *)
  let dominators = Cfg_with_infos.dominators cfg_with_infos in
  let post_dominators = Post_dominators.build cfg in
  (* The entry block usually cannot be instrumented, since the parameters are
     live in physical registers until they are moved to temporaries in its
     successor (which is also the target of self tail calls). *)
  let entry = Cfg.get_block_exn cfg cfg.entry_label in
  let entry_label =
    match insertion_point cfg_with_infos entry, entry.terminator.desc with
    | None, Always successor -> successor
    | Some _, _
    | ( None,
        ( Never | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
        | Switch _ | Return | Raise _ | Tailcall_self _ | Tailcall_func _
        | Call_no_return _ | Invalid _ | Call _ | Prim _ ) ) ->
      cfg.entry_label
  in
  let to_instrument =
    DLL.fold_right (Cfg_with_layout.layout cfg_with_layout) ~init:[]
      ~f:(fun label acc ->
        let block = Cfg.get_block_exn cfg label in
        if
          is_unreachable block
          || not
               (should_instrument ~dominators ~post_dominators
                  ~is_entry:(Label.equal label entry_label)
                  block)
        then acc
        else
          match insertion_point cfg_with_infos block with
          | None -> acc
          | Some prefix_length -> (block, prefix_length) :: acc)
  in
  match to_instrument with
  | [] -> cfg_with_infos
  | _ :: _ ->
    List.iter
      (fun (block, prefix_length) ->
        instrument_block cfg_with_layout block ~prefix_length)
      to_instrument;
    let cfg = { cfg with fun_contains_calls = true } in
    Cfg_with_infos.make (Cfg_with_layout.with_cfg cfg_with_layout cfg)

let emit_guards () =
  let count = !guard_count in
  if count > 0
  then (
    let module D = Asm_targets.Asm_directives in
    let module L = Asm_targets.Asm_label in
    D.switch_to_section Trace_pc_guards;
    D.align ~fill:Zero ~bytes:4;
    for n = 0 to count - 1 do
      let symbol =
        Asm_targets.Asm_symbol.create ~visibility:Local
          (guard_symbol n).sym_name
      in
      D.define_label (L.create_label_for_local_symbol Trace_pc_guards symbol);
      D.int32 0l
    done)
