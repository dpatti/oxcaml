open Utils256

(* The narrowing conversions [vcvtpd2dq], [vcvttpd2dq] and [vcvtpd2ps] always
   write an XMM register but accept either a 128-bit or a 256-bit source. With
   a memory source the assembler cannot tell the two forms apart, so the
   mnemonic needs an explicit x/y suffix.

   Each test below builds the source vector [v], then makes a C call (the
   [expect] computation) while [v] is live, which spills [v] to the stack. The
   register allocator then uses the stack slot directly as the conversion's
   source operand. (A fresh [v] is needed per conversion: only the first use of
   a spilled value is rewritten to a stack operand; later uses get reloads.) *)

let int32x4 a b c d =
  let i0 = Int64.of_int32 a |> Int64.logand 0xffffffffL in
  let i1 = Int64.of_int32 b |> Int64.logand 0xffffffffL in
  let i2 = Int64.of_int32 c |> Int64.logand 0xffffffffL in
  let i3 = Int64.of_int32 d |> Int64.logand 0xffffffffL in
  int32x4_of_int64s
    (Int64.logor (Int64.shift_left i1 32) i0)
    (Int64.logor (Int64.shift_left i3 32) i2)

let () =
  (failmsg := fun () -> Printf.printf "cvt_int32x4");
  let v = Float64.to_float64x2 1.5 (-2.5) in
  let expect = int32x4 2l (-2l) 0l 0l in
  eq_int32x4 ~result:(Builtins.Float64x2.cvt_int32x4 v) ~expect

let () =
  (failmsg := fun () -> Printf.printf "cvtt_int32x4");
  let v = Float64.to_float64x2 1.5 (-2.5) in
  let expect = int32x4 1l (-2l) 0l 0l in
  eq_int32x4 ~result:(Builtins.Float64x2.cvtt_int32x4 v) ~expect

let () =
  (failmsg := fun () -> Printf.printf "cvt_float32x4");
  let v = Float64.to_float64x2 1.5 (-2.5) in
  let expect = Float32.to_float32x4' 1.5s (-2.5s) 0.s 0.s in
  eq_float32x4 ~result:(Builtins.Float64x2.cvt_float32x4 v) ~expect

let () =
  (failmsg := fun () -> Printf.printf "cvt_int32x4 (256)");
  let v = Float64.to_float64x4 1.5 (-2.5) 3.5 (-4.5) in
  let expect = int32x4 2l (-2l) 4l (-4l) in
  eq_int32x4 ~result:(Builtins.Float64x4.cvt_int32x4 v) ~expect

let () =
  (failmsg := fun () -> Printf.printf "cvtt_int32x4 (256)");
  let v = Float64.to_float64x4 1.5 (-2.5) 3.5 (-4.5) in
  let expect = int32x4 1l (-2l) 3l (-4l) in
  eq_int32x4 ~result:(Builtins.Float64x4.cvtt_int32x4 v) ~expect

let () =
  (failmsg := fun () -> Printf.printf "cvt_float32x4 (256)");
  let v = Float64.to_float64x4 1.5 (-2.5) 3.5 (-4.5) in
  let expect = Float32.to_float32x4' 1.5s (-2.5s) 3.5s (-4.5s) in
  eq_float32x4 ~result:(Builtins.Float64x4.cvt_float32x4 v) ~expect
