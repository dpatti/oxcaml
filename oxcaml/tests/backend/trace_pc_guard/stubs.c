/* Coverage callbacks for [-trace-pc-guard], playing the part of the runtime
   that would normally be provided (e.g. by Antithesis). */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <caml/mlvalues.h>

int64_t __sancov_should_track = 0;

static uint32_t *guards_start = NULL;
static uint32_t *guards_stop = NULL;
static int init_calls = 0;

void __sanitizer_cov_trace_pc_guard_init(uint32_t *start, uint32_t *stop)
{
  init_calls++;
  guards_start = start;
  guards_stop = stop;
  for (uint32_t *g = start; g < stop; g++) *g = 0;
}

void __sanitizer_cov_trace_pc_guard(uint32_t *guard)
{
  if (guard < guards_start || guard >= guards_stop) {
    fprintf(stderr, "guard %p outside of [%p, %p)\n", (void *)guard,
            (void *)guards_start, (void *)guards_stop);
    abort();
  }
  *guard = 1;
}

extern uint32_t __start_ocaml_trace_pc_guards[] __attribute__((weak));
extern uint32_t __stop_ocaml_trace_pc_guards[] __attribute__((weak));

__attribute__((constructor)) static void register_guards(void)
{
  if (__start_ocaml_trace_pc_guards != __stop_ocaml_trace_pc_guards)
    __sanitizer_cov_trace_pc_guard_init(__start_ocaml_trace_pc_guards,
                                        __stop_ocaml_trace_pc_guards);
}

value caml_tpcg_set_gate(value enabled)
{
  __sancov_should_track = Bool_val(enabled);
  return Val_unit;
}

value caml_tpcg_reset(value unit)
{
  (void)unit;
  for (uint32_t *g = guards_start; g < guards_stop; g++) *g = 0;
  return Val_unit;
}

value caml_tpcg_num_guards(value unit)
{
  (void)unit;
  return Val_long(guards_stop - guards_start);
}

value caml_tpcg_num_hit(value unit)
{
  (void)unit;
  long n = 0;
  for (uint32_t *g = guards_start; g < guards_stop; g++) n += *g != 0;
  return Val_long(n);
}

value caml_tpcg_init_calls(value unit)
{
  (void)unit;
  return Val_int(init_calls);
}
