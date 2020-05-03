#define R_NO_REMAP
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

SEXP asym_dist(SEXP start, SEXP end);
SEXP line_pattern(SEXP x, SEXP y, SEXP pattern, SEXP expansion);

static const R_CallMethodDef CallEntries[] = {
  {"asym_dist_c", (DL_FUNC) &asym_dist, 2},
  {"line_pattern_c", (DL_FUNC) &line_pattern, 4},
  {NULL, NULL, 0}
};

void R_init_fawkes(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
