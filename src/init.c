#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

SEXP asym_dist(SEXP start, SEXP end);

static const R_CallMethodDef CallEntries[] = {
  {"asym_dist_c", (DL_FUNC) &asym_dist, 2},
  {NULL, NULL, 0}
};

void R_init_fawkes(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
