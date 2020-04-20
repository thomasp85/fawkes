#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

SEXP asym_dist(SEXP start, SEXP end) {
  if (Rf_length(start) != Rf_length(end)) {
    Rf_error("start and end must have the same length");
  }
  int n_points = Rf_length(start) / 2;
  SEXP dist = PROTECT(Rf_allocMatrix(REALSXP, n_points, n_points));
  double* dist_p = REAL(dist);

  double* start_p = REAL(start);
  double* end_p = REAL(end);

  double from_x, from_y, dist_x, dist_y;
  int ind;

  for (int i = 0; i < n_points; ++i) {
    from_x = start_p[i * 2];
    from_y = start_p[i * 2 + 1];
    for (int j = 0; j < n_points; ++j) {
      ind = n_points * i + j;
      if (j == i) {
        dist_p[ind] = 0;
        continue;
      }
      dist_x = end_p[j * 2] - from_x;
      dist_y = end_p[j * 2 + 1] - from_y;
      dist_p[ind] = sqrt(dist_x * dist_x + dist_y * dist_y);
    }
  }
  UNPROTECT(1);
  return dist;
}
