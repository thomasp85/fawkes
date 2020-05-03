#define R_NO_REMAP

#include <Rinternals.h>

static inline double dist(double x0, double y0, double x1, double y1) {
  double xd = x1 - x0;
  double yd = y1 - y0;
  return sqrt(xd*xd + yd*yd);
}
static inline void cut(double *x0, double *y0, double x1, double y1, double at) {
  double x_new = (x1 - *x0) * at + *x0;
  double y_new = (y1 - *y0) * at + *y0;
  *x0 = x_new;
  *y0 = y_new;
}

SEXP line_pattern(SEXP x, SEXP y, SEXP pattern, SEXP expansion) {
  SEXP lines = PROTECT(Rf_allocVector(VECSXP, 3));

  double* x_p = REAL(x);
  double* y_p = REAL(y);

  // Decode the linetype into an array
  double pat[8] = {.0, .0, .0, .0, .0, .0, .0, .0};
  int lty = INTEGER(pattern)[0];
  double lwd = REAL(expansion)[0];
  int n_pat = 0;
  for( ; n_pat < 8 && lty & 15 ; ) {
    pat[n_pat++] = (lty & 15) * lwd;
    lty = lty>>4;
    pat[n_pat++] = (lty & 15) * lwd;
    lty = lty>>4;
  }


  int cur_pat = 0;
  double target = pat[cur_pat % n_pat];
  double current_length = 0.0;
  int on = 1;
  int n = Rf_length(x);
  int i = 1;
  int new_id = 1;
  double x0 = x_p[0];
  double y0 = y_p[0];

  int cur_size = 10;
  int cur_index = 0;
  SEXP x_ret, y_ret, id_ret;
  PROTECT_INDEX pr_x, pr_y, pr_id;
  PROTECT_WITH_INDEX(x_ret = Rf_allocVector(REALSXP, cur_size), &pr_x);
  PROTECT_WITH_INDEX(y_ret = Rf_allocVector(REALSXP, cur_size), &pr_y);
  PROTECT_WITH_INDEX(id_ret = Rf_allocVector(INTSXP, cur_size), &pr_id);
  double* x_p_ret = REAL(x_ret);
  double* y_p_ret = REAL(y_ret);
  int* id_p_ret = INTEGER(id_ret);

  while (i < n) {
    double seg_length = dist(x0, y0, x_p[i], y_p[i]);
    int next = 0;
    if (current_length + seg_length < target) {
      x0 = x_p[i];
      y0 = y_p[i];
      current_length += seg_length;
      i++;
    } else {
      cut(&x0, &y0, x_p[i], y_p[i], (target - current_length) / seg_length);
      next = 1;
    }

    if (next) {
      cur_pat++;
      target = pat[cur_pat % n_pat];
      current_length = 0;
      on = !on;
      if (on) {
        new_id++;
      }
    }

    if (on || next) {
      if (cur_index >= cur_size) {
        cur_size *= 1.5;
        REPROTECT(x_ret = Rf_lengthgets(x_ret, cur_size), pr_x);
        x_p_ret = REAL(x_ret);
        REPROTECT(y_ret = Rf_lengthgets(y_ret, cur_size), pr_y);
        y_p_ret = REAL(y_ret);
        REPROTECT(id_ret = Rf_lengthgets(id_ret, cur_size), pr_id);
        id_p_ret = INTEGER(id_ret);
      }
      x_p_ret[cur_index] = x0;
      y_p_ret[cur_index] = y0;
      id_p_ret[cur_index] = new_id;
      cur_index++;
    }

    next = 0;
  }

  SET_VECTOR_ELT(lines, 0, Rf_lengthgets(x_ret, cur_index));
  SET_VECTOR_ELT(lines, 1, Rf_lengthgets(y_ret, cur_index));
  SET_VECTOR_ELT(lines, 2, Rf_lengthgets(id_ret, cur_index));

  SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, Rf_mkChar("x"));
  SET_STRING_ELT(names, 1, Rf_mkChar("y"));
  SET_STRING_ELT(names, 2, Rf_mkChar("id"));
  Rf_setAttrib(lines, Rf_install("names"), names);

  UNPROTECT(5);
  return lines;
}
