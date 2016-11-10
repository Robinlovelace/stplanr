// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// coord_matches
List coord_matches(SEXP sldf);
RcppExport SEXP stplanr_coord_matches(SEXP sldfSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type sldf(sldfSEXP);
    rcpp_result_gen = Rcpp::wrap(coord_matches(sldf));
    return rcpp_result_gen;
END_RCPP
}
// join_spatiallines_coords
arma::mat join_spatiallines_coords(SEXP sldf, double startx, double starty);
RcppExport SEXP stplanr_join_spatiallines_coords(SEXP sldfSEXP, SEXP startxSEXP, SEXP startySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type sldf(sldfSEXP);
    Rcpp::traits::input_parameter< double >::type startx(startxSEXP);
    Rcpp::traits::input_parameter< double >::type starty(startySEXP);
    rcpp_result_gen = Rcpp::wrap(join_spatiallines_coords(sldf, startx, starty));
    return rcpp_result_gen;
END_RCPP
}