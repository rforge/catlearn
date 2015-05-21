// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// alcovelp
NumericMatrix alcovelp(List st, NumericMatrix tr, std::string dec, bool humble, double absval);
RcppExport SEXP catlearn_alcovelp(SEXP stSEXP, SEXP trSEXP, SEXP decSEXP, SEXP humbleSEXP, SEXP absvalSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< List >::type st(stSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type tr(trSEXP);
    Rcpp::traits::input_parameter< std::string >::type dec(decSEXP);
    Rcpp::traits::input_parameter< bool >::type humble(humbleSEXP);
    Rcpp::traits::input_parameter< double >::type absval(absvalSEXP);
    __result = Rcpp::wrap(alcovelp(st, tr, dec, humble, absval));
    return __result;
END_RCPP
}
