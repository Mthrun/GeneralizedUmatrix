// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// addRowWiseC
NumericMatrix addRowWiseC(NumericMatrix WeightVectors, NumericVector DataPoint);
RcppExport SEXP GeneralizedUmatrix_addRowWiseC(SEXP WeightVectorsSEXP, SEXP DataPointSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type WeightVectors(WeightVectorsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type DataPoint(DataPointSEXP);
    rcpp_result_gen = Rcpp::wrap(addRowWiseC(WeightVectors, DataPoint));
    return rcpp_result_gen;
END_RCPP
}
// Delta3DWeightsC
arma::cube Delta3DWeightsC(Rcpp::NumericVector vx, Rcpp::NumericVector Datasample);
RcppExport SEXP GeneralizedUmatrix_Delta3DWeightsC(SEXP vxSEXP, SEXP DatasampleSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type vx(vxSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type Datasample(DatasampleSEXP);
    rcpp_result_gen = Rcpp::wrap(Delta3DWeightsC(vx, Datasample));
    return rcpp_result_gen;
END_RCPP
}
// trainstepC
arma::cube trainstepC(Rcpp::NumericVector vx, Rcpp::NumericVector vy, Rcpp::NumericMatrix DataSampled, Rcpp::NumericMatrix BMUsampled, double Lines, double Columns, double Radius, bool toroid);
RcppExport SEXP GeneralizedUmatrix_trainstepC(SEXP vxSEXP, SEXP vySEXP, SEXP DataSampledSEXP, SEXP BMUsampledSEXP, SEXP LinesSEXP, SEXP ColumnsSEXP, SEXP RadiusSEXP, SEXP toroidSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type vx(vxSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type vy(vySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type DataSampled(DataSampledSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type BMUsampled(BMUsampledSEXP);
    Rcpp::traits::input_parameter< double >::type Lines(LinesSEXP);
    Rcpp::traits::input_parameter< double >::type Columns(ColumnsSEXP);
    Rcpp::traits::input_parameter< double >::type Radius(RadiusSEXP);
    Rcpp::traits::input_parameter< bool >::type toroid(toroidSEXP);
    rcpp_result_gen = Rcpp::wrap(trainstepC(vx, vy, DataSampled, BMUsampled, Lines, Columns, Radius, toroid));
    return rcpp_result_gen;
END_RCPP
}
