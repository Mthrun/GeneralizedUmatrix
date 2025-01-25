#include <Rcpp.h>
#include <RcppParallel.h>

using namespace RcppParallel;
using namespace Rcpp;
using namespace std;

// [[Rcpp::depends(RcppParallel)]]
struct Delta3DWeightsC : public Worker {                                        // Worker for parallelization
  const RVector<double> DataSample;
  const RMatrix<double> OutputDistances;
  const int Lines, Columns, Weights, Radius;
  const double Factor;
  RVector<double> esom;
  // initialize from Rcpp input and output matrixes (the RMatrix class
  // can be automatically converted to form the Rcpp matrix type)
  Delta3DWeightsC(const NumericVector DataSample, NumericMatrix OutputDistances,
                  const int Lines, const int Columns, const int Weights, const int Radius,
                  const double Factor, NumericVector esom):
    DataSample(DataSample), OutputDistances(OutputDistances), Lines(Lines), Columns(Columns),
    Weights(Weights), Radius(Radius), Factor(Factor), esom(esom) {}
  // function call operator that work for the specified range (begin/end)    esomwts = esomwts - (neighmatrix * inputdiff);
  void operator()(std::size_t begin, std::size_t end) {
    for(std::size_t k = begin; k < end; k++){
      for(int j = 0; j < Columns; j++){
        
        double neighborValue = 1 - (pow(OutputDistances(k,j),2) / (3.14159265*pow(Radius,2)));
        
        if(neighborValue < 0){
          neighborValue = 0;
        }
        
        for(int i = 0; i < Weights; i++){
          int tmpIdx1    = i * Columns * Lines + j * Lines + k;
          double tmpRes0 = esom[tmpIdx1];
          //double tmpRes1 = Factor * (neighborValue * (tmpRes0 - DataSample[i]));
          //double tmpRes2 = tmpRes0 - Factor * (neighborValue * (tmpRes0 - DataSample[i]));
          esom[tmpIdx1]  = tmpRes0 - (Factor * (neighborValue * (tmpRes0 - DataSample[i])));
        }
        
      }
    }
  }
};

// [[Rcpp::depends(RcppParallel)]]
NumericVector RcppParallel_UpdateWeights(NumericVector esom, NumericVector DataSample, NumericMatrix OutputDistances,
                                         int Lines, int Columns, int Weights, int Radius, double Factor){
  Delta3DWeightsC delta3DWeightsC(DataSample, OutputDistances, Lines, Columns, Weights, Radius, Factor, esom); // create the worker
  parallelFor(0, Lines, delta3DWeightsC);                           // call it with parallelFor
  return esom;
}

// [[Rcpp::depends(RcppParallel)]]
struct ToroidDistance : public Worker {                                         // Worker for parallelization
  const RVector<double> aux;                                                    // inputs to read from
  const RMatrix<double> kmatrix, mmatrix, bm1, bm2;
  const int Lines, Columns, LCS;
  RMatrix<double> OutputDistances;                                              // output to write to
  // initialize from Rcpp input and output matrixes (the RMatrix class
  // can be automatically converted to form the Rcpp matrix type)
  ToroidDistance(const NumericVector aux, const NumericMatrix kmatrix, const NumericMatrix mmatrix,
                 const NumericMatrix bm1, const NumericMatrix bm2,
                 const int Lines, const int Columns, const int LCS, NumericMatrix OutputDistances):
    aux(aux), kmatrix(kmatrix), mmatrix(mmatrix), bm1(bm1), bm2(bm2),
    Lines(Lines), Columns(Columns), LCS(LCS), OutputDistances(OutputDistances) {}
  void operator()(std::size_t begin, std::size_t end){ // function call operator that work for the specified range (begin/end)
    for(std::size_t i = begin; i < end; i++){
      for(int j = 0; j < Columns; j++){
        //for(int j = 0; j < i; j++){
        int auxIdx1          = j*Lines + i;
        int auxIdx2          = LCS + j*Lines + i;
        double FirstPart     = pow(kmatrix(i,j) - abs(2 * abs(aux[auxIdx1] - bm1(i,j)) - kmatrix(i,j)), 2);
        double SecondPart    = pow(mmatrix(i,j) - abs(2 * abs(aux[auxIdx2] - bm2(i,j)) - mmatrix(i,j)), 2);
        OutputDistances(i,j) = 0.5*sqrt(FirstPart + SecondPart);
        // Symmetrie ist nicht ausnutzbar: keine quadratische Matrix!
        //OutputDistances(j,i) = OutputDistances(i,j);
      }
    }
  }
};

// [[Rcpp::depends(RcppParallel)]]
NumericMatrix RcppParallel_ToroidDistance(NumericVector aux, NumericMatrix kmatrix, NumericMatrix mmatrix,
                                          NumericMatrix bm1, NumericMatrix bm2,
                                          int Lines, int Columns, int LCS, NumericMatrix OutputDistances){
  ToroidDistance toroidDistance(aux, kmatrix, mmatrix, bm1, bm2, Lines, Columns,// create the worker
                                LCS, OutputDistances);
  parallelFor(0, Lines, toroidDistance);                                        // call it with parallelFor
  //parallelFor(1, Lines, toroidDistance);                                        // call it with parallelFor
  return OutputDistances;
}

// [[Rcpp::depends(RcppParallel)]]
struct NonToroidDistance : public Worker {                                      // Worker for parallelization
  const RVector<double> aux;                                                    // inputs to read from
  const RMatrix<double> bm1, bm2;
  const int Lines, Columns, LCS;
  RMatrix<double> OutputDistances;                                              // output to write to
  // initialize from Rcpp input and output matrixes (the RMatrix class
  // can be automatically converted to form the Rcpp matrix type)
  NonToroidDistance(const NumericVector aux, const NumericMatrix bm1, const NumericMatrix bm2,
                    const int Lines, const int Columns, const int LCS, NumericMatrix OutputDistances):
    aux(aux), bm1(bm1), bm2(bm2), Lines(Lines), Columns(Columns), LCS(LCS), OutputDistances(OutputDistances) {}
  void operator()(std::size_t begin, std::size_t end) { // function call operator that work for the specified range (begin/end)
    for(std::size_t i = begin; i < end; i++){
      for(int j = 0; j < Columns; j++){
        //for(int j = 0; j < i; j++){
        // sqrt(pow(aux.slice(0)-bm1,2) + pow(aux.slice(1)-bm2,2));
        int auxIdx1 = j*Lines + i;
        int auxIdx2 = LCS + j*Lines + i;
        OutputDistances(i,j) = sqrt(pow(aux[auxIdx1] - bm1(i,j), 2) + pow(aux[auxIdx2] - bm2(i,j), 2));
        // Symmetrie ist nicht ausnutzbar: keine quadratische Matrix!
        //OutputDistances(j,i) = OutputDistances(i,j);
      }
    }
  }
};

// [[Rcpp::depends(RcppParallel)]]
NumericMatrix RcppParallel_NonToroidDistance(NumericVector aux, NumericMatrix bm1, NumericMatrix bm2,
                                             int Lines, int Columns, int LCS, NumericMatrix OutputDistances){
  NonToroidDistance nonToroidDistance(aux, bm1, bm2, Lines, Columns, LCS, OutputDistances); // create the worker
  parallelFor(0, Lines, nonToroidDistance);                                                 // call it with parallelFor
  //parallelFor(1, Lines, nonToroidDistance);                                                 // call it with parallelFor
  return OutputDistances;
}

// [[Rcpp::export]]
NumericVector trainstepC2(NumericVector esomwts, // NumericVector
                          NumericVector aux,
                          NumericMatrix DataSampled, NumericMatrix BMUsampled,
                          double Lines, double Columns, double Weights, double Radius,
                          bool toroid, int NoCases){

  NumericVector DataSample(DataSampled.rows());
  NumericVector bmpos(BMUsampled.rows());
  
  NumericMatrix OutputDistances(Lines, Columns); // Careful: non quadratic matrix
  NumericMatrix neighmatrix(Lines, Columns);
  NumericMatrix kmatrix(Lines, Columns);
  NumericMatrix mmatrix(Lines, Columns);
  NumericMatrix bm1(Lines, Columns);
  NumericMatrix bm2(Lines, Columns);
  
  double Factor = 1;
  
  if((NoCases >= 2501) && (Radius <= 16)){
    if(Radius <= 16 && Radius > 8){
      Factor = 0.75;
    }else if (Radius <= 8 && Radius > 4){
      Factor = 0.5;
    }else{
      Factor = 0.1;
    }
  }
  
  int LCS   = Lines * Columns;
  
  for (int i = 0; i < LCS; i++) { // Fill with value
    kmatrix[i] = Lines-1;
  }
  for (int i = 0; i < LCS; i++) { // Fill with value
    mmatrix[i] = Columns-1;
  }
  
  int NumberOfDataSamples = DataSampled.nrow();
  
  for(int p = 0; p < NumberOfDataSamples; p++){
    
    DataSample = DataSampled.row(p);
    bmpos      = BMUsampled.row(p);
    
    for (int i = 0; i < LCS; i++) { // Fill with value
      bm1[i] = bmpos(0);
    }
    for (int i = 0; i < LCS; i++) { // Fill with value
      bm2[i] = bmpos(1);
    }
    
    if(toroid){
      OutputDistances = RcppParallel_ToroidDistance(aux, kmatrix, mmatrix, bm1, bm2, Lines, Columns, LCS, OutputDistances);
    }else{
      OutputDistances = RcppParallel_NonToroidDistance(aux, bm1, bm2, Lines, Columns, LCS, OutputDistances);
    }
    esomwts = RcppParallel_UpdateWeights(esomwts, DataSample, OutputDistances, Lines, Columns, Weights, Radius, Factor);
  }
  
  return(esomwts);
}
