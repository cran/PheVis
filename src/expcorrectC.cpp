#include <Rcpp.h>
#include <Rinternals.h>
using namespace Rcpp;

//' expcorrectC
//' @title expcorrectC
//' @description c++ function to compute exponential cumulation of information.
//'
//' @param mat A matrix where each column is a variable to be cumulated.
//' @param diffdate Number of days between each sojourn. NA for switch of patient and restart cumulation.
//' @param lambda A double to set the exponential cumulation.
//' 
//' @return A matrix corresponding to the mat argument with cumulated exponential decay
//' 
// [[Rcpp::export]]
NumericMatrix expcorrectC( NumericMatrix mat,
                           NumericVector diffdate,
                           double lambda) {
  int nrow = mat.nrow(), ncol = mat.ncol();
  NumericMatrix correctmat(nrow,ncol);
  
  correctmat(0, _) = mat(0, _);
  
  for (int i = 1; i < nrow; i++) {
    NumericVector vecvalues = mat(i, _), vecprevvalues = correctmat(i-1, _);
    
    if(NumericVector::is_na(diffdate(i))) {
      correctmat(i, _) = vecvalues;
    } else {
      correctmat(i, _) = vecvalues + vecprevvalues*std::exp(-diffdate(i)*lambda);
    }
  }
  
  return(correctmat);
}
