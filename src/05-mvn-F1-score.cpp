#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double f1_score(NumericMatrix true_theta, NumericMatrix pred_theta) {
  int TP = 0;
  int FP = 0;
  int TN = 0;
  int FN = 0;
  int n = true_theta.nrow();
  for (int i = 0; i < n; i++) {
    for (int j = i+1; j < n; j++) {
      double true_value = true_theta(i, j);
      double pred_value = pred_theta(i, j);
      if (true_value == 1 && pred_value == 1) {
        TP++;
      } else if (true_value == 0 && pred_value == 1) {
        FP++;
      } else if (true_value == 0 && pred_value == 0) {
        TN++;
      } else if (true_value == 1 && pred_value == 0) {
        FN++;
      }
    }
  }
  // Return named numeric vector
  return 2.0 * TP / (2.0 * TP + FP + FN);
}