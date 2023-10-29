#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector classifying_metrics(NumericMatrix true_theta, NumericMatrix pred_theta, double threshold) {
  int TP = 0;
  int FP = 0;
  int TN = 0;
  int FN = 0;
  int n = true_theta.nrow();
  for (int i = 0; i < n; i++) {
    for (int j = i+1; j < n; j++) {
      if (true_theta(i, j) == 1 && pred_theta(i, j) > threshold) {
        TP += 1;
      } else if (true_theta(i, j) == 0 && pred_theta(i, j) > threshold) {
        FP += 1;
      } else if (true_theta(i, j) == 0 && pred_theta(i, j) <= threshold) {
        TN += 1;
      } else if (true_theta(i, j) == 1 && pred_theta(i, j) <= threshold) {
        FN += 1;
      }
    }
  }
  // Return named numeric vector
  NumericVector v(4);
    v[0] = TP;
    v[1] = FP;
    v[2] = TN;
    v[3] = FN;
    v.names() = CharacterVector::create("TP", "FP", "TN", "FN");
    return v;
}

/*** R
eval({
    true_theta <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)
    pred_theta <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1), nrow = 3, ncol = 3, byrow = TRUE)
    metrics <- classifying_metrics(true_theta, pred_theta, threshold = 0.0)
    stopifnot(metrics == list(TP = 1, FP = 2, TN = 0, FN = 0))
    metrics <- classifying_metrics(true_theta, true_theta, threshold = 0.0)
    stopifnot(metrics == list(TP = 1, FP = 0, TN = 2, FN = 0))
    pred_theta <- matrix(c(1, 0.5, 0.9, 0.5, 1, 0.3, 0.9, 0.3, 1.0), nrow = 3, ncol = 3, byrow = TRUE)
    metrics <- classifying_metrics(true_theta, pred_theta, threshold = 0.4)
    stopifnot(metrics == list(TP = 1, FP = 1, TN = 1, FN = 0))
})
*/
