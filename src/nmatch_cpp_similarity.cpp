#include <Rcpp.h>
#include <string>
#include <vector>
#include <algorithm>
#include "nmatch_utils.h"

using namespace Rcpp;

// Vectorized version with TF-IDF weighted evidence score
// E = sum( sim(x_i, y_i) * (IDF_x_i + IDF_y_i) / 2 )
// [[Rcpp::export]]
DataFrame nmatch_cpp_similarity(const CharacterVector& x,
                                const CharacterVector& y,
                                int nchar_min = 2,
                                const CharacterVector& token_x = CharacterVector(),
                                const NumericVector& idf_x = NumericVector(),
                                double default_idf_x = 1.0,
                                const CharacterVector& token_y = CharacterVector(),
                                const NumericVector& idf_y = NumericVector(),
                                double default_idf_y = 1.0) {

  int n = x.size();

  if (n != y.size()) {
    Rcpp::stop("x and y must have the same length");
  }

  if (token_x.size() != idf_x.size()) {
    Rcpp::stop("token_x and idf_x must have the same length");
  }

  if (token_y.size() != idf_y.size()) {
    Rcpp::stop("token_y and idf_y must have the same length");
  }

  // Output columns
  IntegerVector col_k_x(n), col_k_y(n), col_k_align(n);
  NumericVector col_evidence(n), col_similarity(n);

  // Pre-convert all strings
  std::vector<std::string> x_strings(n);
  std::vector<std::string> y_strings(n);
  for (int i = 0; i < n; i++) {
    x_strings[i] = Rcpp::as<std::string>(x[i]);
    y_strings[i] = Rcpp::as<std::string>(y[i]);
  }

  // Create IDF hash maps
  std::unordered_map<std::string, double> idf_map_x;
  std::unordered_map<std::string, double> idf_map_y;

  for (int k = 0; k < token_x.size(); k++) {
    idf_map_x[Rcpp::as<std::string>(token_x[k])] = idf_x[k];
  }

  for (int k = 0; k < token_y.size(); k++) {
    idf_map_y[Rcpp::as<std::string>(token_y[k])] = idf_y[k];
  }

  // Process each pair
  for (int i = 0; i < n; i++) {

    // Tokenize strings
    std::vector<std::string> tokens_x = tokenize_name(x_strings[i], nchar_min);
    std::vector<std::string> tokens_y = tokenize_name(y_strings[i], nchar_min);

    int k_x = tokens_x.size();
    int k_y = tokens_y.size();
    int min_tokens = std::min(k_x, k_y);
    double max_evidence = -1.0;
    double best_similarity = 0.0;

    if (tokens_x.empty() || tokens_y.empty()) {
      max_evidence = 0.0;
      best_similarity = 0.0;
    } else {

      // Choose smaller set to permute
      if (k_x <= k_y) {
        std::vector<int> indices(k_y);
        std::iota(indices.begin(), indices.end(), 0);

        do {
          double evidence = 0.0;
          double similarity = 0.0;

          for (int j = 0; j < min_tokens; j++) {
            int dist = osa_distance(tokens_x[j], tokens_y[indices[j]]);
            int max_len = std::max((int)tokens_x[j].length(), (int)tokens_y[indices[j]].length());
            double sim = 1.0 - (double)dist / max_len;

            auto it_x = idf_map_x.find(tokens_x[j]);
            double w_x = (it_x != idf_map_x.end()) ? it_x->second : default_idf_x;

            auto it_y = idf_map_y.find(tokens_y[indices[j]]);
            double w_y = (it_y != idf_map_y.end()) ? it_y->second : default_idf_y;

            evidence += sim * sim * (w_x + w_y) / 2.0;
            similarity += sim;
          }

          if (evidence > max_evidence) {
            max_evidence = evidence;
            best_similarity = similarity;
          }

        } while (std::next_permutation(indices.begin(), indices.end()));

      } else {
        std::vector<int> indices(k_x);
        std::iota(indices.begin(), indices.end(), 0);

        do {
          double evidence = 0.0;
          double similarity = 0.0;

          for (int j = 0; j < min_tokens; j++) {
            int dist = osa_distance(tokens_x[indices[j]], tokens_y[j]);
            int max_len = std::max((int)tokens_x[indices[j]].length(), (int)tokens_y[j].length());
            double sim = 1.0 - (double)dist / max_len;

            auto it_x = idf_map_x.find(tokens_x[indices[j]]);
            double w_x = (it_x != idf_map_x.end()) ? it_x->second : default_idf_x;

            auto it_y = idf_map_y.find(tokens_y[j]);
            double w_y = (it_y != idf_map_y.end()) ? it_y->second : default_idf_y;

            evidence += sim * sim * (w_x + w_y) / 2.0;
            similarity += sim;
          }

          if (evidence > max_evidence) {
            max_evidence = evidence;
            best_similarity = similarity;
          }

        } while (std::next_permutation(indices.begin(), indices.end()));
      }
    }

    col_k_x[i]         = k_x;
    col_k_y[i]         = k_y;
    col_k_align[i]     = min_tokens;
    col_evidence[i]    = max_evidence;
    col_similarity[i]  = best_similarity;
  }

  return DataFrame::create(
    Named("k_x")         = col_k_x,
    Named("k_y")         = col_k_y,
    Named("k_align")     = col_k_align,
    Named("similarity")  = col_similarity,
    Named("evidence")    = col_evidence
  );
}
