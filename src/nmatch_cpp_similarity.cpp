#include <Rcpp.h>
#include <string>
#include <vector>
#include <algorithm>
#include "nmatch_utils.h"

using namespace Rcpp;

// Vectorized version with token frequency lookup
// [[Rcpp::export]]
DataFrame nmatch_cpp_similarity(const CharacterVector& x,
                                const CharacterVector& y,
                                int nchar_min = 2,
                                const CharacterVector& token = CharacterVector(),
                                const IntegerVector& token_freq = IntegerVector()) {

  int n = x.size();

  if (n != y.size()) {
    Rcpp::stop("x and y must have the same length");
  }

  if (token.size() != token_freq.size()) {
    Rcpp::stop("token and token_freq must have the same length");
  }

  // Output columns
  IntegerVector col_k_x(n), col_k_y(n), col_k_align(n);
  NumericVector col_sim_total(n);
  IntegerVector col_freq1(n, NA_INTEGER), col_freq2(n, NA_INTEGER), col_freq3(n, NA_INTEGER);

  // Pre-convert all strings
  std::vector<std::string> x_strings(n);
  std::vector<std::string> y_strings(n);
  for (int i = 0; i < n; i++) {
    x_strings[i] = Rcpp::as<std::string>(x[i]);
    y_strings[i] = Rcpp::as<std::string>(y[i]);
  }

  // create hash map
  std::unordered_map<std::string, int> token_map;
  bool use_frequency_lookup = (token.size() > 0);

  if (use_frequency_lookup) {
    for (int lookup_idx = 0; lookup_idx < token.size(); lookup_idx++) {
      token_map[Rcpp::as<std::string>(token[lookup_idx])] = token_freq[lookup_idx];
    }
  }

  // Process each pair
  for (int i = 0; i < n; i++) {

    // tokenize strings
    std::vector<std::string> tokens_x = tokenize_name(x_strings[i], nchar_min);
    std::vector<std::string> tokens_y = tokenize_name(y_strings[i], nchar_min);

    int k_x = tokens_x.size();
    int k_y = tokens_y.size();
    int min_tokens = std::min(k_x, k_y);
    double max_similarity = -1.0;

    // Variables to store best matching tokens for frequency lookup
    std::vector<std::string> best_tokens_x;
    std::vector<std::string> best_tokens_y;

    if (tokens_x.empty() || tokens_y.empty()) {
      max_similarity = 0.0;
      // No tokens to get frequencies from
    } else {

      // Choose smaller set to permute
      if (k_x <= k_y) {
        std::vector<int> indices(k_y);
        std::iota(indices.begin(), indices.end(), 0);

        do {
          double similarity = 0.0;

          for (int j = 0; j < min_tokens; j++) {
            int dist = osa_distance(tokens_x[j], tokens_y[indices[j]]);
            int max_len = std::max((int)tokens_x[j].length(), (int)tokens_y[indices[j]].length());
            similarity += 1.0 - (double)dist / max_len;
          }

          if (similarity > max_similarity) {
            max_similarity = similarity;
            best_tokens_x.clear();
            best_tokens_y.clear();
            for (int j = 0; j < min_tokens; j++) {
              best_tokens_x.push_back(tokens_x[j]);
              best_tokens_y.push_back(tokens_y[indices[j]]);
            }
          }

        } while (std::next_permutation(indices.begin(), indices.end()));

      } else {
        std::vector<int> indices(k_x);
        std::iota(indices.begin(), indices.end(), 0);

        do {
          double similarity = 0.0;

          for (int j = 0; j < min_tokens; j++) {
            int dist = osa_distance(tokens_x[indices[j]], tokens_y[j]);
            int max_len = std::max((int)tokens_x[indices[j]].length(), (int)tokens_y[j].length());
            similarity += 1.0 - (double)dist / max_len;
          }

          if (similarity > max_similarity) {
            max_similarity = similarity;
            best_tokens_x.clear();
            best_tokens_y.clear();
            for (int j = 0; j < min_tokens; j++) {
              best_tokens_x.push_back(tokens_x[indices[j]]);
              best_tokens_y.push_back(tokens_y[j]);
            }
          }

        } while (std::next_permutation(indices.begin(), indices.end()));
      }
    }

    // Look up frequencies for best matching token pairs (up to 3)
    if (use_frequency_lookup && !best_tokens_x.empty()) {
      IntegerVector* freq_cols[3] = { &col_freq1, &col_freq2, &col_freq3 };

      for (int pair_idx = 0; pair_idx < std::min(3, (int)best_tokens_x.size()); pair_idx++) {
        auto it_x = token_map.find(best_tokens_x[pair_idx]);
        auto it_y = token_map.find(best_tokens_y[pair_idx]);

        if (it_x != token_map.end() && it_y != token_map.end()) {
          (*freq_cols[pair_idx])[i] = it_x->second + it_y->second;
        }
        // else remains NA_INTEGER (already initialised)
      }
    }

    col_k_x[i]       = k_x;
    col_k_y[i]       = k_y;
    col_k_align[i]   = min_tokens;
    col_sim_total[i] = max_similarity;
  }

  return DataFrame::create(
    Named("k_x")       = col_k_x,
    Named("k_y")       = col_k_y,
    Named("k_align")   = col_k_align,
    Named("sim_total") = col_sim_total,
    Named("freq1")     = col_freq1,
    Named("freq2")     = col_freq2,
    Named("freq3")     = col_freq3
  );
}

