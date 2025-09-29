#include <Rcpp.h>
#include <string>
#include <vector>
#include <algorithm>
#include <regex>

using namespace Rcpp;

// Tokenize a name string
// [[Rcpp::export]]
std::vector<std::string> tokenize_name(const std::string& name, int nchar_min = 2) {
  std::vector<std::string> tokens;
  std::regex word_regex("[a-zA-Z]+");
  std::sregex_iterator iter(name.begin(), name.end(), word_regex);
  std::sregex_iterator end;

  for (; iter != end; ++iter) {
    std::string token = iter->str();
    if (!token.empty() && token.length() >= nchar_min) {
      tokens.push_back(token);
    }
  }

  return tokens;
}


// More memory-efficient Levenshtein distance using only two rows
int levenshtein_distance(const std::string& s1, const std::string& s2) {
  int m = s1.length();
  int n = s2.length();

  if (m == 0) return n;
  if (n == 0) return m;

  // Use only two rows instead of full matrix
  std::vector<int> prev_row(n + 1);
  std::vector<int> curr_row(n + 1);

  // Initialize first row
  for (int j = 0; j <= n; j++) {
    prev_row[j] = j;
  }

  for (int i = 1; i <= m; i++) {
    curr_row[0] = i;

    for (int j = 1; j <= n; j++) {
      if (s1[i-1] == s2[j-1]) {
        curr_row[j] = prev_row[j-1];
      } else {
        curr_row[j] = 1 + std::min({prev_row[j], curr_row[j-1], prev_row[j-1]});
      }
    }

    // Swap rows
    prev_row.swap(curr_row);
  }

  return prev_row[n];
}


// Vectorized version
// [[Rcpp::export]]
IntegerMatrix nmatch_cpp(const CharacterVector& x, const CharacterVector& y, int nchar_min = 2) {
  int n = x.size();

  // Check that vectors x and y have same length
  if (n != y.size()) {
    Rcpp::stop("x and y must have the same length");
  }

  // Create output matrix with 4 columns: k_x, k_y, k_align, min_distance
  IntegerMatrix result(n, 4);
  colnames(result) = CharacterVector::create("k_x", "k_y", "k_align", "min_dist");

  // Process each pair
  for (int i = 0; i < n; i++) {
    std::string x_str = Rcpp::as<std::string>(x[i]);
    std::string y_str = Rcpp::as<std::string>(y[i]);

    // tokenize strings
    std::vector<std::string> tokens_x = tokenize_name(x_str, nchar_min);
    std::vector<std::string> tokens_y = tokenize_name(y_str, nchar_min);

    int k_x = tokens_x.size();
    int k_y = tokens_y.size();
    int min_tokens = std::min(k_x, k_y);
    int min_distance;

    if (tokens_x.empty() || tokens_y.empty()) {
      // min_distance = std::max(k_x, k_y) * 10;
      min_distance = 9999; // TODO: revisit
    } else {
      min_distance = INT_MAX;

      // Choose smaller set to permute
      if (k_x <= k_y) {
        std::vector<int> indices(k_y);
        std::iota(indices.begin(), indices.end(), 0);

        do {
          int distance = 0;
          for (int j = 0; j < min_tokens && distance < min_distance; j++) {
            distance += levenshtein_distance(tokens_x[j], tokens_y[indices[j]]);
          }

          if (distance < min_distance) {
            min_distance = distance;
            if (min_distance == 0) break;
          }

        } while (std::next_permutation(indices.begin(), indices.end()));

      } else {
        std::vector<int> indices(k_x);
        std::iota(indices.begin(), indices.end(), 0);

        do {
          int distance = 0;
          for (int j = 0; j < min_tokens && distance < min_distance; j++) {
            distance += levenshtein_distance(tokens_x[indices[j]], tokens_y[j]);
          }

          if (distance < min_distance) {
            min_distance = distance;
            if (min_distance == 0) break;
          }

        } while (std::next_permutation(indices.begin(), indices.end()));
      }
    }

    // Store results in matrix
    result(i, 0) = k_x;        // k_x
    result(i, 1) = k_y;        // k_y
    result(i, 2) = min_tokens;    // k_align
    result(i, 3) = min_distance;  // min_dist
  }

  return result;
}
