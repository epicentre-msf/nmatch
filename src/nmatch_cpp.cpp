#include <Rcpp.h>
#include <string>
#include <vector>
#include <algorithm>
#include <unordered_map>

using namespace Rcpp;

// Function to tokenize a name string
// [[Rcpp::export]]
std::vector<std::string> tokenize_name(const std::string& name, int nchar_min = 2) {
  std::vector<std::string> tokens;
  std::string current_token;

  for (char c : name) {
    // Check if character is a delimiter (space, dash, or underscore)
    if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '-' || c == '_') {
      // End current token if it exists and meets length requirement
      if (!current_token.empty() && current_token.length() >= nchar_min) {
        tokens.push_back(current_token);
      }
      current_token.clear();
    } else {
      // Add character to current token
      current_token += c;
    }
  }

  // Account for last token, if it meets length requirement
  if (!current_token.empty() && current_token.length() >= nchar_min) {
    tokens.push_back(current_token);
  }

  return tokens;
}


// Levenshtein distance
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


// Vectorized version with token frequency lookup
// [[Rcpp::export]]
IntegerMatrix nmatch_cpp_tfreq(const CharacterVector& x,
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

  // Create output matrix with 7 columns
  IntegerMatrix result(n, 7);
  colnames(result) = CharacterVector::create("k_x", "k_y", "k_align", "dist_total", "freq1", "freq2", "freq3");

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
    int min_distance;

    // Variables to store best matching tokens for frequency lookup
    std::vector<std::string> best_tokens_x;
    std::vector<std::string> best_tokens_y;

    if (tokens_x.empty() || tokens_y.empty()) {
      min_distance = 9999;
      // No tokens to get frequencies from
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
            // Store the best matching tokens
            best_tokens_x.clear();
            best_tokens_y.clear();
            for (int j = 0; j < min_tokens; j++) {
              best_tokens_x.push_back(tokens_x[j]);
              best_tokens_y.push_back(tokens_y[indices[j]]);
            }
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
            // Store the best matching tokens
            best_tokens_x.clear();
            best_tokens_y.clear();
            for (int j = 0; j < min_tokens; j++) {
              best_tokens_x.push_back(tokens_x[indices[j]]);
              best_tokens_y.push_back(tokens_y[j]);
            }
            if (min_distance == 0) break;
          }

        } while (std::next_permutation(indices.begin(), indices.end()));
      }
    }

    // Look up frequencies for best matching token pairs (up to 3)
    std::vector<int> frequencies(3, NA_INTEGER);  // Initialize with NA

    if (use_frequency_lookup && !best_tokens_x.empty()) {
      // Calculate summed frequencies for each matching pair
      for (int pair_idx = 0; pair_idx < std::min(3, (int)best_tokens_x.size()); pair_idx++) {

        // Look up frequency for token from x
        std::string token_x = best_tokens_x[pair_idx];
        auto it_x = token_map.find(token_x);

        // Look up frequency for corresponding token from y
        std::string token_y = best_tokens_y[pair_idx];
        auto it_y = token_map.find(token_y);

        // Only calculate sum if BOTH tokens are found
        if (it_x != token_map.end() && it_y != token_map.end()) {
          frequencies[pair_idx] = it_x->second + it_y->second;
        } else {
          frequencies[pair_idx] = NA_INTEGER;  // NA if either token not found
        }
      }
    }

    // Store results in matrix
    result(i, 0) = k_x;            // k_x
    result(i, 1) = k_y;            // k_y
    result(i, 2) = min_tokens;     // k_align
    result(i, 3) = min_distance;   // min_dist
    result(i, 4) = frequencies[0]; // freq1
    result(i, 5) = frequencies[1]; // freq2
    result(i, 6) = frequencies[2]; // freq3
  }

  return result;
}
