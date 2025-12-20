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


// Optimal String Alignment distance (restricted Damerau-Levenshtein)
// [[Rcpp::export]]
int osa_distance(const std::string& s1, const std::string& s2) {
  int m = s1.length();
  int n = s2.length();
  
  if (m == 0) return n;
  if (n == 0) return m;
  
  // For OSA, we need three rows: prev_prev, prev, curr
  std::vector<int> prev_prev_row(n + 1);
  std::vector<int> prev_row(n + 1);
  std::vector<int> curr_row(n + 1);
  
  // Initialize first row
  for (int j = 0; j <= n; j++) {
    prev_row[j] = j;
  }
  
  for (int i = 1; i <= m; i++) {
    curr_row[0] = i;
    
    for (int j = 1; j <= n; j++) {
      int cost = (s1[i-1] == s2[j-1]) ? 0 : 1;
      
      // Standard operations: insertion, deletion, substitution
      curr_row[j] = std::min({
        prev_row[j] + 1,        // deletion
        curr_row[j-1] + 1,      // insertion
        prev_row[j-1] + cost    // substitution
      });
      
      // Transposition (swap adjacent characters)
      if (i > 1 && j > 1 && 
          s1[i-1] == s2[j-2] && s1[i-2] == s2[j-1]) {
        curr_row[j] = std::min(curr_row[j], prev_prev_row[j-2] + cost);
      }
    }
    
    // Rotate rows: prev_prev <- prev <- curr
    prev_prev_row.swap(prev_row);
    prev_row.swap(curr_row);
  }
  
  return prev_row[n];
}


// C++ version of match_eval_token function
bool match_eval_token_cpp(int nchar_x, int nchar_y, int dist) {
  int nchar_max = std::max(nchar_x, nchar_y);

  bool is_match = (nchar_max <= 3 && dist == 0) ||
                  (nchar_max == 4 && dist <= 1) ||
                  (nchar_max >= 5 && nchar_max <= 8 && dist <= 2) ||
                  (nchar_max >= 9 && dist <= 3);

  return is_match;
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

  // Create output matrix with 8 columns
  IntegerMatrix result(n, 8);
  colnames(result) = CharacterVector::create("k_x", "k_y", "k_align", "n_match", "dist_total", "freq1", "freq2", "freq3");

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
    int n_match = 0;

    // Variables to store best matching tokens for frequency lookup
    std::vector<std::string> best_tokens_x;
    std::vector<std::string> best_tokens_y;
    std::vector<int> best_distances; // Store distances for each token pair

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
          std::vector<int> token_distances(min_tokens);

          for (int j = 0; j < min_tokens && distance < min_distance; j++) {
            int token_dist = osa_distance(tokens_x[j], tokens_y[indices[j]]);
            token_distances[j] = token_dist;
            distance += token_dist;
          }

          if (distance < min_distance) {
            min_distance = distance;
            // Store the best matching tokens and their distances
            best_tokens_x.clear();
            best_tokens_y.clear();
            best_distances.clear();
            for (int j = 0; j < min_tokens; j++) {
              best_tokens_x.push_back(tokens_x[j]);
              best_tokens_y.push_back(tokens_y[indices[j]]);
              best_distances.push_back(token_distances[j]);
            }
            if (min_distance == 0) break;
          }

        } while (std::next_permutation(indices.begin(), indices.end()));

      } else {
        std::vector<int> indices(k_x);
        std::iota(indices.begin(), indices.end(), 0);

        do {
          int distance = 0;
          std::vector<int> token_distances(min_tokens);

          for (int j = 0; j < min_tokens && distance < min_distance; j++) {
            int token_dist = osa_distance(tokens_x[indices[j]], tokens_y[j]);
            token_distances[j] = token_dist;
            distance += token_dist;
          }

          if (distance < min_distance) {
            min_distance = distance;
            // Store the best matching tokens and their distances
            best_tokens_x.clear();
            best_tokens_y.clear();
            best_distances.clear();
            for (int j = 0; j < min_tokens; j++) {
              best_tokens_x.push_back(tokens_x[indices[j]]);
              best_tokens_y.push_back(tokens_y[j]);
              best_distances.push_back(token_distances[j]);
            }
            if (min_distance == 0) break;
          }

        } while (std::next_permutation(indices.begin(), indices.end()));
      }

      // Count matches using the match_eval_token logic
      for (int j = 0; j < best_tokens_x.size(); j++) {
        int nchar_x = best_tokens_x[j].length();
        int nchar_y = best_tokens_y[j].length();
        int dist = best_distances[j];

        if (match_eval_token_cpp(nchar_x, nchar_y, dist)) {
          n_match++;
        }
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
    result(i, 3) = n_match;        // n_match
    result(i, 4) = min_distance;   // min_dist
    result(i, 5) = frequencies[0]; // freq1
    result(i, 6) = frequencies[1]; // freq2
    result(i, 7) = frequencies[2]; // freq3
  }

  return result;
}

