#include <Rcpp.h>
#include <string>
#include <vector>
#include <algorithm>
#include <cmath>
#include "nmatch_utils.h"

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
