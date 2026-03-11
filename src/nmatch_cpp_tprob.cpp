#include <Rcpp.h>
#include <string>
#include <vector>
#include <algorithm>
#include <cmath>
#include <unordered_map>
#include "nmatch_utils.h"

using namespace Rcpp;

// Vectorized version with token probability lookup
// [[Rcpp::export]]
DataFrame nmatch_cpp_tprob(const CharacterVector& x,
                           const CharacterVector& y,
                           int nchar_min = 2,
                           const CharacterVector& token_x = CharacterVector(),
                           const IntegerVector& dist_x = IntegerVector(),
                           const NumericVector& prob_x = NumericVector(),
                           const CharacterVector& token_y = CharacterVector(),
                           const IntegerVector& dist_y = IntegerVector(),
                           const NumericVector& prob_y = NumericVector()) {

  int n = x.size();

  if (n != y.size()) {
    Rcpp::stop("x and y must have the same length");
  }

  // Validate token lookup vectors
  if (token_x.size() != dist_x.size() || token_x.size() != prob_x.size()) {
    Rcpp::stop("token_x, dist_x, and prob_x must have the same length");
  }
  
  if (token_y.size() != dist_y.size() || token_y.size() != prob_y.size()) {
    Rcpp::stop("token_y, dist_y, and prob_y must have the same length");
  }

  // Replace IntegerMatrix with separate vectors
  IntegerVector k_x_vec(n);
  IntegerVector k_y_vec(n);
  IntegerVector k_align_vec(n);
  IntegerVector n_match_vec(n);
  IntegerVector dist_total_vec(n);
  NumericVector prob_avg_1(n, NA_REAL);
  NumericVector prob_avg_2(n, NA_REAL);
  NumericVector prob_avg_3(n, NA_REAL);
  NumericVector prob_product(n, NA_REAL);
  
  // Pre-convert all strings
  std::vector<std::string> x_strings(n);
  std::vector<std::string> y_strings(n);
  for (int i = 0; i < n; i++) {
    x_strings[i] = Rcpp::as<std::string>(x[i]);
    y_strings[i] = Rcpp::as<std::string>(y[i]);
  }

  // Create hash maps for probability lookups
  // Map: token -> (distance -> probability)
  bool use_prob_lookup = (token_x.size() > 0 && token_y.size() > 0);
  
  std::unordered_map<std::string, std::unordered_map<int, double>> prob_map_x;
  std::unordered_map<std::string, std::unordered_map<int, double>> prob_map_y;
  
  if (use_prob_lookup) {
    // Build probability map for x tokens
    for (int idx = 0; idx < token_x.size(); idx++) {
      std::string tok = Rcpp::as<std::string>(token_x[idx]);
      int dist = dist_x[idx];
      double prob = prob_x[idx];
      prob_map_x[tok][dist] = prob;
    }
    
    // Build probability map for y tokens
    for (int idx = 0; idx < token_y.size(); idx++) {
      std::string tok = Rcpp::as<std::string>(token_y[idx]);
      int dist = dist_y[idx];
      double prob = prob_y[idx];
      prob_map_y[tok][dist] = prob;
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

    // Variables to store best matching tokens for probability lookup
    std::vector<std::string> best_tokens_x;
    std::vector<std::string> best_tokens_y;
    std::vector<int> best_distances; // Store distances for each token pair

    if (tokens_x.empty() || tokens_y.empty()) {
      min_distance = 9999;
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

    if (use_prob_lookup && !best_tokens_x.empty()) {
      // For each token in the best alignment (up to 3)
      for (int tok_idx = 0; tok_idx < std::min(3, (int)best_tokens_x.size()); tok_idx++) {
        std::string tok_x = best_tokens_x[tok_idx];
        std::string tok_y = best_tokens_y[tok_idx];
        int dist = best_distances[tok_idx];
        
        double prob_from_x = NA_REAL;
        double prob_from_y = NA_REAL;
        
        // Look up prob_x for token_y at this distance
        auto tok_it_x = prob_map_x.find(tok_x);
        if (tok_it_x != prob_map_x.end()) {
          auto dist_it = tok_it_x->second.find(dist);
          if (dist_it != tok_it_x->second.end()) {
            prob_from_x = dist_it->second;
          }
        }
        
        // Look up prob_y for token_x at this distance
        auto tok_it_y = prob_map_y.find(tok_y);
        if (tok_it_y != prob_map_y.end()) {
          auto dist_it = tok_it_y->second.find(dist);
          if (dist_it != tok_it_y->second.end()) {
            prob_from_y = dist_it->second;
          }
        }
        
        // Calculate average if both found
        if (!ISNA(prob_from_x) && !ISNA(prob_from_y)) {
          // double avg_prob = (prob_from_x + prob_from_y) / 2.0;
          double avg_prob = std::exp((std::log(prob_from_x) + std::log(prob_from_y)) / 2.0);
          
          if (tok_idx == 0) prob_avg_1[i] = avg_prob;
          else if (tok_idx == 1) prob_avg_2[i] = avg_prob;
          else if (tok_idx == 2) prob_avg_3[i] = avg_prob;
        }
      }

      // Calculate product of non-missing probabilities
      double product = 1.0;
      int count = 0;
      
      if (!ISNA(prob_avg_1[i])) {
        product *= prob_avg_1[i];
        count++;
      }
      if (!ISNA(prob_avg_2[i])) {
        product *= prob_avg_2[i];
        count++;
      }
      if (!ISNA(prob_avg_3[i])) {
        product *= prob_avg_3[i];
        count++;
      }
      
      if (count > 0) {
        prob_product[i] = product;
      }
    }

    // Store results in vectors instead of matrix
    k_x_vec[i] = k_x;
    k_y_vec[i] = k_y;
    k_align_vec[i] = min_tokens;
    n_match_vec[i] = n_match;
    dist_total_vec[i] = min_distance;
  }

  // Return DataFrame instead of IntegerMatrix
  return DataFrame::create(
    Named("k_x") = k_x_vec,
    Named("k_y") = k_y_vec,
    Named("k_align") = k_align_vec,
    Named("n_match") = n_match_vec,
    Named("dist_total") = dist_total_vec,
    Named("p1") = prob_avg_1,
    Named("p2") = prob_avg_2,
    Named("p3") = prob_avg_3,
    Named("p_product") = prob_product
  );
}

