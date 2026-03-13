#include <Rcpp.h>
#include <string>
#include <vector>
#include <algorithm>
#include <cmath>
#include <map>
#include <unordered_map>
#include "nmatch_utils.h"

using namespace Rcpp;

// Look up P(dist <= query_dist | token) using floor semantics on the sorted
// distance map. Returns default_prob if query_dist is below the smallest
// stored distance (i.e. the observed distance is more extreme than any in the
// training data) and the last stored value if query_dist exceeds the maximum.
static double lookup_cum_prob(const std::map<int, double>& dist_map,
                              int query_dist, double default_prob) {
  if (dist_map.empty()) return default_prob;
  auto it = dist_map.upper_bound(query_dist); // first entry with key > query_dist
  if (it == dist_map.begin()) return default_prob; // query_dist < smallest stored
  --it;
  return it->second;
}

// Vectorized version with token probability lookup
// Evidence = maximum sum(-log geomean(P_x, P_y)) across aligned token pairs
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

  if (token_x.size() != dist_x.size() || token_x.size() != prob_x.size()) {
    Rcpp::stop("token_x, dist_x, and prob_x must have the same length");
  }

  if (token_y.size() != dist_y.size() || token_y.size() != prob_y.size()) {
    Rcpp::stop("token_y, dist_y, and prob_y must have the same length");
  }

  // Output vectors
  IntegerVector k_x_vec(n), k_y_vec(n), k_align_vec(n), n_match_vec(n), dist_total_vec(n);
  NumericVector prob_avg_1(n, NA_REAL), prob_avg_2(n, NA_REAL), prob_avg_3(n, NA_REAL);
  NumericVector similarity_vec(n, NA_REAL);
  NumericVector weight_vec(n, NA_REAL);

  // Pre-convert strings
  std::vector<std::string> x_strings(n), y_strings(n);
  for (int i = 0; i < n; i++) {
    x_strings[i] = Rcpp::as<std::string>(x[i]);
    y_strings[i] = Rcpp::as<std::string>(y[i]);
  }

  // Build probability maps: token -> sorted map<dist, cum_prob>
  bool use_prob_lookup = (token_x.size() > 0 && token_y.size() > 0);

  std::unordered_map<std::string, std::map<int, double>> prob_map_x, prob_map_y;

  if (use_prob_lookup) {
    for (int idx = 0; idx < token_x.size(); idx++) {
      prob_map_x[Rcpp::as<std::string>(token_x[idx])][dist_x[idx]] = prob_x[idx];
    }
    for (int idx = 0; idx < token_y.size(); idx++) {
      prob_map_y[Rcpp::as<std::string>(token_y[idx])][dist_y[idx]] = prob_y[idx];
    }
  }

  // Process each pair
  for (int i = 0; i < n; i++) {

    std::vector<std::string> tokens_x = tokenize_name(x_strings[i], nchar_min);
    std::vector<std::string> tokens_y = tokenize_name(y_strings[i], nchar_min);

    int k_x = tokens_x.size();
    int k_y = tokens_y.size();
    int min_tokens = std::min(k_x, k_y);

    std::vector<std::string> best_tokens_x, best_tokens_y;
    std::vector<int> best_distances;
    int dist_of_best = 0;

    if (tokens_x.empty() || tokens_y.empty()) {
      dist_of_best = 9999;

    } else {
      // Always align by minimising summed string distance
      int min_distance = INT_MAX;

      if (k_x <= k_y) {
        std::vector<int> indices(k_y);
        std::iota(indices.begin(), indices.end(), 0);
        do {
          int distance = 0;
          std::vector<int> token_distances(min_tokens);
          for (int j = 0; j < min_tokens && distance < min_distance; j++) {
            token_distances[j] = osa_distance(tokens_x[j], tokens_y[indices[j]]);
            distance += token_distances[j];
          }
          if (distance < min_distance) {
            min_distance = distance;
            best_tokens_x.clear(); best_tokens_y.clear(); best_distances.clear();
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
            token_distances[j] = osa_distance(tokens_x[indices[j]], tokens_y[j]);
            distance += token_distances[j];
          }
          if (distance < min_distance) {
            min_distance = distance;
            best_tokens_x.clear(); best_tokens_y.clear(); best_distances.clear();
            for (int j = 0; j < min_tokens; j++) {
              best_tokens_x.push_back(tokens_x[indices[j]]);
              best_tokens_y.push_back(tokens_y[j]);
              best_distances.push_back(token_distances[j]);
            }
            if (min_distance == 0) break;
          }
        } while (std::next_permutation(indices.begin(), indices.end()));
      }

      dist_of_best = min_distance;
    }

    // Count matches and sum similarity from best alignment
    int n_match = 0;
    double sim_total = 0.0;
    for (int j = 0; j < (int)best_tokens_x.size(); j++) {
      int nchar_x = best_tokens_x[j].length();
      int nchar_y = best_tokens_y[j].length();
      int d = best_distances[j];
      if (match_eval_token_cpp(nchar_x, nchar_y, d)) n_match++;
      sim_total += 1.0 - (double)d / std::max(nchar_x, nchar_y);
    }
    if (!best_tokens_x.empty()) similarity_vec[i] = sim_total;

    // Per-pair probability outputs from best alignment
    if (use_prob_lookup && !best_tokens_x.empty()) {
      int n_pairs = best_tokens_x.size();
      double wt = 0.0;

      for (int j = 0; j < n_pairs; j++) {
        int d = best_distances[j];

        auto it_x = prob_map_x.find(best_tokens_x[j]);
        double px = (it_x != prob_map_x.end())
          ? lookup_cum_prob(it_x->second, d, it_x->second.begin()->second) : 1.0;

        auto it_y = prob_map_y.find(best_tokens_y[j]);
        double py = (it_y != prob_map_y.end())
          ? lookup_cum_prob(it_y->second, d, it_y->second.begin()->second) : 1.0;

        double geomean_p = std::exp((std::log(px) + std::log(py)) / 2.0);
        wt += -std::log(geomean_p);

        if (j == 0) prob_avg_1[i] = geomean_p;
        if (j == 1) prob_avg_2[i] = geomean_p;
        if (j == 2) prob_avg_3[i] = geomean_p;
      }

      weight_vec[i] = wt;
    }

    k_x_vec[i]        = k_x;
    k_y_vec[i]        = k_y;
    k_align_vec[i]    = min_tokens;
    n_match_vec[i]    = n_match;
    dist_total_vec[i] = dist_of_best;
  }

  return DataFrame::create(
    Named("k_x")        = k_x_vec,
    Named("k_y")        = k_y_vec,
    Named("k_align")    = k_align_vec,
    Named("n_match")    = n_match_vec,
    Named("dist_total") = dist_total_vec,
    Named("p1")         = prob_avg_1,
    Named("p2")         = prob_avg_2,
    Named("p3")         = prob_avg_3,
    Named("similarity") = similarity_vec,
    Named("weight")     = weight_vec
  );
}
