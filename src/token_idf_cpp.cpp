#include <Rcpp.h>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <cmath>
#include "nmatch_utils.h"

using namespace Rcpp;

// Compute token IDF weights from a character vector of standardized names.
// For each name, tokens are deduplicated before counting, so df(t) reflects
// the number of names containing t, not total occurrences.
// Returns a data frame with columns `token` (character) and `idf` (numeric).
// [[Rcpp::export]]
DataFrame token_idf_cpp(const CharacterVector& names, int nchar_min = 2) {

  int N = names.size();

  std::unordered_map<std::string, int> doc_freq;
  doc_freq.reserve(N * 4);  // rough initial capacity

  for (int i = 0; i < N; i++) {
    std::vector<std::string> tokens = tokenize_name(
      Rcpp::as<std::string>(names[i]), nchar_min
    );

    // Deduplicate within this name before counting
    std::unordered_set<std::string> seen;
    for (const std::string& tok : tokens) {
      if (seen.insert(tok).second) {
        doc_freq[tok]++;
      }
    }
  }

  int n_tokens = doc_freq.size();
  CharacterVector out_token(n_tokens);
  NumericVector   out_idf(n_tokens);

  int k = 0;
  double log_N = std::log(static_cast<double>(N));
  for (const auto& kv : doc_freq) {
    out_token[k] = kv.first;
    out_idf[k]   = log_N - std::log(static_cast<double>(kv.second));
    k++;
  }

  return DataFrame::create(
    Named("token") = out_token,
    Named("idf")   = out_idf,
    Named("stringsAsFactors") = false
  );
}
