# Compare sets of proper names accounting for common types of variation in format and style

Compare proper names across two sources using string-standardization to
account for variation in punctuation, accents, and character case,
token-permutation to account for variation in name order, and fuzzy
matching to handle alternate spellings. The specific steps are:

1.  Standardize strings. The default function is
    [`name_standardize`](https://epicentre-msf.github.io/nmatch/reference/name_standardize.md)
    which removes accents and punctuation, standardizes case, and
    removes extra whitespace. E.g. "Brontë, Emily J." is standardized to
    "BRONTE EMILY J".

2.  Tokenize standardized names, optionally retaining only tokens larger
    than a given nchar limit.

3.  For each pair of names, calculate string distance between all
    combinations of tokens, and find the best overall token alignment
    (i.e. the alignment that minimizes the summed string distance). If
    two names being compared differ in their number of tokens, the
    alignment is made with respect to the smaller number of tokens. E.g.
    If comparing "Angela Dorothea Merkel" to "Merkel Angela", the token
    "Dorothea" would ultimately be omitted from the best alignment.

4.  For each pair of tokens in the best alignment, classify whether or
    not the tokens match (TRUE/FALSE) based on their respective lengths
    and the string distance between them.

5.  Summarize the number of tokens in each name, the number of tokens in
    the best alignment, the number of aligned tokens that match, and the
    summed string distance of the best alignment.

6.  Classify overall match status (TRUE/FALSE) based on the match
    details described in (5). By default, two names are considered to be
    matching if two or more tokens match across names (e.g. "Merkel
    Angela" matches "Angela Dorothea Merkel"), or if both names consist
    of only a single token which is matching (e.g. "Beyonce" matches
    "Beyoncé").

## Usage

``` r
nmatch(
  x,
  y,
  token_split = "[-_[:space:]]+",
  nchar_min = 2L,
  dist_method = "osa",
  std = name_standardize,
  ...,
  return_full = FALSE,
  return_alignment = FALSE,
  eval_fn_token = match_eval_token,
  eval_fn = match_eval,
  eval_params = list(n_match_crit = 2),
  token_freq = NULL
)
```

## Arguments

- x, y:

  Vectors of proper names to compare. Must be of same length.

- token_split:

  Regex pattern to split strings into tokens. Defaults to
  `"[-_[:space:]]+"`, which splits at each sequence of one more dash,
  underscore, or space character.

- nchar_min:

  Minimum token size to compare. Defaults to `2L`.

- dist_method:

  Method to use for string distance calculation (see
  [stringdist-metrics](https://rdrr.io/pkg/stringdist/man/stringdist-metrics.html)).
  Defaults to `"osa"`.

- std:

  Function to standardize strings during matching. Defaults to
  [`name_standardize`](https://epicentre-msf.github.io/nmatch/reference/name_standardize.md).
  Set to `NULL` to omit standardization.

- ...:

  additional arguments passed to `std()`

- return_full:

  Logical indicating whether to return data frame with full summary of
  match details (`TRUE`), or only a logical vector corresponding to
  final match status (`FALSE`). Defaults to `FALSE`.

- return_alignment:

  Logical indicating whether to return additional list column 'align'
  containing data frames with details on best alignment for each pair of
  names. Defaults to `FALSE`. Only used if `return_full = TRUE`. summary
  of match details (`TRUE`), or only a logical vector corresponding to
  final match status (`FALSE`). Defaults to `FALSE`.

- eval_fn_token:

  Function to determine token match status. Defaults to
  [`match_eval_token`](https://epicentre-msf.github.io/nmatch/reference/match_eval_token.md).
  See section *Custom classification functions* for more details.

- eval_fn:

  Function to determine overall match status. Defaults to
  [`match_eval`](https://epicentre-msf.github.io/nmatch/reference/match_eval.md).
  See section *Custom classification functions* for more details.

- eval_params:

  List of additional arguments passed to `eval_fn`

- token_freq:

  Optional data frame containing the frequencies of name tokens within
  the population of interest. Must have two columns

  - token_std: standardized tokens (using the same function as `std`)

  - freq: token frequency

## Value

If `return_full = FALSE` (the default), returns a logical vector
indicating which elements of `x` and `y` are matches.

If `return_full = TRUE`, returns a tibble-style data frame summarizing
the match details, including columns:

- `is_match`: logical vector indicating overall match status

- `k_x`: number of tokens in `x` (excludes tokens smaller than
  `nchar_min`)

- `k_y`: number of tokens in `y` (excludes tokens smaller than
  `nchar_min`)

- `k_align`: number of aligned tokens (i.e. `min(k_x, k_y)`)

- `n_match`: number of aligned tokens that match (i.e. distance \<=
  `dist_max`)

- `dist_total`: summed string distance across aligned tokens

## Examples

``` r
names1 <- c(
  "Angela Dorothea Merkel",
  "Emmanuel Jean-Michel Fr\u00e9d\u00e9ric Macron",
  "Mette Frederiksen",
  "Katrin Jakobsd\u00f3ttir",
  "Pedro S\u00e1nchez P\u00e9rez-Castej\u00f3n"
)

names2 <- c(
  "MERKEL, Angela",
  "MACRON, Emmanuel J.-M. F.",
  "FREDERICKSON, Mette",
  "JAKOBSDOTTIR  Kathríne",
  "PEREZ-CASTLEJON, Pedro"
)

# return logical vector specifying which names are matches
nmatch(names1, names2)
#> [1] TRUE TRUE TRUE TRUE TRUE

# return data frame with full match details
nmatch(names1, names2, return_full = TRUE)
#> # A tibble: 5 × 8
#>   is_match    id   k_x   k_y k_align n_match dist_total freq_score
#>   <lgl>    <int> <int> <int>   <int>   <int>      <int> <chr>     
#> 1 TRUE         1     3     2       2       2          0 NA        
#> 2 TRUE         2     5     2       2       2          0 NA        
#> 3 TRUE         3     2     2       2       2          2 NA        
#> 4 TRUE         4     2     2       2       2          2 NA        
#> 5 TRUE         5     4     3       3       3          1 NA        

# use a custom function to classify matches
classify_matches <- function(k_align, n_match, dist_total, ...) {
  n_match == k_align & dist_total < 2
}

nmatch(names1, names2, return_full = TRUE, eval_fn = classify_matches)
#> # A tibble: 5 × 8
#>   is_match    id   k_x   k_y k_align n_match dist_total freq_score
#>   <lgl>    <int> <int> <int>   <int>   <int>      <int> <chr>     
#> 1 TRUE         1     3     2       2       2          0 NA        
#> 2 TRUE         2     5     2       2       2          0 NA        
#> 3 FALSE        3     2     2       2       2          2 NA        
#> 4 FALSE        4     2     2       2       2          2 NA        
#> 5 TRUE         5     4     3       3       3          1 NA        
```
