# Compare sets of proper names accounting for common types of variation in format and style, optimized using Rcpp

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

## Usage

``` r
nmatch_fast(
  x,
  y,
  token_split = "[-_[:space:]]+",
  nchar_min = 2L,
  std = name_standardize,
  ...,
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

- std:

  Function to standardize strings during matching. Defaults to
  [`name_standardize`](https://epicentre-msf.github.io/nmatch/reference/name_standardize.md).
  Set to `NULL` to omit standardization.

- ...:

  additional arguments passed to `std()`

- token_freq:

  Optional data frame containing the frequencies of name tokens within
  the population of interest. Must have two columns

  - token_std: standardized tokens (using the same function as `std`)

  - freq: token frequency

## Value

Returns an integer matrix summarizing the match details, including
columns:

- `k_x`: number of tokens in `x` (excludes tokens smaller than
  `nchar_min`)

- `k_y`: number of tokens in `y` (excludes tokens smaller than
  `nchar_min`)

- `k_align`: number of aligned tokens (i.e. `min(k_x, k_y)`)

- `n_match`: number of tokens that match (see
  [match_eval_token](https://epicentre-msf.github.io/nmatch/reference/match_eval_token.md)
  for match logic)

- `dist_total`: summed string distance across aligned tokens

- `freq1`: summed frequency of first pair of aligned tokens (or NA if
  argument token_freq not provided)

- `freq2`: summed frequency of first pair of aligned tokens (or NA if
  argument token_freq not provided)

- `freq3`: summed frequency of first pair of aligned tokens (or NA if
  argument token_freq not provided)

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

# return matrix with match details
nmatch_fast(names1, names2)
#>      k_x k_y k_align n_match dist_total freq1 freq2 freq3
#> [1,]   3   2       2       2          0    NA    NA    NA
#> [2,]   5   2       2       2          0    NA    NA    NA
#> [3,]   2   2       2       2          2    NA    NA    NA
#> [4,]   2   2       2       2          2    NA    NA    NA
#> [5,]   4   3       3       3          1    NA    NA    NA
```
