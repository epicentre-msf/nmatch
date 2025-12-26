# Evaluate token match details to determine overall match status

Evaluate token match details to determine overall match status

## Usage

``` r
match_eval(k_x, k_y, n_match, n_match_crit, ...)
```

## Arguments

- k_x:

  Integer vector specifying number of tokens in names `x`

- k_y:

  Integer vector specifying number of tokens in names `y`

- n_match:

  Integer vector specifying number of aligned tokens between x and y
  that are matching (i.e. based on argument `dist_max` in
  [`nmatch`](https://epicentre-msf.github.io/nmatch/reference/nmatch.md))

- n_match_crit:

  Minimum number of matching tokens for names x and y to be considered
  an overall match

- ...:

  Additional arguments (not used)

## Value

Logical vector indicating whether names `x` and `y` match, based on the
token match details provided as arguments
