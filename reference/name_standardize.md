# String standardization

Standardize strings prior to performing a match, using the following
transformations:

1.  standardize case
    ([`base::toupper`](https://rdrr.io/r/base/chartr.html))

2.  remove accents/diacritics
    ([`stringi::stri_trans_general`](https://rdrr.io/pkg/stringi/man/stri_trans_general.html))

3.  replace punctuation characters with whitespace

4.  remove extraneous space characters with
    ([`stringr::str_squish`](https://stringr.tidyverse.org/reference/str_trim.html))

## Usage

``` r
name_standardize(x)
```

## Arguments

- x:

  a string

## Value

The standardized version of `x`

## Examples

``` r
name_standardize("angela_merkel")
#> [1] "ANGELA MERKEL"
name_standardize("QUOIREZ, Fran\U00E7oise D.")
#> [1] "QUOIREZ FRANCOISE D"
```
