
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nmatch: Fuzzy matching for proper names

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/epicentre-msf/nmatch/workflows/R-CMD-check/badge.svg)](https://github.com/epicentre-msf/nmatch/actions)
<!-- badges: end -->

Compare or match proper names from different sources, accounting for
potential variation in format, name order, and spelling (e.g. “Beyoncé
Knowles” vs. “KNOWLES-CARTER, Beyonce”). Specifically, uses
string-standardization to account for variation in punctuation, accents,
and character case, token-permutation to account for variation in name
order, and fuzzy matching to handle alternate spellings.

### Installation

Install from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("epicentre-msf/nmatch")
```

### Example usage

``` r
library(nmatch)
library(dplyr)
library(purrr)
library(tidyr)

# example data with names to compare
data("names_ex")

# compare with default settings
names_ex %>% 
  mutate(is_match = nmatch(name_source1, name_source2))
#>                  name_source1           name_source2 is_match
#> 1             Beyoncé Knowles Beyonce Knowles-Carter     TRUE
#> 2    Frédéric François Chopin    CHOPIN, Fryderyk F.    FALSE
#> 3    Kendrick Lamar Duckworth         LAMAR, Kendrik     TRUE
#> 4 Calvin Cordozar Broadus Jr.             Snoop Dogg    FALSE
#> 5 Céline Marie Claudette Dion           DION, Céline     TRUE
#> 6         Aubrey Drake Graham                  Drake    FALSE

# return full match details
names_ex %>% 
  mutate(match_df = purrr::map2(name_source1, name_source2, nmatch, return_full = TRUE)) %>% 
  tidyr::unnest("match_df")
#> # A tibble: 6 × 11
#>   name_source1                name_source2     is_match    id   k_x   k_y k_align n_match dist_total freq_score align   
#>   <chr>                       <chr>            <lgl>    <int> <int> <int>   <int>   <int>      <int> <chr>      <list>  
#> 1 Beyoncé Knowles             Beyonce Knowles… TRUE         1     2     3       2       2          0 <NA>       <tibble>
#> 2 Frédéric François Chopin    CHOPIN, Frydery… FALSE        1     3     2       2       1          3 <NA>       <tibble>
#> 3 Kendrick Lamar Duckworth    LAMAR, Kendrik   TRUE         1     3     2       2       2          1 <NA>       <tibble>
#> 4 Calvin Cordozar Broadus Jr. Snoop Dogg       FALSE        1     4     2       2       0         10 <NA>       <tibble>
#> 5 Céline Marie Claudette Dion DION, Céline     TRUE         1     4     2       2       2          0 <NA>       <tibble>
#> 6 Aubrey Drake Graham         Drake            FALSE        1     3     1       1       1          0 <NA>       <tibble>

# use a custom function to classify matches, based on any of the vars returned
# when return_full = TRUE (i.e. k_x, k_y, k_align, n_match, dist_total)
# note that fn must include argument `...` even if not used
classify_matches <- function(k_align, dist_total, ...) {
  k_align >= 1 & dist_total <= 3
}

names_ex %>% 
  mutate(is_match = nmatch(name_source1, name_source2, eval_fn = classify_matches))
#>                  name_source1           name_source2 is_match
#> 1             Beyoncé Knowles Beyonce Knowles-Carter     TRUE
#> 2    Frédéric François Chopin    CHOPIN, Fryderyk F.     TRUE
#> 3    Kendrick Lamar Duckworth         LAMAR, Kendrik     TRUE
#> 4 Calvin Cordozar Broadus Jr.             Snoop Dogg    FALSE
#> 5 Céline Marie Claudette Dion           DION, Céline     TRUE
#> 6         Aubrey Drake Graham                  Drake     TRUE
```

#### Using with [fuzzyjoin](https://github.com/dgrtwo/fuzzyjoin)

``` r
library(fuzzyjoin)

# example hospital datasets (IPD and ICU) with patients names in different
# formats (note these names are just random combinations of common French names)
data("dat_ipd")
data("dat_icu")

# join IPD and ICU datasets by name variables, using fuzzy matching with nmatch()
fuzzyjoin::fuzzy_join(
  dat_ipd,
  dat_icu,
  by = c("name_ipd" = "name_icu"),
  match_fun = nmatch::nmatch,
  mode = "left"
)
#> # A tibble: 10 × 4
#>    name_ipd                           date_ipd   name_icu                 date_icu  
#>    <chr>                              <date>     <chr>                    <date>    
#>  1 COLLET, André Daniel               2020-07-17 André D. Colet           2020-07-19
#>  2 LÈFEVRE, Françoise Sylvie          2020-06-03 Francoise Lefevre        2020-06-05
#>  3 DUBOIS, Monique Léa                2020-06-10 Monique Léa Dubois       2020-06-14
#>  4 GUÉRIN, Jacqueline Hélène          2020-07-09 Jacqueline Hélène Guérin 2020-07-09
#>  5 MARTIN, Philippe Arnaud            2020-06-11 <NA>                     NA        
#>  6 DUMONT, René Stéphane              2020-07-26 Renae  Dumont            2020-07-29
#>  7 LÉVEILLÉ, Anne-Charlotte Catherine 2020-06-01 <NA>                     NA        
#>  8 MARCHAND, Louis Enzo               2020-07-17 <NA>                     NA        
#>  9 SANCHEZ, Isabelle Suzanne          2020-07-03 <NA>                     NA        
#> 10 ROUX, Nathalie Elisabeth           2020-07-06 Natalia Roux             2020-07-06
```

Note that matching large datasets using `fuzzy_join()` and `nmatch()`
may be very slow. The nmatch package will eventually contain stand-alone
join functions that are optimized for name-matching.
