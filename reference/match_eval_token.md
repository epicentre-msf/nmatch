# Evaluate string distance and token lengths to determine whether two tokens match

Evaluate string distance and token lengths to determine whether two
tokens match

## Usage

``` r
match_eval_token(nchar_x, nchar_y, nchar_max, dist, ...)
```

## Arguments

- nchar_x:

  Number of characters in token `x`

- nchar_y:

  Number of characters in token `y`

- nchar_max:

  `max(c(x_nchar, y_nchar))`

- dist:

  String distance between tokens x and y

- ...:

  Additional arguments (not used)

## Value

Logical vector indicating whether tokens `x` and `y` match, based on
their respective lengths and the string distance between them
