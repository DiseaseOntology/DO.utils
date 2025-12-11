# Round Number Up

Round Number Up

## Usage

``` r
round_up(x, digits = 0)
```

## Arguments

- x:

  A numeric vector.

- digits:

  The number of decimal places, as an integer. Negative values are
  allowed (see 'Details').

## Details

Rounding to a negative number of digits means rounding to a power of
ten, so for example round_up(x, digits = -2) rounds to the nearest
hundred.
