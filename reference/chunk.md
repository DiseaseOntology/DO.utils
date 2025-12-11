# Partition vectors

`partition` divides vectors into partitions of a specified length and
returns them as a list. If `x` is not completely divisible by `n` the
last list item will have less than `n` elements. Similar functions may
be named "chunk".

## Usage

``` r
partition(x, n)
```

## Arguments

- x:

  A vector.

- n:

  An integer specifying instances per partition.

## Value

A list with `x/n` items (rounded up) each containing `n` elements from
`x`. When `x %% n != 0` the last list item will have \< `n` elements
from `x`.

## Examples

``` r
partition(letters[1:10], 5)
#> $`0`
#> [1] "a" "b" "c" "d" "e"
#> 
#> $`1`
#> [1] "f" "g" "h" "i" "j"
#> 
partition(1:10, 3)
#> $`0`
#> [1] 1 2 3
#> 
#> $`1`
#> [1] 4 5 6
#> 
#> $`2`
#> [1] 7 8 9
#> 
#> $`3`
#> [1] 10
#> 
```
