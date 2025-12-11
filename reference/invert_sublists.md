# Invert Sublists

Inverts elements in lists within a list such that those elements (at
depth 3) are regrouped by position or name within each sublist (depth
2).

## Usage

``` r
invert_sublists(x, use_sublist_names = FALSE)
```

## Arguments

- x:

  A list of non-empty lists.

- use_sublist_names:

  Whether to use sublist names for inversion, as `TRUE` or `FALSE`
  (default). See 'Details' for specifics.

## Details

The `use_sublist_names` alters how sublists are inverted as follows:

- When `FALSE`, sublist elements are inverted by position within
  sublists. Sublists must all have the same number of elements. The
  original sublist names will be preserved (now at depth 3) but original
  depth 3 names (now sublists) will be dropped.

- When `TRUE`, all sublists elements (depth 3) must be named and names
  must not be duplicated within any given sublist. Also, matching names
  must exist across all sublists, as names are used for inversion
  itself, as well as, to name inverted sublists (names from depth 3 are
  moved to depth 2) in output. *All sublists must still have the same
  number of elements.*

## Examples

``` r
.l <- list(
    list(1, "a", TRUE),
    list(2, "b", FALSE)
)
invert_sublists(.l)
#> [[1]]
#> [[1]][[1]]
#> [1] 1
#> 
#> [[1]][[2]]
#> [1] 2
#> 
#> 
#> [[2]]
#> [[2]][[1]]
#> [1] "a"
#> 
#> [[2]][[2]]
#> [1] "b"
#> 
#> 
#> [[3]]
#> [[3]][[1]]
#> [1] TRUE
#> 
#> [[3]][[2]]
#> [1] FALSE
#> 
#> 

# sublist element ('item') names are dropped when inverting by position but
#    sublist names are retained
.l_nm <- list(
    subl1 = list(item1 = 1, item2 = "a", item3 = TRUE),
    subl2 = list(item1 = 2, item2 = "b", item3 = FALSE)
)
invert_sublists(.l_nm, use_sublist_names = FALSE)
#> [[1]]
#> [[1]]$subl1
#> [1] 1
#> 
#> [[1]]$subl2
#> [1] 2
#> 
#> 
#> [[2]]
#> [[2]]$subl1
#> [1] "a"
#> 
#> [[2]]$subl2
#> [1] "b"
#> 
#> 
#> [[3]]
#> [[3]]$subl1
#> [1] TRUE
#> 
#> [[3]]$subl2
#> [1] FALSE
#> 
#> 

# all names are retained when inverting by name
.res1 <- invert_sublists(.l_nm, use_sublist_names = TRUE)
.res1
#> $item1
#> $item1$subl1
#> [1] 1
#> 
#> $item1$subl2
#> [1] 2
#> 
#> 
#> $item2
#> $item2$subl1
#> [1] "a"
#> 
#> $item2$subl2
#> [1] "b"
#> 
#> 
#> $item3
#> $item3$subl1
#> [1] TRUE
#> 
#> $item3$subl2
#> [1] FALSE
#> 
#> 

# names are used for inverting, so order can be different
.l_nm2 <- list(
    subl1 = list(item1 = 1, item2 = "a", item3 = TRUE),
    subl2 = list(item2 = "b", item3 = FALSE, item1 = 2) # order changed
)

## invert by name -> results the same, despite order change
.res2 <- invert_sublists(.l_nm2, use_sublist_names = TRUE)
identical(.res1, .res2)
#> [1] TRUE

## invert by pos -> results differ
.res3 <- invert_sublists(.l_nm2, use_sublist_names = FALSE)
identical(.res1, .res3)
#> [1] FALSE
```
