# Convert to Character

Provides all character conversion methods of
[`base::as.character()`](https://rdrr.io/r/base/character.html) and
additional methods for lists and data.frames. `to_character()` was
created to enable
[`collapse_to_string()`](https://allenbaron.github.io/DO.utils/reference/collapse_to_string.md)
to handle many data types while avoiding unintended conversions to
character, which would have occurred if these methods were added to
[`as.character()`](https://rdrr.io/r/base/character.html).

## Usage

``` r
to_character(x, ...)
```

## Arguments

- x:

  object to be coerced or tested.

- ...:

  further arguments passed to or from other methods.
