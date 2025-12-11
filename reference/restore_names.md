# Restore Element Names

Restores names to elements within objects after name-removing events.

## Usage

``` r
restore_names(x, names_from)
```

## Arguments

- x:

  object needing names retored

- names_from:

  object to restore names from

## Details

Necessary for the following scenarios:

- After string operations with `stringr` or `stringi`, until `stringi`
  fixes [issue \#59](https://github.com/gagolews/stringi/issues/59)
