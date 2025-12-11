# Set Keys for Scopus API Access

Sets Elsevier API key and/or institutional token as environment
variables (`Elsevier_API` and `Elsevier_insttoken`) for access by
subsequent Scopus functions.

## Usage

``` r
set_scopus_keys(api_key, insttoken)
```

## Arguments

- api_key:

  Elsevier API key, as a string.

- insttoken:

  Elsevier institutional token, as a string.

## Value

Named logical vector stating whether setting operation was successful
for each key provided as input (invisibly).

## See also

citedby_scopus
