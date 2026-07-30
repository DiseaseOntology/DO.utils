# OMIM Adjective Qualifier Vector

A character vector of uppercase OMIM qualifier tokens that trigger name
rearrangement in
[`parse_omim_name()`](https://diseaseontology.github.io/DO.utils/reference/parse_omim_name.md).
These are adjective/onset qualifiers that OMIM places as comma-separated
tokens after the primary disease term but that belong before it in
natural-language order (e.g. `CONGENITAL`, `SYNDROMIC`, `PROGRESSIVE`).

## Usage

``` r
omim_qualifiers
```

## Format

A character vector of uppercase qualifier tokens.

## Details

Structural qualifiers — pure numbers, `TYPE`/`MULTIPLE TYPES`, and
definitive inheritance terms such as `AUTOSOMAL RECESSIVE` or `X-LINKED`
— are handled by hardcoded rules in
[`parse_omim_name()`](https://diseaseontology.github.io/DO.utils/reference/parse_omim_name.md)
and are not included here.

## See also

[`parse_omim_name()`](https://diseaseontology.github.io/DO.utils/reference/parse_omim_name.md)
which uses this vector by default;
[disease_eponyms](https://diseaseontology.github.io/DO.utils/reference/disease_eponyms.md)
and
[disease_cap_patterns](https://diseaseontology.github.io/DO.utils/reference/disease_cap_patterns.md)
for companion capitalization datasets.
