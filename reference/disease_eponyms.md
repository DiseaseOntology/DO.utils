# Disease Name Eponym Replacement Vector

A named character vector for correcting proper noun (eponym)
capitalization in disease entry names after they have been parsed by
[`parse_omim_name()`](https://diseaseontology.github.io/DO.utils/reference/parse_omim_name.md).
Names are the lowercase form of each word; values are the correctly
capitalized replacement (e.g. `c("waardenburg" = "Waardenburg")`).

## Usage

``` r
disease_eponyms
```

## Format

A named character vector. Names are lowercase words; values are their
correctly capitalized replacements. Length 0 until first curation run.

## Details

[`parse_omim_name()`](https://diseaseontology.github.io/DO.utils/reference/parse_omim_name.md)
uses this dataset as its default `eponyms` argument, applying
whole-word, case-insensitive substitutions to the lowercased output.

## See also

[`parse_omim_name()`](https://diseaseontology.github.io/DO.utils/reference/parse_omim_name.md)
which uses this vector by default;
[disease_cap_patterns](https://diseaseontology.github.io/DO.utils/reference/disease_cap_patterns.md)
for the companion phrase-level replacement vector; the [Maintainer
Guide](https://allenbaron.github.io/DO.utils/articles/maintainer-guide.html)
for the full dataset-building and curation workflow.
