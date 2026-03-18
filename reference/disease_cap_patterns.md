# Disease Name Capitalization Pattern Vector

A named character vector of phrase-level regex substitutions applied to
lowercased disease entry names by
[`parse_omim_name()`](https://diseaseontology.github.io/DO.utils/reference/parse_omim_name.md),
*after* word-level
[disease_eponyms](https://diseaseontology.github.io/DO.utils/reference/disease_eponyms.md)
replacements. Use `disease_cap_patterns` for words whose correct
capitalization depends on context (e.g. `SHORT` as an acronym in *SHORT
syndrome* vs `short` as an adjective elsewhere).

## Usage

``` r
disease_cap_patterns
```

## Format

A named character vector. Names are case-insensitive regex patterns;
values are their replacements. Length 0 until first curation run.

## Details

Names are case-insensitive regex patterns matched against the full
lowercased name; values are the replacement strings. Longer patterns
take priority over shorter ones, and patterns override conflicting
[disease_eponyms](https://diseaseontology.github.io/DO.utils/reference/disease_eponyms.md)
substitutions.

## See also

[`parse_omim_name()`](https://diseaseontology.github.io/DO.utils/reference/parse_omim_name.md)
which uses this vector by default;
[disease_eponyms](https://diseaseontology.github.io/DO.utils/reference/disease_eponyms.md)
for the companion word-level replacement vector; the [Maintainer
Guide](https://allenbaron.github.io/DO.utils/articles/maintainer-guide.html)
for the full dataset-building and curation workflow.
