# Citation Matching

Essentially, [`base::match()`](https://rdrr.io/r/base/match.html) but
tailored to citations. Returns a vector of the positions of (first)
matches of a citation input (first argument) in a reference citation
input (second argument) based on common publication IDs (PMID, PMCID,
DOI).

## Usage

``` r
match_citations(x, ref, add_col = NULL, nomatch = NA_integer_)
```

## Arguments

- x, ref:

  a vector of PMID, PMCID, DOI, or Scopus IDs; or a "citation" dataframe
  with 1 or more of these as columns (column names should correspond to
  ID type; case-insensitive)

- add_col:

  The name of the column to add to `x` with the match results, as a
  string, or `NULL` (default) if results should be returned as a vector.

- nomatch:

  the value to be returned in the case when no match is found. Note that
  it is coerced to `integer`.

## Value

If `x` is a vector or `add_col` is FALSE (default), an integer vector of
the same length as `x` (if a data.frame, of length equal to the number
of rows). If `x` is a data.frame and `add_col` is TRUE, a mutated
data.frame with a `cite_match` integer column identifying match
positions in `ref`.

## Details

The citation inputs can be vectors, data.frames, or a mix. For matching,
each input requires at least one vector containing one of the three
standard, consistent publication identifiers (PMID, PMCID, or DOI).
Columns in data.frames are identified by name (case-insensitive, e.g.
'PMID' or 'pmid').

When both inputs are data.frames, `match_citations()` will match on all
ID types present in both inputs, returning matches from the highest
priority ID type (priority: PMID \> PMCID \> DOI \> Scopus EID). It also
tells the user what ID columns are identified and the order used in a
message.

When no matching citation from `ref` can be found, `NA` is returned.
