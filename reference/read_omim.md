# Read OMIM Data

Reads and formats OMIM data copied or manually downloaded from
https://omim.org/, or downloaded with
[`download_omim()`](https://allenbaron.github.io/DO.utils/reference/download_omim.md)
(permission required), and appends columns to speed up subsequent
curation activities. `read_omim()` will attempt to process and correct
headers including fixing multi-line or misarranged column headers, and
will trim whitespace.

## Usage

``` r
read_omim(file, keep_mim = c("#", "%"), ...)
```

## Arguments

- file:

  The path to a file (possibly compressed) with copy/pasted or manually
  downloaded data from https://omim.org/ (see "Manual Input
  Requirements" for details), or downloaded with
  [`download_omim()`](https://allenbaron.github.io/DO.utils/reference/download_omim.md).

- keep_mim:

  \[**OMIM search data only**\] The MIM symbols representing the data
  types to keep, as a character vector, or `NULL` to retain all
  (default: `"#"` and `"%"`).

  The [OMIM](https://www.omim.org/help/faq#1_3) defined MIM symbols are:

  |            |                                    |
  |------------|------------------------------------|
  | MIM symbol | MIM type                           |
  | `*`        | gene                               |
  | `+`        | gene, includes phenotype           |
  | `#`        | phenotype                          |
  | `%`        | phenotype, unknown molecular basis |
  | `^`        | deprecated                         |
  | `none`     | phenotype, suspected/overlap       |

- ...:

  Arguments passed on to
  [`read_delim_auto`](https://allenbaron.github.io/DO.utils/reference/read_delim_auto.md)

  :   

## Value

An `omim_tbl` (tibble) with an `omim` column containing OMIM CURIEs as
formatted in DO xrefs, followed by complete OMIM data arranged as seen
on omim.org for OMIM **entries** (where possible). If the omim.org
"Download as" button was used to download the data, the `omim_tbl` will
be additionally modified based on the download type:

- Search list download: Additional `omim_search` class and `search`
  column containing the search used.

- OMIM phenotypic series titles download: Additional `omim_PS_titles`
  class.

- OMIM phenotypic series download: Additional `omim_PS` class and a row
  representing the OMIM phenotypic series itself.

Output with columns typical OMIM phenotype entries, including `omim_PS`,
will have an additional `geno_inheritance` column containing a best
guess at inheritance from the GENO ontology. This simplifies adding
inheritance as logical subClassOf axioms supporting curation.

NOTE: OMIM phenotypic series on https://omim.org/ include the same data
as entries but column are ordered differently.

## Manual Input Requirements

The `file` with OMIM data copied or manually downloaded must include
column headers at the top and should be tab- or comma-separated. If the
data *is* *copied & pasted from omim.org* either to a spreadsheet
program or to a text file, the data will *likely* be tab-separated. To
improve parsing success, it is recommended that the copy-pasted data
include both the copmlete table and the table type (e.g. "Phenotype-Gene
Relationships"), which is usually directly above the table on a given
omim.org entry. In most casees, there should not be a need to fix data
copy/pasted from OMIM manually prior to reading with `read_omim()`.

## Available downloads

Some data are available for download without an API key. This includes:

1.  The full list of phenotypic series titles
    (<https://www.omim.org/phenotypicSeriesTitles/>)

2.  Any complete phenotypic series, from its PS page
    (https://www.omim.org/phenotypicSeries/{PS number})

    - Example: Achondrogenesis, MIM:PS200600
      (<https://www.omim.org/phenotypicSeries/PS200600>)

3.  OMIM gene entry ID relationships to external gene IDs, mim2gene.txt.

    - From downloads page: <https://www.omim.org/downloads/>

    - Direct: <https://www.omim.org/static/omim/data/mim2gene.txt>

Additional data is available with an approved API key.
