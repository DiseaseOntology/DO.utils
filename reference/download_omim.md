# Download OMIM Files

Downloads one or more specified files from [OMIM](https://www.omim.org/)
(the Online catalog of Mendelian Inheritance in Man). `mim2gene.txt` is
the only file accessible without an API key from OMIM.

## Usage

``` r
download_omim(omim_file, dest_dir, api_key = NULL, on_failure = "abort", ...)
```

## Arguments

- omim_file:

  A character vector of one or more OMIM file name(s).

- dest_dir:

  Path to directory where files will be saved; all OMIM files are saved
  in .txt format and consist of tab-separated values with a copyright
  header.

- api_key:

  Your API key from OMIM, as a string; required if downloading any file
  other than 'mim2gene.txt'.

  [Register for downloads](https://www.omim.org/downloads) at OMIM.

- on_failure:

  A string indicating how to handle download failure:

  - "warn" - produce a warning; includes exit codes for debugging

  - "abort" - abort execution

  - "list_failed" - list URLs that failed (*output format differs*, see
    `Value`)

  - "warn-list_failed" - combination of "warn" and "list_failed"

  - "skip" - do nothing

- ...:

  Additional arguments passed on to
  [`utils::download.file()`](https://rdrr.io/r/utils/download.file.html).

## Value

Unless `on_failure` includes "list_failed", the successfully downloaded
`dest_file`(s); otherwise, a 2-vector list where `successful` =
`dest_file`(s) and `failed` = `url`(s).

## See also

[`read_omim()`](https://allenbaron.github.io/DO.utils/reference/read_omim.md)
to read downloaded files as tibble/data.frames.
