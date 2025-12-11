# Download File(s) from the Internet

`download_file()` is a vectorized version of
[`utils::download.file()`](https://rdrr.io/r/utils/download.file.html)
that does not rely on specifying the "libcurl" method for vectorization.

## Usage

``` r
download_file(url, dest_file, on_failure = "warn", ...)
```

## Arguments

- url:

  A character vector naming the URL of resource(s) to be downloaded.

- dest_file:

  A character vector with the file path(s) where downloaded file(s) will
  be saved.

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
