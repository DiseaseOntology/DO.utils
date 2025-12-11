# Convert Ontology Files to OFN

Converts ontology files to OWL functional syntax (OFN) using ROBOT
(<http://robot.obolibrary.org/>).

## Usage

``` r
convert_to_ofn(path, out_path = NULL, gzip = FALSE, .robot_path = NULL)
```

## Arguments

- path:

  The path to the ontology file, as a string.

- out_path:

  The path of the desired output OFN output file, as a string. If `NULL`
  (default), a temporary file will be created.

- gzip:

  Whether output should be gzipped, as a boolean (default: `FALSE`). If
  `TRUE`, the output file will end with a '.gz' file extension.

- .robot_path:

  The path to a ROBOT executable or .jar file, as a string. When `NULL`
  (default), if a system ROBOT executable is available it will be used,
  otherwise an error will be signaled.

  **NOTE:** `DO.utils` caches the last ROBOT used for future use.

## Value

The output path of the OFN file, invisibly.
