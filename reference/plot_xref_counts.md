# Plot Xref Counts

Plots the count of cross-references by source in the Human Disease
Ontology.

## Usage

``` r
plot_xref_counts(
  DO_repo,
  out_dir = "graphics/website",
  w = 8,
  h = 5.6,
  .robot_path = NULL
)
```

## Arguments

- DO_repo:

  The local path to the HumanDiseaseOntology repo, as a string.

- out_dir:

  The directory where the plot `"DO_xref_count.png"` should be saved, as
  a string. If `NULL` the plot is not saved to disk.

- w:

  The width of the plot in inches, as numeric.

- h:

  The height of the plot in inches, as numeric.

- .robot_path:

  The path to a ROBOT executable or .jar file, as a string. When `NULL`
  (default), if a system ROBOT executable is available it will be used,
  otherwise an error will be signaled.

  **NOTE:** `DO.utils` caches the last ROBOT used for future use.
