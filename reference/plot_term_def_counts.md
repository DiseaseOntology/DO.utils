# Plot DO Term & Definition Counts

Plots the count of *non-obsolete* terms and definitions in the Human
Disease Ontology over time (using data from each release).

## Usage

``` r
plot_term_def_counts(
  release_file = "data/DO_release/DO_release_details.csv",
  counts_file = "data/DO_release/DO_term_def_counts.csv",
  out_dir = "graphics/website",
  w = 8,
  h = 5.6
)
```

## Arguments

- release_file:

  The path to the file containing DO release details, as a string.

- counts_file:

  The path to the file containing the count of DO terms and definitions
  by release, as a string.

- out_dir:

  The directory where the plot `"DO_term_def_count.png"` should be
  saved, as a string. If `NULL` the plot is not saved to disk.

- w:

  The width of the plot in inches, as numeric.

- h:

  The height of the plot in inches, as numeric.

## Data Preparation

To prepare data, execute:

1.  `scripts/DO_term_def_counts.R` - requires installation of a python
    virtual environment using `scripts/install_reticulate_python.R`.

2.  `scripts/DO_release_details.R`
