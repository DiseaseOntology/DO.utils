# Plot Definition Sources

Plots the count of definition sources for *non-obsolete* terms the Human
Disease Ontology.

## Usage

``` r
plot_def_src(DO_repo, out_dir = "graphics/website", w = 8, h = 5.6)
```

## Arguments

- DO_repo:

  The local path to the `HumanDiseaseOntology` repo, as a string.

- out_dir:

  The directory where the plot "DO_def_src.png" should be saved, as a
  string. If `NULL` the plot is not saved to disk.

- w:

  The width of the plot in inches, as numeric.

- h:

  The height of the plot in inches, as numeric.

## Data Preparation

If this plot will be added to disease-ontology.org, the latest release
of the `HumanDiseaseOntology` Github repo should be checked out prior to
running this function.
