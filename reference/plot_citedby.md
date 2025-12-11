# Plot Publications Citing DO by Year

Plots the count of publications that cite the Human Disease Ontology by
year.

## Usage

``` r
plot_citedby(
  data_file = "data/citedby/DO_citedby.csv",
  out_dir = "graphics/website",
  color_set = c(Article = "#4C3E45", `Clinical Trial` = "#B9964B", Book = "#83C85F",
    Conference = "#95B1BB", Review = "#934FBB", Other = "#C45055", Retracted = "#000000"),
  retracted = "warn",
  w = 6,
  h = 3.15
)
```

## Arguments

- data_file:

  The path to the file containing the list of publications citing the
  DO, as a string.

- out_dir:

  The directory where the plot `"DO_cited_by_count.png"` should be
  saved, as a string. If `NULL` the plot is not saved to disk.

- color_set:

  A named set of 7 colors, one for each of the possible publication
  types (see Colors section) or the prefix of the color set to use from
  [DO_colors](https://allenbaron.github.io/DO.utils/reference/DO_colors.md),
  as a character vector.

- retracted:

  How to handle retracted publications, as a string. One of:

  - "warn" (default) to drop them with a warning.

  - "include" to display them in the plot in their own category.

  - "other" to include them in the "Other" category.

- w:

  The width of the plot in inches, as numeric.

- h:

  The height of the plot in inches, as numeric.

## Data Preparation

To prepare data, execute `scripts/citedby_full_procedure.R`.

## Colors

If specifying a color set manually, one color should be included for
each of the following publication types: "Article", "Book", "Clinical
Trial", "Conference", "Review", "Other", "Retracted". "Other" serves as
a catch all category (generally a small subset of otherwise
uncategorized publications).

Sets available in
[DO_colors](https://allenbaron.github.io/DO.utils/reference/DO_colors.md)
include: "sat" (saturated), "accent1", "accent2", and "orange". The
default and light versions of the specified color set will be used to
generate a gradient.
