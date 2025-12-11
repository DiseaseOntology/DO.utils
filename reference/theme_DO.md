# DO Theme for Stats Plots

The default ggplot2 theme used for statistical plots uploaded to
disease-ontology.org. This *only* manages the base plot style and does
*not* incorporate DO's color scheme.

## Usage

``` r
theme_DO(
  base_size = 11,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22
)
```

## Arguments

- base_size:

  base font size, given in pts.

- base_family:

  base font family

- base_line_size:

  base size for line elements

- base_rect_size:

  base size for rect elements

## Background

For more information refer to ggplot2's theme documentation.
