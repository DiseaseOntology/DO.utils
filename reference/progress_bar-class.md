# Progress Bar (RefClass)

Instantiate a base R-style progress bar and advances it by one 'tick' at
a time.

## Fields

- `bar`:

  A `progress_bar` object created by
  [`utils::txtProgressBar()`](https://rdrr.io/r/utils/txtProgressBar.html).

- `tick`:

  An integer to track progress.

## Methods

- `advance(...)`:

  Advances progress bar one tick.

- `initialize(n, ...)`:

  Initializes a progress bar when called with
  `progress_bar$new(n, ...)`.

  `n`

  :   : Total number of ticks.

  `...`

  :   : Arguments passed on to
      [`utils::txtProgressBar()`](https://rdrr.io/r/utils/txtProgressBar.html).
