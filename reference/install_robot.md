# Install OBO Foundry ROBOT Tool (Mac/Linux ONLY)

Installs latest ROBOT and robot.jar files to default system path
(/usr/local/bin). Only tested for Macs but also will likely work for
Linux.

## Usage

``` r
install_robot(...)
```

## Arguments

- ...:

  arguments passed on to
  [`utils::download.file()`](https://rdrr.io/r/utils/download.file.html);
  should not be needed

## Value

The version of robot.jar, if successful, otherwise a warning indicating
where failures occurred along with their non-zero integer codes. See
documentation of
[`utils::download.file()`](https://rdrr.io/r/utils/download.file.html)
(for download of robot/robot.jar) and
[`system2()`](https://rdrr.io/r/base/system2.html) (for making robot
executable; uses system `chmod`) under "Value" for more details.
