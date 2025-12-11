# Execute Robot Commands

Light wrapper for OBO Foundry ROBOT program (OBO Foundry). See [ROBOT
documentation](http://robot.obolibrary.org/) for information about
subcommands and arguments.

## Usage

``` r
robot(..., .robot_path = NULL)
```

## Arguments

- ...:

  Command(s) passed on to ROBOT, including subcommand(s), either as a
  single string or named strings, where the name corresponds to the long
  or short form of the option and the value being the option value.
  Where options do not use a value, use "". Subcommands should be
  unnamed.

  **Examples:**

  - Subcommands: `"query"`, `"export"`

  - Common options:

    - `"--input doid.owl"` or `input = "doid.owl"` (named argument).

    - `"-o result.owl"` (i.e. `--output`) or `o = "result.owl"` (named
      argument).

    - `"--remove-annotations"` (option of `annotate`) or
      `"remove-annotations" = ""` (named argument form).

- .robot_path:

  The path to a ROBOT executable or .jar file, as a string. When `NULL`
  (default), if a system ROBOT executable is available it will be used,
  otherwise an error will be signaled.

  **NOTE:** `DO.utils` caches the last ROBOT used for future use.

## ROBOT Setup

- Requires installation of Java (supported versions are listed under the
  "Getting Started" section at http://robot.obolibrary.org/).

- Can use OBO Foundry ROBOT tool installed on the system path. This may
  be achieved with
  [`install_robot()`](https://allenbaron.github.io/DO.utils/reference/install_robot.md)
  or as described at http://robot.obolibrary.org/.

- Can also use a standalone robot.jar file (`.robot_path` must be
  specified) in which case it is executed with `java -Xmx10G -jar` (10
  GB memory).

- On the first use of each R session, the ROBOT executable/.jar
  specified will be tested. It must succeed prior to any command
  execution. On success, `robot()` will announce the version & path
  being used. If at any time a new `.robot_path` is specified, this
  previous version will be replaced with a new announcement.

## Citation

R.C. Jackson, J.P. Balhoff, E. Douglass, N.L. Harris, C.J. Mungall, and
J.A. Overton. ROBOT: A tool for automating ontology workflows. BMC
Bioinformatics, vol. 20, July 2019.
