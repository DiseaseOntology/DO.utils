# Format Logical Axioms

Format logical axioms in the style of Protege. Input axioms must be in
OWL functional syntax.

## Usage

``` r
format_axiom(
  x,
  property_df = NULL,
  generify_obo = FALSE,
  placeholders = c("<<<", ">>>", "%%%", "@@@"),
  max_phrases = 15L
)
```

## Arguments

- x:

  Complete logical axioms in OWL functional syntax, as a character
  vector.

- property_df:

  \[Optional\] A data.frame consisting of relevant annotation and object
  properties that may be used to make properties more readable in
  axioms. If provided, data.frame should include two columns:

  - `property`: Each property's URI.

  - `label`: Each property's `rdfs:label` (or equivalent).

  *See 'Formatting Options' for details.*

- generify_obo:

  Whether to make OBO ontology classes and properties generic, as `TRUE`
  or `FALSE` (default). *See 'Formatting Options' for* *details.*

- placeholders:

  A set of 4 strings that will be used internally to preserve axiom
  parentheses `[1:2]`, phrase spacing `[3]`, and complete phrases `[4]`,
  as a character vector. These should not generally need to be edited
  and are only exposed in case of conflicts within axioms.

- max_phrases:

  The maximum number of phrases to update in an axiom, as an integer.
  This protects against an infinite while loops and would only need to
  be edited if one or more `Object*` OWL functional phrases are returned
  unformatted.

## Formatting Options

`format_axiom()` will always rearrange equivalent class and subclass of
logical axioms from OWL functional syntax to a more readable form,
similar to Protege. However, classes and properties in axioms will be
returned as URIs or CURIEs (as formatted in `x`). This may be desirable
for further programmatic examination but will still be difficult for a
human to read. To make them more readable, the `property_df` and
`generify_obo` arguments can be used.

`property_df` is used to replace OBO ontology property URIs or CURIEs
with namespace-prefixed labels: e.g. `obo:RO_0004026` would become
`RO:'disease has location'`. Non-OBO properties will not be modified.

`generify_obo` replaces individual OBO URIs or CURIEs with either
namespace-prefixed types, e.g. `obo:UBERON_0001032` would become
`UBERON:anatomy`, or namespace only. This format simplifies axioms
making basic analysis of axiom patterns easier. *See the documentation
for*
*[`generify_obo()`](https://allenbaron.github.io/DO.utils/reference/generify_obo.md)
for details.*
