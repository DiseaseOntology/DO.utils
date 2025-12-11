# Generify OBO Classes/Properties (INTERNAL)

[`format_axiom()`](https://allenbaron.github.io/DO.utils/reference/format_axiom.md)
internal helper that replaces URI/CURIEs defined by OBO ontologies with
namespace-prefixed descriptions/types, where defined internally, or just
the namespace. Will not modify non-OBO class URI/CURIEs.

## Usage

``` r
generify_obo(x)
```

## Arguments

- x:

  Complete logical axioms in OWL functional syntax, as a character
  vector.

## Internally-defined OBO namespace-types

The following namespaces have 'namespace:types' defined by this
function:

- CL: 'CL:cell'

- CHEBI: 'CHEBI:chemical'

- DISDRIV: 'DISDRIV:disease_driver'

- DOID: 'DOID:disease'

- FOODON: 'FOODON:food_material'

- ECO: 'ECO:evidence'

- GENO: 'GENO:inheritance_pattern'

- NCBITaxon: 'NCBITaxon:organism'

- MIM: 'MIM:susceptibility'

- OMIM: 'MIM:susceptibility'

- UBERON: 'UBERON:anatomy'

- UPHENO: 'UPHENO:phenotype'

- SO: 'SO:sequence'

- SYMP: 'SYMP:symptom'

- TRANS: 'TRANS:pathogen_transmission'

Ontologies with multiple object types used by the DO (e.g. HP, onset or
phenotype), that are imported via DISDRIV (e.g. ExO), or used for their
properties (e.g. RO) are not defined internally and will be formatted
namespace only. .
