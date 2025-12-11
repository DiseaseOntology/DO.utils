# Extract Equivalent Class Axioms

Extract `owl:equivalentClass` axioms from the doid-edit.owl file.

## Usage

``` r
extract_eq_axiom(DO_repo)
```

## Arguments

- DO_repo:

  The local path to the `HumanDiseaseOntology` repo, as a string.

## Value

Equivalent class axioms in OWL functional format, as a character vector.

## See also

Other `extract_*_axiom` functions:
[`extract_class_axiom()`](https://allenbaron.github.io/DO.utils/reference/extract_class_axiom.md),
[`extract_subclass_axiom()`](https://allenbaron.github.io/DO.utils/reference/extract_subclass_axiom.md)
