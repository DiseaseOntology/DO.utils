# Extract Class Axioms

Extract `owl:equivalentClass` and `owl:subClassOf` axioms from the
doid-edit.owl file.

## Usage

``` r
extract_class_axiom(DO_repo)
```

## Arguments

- DO_repo:

  The local path to the `HumanDiseaseOntology` repo, as a string.

## Value

A list of two character vectors (`eq` and `subclass`) containing axioms
in OWL functional format.

## See also

Other `extract_*_axiom` functions:
[`extract_eq_axiom()`](https://allenbaron.github.io/DO.utils/reference/extract_eq_axiom.md),
[`extract_subclass_axiom()`](https://allenbaron.github.io/DO.utils/reference/extract_subclass_axiom.md)
