# Extract 'Subclass Of' Axioms

Extract `owl:subClassOf` axioms from the doid-edit.owl file.

## Usage

``` r
extract_subclass_axiom(DO_repo)
```

## Arguments

- DO_repo:

  The local path to the `HumanDiseaseOntology` repo, as a string.

## Value

'Subclass Of' axioms in OWL functional format, as a character vector.

## See also

Other `extract_*_axiom` functions:
[`extract_class_axiom()`](https://allenbaron.github.io/DO.utils/reference/extract_class_axiom.md),
[`extract_eq_axiom()`](https://allenbaron.github.io/DO.utils/reference/extract_eq_axiom.md)
