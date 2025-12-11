# Instantiate a DO Repository

Instantiate a `DOrepo` object from the
[pyDOID](https://github.com/allenbaron/pyDOID) package (powered by
[reticulate](https://github.com/rstudio/reticulate)).

## Usage

``` r
DOrepo(path)
```

## Arguments

- path:

  The local path to the
  [HumanDiseaseOntology](https://github.com/DiseaseOntology/HumanDiseaseOntology)
  git repository, as a string.

## Classes & Methods

The `DOrepo` class inherits the methods & subclasses of the
[GitPython.Repo](https://gitpython.readthedocs.io/en/stable/tutorial.html#meet-the-repo-type)
class, along with the following:

- `DOrepo$tag_iterate(function, start = [str], end = [str])`: Iterate
  through DO releases (tags) from `start` to `end` executing a
  `function` at each. `start` and `end` are *optional*; if unspecified
  the first or last release/tag of the repo will be assumed.

- `DOrepo$capture_head()`: Capture the current `head` of the repo;
  useful for restoring state after git checkout(s). *USE*: If assigned
  to variable `x`, restore state with `x$checkout()`.

- `DOrepo$doid` and `DOrepo$doid_merged`: Access the doid.owl and
  doid-merged.owl files of the repository. These inherit from the
  `pyDOID.owl.xml` class, see
  [`owl_xml()`](https://allenbaron.github.io/DO.utils/reference/owl_xml.md)
  for methods. *NOTE* that, unlike
  [`owl_xml()`](https://allenbaron.github.io/DO.utils/reference/owl_xml.md),
  doid.owl and doid-merged.owl are not loaded into memory upon
  instantiation of `DOrepo`, but are loaded automatically when the first
  `$query()` is executed or manually via `$load()`.

## See also

Other pyDOID classes:
[`owl_xml()`](https://allenbaron.github.io/DO.utils/reference/owl_xml.md)
