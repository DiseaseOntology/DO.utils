# Namespace Prefixes

Prefixes for namespaces, primarily sourced from
[`ROBOT`](http://robot.obolibrary.org/), that cover most uses in OBO
Foundry ontologies, with slight modification to match the prefixes as
used in the Human Disease Ontology and a few additional prefixes for
cross-references and DO-related SPARQL query support. These prefixes
*should* be up-to-date with the latest OBO Foundry ontologies
(https://github.com/ontodev/robot/issues/51).

## Usage

``` r
ns_prefix

not_obo_prefix

obo_prefix

obo_ont_prefix

obo_prop_prefix
```

## Format

Character vectors of namespaces named by their corresponding prefix:

`ns_prefix`: Superset of all 546 prefix/namespaces listed below.

`not_obo_prefix`: 21 prefix/namespaces outside but commonly used in the
OBO Foundry (e.g. dc = "http://purl.org/dc/elements/1.1/"), or in
federated SPARQL queries with the DO
(e.g. up = "http://purl.uniprot.org/core/").

`obo_prefix`: Superset of all 525 OBO Foundry prefix/namespaces,
including those listed below and, additionally, including the general
`obo` prefix.

`obo_ont_prefix`: 261 OBO Foundry ontology *primary* prefix/namespaces
(e.g. AAO = "http://purl.obolibrary.org/obo/AAO\_").

`obo_prop_prefix`: `oboInOwl` (along with the common prefix variant
`oio`) and 262 OBO Foundry ontology *property* namespaces (*these may
not all be in actual use*)
(e.g. aao = "http://purl.obolibrary.org/obo/aao#")

## Source

`ROBOT export-prefixes` (v1.9.8) via `data-raw/ns_prefix.R`; last
updated 2025-12-09.
