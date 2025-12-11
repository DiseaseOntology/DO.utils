# Namespace Prefixes

Prefixes for namespaces, primarily sourced from
[ROBOT](http://robot.obolibrary.org/), that cover most uses in OBO
Foundry ontologies, with slight modification to match the prefixes as
used in the Human Disease Ontology and a few additional prefixes for
cross-references and DO-related SPARQL query support.

## Usage

``` r
ns_prefix

obo_prefix

obo_prop_prefix

not_obo_prefix
```

## Format

Character vectors of namespaces named by their corresponding prefix:

`ns_prefix`: Superset of all 533 objects listed below.

`obo_prefix`: 255 *standard* OBO Foundry namespaces
(e.g. AAO = "http://purl.obolibrary.org/obo/AAO\_").

`obo_prop_prefix`: 255 OBO Foundry *property* namespaces; *these may not
all be in actual use* (e.g. aao = "http://purl.obolibrary.org/obo/aao#")

`not_obo_prefix`: 21 namespaces outside but commonly used in the OBO
Foundry (e.g. dc = "http://purl.org/dc/elements/1.1/"), or in federated
SPARQL queries with the DO (e.g. up = "http://purl.uniprot.org/core/").

## Source

`ROBOT` (v1.9.4) via data-raw/ns_prefix.R; last updated 2023-06-12.
