# Documentation Style Guide

## File Organization
Functions should be grouped in files based on the action they perform (verb, e.g. `extract`,
`read`, etc.). Exceptions are allowed when a compelling reason can be identified which makes code easier to read, find, and/or debug.

Functions used internally only as "helpers" for a given function (and of little to no significance otherwise) should be in a `{verb}_helpers.R` file (e.g. `extract_doid_url()` is in "extract.R", it's internal function `has_doid_url()` is in "extract_helpers.R").


## General
Most text should be written in sentence case, use proper punctuation, and end in a period.


## NEWS
1. Should include a "Dependendencies" section whenever dependencies change.
    - Added dependencies should be prefaced with the dependency type (italicized), package name, and the reason it was added. _Example:_ "_Suggests_ tidygraph, which is required for `format_subtree()`.")
    - Removed dependendencies should be written in the same style but prefaced with "No longer". _Example:_ "No longer _Suggests_ tidygraph because ...")
2. Should identify new functionality in a "New" section header _any_ time a brand new general functionality is introduced.
3. Should include an "Updates" section header if there is a "New" section header under which all updates to existing functionality should be described.
4. Should list all new **exported** functions, and only exported functions unless there is a compelling reason to list an internal function.
5. Should include section (sub)headers for each general functionality area if more than one is new or updated.
6. Should _NOT_ be written in the style of a git commit, or from the developer's perspective.
    - For general functionality:
        1. Use active voice to describe what a user can do with the changes, preferably in a single, introductory sentence. _Example:_ "Create a text-based subtree/hierarchy."
        2. Include additional text that provides context after describing the functions that provide said functionality.
    - For functions, begin with the function name and then describe what it does, following the style in Functions > Description.
7. Can use a bulleted list to improve quick scanning and brevity.


## Functions

### Names
1. Should use words, preferably unabbreviated, separated by "_".
2. Should be as succinct as possible, while conveying clearly what the function does.
3. Should start with a verb, such that they imply taking action.
4. Should end with "(INTERNAL)" for documented functions that are not exported.

#### Examples
Good: `cast_to_string()`


### Title
1. Should _VERY_ succinctly describe what a function does.
2. Should start with an infinitive verb (without "to").
3. Should be capitalized (as is standard for titles).
4. Should end without punctuation.

#### Examples
Good: "Create a String from Inputs"


### Description
Should be formatted in an active voice and immediately describe the primary goal of the function, preferably without naming it (e.g. the description for `sum` would be "Adds numbers together.")


### Arguments

#### Names
1. Should use words, preferably unabbreviated, separated by "_".
2. Should be as succinct as possible, while conveying clearly what it represents.
3. Should be nouns.
4. Should be consistent and ordered consistently across functions, where possible.

#### Descriptions
1. Should be formatted as sentences, with the first word capitalized and with a period at the end, except when ending in bulleted lists.
2. Should, **in the first sentence**, describe clearly what the argument represents at the beginning (in the abstract, for human understanding) and list the acceptable input types at the end.
3. Should _not_ include lengthy descriptions or descriptions relating to multiple arguments. These should be written in a title `@section` and, where necessary, have pointers from applicable arguments (e.g. "(see {section_name})").


## SPARQL

### General
- Use ".rq" extension for SPARQL queries.
- Include a header as the first line of each SPARQL file indicating the file type, intended target, and a brief but detailed description.
    - Format: "# SPARQL query: {target1|target2}, {brief_description}"
    - Example: From DO_branch_counts.rq --> `# SPARQL query: doid.owl, count non-obsolete classes by DO branch`
- Where applicable, queries listing classes should include `?id` and `?label`.
- `COUNT` queries should only be created when counting a list counterpart with an external program (R/python) is **noticeably slow**.
- Exclude obsolete classes from queries.
    - Append an identifier to the file name ("w_obsolete", "w_obs") before the file type of exceptions where obsolete terms are included.
- Identify alternative, non-default, or less frequently used SPARQL queries with an underscore prefix in the file name for queries producing similar data.
    - Example: `_DO_import_w_obsolete.rq`
    - Where exceptions are made and a query is denoted with an underscore in the file name despite appearing by name to be the preferred query, indicate a brief but detailed reason for the alternative status in brackets at the end of the header line.
        - Example: `_DO_branch.rq` --> `# SPARQL query: doid.owl, list all non-obsolete classes by DO branch [slower: ~ 4s]`

### PREFIX Statements
Prefix statements should be included:

1. In order from most to least general
2. Only if a term from a given resource is used.
3. These prefixes should always be included when their terms are used (ordered list):
    - PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    - PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    - PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    - PREFIX owl: <http://www.w3.org/2002/07/owl#>
    - PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    - PREFIX obo: <http://purl.obolibrary.org/obo/>
    - PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>


### Variables
Variables should be used in a consistent manner to ensure the results of multiple queries can be easily combined.

1. The following variables will be used with a fixed meaning:
    - `?class` = `owl:Class` URI
    - `?id` = `oboInOwl:id` value
    - `?label` = `rdfs:label` value (or other human-readable label)
    - `?def` = `obo:IAO_0000115` value (or other human-readable definition)
    - `?parent` = the immediate superclass of a specified `?class`
    - `?namespace` = prefix portion of a URI/CURIE corresponding to a specific resource
    - `?n` = `COUNT` (_optionally_, may be `DISTINCT`)
    - `?xref` = `oboInOwl:hasDbXref` value, when annotated directly to `?class`
    - `?subset` = unique name of subset within resource (_i.e._ not full URI)
    - `?obsolete` = `owl:deprecated` value
2. Class attributes which refer to classes outside of `?class` should be prefixed with the name of the variable the class is found in with an underscore as the separator.
    - Example: The `oboInOwl:id` value of a `?parent` class should be captured in a variable named `?parent_id`.


### Formatting
- Don't add spaces before or after parentheses.
- Break long elements across lines at separators (parentheses | commas > boolean operators).
- Group elements within statements by purpose.
- Separate purpose groups with a blank line.
- Order purpose groups as follows:
    1. Triples limiting query.
    2. Non-triple elements affecting query scope (_e.g._ `FILTER`, `FILTER NOT EXISTS`, `VALUES`)
    3. Elements that return non-limiting data (_e.g._ `BIND`, `OPTIONAL`)
