# Notes on Use of Scopus Search

# Query inputs

- As of Nov-2021 only `REFTITLE({DO_pubs titles})` works to identify articles citing DO publications.
- Parentheses in queries may not be honored and are not mentioned as having an effect on order of operations (see `docs/Scopus-Search_API-Tips.pdf`).
- `PUBYEAR` in query should be entered without parentheses; `PUBYEAR = 2022` or `PUBYEAR > 2022`, not `PUBYEAR(2022)`


# Inputs Allowed to GET request

These parameters are listed in `docs/Scopus-Search_API-Details.pdf`. Most affect the data returned. The only exception I'm aware of is `date`.

- `date` limits the date range of desired publications.
    - Example: `date = 2022` is equivalent to `AND PUBYEAR = 2022` added in the query field (of `rscopus::scopus_search()`).
    - Use of `date` here is _FASTER_ than use of `PUBYEAR` in query and _MORE FLEXIBLE_
