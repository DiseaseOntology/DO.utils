# Codebook

This codebook details the variables and their expected values from sources used by this package to obtain 'cited by' data, as returned by functions used in this package which access the specified API.

# PubMed (Entrez API)

## Original Variables

- `esummary_id`: the PubMed ID of a publication (returned from `citedby_pmid()`); redundant with `.attrs` and `ArticleIds` > `ArticleId` > `Value`, where `IdType` = "pubmed"
- `PubDate`
- `EPubDate`
- `Source`: abbreviated journal name
- `Authors`
- `LastAuthor`
- `Title`: title of article
- `SortTitle`: lowercase title of article without stop words (e.g. 'the')
- `Volume`
- `Pages`
- `Lang`
- `NlmUniqueID`
- `ESSN`
- `PubType`: the type of publication; for a list, see https://pubmed.ncbi.nlm.nih.gov/help/#publication-types; for detailed descriptions see [MeSH Publication Types with Scope Notes](https://www.nlm.nih.gov/mesh/pubtypes.html)
- `RecordStatus`
- `PubStatus`
- `ArticleIds`: a list of `ArticleId` list items with 3 vectors named `IdType`, `IdTypeN`, and `Value`
    - `IdType`: doi, eid, mid, pii, pmc, pmcid, pubmed, rid
    - `IdTypeN`: a number that always corresponds to the `IdType`; _meaning/purpose unclear_
        - doi = 3; eid, mid, pmc, & rid = 8; pii = 4, pmcid = 5, pubmed = 1
        - **NOTE:** eid = pmid; it is _NOT_ the same as the Scopus EID
    - `Value`: the identifier of `IdType` for the publication
- `History`: a list of `PubMedPubDate` list items with 2 vectors named `PubStatus` and `Date`
    - `PubStatus`: "received", "accepted", "entrez", "medline" (possibly others)
    - `Date`: the date corresponding to the specified status
- `References`
- `Attributes`
- `PmcRefCount`
- `FullJournalName`
- `ELocationID`
- `DocType`
- `SrcContribList`
- `DocContribList`
- `SortPubDate`
- `SortFirstAuthor`: first author of the manuscript formatted as `<LastName> <First & Middle Initial>` (example: Schriml LM)
- `.attrs`: the PubMed ID of a publication; redundant with `esummary_id` and `ArticleIds` > `ArticleId` > `Value`, where `IdType` = "pubmed"
- `Issue`
- `ISSN`


## Additional Variables after Processing

- Variables (potentially) hoisted from ArticleIds:
    - `doi`
    - `eid`
    - `mid`
    - `pmcid`: corresponds with original codebook `IdType` = "pmc"
    - `rid`
    - `pii`
    - `pmcid_long`: corresponds with original codebook `IdType` = "pmcid"
    - `pmid`: corresponds with original codebook `IdType` = "pubmed"


# Scopus (Scopus Search API)
