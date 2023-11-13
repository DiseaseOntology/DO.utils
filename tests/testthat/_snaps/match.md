# match_citations(): works for all possible w/matches (df only)

    Code
      match_citations(df_all, df_NA)
    Message
      Matching by types: 
      * pmid
      * pmcid
      * doi
      * scopus_eid
    Output
      [1] 1 2 3 4

---

    Code
      match_citations(df_all, rev(df_NA))
    Message
      Matching by types: 
      * pmid
      * pmcid
      * doi
      * scopus_eid
    Output
      [1] 1 2 3 4

# match_citations(): works for single, similar type w/matches

    Code
      match_citations(df_all[2], df_NA[2])
    Message
      Matching by type: pmcid
    Output
      [1] NA  2 NA NA

---

    Code
      match_citations(df_all[1:2], df_NA[2])
    Message
      Matching by type: pmcid
    Output
      [1] NA  2 NA NA

---

    Code
      match_citations(df_all[[3]], df_NA[[3]])
    Output
      [1] NA NA  3 NA

