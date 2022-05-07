# match_citations message is correct for all possible matches in proper order

    Code
      match_citations(df1, df2)
    Message <simpleMessage>
      Matching by types: 
      * pmid
      * pmcid
      * doi
      * scopus_eid
    Output
      [1] 1 2 3 4

# match_citations message is correct for all possible matches in reverse order

    Code
      match_citations(df1, df2)
    Message <simpleMessage>
      Matching by types: 
      * pmid
      * pmcid
      * doi
      * scopus_eid
    Output
      [1] 1 2 3 4

# match_citations message is correct for the highest priority single match

    Code
      match_citations(df1, df2)
    Message <simpleMessage>
      Matching by type: pmid
    Output
      [1]  1 NA NA NA

# match_citations message is correct when 1st match is dropped

    Code
      match_citations(df1, df2)
    Message <simpleMessage>
      Matching by type: pmcid
    Output
      [1] NA  2 NA NA

