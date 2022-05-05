# extract_pmid elink_list method works

    Code
      extract_pmid(elink_list)
    Warning <no_result>
      Discarded (0 PubMed citedby results)
      * 35504280
      * 88888888
    Output
      $`34755882`
      [1] "35380658" "33594374"
      

---

    Code
      extract_pmid(elink_list, no_result = "message")
    Message <no_result>
      Discarded (0 PubMed citedby results)
      * 35504280
      * 88888888
    Output
      $`34755882`
      [1] "35380658" "33594374"
      

---

    Code
      extract_pmid(elink_list, no_result = "error")
    Error <no_result>
      35504280: 0 PubMed citedby results

