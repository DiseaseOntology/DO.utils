# extract_pmid elink_list method works

    Code
      extract_pmid(elink_list)
    Condition <no_result>
      Warning:
      No PubMed 'cited by' results, discarded: i 35504280 i 88888888
    Output
      $`34755882`
      [1] "35380658" "33594374"
      

---

    Code
      extract_pmid(elink_list, no_result = "message")
    Message <no_result>
      No PubMed 'cited by' results, discarded: i 35504280 i 88888888
    Output
      $`34755882`
      [1] "35380658" "33594374"
      

