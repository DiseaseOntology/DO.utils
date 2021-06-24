library(httr)

# get_do_issues <- function() {
do_issues_url <- "https://api.github.com/repos/DiseaseOntology/HumanDiseaseOntology/issues"

query <- list(
    state = "all",
    sort = "created",
    direction = "asc",
    per_page = 100,
    page = 1
)

do_issue_p1 <- httr::GET(
    do_issues_url,
    config = accept("application/vnd.github.v3+json"),
    query = query
)

httr::headers(do_issue_p1)$link %>% httr::parsed_content()

body <- httr::content(do_issue_p1, as = "parsed")
