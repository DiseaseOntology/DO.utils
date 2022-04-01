

# audit_DO_url() helpers --------------------------------------------------

#' Initiate a Domain Repository
#'
#' Create a repository to store HTTP information for each domain.
#'
#' @param domain The domain(s) for which to instantiate repositories, as a
#'     character vector.
#' @param check_robots Whether to check the robots.txt for the specified
#'     domain(s), as a boolean vector with one value per domain. This utilizes
#'     [robotstxt::robotstxt()].
#' @inheritDotParams robotstxt::robotstxt
#'
#' @keywords internal
init_domain_repo <- function(domain, check_robots, delay = 5, ...) {
    assertthat::assert_that(length(check_robots) == length(domain))

    if (length(domain) == 1) {
        return(init_domain_repo_(domain, check_robots))
    }

    repo <- purrr::map2(
        .x = domain,
        .y = check_robots,
        ~ instantiate_domain_repo_(.x, .y, ...)
    ) %>%
        stats::setNames(domain)

    repo
}

init_domain_repo_ <- function(domain, check_robots, delay = 5L, ...) {
    # assert_string(domain)
    # assert_scalar_logical(check_robots)

    if (check_robots) {
        repo <- list(
            domain = domain,
            robotstxt = robotstxt::robotstxt(domain = domain, ...)
        )
        repo <- append(
            repo,
            list(
                last_delay = get_delay(repo$rt, default = delay),
                last_status = NULL,
                url_total = NULL,
                url_remaining = NULL,
                wait_until = NULL
            )
        )
    } else {
        repo <- list(
            domain = domain,
            rt = NULL,
            last_delay = delay,
            last_status = NULL,
            url_total = NULL,
            url_remaining = NULL,
            wait_until = NULL
        )
    }

    class(repo) <- c("domain_repo", class(repo))
    repo
}
