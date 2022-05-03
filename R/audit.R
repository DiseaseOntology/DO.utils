

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

# Singular version of `init_domain_repo`
init_domain_repo_ <- function(domain, check_robots, delay) {
    assert_string(domain)
    assert_scalar_logical(check_robots)
    assert_numeric(delay)

    repo <- list(
        domain = domain,
        robots = NULL,
        delay = NULL,
        had_error = NULL,
        wait_until = NULL,
        n_429 = 0L,
        url_total = NULL,
        url_remaining = NULL,
        last = NULL
    )

    if (check_robots) {
        repo$last <- try_robots_txt(domain)

        if (repo$last$status == "Success") {
            # parse robots + robxp
            repo$robots <- list(
                txt = repo$last$content[[1]],
                robxp = spiderbar::robxp(repo$last$content[[1]])
            )
            repo$robots$delay = extract_robots_delay(repo$robots$robxp)
            repo$delay <- dplyr::coalesce(repo$robots$delay, delay)
        } else if (repo$last$status_code == 404) {
            repo$robots <- NA
            repo$delay <- delay
        } else if (repo$last$status_code %in% c(429, 503)) {
            repo$robots <- "retry"
            repo$wait_until <- extract_retry_after(
                repo$last$response[[1]],
                format = "datetime"
            )

            if (is.na(repo$wait_until)) {
                repo$wait_until <- Sys.time() + delay
            }
        } else {
            repo$last <- validate_url(domain)

            if (repo$last$status_code == "Success" ||
                    repo$last$status_code %in% c(429, 503)) {
                repo$robots <- "retry"
                repo$wait_until <- extract_retry_after(
                    repo$last$response[[1]],
                    format = "datetime"
                )

                if (is.na(repo$wait_until)) {
                    repo$wait_until <- Sys.time() + delay
                }
            } else {
                repo$had_error <- TRUE
            }
        }
    }

    class(repo) <- c("domain_repo", class(repo))
    repo
}
