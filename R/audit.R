

# audit_DO_url() helpers --------------------------------------------------

#' Initiate a Domain Repository
#'
#' Create a `domain_repo` list to store, update, and use HTTP information for
#' specified domain(s). _Designed to be used with URL audits._
#'
#' @param domain The domain(s) for which to instantiate repositories, as a
#'     character vector.
#' @param check_robots Whether to check the robots.txt for the specified
#'     domain(s), as a boolean vector with one value (to be applied to all
#'     domains) or one value per domain.
#' @param delay The default delay to use, as a positive integer vector with
#'     one value (to be applied to all domains) or one value per domain. If
#'     `check_robots = TRUE` this delay will be overridden by a domain's
#'     robots.txt delay (if present).
#'
#' @returns
#' For a single domain, a `domain_repo`; otherwise a list of `domain_repo`
#' objects.
#'
#' @export
init_domain_repo <- function(domain, check_robots, delay) {

    if (dplyr::n_distinct(domain) != length(domain)) {
        rlang::abort(
            message = "`domain` must not include duplicates"
        )
    }
    assertthat::assert_that(
        length(check_robots) == 1 || length(check_robots) == length(domain),
        is.logical(check_robots),
        length(delay) == 1 || length(delay) == length(domain),
        is.numeric(delay) && all(delay >= 0)
    )

    if (length(delay) == 1) {
        delay <- rep(delay, length(domain))
    }

    if (length(domain) == 1) {
        return(init_domain_repo_(domain, check_robots, delay))
    }

    repo <- purrr::pmap(
        list(domain, check_robots, delay),
        function(dom, cr, d) init_domain_repo_(dom, cr, d)
    ) %>%
        purrr::set_names(domain)

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


robots_allowed <- function(x, url, agent = DO_agent(), ...) {
    UseMethod("robots_allowed")
}

#' @export
robots_allowed.list <- function(x, url, agent = DO_agent(),
                                no_domain_repo = "warning", ...) {
    no_domain_repo <- match.arg(no_domain_repo, c("warning", "error"))

    x_class <- purrr::map_chr(x, ~ class(.x)[1])
    if (!all(x_class == "domain_repo")) {
        rlang::abort(
            message = "All elements of `x` must be `domain_repo` objects."
        )
    }

    url_df <- tibble::tibble(
        # use index to ensure output matches input in case of duplicate URLs
        .idx = 1:length(url),
        .url = url,
        .domain = parse_url(.url)$domain,
        has_dr = .domain %in% purrr::map_chr(x, get_domain_name)
    )

    if (any(!url_df$has_dr)) {
        url_no_dr <- dplyr::filter(url_df, !has_dr)
        dom_no_dr <- unique(url_no_dr$.domain)
        rlang::signal(
            class = no_domain_repo,
            message = c(
                paste(
                    "`x` does not contain a domain_repo for",
                    nrow(url_no_dr), "URLs in:"
                ),
                purrr::set_names(dom_no_dr, rep("*", length(dom_no_dr)))
            )
        )
    }

    no_dr <- dplyr::filter(url_df, !has_dr) %>%
        dplyr::mutate(allowed = NA)
    w_dr <- url_df %>%
        dplyr::filter(has_dr)

    if (nrow(w_dr) > 0) {
        w_dr <- w_dr %>%
            dplyr::group_by(.domain) %>%
            dplyr::mutate(
                allowed = robots_allowed(
                    access_domain_repo(x, unique(.domain)),
                    .url,
                    agent
                )
            )  %>%
            dplyr::ungroup()
    }

    out <- dplyr::bind_rows(no_dr, w_dr) %>%
        dplyr::arrange(.idx)

    out$allowed
}

#' @export
robots_allowed.domain_repo <- function(x, url, agent = DO_agent(), ...) {
    if (class(x$robots[["robxp"]]) != "robxp") {
        rlang::warn(
            class = "robots_unavailable",
            message = c(
                "No robots.txt info in `x`: cannot determine if accessing URLs is allowed",
                "*" = x$domain
            )
        )
        return(rep(NA, length(url)))
    }

    spiderbar::can_fetch(
        obj = x$robots$robxp,
        path = url,
        user_agent = agent
    )
}

get_domain_name <- function(domain_repo) {
    domain_repo$domain
}

access_domain_repo <- function(x, domain) {
    x[purrr::map_chr(x, get_domain_name) == domain]
}
