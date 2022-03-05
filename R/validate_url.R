#' Validate URLs in DO
#'
#' Test URLS in the Human Disease Ontology to determine if they are still valid
#' and return information to promote correction/curation.
#'
#' @inheritParams read_doid_edit
#'
#' @export
validate_DO_url <- function(DO_repo) {
    df <- read_doid_edit(DO_repo) %>%
        extract_doid_url() %>%
        dplyr::mutate(tmp = robotstxt:::parse_url(.data$url)) %>%
        tidyr::unnest(.data$tmp, keep_empty = TRUE)

    dom <- unique(df$domain)
    robots <-
    #     # check robots.txt from domains ...
    #     # change settings for different domains
    #     # check if URLs exist
    #
    #     RCurl::url.exists(
}


#' A Reference Class to represent URL validation results by domain.
#'
#' @field domain The URL of the web domain, as a string.
#' @field domain_children The full URLs belonging to this domain, as a character
#'     vector.
#' @field robots_txt The information from robots.txt for this domain, as
#'     created by `use_robotstxt()` (see [robotstxt::robotstxt()] for details
#'     about `robotstxt` objects).
#' @field delay The latest crawl-delay (in seconds) specified by this domain,
#'     as an integer.
#' @field last_status The HTTP status of the last URL accessed in this domain,
#'     as an integer.
#' @field access_history

web_domain <- methods::setRefClass(
    "web_domain",
    fields = list(
        domain = "character",
        domain_children = "data.frame",
        delay = "character",
        last_status = "numeric",
        robots_txt = "robotstxt",
        access_history = "list"
    ),
    methods = list(
        use_robotstxt = function(.user_agent = pkg_user_agent, ...) {
            "Access & parse robots.txt information for this web domain."
            robots_txt <<- robotstxt::robotstxt(
                domain,
                user_agent = .user_agent,
                ...
            )
            delay <<- get_delay(robots_txt)
        },
        children_allowed = function(.user_agent = pkg_user_agent, ...) {
            if (class(robots_txt) != "robotstxt") {
                use_robotstxt(...)
            }
            domain_children <<- domain_children %>%
                dplyr::mutate(
                    allowed = robots_txt$check(.data$url)
                )
        },
        return_tidy = function(...) {
            # after running everything make this return a tidy data.frame with
            #   all results; may not be necessary if I keep all results in
            #   'domain_children'
        }
    )
)


respectfully_validate_url <- function(url, ...) {
    robot_res <- check_robotstxt(url, ...)

    # group URLs by domain
    # validate by domain,
    # ??? do not test URLs with unapproved paths?
    # 1. respect crawl delay
    # 2. retry ???x on error, checking for 429 errors (what response = delay?)
}

validate_url <- function(url, config = httr::user_agent(pkg_user_agent),
                         include_raw_resp = TRUE, ...) {

    # handle URLs where server cannot be reached
    resp <- try_url(url, config = config, ...)
    res_df <- parse_http_resp(resp, include_raw = include_raw_resp)

    res_df
}



try_url <- function(url, config = httr::user_agent(pkg_user_agent), ...) {
    tryCatch(
        httr::HEAD(url, config = config, ...),
        condition = function(c) list(url = url, exception = c)
    )
}


parse_http_resp <- function(resp, include_raw = TRUE) {
    # handle R errors (not http errors)
    if("exception" %in% names(resp)) {

        exc <- class(resp$exception)
        std_type <- c("message", "warning", "error")

        resp_tidy <- tibble::tibble(
            url = resp$url,
            # set status to common R type where possible for consistency
            status = dplyr::if_else(
                any(std_type %in% class(exc)),
                paste0("R", std_type[std_type %in% class(exc)]),
                paste0("R", exc[1])
            ),
            status_code = NA_integer_,
            exception = conditionMessage(exc)
        )
    } else {
        last_url <- resp %>%
            .$all_headers %>%
            purrr::map_chr(~ .x$headers$location) %>%
            tail(1)
        resp_tidy <- tibble::tibble(
            valid = !httr::http_error(resp),
            status_code = httr::status_code(resp),
            # include redirect URL, where applicable
            redirect_url = dplyr::if_else(
                is.null(last_url) || last_url == resp$url,
                NULL,
                last_url
            )
        )

        if (isTRUE(include_raw)) {
            resp_tidy$response <- resp
        }
    }
    resp_tidy
}


