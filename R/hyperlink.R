#' Build Hyperlinks
#'
#' Builds hyperlinks for Google Sheets, Excel, or HTML by appending a value to
#' the end of a base url.
#'
#' @inheritParams append_to_url
#' @inheritParams format_hyperlink
#' @param text _(OPTIONAL)_ The text to display for each link, as a character
#' vector. The default uses `x` as the text. If `NULL`, the full URL will serve
#' as the text.  If a string, the value will be used for the text of each
#' hyperlink.
#' @param preserve The value to return when `url` is `NA`, as a string. One of
#' "url" or "text" (default). Note that the default for `build_hyperlink()`
#' is opposite the default of [format_hyperlink()] because `text` is provided by
#' default.
#'
#' @seealso Functions used internally: [append_to_url()] and
#'     [format_hyperlink()].
#'
#' @examples
#' build_hyperlink(
#'     x = "DiseaseOntology",
#'     url = "github",
#'     as = "html",
#'     text = "A hyperlink!"
#' )
#'
#' # create CURIE links by passing local identifiers as `x` and prefixes as `url`
#' build_hyperlink(
#'     x = c("4", "D004194"),
#'     url = c("DOID", "MESH"),
#'     as = "gs",
#'     text = c("DOID:4", "MESH:D004194")
#' )
#'
#' # provide internal URL names or direct URLs to append to
#' # BE SURE to use `preserve = 'url'` when text is `NA`.
#' build_hyperlink(
#'     x = c("4", "fakeID"),
#'     url = c("DOID", "https://madeup.url.com"),
#'     as = "gs",
#'     text = c("DOID:4", NA),
#'     sep = c("_", "/"),
#'     preserve = "url"
#' )
#'
#' @family Hyperlink functions
#' @export
build_hyperlink <- function(x, url, as, ..., sep = "", text = x,
                            preserve = "text") {
    full_url <- append_to_url(x, url, sep)
    hyperlink <- format_hyperlink(
        full_url,
        as = as,
        ...,
        text = text,
        preserve = preserve
    )

    hyperlink
}
