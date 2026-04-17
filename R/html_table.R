#' Build a DataTables-Compatible HTML Table
#'
#' Generates a complete `<table>` HTML string suitable for use with the
#' [DataTables](https://datatables.net/) JavaScript library. Each column is
#' described via `col_spec`, which maps display headers to data columns and
#' optionally to a `data-search` attribute for per-cell searching.
#'
#' @param data A data frame containing the table data.
#' @param col_spec A named list defining each table column. Each name becomes
#'   the `<th>` header text. Each element is a list with:
#'   - `content`: name of the column in `data` used as `<td>` content (may be
#'     pre-rendered HTML).
#'   - `search`: *(optional)* name of the column in `data` used as the
#'     `data-search` attribute value.
#'   - `td_attr`: *(optional)* named list of additional static attributes
#'     applied to every `<td>` in the column.
#' @param tbl_id The `id` attribute for the `<table>` element, as a string.
#' @param tbl_attr A named list of additional attributes for the `<table>`
#'   element (e.g. `list(style = "width:100%;", class = "display")`).
#' @param header_wrap An optional length-2 character vector `c(prefix, suffix)`
#'   wrapping each `<th>` text. Useful for template engine syntax such as
#'   Babel/Jinja2, e.g. `c('{{ _("', '") }}')`.
#' @param indent Either an integer giving the starting indent level for the
#'   `<table>` tag (each level adds two spaces; defaults to `0L`), or an
#'   `html_indent` object as returned by [get_html_indent()] to match the
#'   indentation style of an existing HTML file.
#'
#' @returns A single character string containing the complete table HTML,
#'   with newline-separated lines and consistent indentation.
#'
#' @export
build_datatable_html <- function(data, col_spec, tbl_id = NULL,
                                 tbl_attr = list(), header_wrap = NULL,
                                 indent = 0L) {
    stopifnot(
        is.data.frame(data),
        is.list(col_spec), length(col_spec) > 0, !is.null(names(col_spec)),
        is.null(header_wrap) || (is.character(header_wrap) && length(header_wrap) == 2),
        inherits(indent, "html_indent") || (is.numeric(indent) && length(indent) == 1)
    )

    if (inherits(indent, "html_indent")) {
        i <- function(n) paste0(indent$base, strrep(indent$unit, n))
    } else {
        indent <- as.integer(indent)
        i <- function(n) strrep("  ", indent + n)
    }

    # --- <th> elements ---
    th_text <- names(col_spec)
    if (!is.null(header_wrap)) {
        th_text <- paste0(header_wrap[1], th_text, header_wrap[2])
    }
    th_html <- paste0(i(3), "<th>", th_text, "</th>")

    # --- <tr> rows ---
    row_html <- vapply(
        seq_len(nrow(data)),
        function(row_i) {
            cells <- vapply(
                col_spec,
                function(spec) {
                    content <- data[[spec$content]][row_i]
                    extra_attr <- if (!is.null(spec$td_attr)) spec$td_attr else list()
                    if (!is.null(spec$search)) {
                        search_val <- data[[spec$search]][row_i]
                        extra_attr <- c(list(`data-search` = search_val), extra_attr)
                    }
                    attr_str <- if (length(extra_attr) > 0) {
                        do.call(set_html_attr, extra_attr)
                    } else {
                        ""
                    }
                    paste0(i(3), "<td", attr_str, ">", content, "</td>")
                },
                FUN.VALUE = character(1)
            )
            paste(
                c(paste0(i(2), "<tr>"), unname(cells), paste0(i(2), "</tr>")),
                collapse = "\n"
            )
        },
        FUN.VALUE = character(1)
    )

    # --- assemble table attributes ---
    tbl_attrs <- tbl_attr
    if (!is.null(tbl_id)) tbl_attrs <- c(list(id = tbl_id), tbl_attrs)
    tbl_attr_str <- if (length(tbl_attrs) > 0) do.call(set_html_attr, tbl_attrs) else ""

    # --- assemble full table ---
    paste(
        c(
            paste0(i(0), "<table", tbl_attr_str, ">"),
            paste0(i(1), "<thead>"),
            paste0(i(2), "<tr>"),
            th_html,
            paste0(i(2), "</tr>"),
            paste0(i(1), "</thead>"),
            paste0(i(1), "<tbody>"),
            row_html,
            paste0(i(1), "</tbody>"),
            paste0(i(0), "</table>")
        ),
        collapse = "\n"
    )
}


# Internal functions for HTML tables -------------------------------------

# Add table indentation
add_table_indent <- function(html, indent) {
    increment_key <- c(
        "[ \t]*(<[ /]*table)" = 0,
        "[ \t]*(<[ /]*t(head|body|foot))" = 1,
        "[ \t]*(<[ /]*tr)" = 2,
        "[ \t]*(< *t[hd][^e])" = 3
    )
    indent_key <- purrr::map_chr(
        increment_key,
        ~ paste0(indent$base, strrep(indent$unit, .x), "\\1")
    )

    stringr::str_replace_all(html, indent_key)
}

#' Copy <thead> to <tfoot> for a single <table> in HTML
#' @noRd
copy_thead_to_tfoot <- function(table_html) {
  tfoot <- stringr::str_extract(
    table_html,
    stringr::regex("[ \t]*<thead>.*</thead>", dotall = TRUE)
  ) |>
    stringr::str_replace_all("thead", "tfoot")

  tfoot_exists <- stringr::str_detect(table_html, "<tfoot>")
  if (!tfoot_exists) {
    out <- stringr::str_replace(
      table_html,
      "([ \t]*</table>)",
      paste0(tfoot, "\n\\1")
    )
  } else {
    exist_tfoot_raw <- stringr::str_extract(
      table_html,
      stringr::regex("[ \t]*<tfoot>.*</tfoot>", dotall = TRUE)
    )
    exist_tfoot <- exist_tfoot_raw |>
      stringr::str_split("\n") |>
      unlist()
    min_tab <- min(stringr::str_count(exist_tfoot, "\t"))
    tfoot_show <- stringr::str_remove(
      exist_tfoot,
      paste0("^\t{0,", min_tab, "}|[[:space:]]+$")
    ) |>
      paste0(collapse = "\n")
    cli::cli_alert_info(
      c(
        "<tfoot> already exists in the HTML:",
        i = tfoot_show
      )
    )
    overwrite <- readLines(
      "Would you like to overwrite the existing <tfoot>? [y/n]:  "
    )
    while (!overwrite %in% c("y", "n")) {
      overwrite <- readLines(
        "Please enter 'y' or 'n':  "
      )
    }
    if (overwrite == "n") {
      cli::cli_alert_info("Keeping existing <tfoot>")
    } else {
      out <- stringr::str_replace(
        table_html,
        stringr::regex("[ \t]*<tfoot>.*</tfoot>", dotall = TRUE),
        tfoot
      )
    }
  }

  out
}
