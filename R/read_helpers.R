#' Preprocesses omim.org Data (INTERNAL)
#'
#' Determines if OMIM data is from "Download as" button on a Phenotypic Series
#' site or copy-pasted, and pre-processes file to fix headers and retain only
#' data.
#'
#' @inheritParams read_omim
#' @inheritDotParams read_delim_auto -show_col_types
#'
#' @returns A tibble with data corresponding to the OMIM PS table, with the
#' PS phenotype included _ONLY_ when source was the "Download as" button;
#' copy and pasting the PS table with the PS header is not allowed.
#'
#' @keywords internal
preprocess_omim_dl <- function(file, ...) {
    raw <- readr::read_lines(file, n_max = 10)
    is_official <- any(
        stringr::str_detect(raw, stringr::coll("copyright", ignore_case = TRUE))
    )

    if (is_official) {
        # include PS header as first line
        ps_line_num <- stringr::str_detect(raw, "PS[0-9]+") %>%
            which() %>%
            utils::tail(1)
        blank_lines <- which(raw == "")
        include <- blank_lines[blank_lines > ps_line_num][1] - ps_line_num - 2

        ps <- purrr::set_names(
            stringr::str_split(raw[ps_line_num], " +- +")[[1]][1:2],
            c("Phenotype", "Phenotype MIM number")
        )

        df <- read_delim_auto(
            file,
            skip = ps_line_num,
            n_max = include,
            name_repair = "minimal",
            trim_ws = TRUE,
            col_types = readr::cols(.default = readr::col_character()),
            ...
        ) %>%
            dplyr::add_row(!!!ps)
    } else {
        df <- read_delim_auto(
            file,
            name_repair = "minimal",
            trim_ws = TRUE,
            col_types = readr::cols(.default = readr::col_character()),
            ...
        )

        # fix headers
        header_in_df <- purrr::pmap_lgl(df, function(...) sum(is.na(c(...))) > 3)
        if (sum(header_in_df) > 3) {
            rlang::abort("Copied OMIM data in unexpected format.")
        }
        if (any(header_in_df)) {
            headers <- purrr::map2_chr(
                names(df),
                unlist(df[1, ]),
                ~ collapse_to_string(.x, .y, delim = " ", na.rm = TRUE)
            )

            missing_header <- headers == ""
            if (any(missing_header)) {
                new_header <- purrr::map2_chr(
                    unlist(df[2, ]),
                    unlist(df[3, ]),
                    ~ collapse_to_string(.x, .y, delim = " ", na.rm = TRUE)
                )
                new_header <- new_header[!is.na(new_header)]

                if (sum(missing_header) != length(new_header)) {
                    rlang::abort("Problem fixing missing headers...")
                }
                headers[missing_header] <- new_header
            }

            names(df) <- headers
            df <- df[!header_in_df, ]
        }
    }

    names(df) <- names(df) %>%
        make.names(unique = TRUE) %>%
        stringr::str_replace_all("\\.", "_") %>%
        stringr::str_to_lower()

    df
}
