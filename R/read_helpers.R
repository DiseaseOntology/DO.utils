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
    .lines <- readr::read_lines(file)
    is_official <- any(
        stringr::str_detect(.lines, stringr::coll("copyright", ignore_case = TRUE))
    )

    if (is_official) {
        # determine location of the table & PS info
        header_n <- stringr::str_detect(
            .lines,
            "Location.+Phenotype"
        ) %>%
            which()
        blank_.lines <- which(stringr::str_detect(.lines, "^[[:space:]]*$"))
        include <- blank_.lines[blank_.lines > header_n][1] - header_n - 1

        ps <- purrr::set_names(
            stringr::str_split(.lines[header_n - 1], " +- +")[[1]][1:2],
            c("Phenotype", "Phenotype MIM number")
        )

        df <- read_delim_auto(
            file,
            skip = header_n - 1,
            n_max = include,
            name_repair = "minimal",
            trim_ws = TRUE,
            col_types = readr::cols(.default = readr::col_character()),
            ...
        ) %>%
            dplyr::add_row(!!!ps, .before = 1)
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
