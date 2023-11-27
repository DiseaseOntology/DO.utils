#' Preprocesses omim.org Data (INTERNAL)
#'
#' Determines if OMIM data is from "Download as" button on a Phenotypic Series
#' site or copy-pasted, and pre-processes file to fix headers and retain only
#' data.
#'
#' @inheritDotParams read_delim_auto -show_col_types
#'
#' @noRd
preprocess_omim_dl <- function(file, ...) {
    .lines <- readr::read_lines(file)
    is_official <- any(
        stringr::str_detect(
            .lines,
            stringr::regex("copyright.*omim", ignore_case = TRUE)
        )
    )

    if (is_official) {
        # determine official download type: search, PS, or PS_titles
        dl_type <- stringr::str_extract(
            .lines[1],
            stringr::regex(
                "search|phenotypic series( titles)?",
                ignore_case = TRUE
            )
        ) %>%
            stringr::str_to_lower() %>%
            stringr::str_replace_all(
                c("phenotypic series" = "PS", " " = "_")
            )

        # determine location of the table & read
        header_n <- identify_omim_header_row(.lines)
        blank_.lines <- which(stringr::str_detect(.lines, "^[[:space:]]*$"))
        include <- blank_.lines[blank_.lines > header_n][1] - header_n - 1

        df <- read_delim_auto(
            file,
            skip = header_n - 1,
            n_max = include,
            name_repair = "minimal",
            trim_ws = TRUE,
            col_types = readr::cols(.default = readr::col_character()),
            ...
        )
        attr(df, "omim_official") <- TRUE

        if (dl_type == "PS") {
            ps <- purrr::set_names(
                stringr::str_split(.lines[header_n - 1], " +- +")[[1]][1:2],
                c("Phenotype", "Phenotype MIM number")
            )
            df <- dplyr::add_row(df, !!!ps, .before = 1)
        }
    } else {
        dl_type <- NA
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
            attr(df, "omim_official") <- FALSE
        }
    }

    names(df) <- names(df) %>%
        make.names(unique = TRUE) %>%
        stringr::str_replace_all(c("[._]+" = "_", "_$" = "")) %>%
        stringr::str_to_lower()

    if (!is.na(dl_type)) {
        attr(df, "omim_type") <- dl_type
    } else {
        attr(df, "omim_type") <- "generic"
    }

    df
}

identify_omim_header_row <- function(.lines) {
    dl_stmt <- stringr::str_detect(.lines, "Downloaded")
    tab_separated <- stringr::str_count(.lines, "\t") > 0

    header_n <- which(tab_separated & !dl_stmt)[1]
    header_n
}
