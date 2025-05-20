#' Create a Curation Template
#'
#' Create a curation template in a Google Sheet, optionally including data.
#'
#' @inheritParams googlesheets4::range_write
#' @param .data Data to add to the curation sheet. If `NULL` (default), an empty
#' curation sheet will be created.
#' @param sheet (OPTIONAL) The sheet name, as a string. If `NULL` (default), the
#' sheet name will default to "curation-" with today's date appended (formatted
#' as "%Y%m%d"; see [format.Date()]).
#' @param nrow The number of rows to create in the curation template when
#' `.data = NULL` (default: `50`).
#'
#' @returns The Google Sheet info (`ss`), as a [googlesheets4::sheets_id].
#'
#' @export
curation_template <- function(.data = NULL, ss = NULL, sheet = NULL, ...,
                              nrow = 50) {
  if (is.null(.data)) {
    curation_template_empty(ss = ss, sheet = sheet, nrow = nrow, ...)
  } else {
    UseMethod("curation_template")
  }
}


curation_template_empty <- function(ss = NULL, sheet = NULL, ..., nrow = 50) {
  val <- rep(NA, nrow)

  # inspired by https://stackoverflow.com/a/60495352/6938922
  cur_df <- tibble::as_tibble(rlang::rep_named(curation_cols, list(val)))

  class(cur_df) <- c("curation_template", class(cur_df))
  if (is.null(sheet)) sheet <- paste0("curation-", format(Sys.Date(), "%Y%m%d"))
  gs_info <- googlesheets4::write_sheet(cur_df, ss, sheet)

  if (is.null(ss)) ss <- gs_info

  # add curation template data validation
  gs_range <- spreadsheet_range(cur_df, "annotation", sheet = sheet)
  range_add_dropdown(ss, gs_range, values = .curation_opts$header)

  # freeze first two columns
  googlesheets4::with_gs4_quiet(
    googlesheets4:::sheet_freeze(ss, sheet = sheet, ncol = 2)
  )

  invisible(gs_info)
}

#' @param with_existing Whether to include a disease in the curation template if
#' it already exists in the DO (default: `FALSE`).
#' @param next_id (OPTIONAL) The "next" unused ID _in your assigned range_ to
#' use for a new disease in the curation template, as a valid DOID string (see
#' [is_valid_doid()] for details). Additional IDs will be generated in sequence,
#' as needed. If `NULL` (default), the curation template will be generated with
#' placeholder IDs in sequence (starting with "tempID_1").
#'
#' **NOTE:** There is no guarantee that the IDs generated will remain within
#' your assigned range or be unique in the ontology. _Review them carefully!_
#' @inheritParams format_doid
#'
#' @export
curation_template.omim_inventory <- function(.data, ss = NULL,
                                             sheet = NULL, ...,
                                             with_existing = FALSE,
                                             bg_color = NULL,
                                             next_id = NULL, strict = TRUE) {
  # ignore included entries & susceptibilities, note which
  wip <- .data |>
    dplyr::filter(
      !stringr::str_detect(.data$phenotype, "^\\{"),
      !stringr::str_detect(
        .data$phenotype,
        stringr::regex("susceptibility", ignore_case = TRUE)
      )
    )

  new_n <- sum(!wip$exists)
  if (new_n == 0 && !with_existing) {
    rlang::inform(
      c(
        "No curation template created. All OMIM phenotypes already exist in DO.",
        i = "Set `with_existing = TRUE` to create a template with existing diseases."
      )
    )
    return(invisible(NULL))
  }

  # generate new IDs, if needed
  if (is.null(next_id)) {
    new_ids <- paste0("tempID_", seq_len(new_n))
  } else {
    next_id <- format_doid(next_id, strict = strict)
    prefix <- stringr::str_extract(next_id, "^DOID:0+")
    lui_num <- stringr::str_remove(next_id, "^DOID:0+") |>
      seq(length.out = new_n)
    new_ids <- paste0(prefix, lui_num)
  }

  if (with_existing) {
    cols_retain <- c(
      "omim", "phenotype", "inheritance", "geno_inheritance",
      "mapping_type", "exists", "doid", "do_label", "do_dep", "multimaps"
    )
    revised <- wip |>
      dplyr::select(dplyr::all_of(cols_retain)) |>
      add_mapping_type_header(.data$mapping_type) |>
      dplyr::rename(
        mapping_value = "omim",
        geno_value = "geno_inheritance",
            dep_value = "do_dep"
      ) |>
      dplyr::mutate(
        id = replace(.data$doid, !.data$exists, new_ids),
        label_value = dplyr::if_else(
          !is.na(.data$do_label),
          .data$do_label,
          .data$phenotype
        ),
          # add links for existing DOIDs & OMIM phenotypes
        label_links = build_hyperlink(
          stringr::str_remove(.data$doid, "^DOID:"),
          "DOID",
          as = "gs",
          text = NULL,
          preserve = "url"
        ),
        mapping_links = build_hyperlink(
          stringr::str_remove(.data$mapping_value, "^O?MIM:"),
          "MIM",
          as = "gs",
          text = NULL,
          preserve = "url"
        ),
        label_curation_notes = dplyr::if_else(
          .data$exists,
          "exists",
          NA_character_
        ),
        # add OMIM phenotype label to curation notes, with multimap
        #   info, if any (for reference)
        mapping_curation_notes = dplyr::if_else(
          !is.na(.data$multimaps),
          paste0(
            .data$phenotype, " (multimaps: ", .data$multimaps, ")"
          ),
          .data$phenotype
        ),
        # set remaining headers
        label_header = "label",
        geno_header = dplyr::if_else(
          !is.na(.data$geno_inheritance),
          "sc axiom: inheritance",
          NA_character_
        ),
        dep_header = dplyr::if_else(
          .data$do_dep,
          "deprecate",
          NA_character_
        )
      ) |>
      dplyr::select(
        "id", dplyr::matches("_(value|header|links|curation_notes)")
      )
  } else {
    cols_retain <- c(
      "omim", "phenotype", "inheritance", "geno_inheritance", "mapping_type"
    )
    revised <- wip |>
      dplyr::filter(!exists) |>
      dplyr::select(dplyr::all_of(cols_retain)) |>
      dplyr::rename(
        label_value = "phenotype",
        mapping_value = "omim",
        geno_value = "geno_inheritance"
      ) |>
      dplyr::mutate(
        id = new_ids,
        # add links for OMIM phenotypes
        mapping_links = build_hyperlink(
          stringr::str_remove(.data$mapping_value, "^O?MIM:"),
          "MIM",
          as = "gs",
          text = NULL,
          preserve = "url"
        ),
        # set remaining headers
        label_header = "label",
        geno_header = dplyr::if_else(
          !is.na(.data$geno_inheritance),
          "sc axiom: inheritance",
          NA_character_
        )
      )
  }

  # reformat to fit curation template
  revised_nm <- names(revised)
  sets <- nms[stringr::str_detect(revised_nm, "header")] |>
    stringr::str_remove("_header")
  gs_data <- purrr::map(
    sets,
    function(.s) {
      dplyr::select(revised_df, "id", dplyr::starts_with(.s)) |>
        dplyr::rename_with(.fn = ~ rename_header_value(.x, .s)) |>
        # dplyr::filter(
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    }
  ) |>
    dplyr::bind_rows() |>
    append_empty_col(curation_cols, order = TRUE)

  out <-
  attr(out, "ignored") <- dplyr::anti_join(.data, wip)
}
  #   renamed <- dplyr::select(
  #     .data,
  #     id = "doid",
  #     label = "do_label",

  #   omim_recode <- c(
  #   "label" = "phenotype",
  #   "skos mapping(s): exact" = "omim",
  #   "sc axiom: inheritance" = "geno_inheritance"
  # )
  # revised <- .data |>
  #   dplyr::select(

  #   )



# c(
#   "omim" = value, "location", "phenotype" = curation_notes, "phenotype_mim_number", "inheritance",
#   "phenotype_mapping_key", "gene_locus", "gene_locus_mim_number",
#   "geno_inheritance" = value (need header), "mapping_type" = pseudo-header, "exists", "doid" = id, "do_label" = label,
#   "do_dep" = psuedo-header, "multimaps"
# )


# helpers --------------------------------------------------------------------

# define expected columns for curation template (in order)
curation_cols <- c(
  "id", "annotation", "value", "remove", "curation_notes", "links",
  "action_notes", "status"
)


#' Calculate a Spreadsheet Range
#'
#' Calculate a range for a spreadsheet program (Google Sheets or Excel).
#'
#' @inheritParams curation_template
#' @param .data A tibble.
#' @param .col The column to use for the range, as a string.
#' @param rows (OPTIONAL) The rows to use for the range, either as a continous
#' integer vector or as a string (i.e. "1:10"). If `NULL` (default), the entire
#' column will be used.
#' @param n_header The number of header rows to skip (default: `1`).
#'
#' @keywords internal
spreadsheet_range <- function(.data, .col, sheet = NULL, rows = NULL,
                                 n_header = 1) {
  col_letter <- LETTERS[which(names(.data) == .col)]
  if (length(col_letter) != 1) {
    rlange::abort("Exactly one column must be specified in `.col`")
  }

  if (is.null(rows)) {
    row_ends <- c(1, nrow(.data)) + n_header
  } else if (is.numeric(rows)) {
    # check one continuous range
    collapsed_range <- to_range(rows, sep = c(",", ":"))
    if (stringr::str_count(collapsed_range, "[,:]") > 1) {
      rlang::abort(
        c("`rows` must be one continuous range", x = collapsed_range)
      )
    }
    row_ends <- c(rows[1], tail(rows, 1)) + n_header
  } else {
    row_ends <- as.integer(stringr::str_split(row_ends, ":")[[1]]) + n_header
  }

  range <- paste0(col_letter, row_ends, collapse = ":")
  if (!is.null(sheet)) {
    range <- paste0(sheet, "!", range)
  }
  range
}


z <- c("http://www.w3.org/2004/02/skos/core#exactMatch",
  "http://www.w3.org/2004/02/skos/core#broadMatch",
  "http://www.geneontology.org/formats/oboInOwl#hasDbXref",
  "skos:narrowMatch",
  "oboInOwl:hasDbXref"
)

# will split if multiple mapping types in one cell --> output _may_ be longer!!
add_mapping_type_header <- function(.df, mapping_type) {
  out <- lengthen_col(.df, mapping_type)

  header_pos <- stringdist::amatch(
    out$mapping_type,
    .curation_opts$template,
    method = "lcs",
    maxDist = 50 # minimum for URI matching, identified through trial & error
  )
  if (!identical(is.na(out$mapping_type), is.na(header_pos))) {
    rlang::warn("NAs introduced by coercion of mapping_type to robot header")
  }

  out <- dplyr::mutate(
    out,
    mapping_header = .curation_opts$header[header_pos] |>
      tidyr::replace_na("skos mapping(s): exact")
  )

  out
}

reformat_revised_omim <- function(revised_df) {
  nms <- names(revised_df)
  sets <- nms[stringr::str_detect(nms, "header")] |>
    stringr::str_remove("_header")
  out <- purrr::map(
    sets,
    function(.s) {
      dplyr::select(revised_df, "id", dplyr::starts_with(.s)) |>
        dplyr::rename_with(.fn = ~ rename_header_value(.x, .s)) |>
        # dplyr::filter(
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    }
  ) |>
    dplyr::bind_rows()

  out
}


# rename_header_value <- function(col_nm, set) {
#   dplyr::case_when(
#     stringr::str_detect(col_nm, "header$") ~ "annotation",
#       stringr::str_detect(col_nm, "value$") ~ "value",
#     TRUE ~ col_nm
#   )
# }

rename_header_value <- function(col_nm, set) {
  stringr::str_remove(col_nm, paste0(set, "_")) |>
    dplyr::recode("header" = "annotation")
}


# set color formatting for first disease line
# gs_col2rgb <- function(color) {
#   gs_color_df <- tibble::as_tibble(
#     index = 1:length(gs_color),
#     nm = names(gs_color),
#     hex = gs_color
#   )
#   input_type <- dplyr::case_when(
#     color %in% names(gs_color) ~ "nm",
#     color %in% gs_color ~ "hex"
#     TRUE ~ NA_character_
#   )
#   color_undef <- is.na(input_type)
#   if (any(color_undef)) {
#     rlang::abort(
#       c(
#         ,
#         purrr::set_names(
#           paste0(color[color_undef], " [", which(color_undef), "]"),
#           rep("x", sum(color_undef)
#         )
#       )
#     )
#   }

#   color_nm_hex <- color

#    gs_hex <- gs_nm <- color
#    gs_nm <- replace(gs_nm, by_hex, names(gs_color[gs_color == color[by_hex]))
#    gs_hex <- replace(gs_hex, by_nm, gs_color[by_nm])


#    gs_hex <- replace(gs_hex, by_nm, gs_color[names(gs_color) == color[by_nm])
#    gs_nm <- gs_nm[co

#   out <- col2rgb(gs_hex)
#   colnames(out) <- gs_nm
#   out
# }






# Example usage
# color_input <- c("red", "#00FF00", "blue")
# gs_col2rgb(color_input)
