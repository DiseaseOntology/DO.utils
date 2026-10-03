# build_datatable_html() tests --------------------------------------------

test_that("build_datatable_html() creates a table identifiable by DataTables", {
  df <- data.frame(
    disease = c("cancer", "asthma"),
    doid = c("DOID:162", "DOID:2841")
  )
  col_spec <- list(
    Disease = list(content = "disease"),
    DOID = list(content = "doid")
  )

  html <- build_datatable_html(
    df,
    col_spec,
    tbl_id = "disease-table",
    tbl_attr = list(class = "display")
  )

  # DataTables targets a table via a unique id and/or class selector
  expect_match(html, '<table id="disease-table" class="display">')

  # header and body row cell counts must match col_spec length for
  # DataTables to correctly map columns
  lines <- strsplit(html, "\n")[[1]]
  expect_length(grep("^\\s*<th>", lines), length(col_spec))
  tbody_lines <- lines[
    (grep("<tbody>", lines) + 1):(grep("</tbody>", lines) - 1)
  ]
  expect_length(grep("^\\s*<td", tbody_lines), nrow(df) * length(col_spec))

  expect_equal(
    html,
    paste(
      '<table id="disease-table" class="display">',
      "  <thead>",
      "    <tr>",
      "      <th>Disease</th>",
      "      <th>DOID</th>",
      "    </tr>",
      "  </thead>",
      "  <tbody>",
      "    <tr>",
      "      <td>cancer</td>",
      "      <td>DOID:162</td>",
      "    </tr>",
      "    <tr>",
      "      <td>asthma</td>",
      "      <td>DOID:2841</td>",
      "    </tr>",
      "  </tbody>",
      "</table>",
      sep = "\n"
    )
  )
})


test_that("build_datatable_html() replace_na works", {
  df <- data.frame(disease = c("cancer", NA), doid = c("DOID:162", "DOID:2841"))
  col_spec <- list(
    Disease = list(content = "disease"),
    DOID = list(content = "doid")
  )

  expect1 <- paste(
    "<table>",
    "  <thead>",
    "    <tr>",
    "      <th>Disease</th>",
    "      <th>DOID</th>",
    "    </tr>",
    "  </thead>",
    "  <tbody>",
    "    <tr>",
    "      <td>cancer</td>",
    "      <td>DOID:162</td>",
    "    </tr>",
    "    <tr>",
    "      <td></td>",
    "      <td>DOID:2841</td>",
    "    </tr>",
    "  </tbody>",
    "</table>",
    sep = "\n"
  )
  expect_equal(build_datatable_html(df, col_spec), expect1)

  expect2 <- paste(
    "<table>",
    "  <thead>",
    "    <tr>",
    "      <th>Disease</th>",
    "      <th>DOID</th>",
    "    </tr>",
    "  </thead>",
    "  <tbody>",
    "    <tr>",
    "      <td>cancer</td>",
    "      <td>DOID:162</td>",
    "    </tr>",
    "    <tr>",
    "      <td>NA</td>",
    "      <td>DOID:2841</td>",
    "    </tr>",
    "  </tbody>",
    "</table>",
    sep = "\n"
  )
  expect_equal(
    build_datatable_html(df, col_spec, replace_na = NULL),
    expect2
  )
})
