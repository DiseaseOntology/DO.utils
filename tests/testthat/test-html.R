# build_hyperlink() tests -------------------------------------------------

test_that("build_hyperlink() example works", {
    expect_equal(
        build_hyperlink(
            x = "DiseaseOntology",
            url = "github",
            as = "html",
            text = "A hyperlink!"
        ),
        "<a href=\"https://github.com/DiseaseOntology\">A hyperlink!</a>"
    )
})

test_that("build_hyperlink() works with vectors", {
    gs_expect <- c(
        '=HYPERLINK(\"http://purl.obolibrary.org/obo/DOID_4\", \"DOID:4\")',
        '=HYPERLINK(\"https://meshb.nlm.nih.gov/record/ui?ui=D004194\", \"MESH:D004194")'
    )
    class(gs_expect) <- c("googlesheets4_formula", "vctrs_vctr")

    expect_equal(
        build_hyperlink(
            x = c("4", "D004194"),
            url = c("DOID", "MESH"),
            as = "gs",
            text = c("DOID:4", "MESH:D004194")
        ),
        gs_expect
    )

    expect_equal(
        build_hyperlink(
            x = c("4", "D004194"),
            url = c("DOID", "https://meshb.nlm.nih.gov/record/ui"),
            as = "gs",
            text = c("DOID:4", "MESH:D004194"),
            sep = c("_", "?ui=")
        ),
        gs_expect
    )
})


# html_in_rows() tests ----------------------------------------------------

test_that("html_in_rows() works", {
    expect_equal(
        html_in_rows(c("<b>Hi!</b>", "", "", "What's", "your", "name")),
        c(
            "    <tr>",
            "      <td><b>Hi!</b></td>",
            "      <td></td>",
            "      <td></td>",
            "    </tr>",
            "    <tr>",
            "      <td>What's</td>",
            "      <td>your</td>",
            "      <td>name</td>",
            "    </tr>"
        )
    )
    expect_equal(
        html_in_rows(
            c("<b>Hi!</b>", "", "", "What's", "your", "name"),
            row_attr = c(class = "hiyah"),
            cell_attr = c(class = "special")
        ),
        c(
            "    <tr class=\"hiyah\">",
            "      <td class=\"special\"><b>Hi!</b></td>",
            "      <td class=\"special\"></td>",
            "      <td class=\"special\"></td>",
            "    </tr>",
            "    <tr class=\"hiyah\">",
            "      <td class=\"special\">What's</td>",
            "      <td class=\"special\">your</td>",
            "      <td class=\"special\">name</td>",
            "    </tr>"
        )
    )
    expect_equal(
        html_in_rows(
            c("<b>Hi!</b>", "", "", "What's", "your", "name"),
            row_attr = c(class = "hiyah"),
            cell_attr = c(class = "special"),
            per_row = 2
        ),
        c(
            "    <tr class=\"hiyah\">",
            "      <td class=\"special\"><b>Hi!</b></td>",
            "      <td class=\"special\"></td>",
            "    </tr>",
            "    <tr class=\"hiyah\">",
            "      <td class=\"special\"></td>",
            "      <td class=\"special\">What's</td>",
            "    </tr>",
            "    <tr class=\"hiyah\">",
            "      <td class=\"special\">your</td>",
            "      <td class=\"special\">name</td>",
            "    </tr>"
        )
    )
    expect_equal(
        html_in_rows(
            c("<b>Hi!</b>", "", "", "What's", "your", "name"),
            row_attr = c(class = "hiyah"),
            cell_attr = c(class = "special"),
            per_row = 2
        ),
        c(
            "    <tr class=\"hiyah\">",
            "      <td class=\"special\"><b>Hi!</b></td>",
            "      <td class=\"special\"></td>",
            "    </tr>",
            "    <tr class=\"hiyah\">",
            "      <td class=\"special\"></td>",
            "      <td class=\"special\">What's</td>",
            "    </tr>",
            "    <tr class=\"hiyah\">",
            "      <td class=\"special\">your</td>",
            "      <td class=\"special\">name</td>",
            "    </tr>"
        )
    )
    expect_equal(
        html_in_rows(c("<b>Hi!</b>", "", "", "What's", "your", "name"), indent_n = 4),
        c(
            "        <tr>",
            "          <td><b>Hi!</b></td>",
            "          <td></td>",
            "          <td></td>",
            "        </tr>",
            "        <tr>",
            "          <td>What's</td>",
            "          <td>your</td>",
            "          <td>name</td>",
            "        </tr>"
        )
    )
})
