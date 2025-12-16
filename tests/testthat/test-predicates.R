# is_valid_obo() tests ----------------------------------------------------
obo <- c(
    ### may pass, according to allow & ns_type ###
    curie = "DOID:14566",             #1
    curie = "so:has_origin",          #2
    obo_curie = "obo:DOID_14566",     #3
    obo_curie = "obo:so#has_origin",  #4
    uri = "http://purl.obolibrary.org/obo/DOID_0001816",       #5
    uri = "http://purl.obolibrary.org/obo/so#has_origin",      #6
    "<uri>" = "<http://purl.obolibrary.org/obo/CL_0000066>",   #7
    ns.lui = "DOID_14566",            #8
    ns.lui = "so#has_origin",         #9
    ### 9-17 should always be FALSE ###
    # lui
    "0001816",
    "has_origin",
    # wrong separator
    "obo:DOID:14566",
    # bracketed curie
    "<obo:DOID_14566>",
    # spaces
    "obo:DOID_14566 ",
    "obo: DOID_14566",
    # non-OBO
    "foaf:Person",
    "http://xmlns.com/foaf/0.1/Person"
)

test_that("is_valid_obo() works", {
    # curie
    expect_true(is_valid_obo("DOID:14566"))
    expect_true(is_valid_obo("so:has_origin"))
    # obo_curie
    expect_true(is_valid_obo("obo:DOID_14566"))
    expect_true(is_valid_obo("obo:so#has_origin"))
    # uri
    expect_true(is_valid_obo("http://purl.obolibrary.org/obo/DOID_0001816"))
    expect_true(is_valid_obo("http://purl.obolibrary.org/obo/so#has_origin"))
    # <uri>
    expect_true(is_valid_obo("<http://purl.obolibrary.org/obo/CL_0000066>"))
    # ns.lui, must be specifically requested
    expect_false(is_valid_obo("DOID_14566"))
    # multiple inputs
    expect_equal(is_valid_obo(c("DOID:14566", "obo:DOID_14566")), c(TRUE, TRUE))
    expect_equal(is_valid_obo(c("DOID:14566", "DOID_14566")), c(TRUE, FALSE))

    ### OTHER = always FALSE ###
    expect_false(is_valid_obo("0001816")) # no prefix
    expect_false(is_valid_obo("obo:DOID:14566")) # wrong separator
    expect_false(is_valid_obo("<obo:DOID_14566>")) # curie brackets not allowed
      # has space
    expect_false(is_valid_obo("obo:DOID_14566 "))
    expect_false(is_valid_obo("obo: DOID_14566"))

    # ERROR: non-character input
    expect_error(is_valid_obo(1L))
})

test_that("is_valid_obo() allow arg works", {
    expect_is_valid_obo_format <- function(x, allow) {
        res <- setNames(rep(F, length(x)), names(x))
        if ("standard" %in% allow) {
            allow <- union(
                setdiff(allow, "standard"),
                c("curie", "obo_curie", "uri", "<uri>")
            )
        }
        res[names(x) %in% allow] <- T
        expect_equal(
            is_valid_obo(x, allow = allow),
            res,
            info = paste0("allow argument: ", paste(allow, collapse = ", "))
        )
    }

    expect_is_valid_obo_format(obo, allow = "standard")
    expect_is_valid_obo_format(obo, allow = "curie")
    expect_is_valid_obo_format(obo, allow = "obo_curie")
    expect_is_valid_obo_format(obo, allow = "uri")
    expect_is_valid_obo_format(obo, allow = "<uri>")
    expect_is_valid_obo_format(obo, allow = "ns.lui")
    # multiple inputs accepted
    expect_is_valid_obo_format(obo, allow = c("standard", "ns.lui"))
    expect_is_valid_obo_format(obo, allow = c("uri", "<uri>"))
    expect_is_valid_obo_format(obo, allow = c("obo_curie", "ns.lui"))
    expect_is_valid_obo_format(obo, allow = c("curie", "<uri>"))
})

test_that("is_valid_obo(ns_type = 'ont') works", {
    expect_true(is_valid_obo("http://purl.obolibrary.org/obo/DOID_0001816", ns_type = "ont"))
    expect_true(is_valid_obo("<http://purl.obolibrary.org/obo/CL_0000066>", ns_type = "ont"))
    expect_true(is_valid_obo("obo:DOID_14566", ns_type = "ont"))
    expect_true(is_valid_obo("DOID:14566", ns_type = "ont"))
    # only primary ontology namespaces allowed
    expect_false(is_valid_obo("http://purl.obolibrary.org/obo/so#has_origin", ns_type = "ont"))
    expect_false(is_valid_obo("<http://purl.obolibrary.org/obo/so#has_origin>", ns_type = "ont"))
    expect_false(is_valid_obo("obo:so#has_origin", ns_type = "ont"))
    expect_false(is_valid_obo("doid:14566", ns_type = "ont"))
    expect_false(is_valid_obo("0001816", ns_type = "ont")) # no prefix
    expect_false(is_valid_obo("obo:DOID#14566", ns_type = "ont")) # capitalized namespace
    expect_false(is_valid_obo("obo:doid_14566 ", ns_type = "ont")) # wrong separator
    expect_false(is_valid_obo("obo: doid#14566", ns_type = "ont")) # has space
    # non-character input
    expect_error(is_valid_obo(1L, ns_type = "ont"))
})

test_that("is_valid_obo(ns_type = 'prop') works & replaces is_valid_obo_prop()", {
    # these are all the original tests for is_valid_obo_prop() converted to use
    # is_valid_obo() with ns_type = "prop" --> all work the same
    expect_true(is_valid_obo("http://purl.obolibrary.org/obo/so#has_origin", ns_type = "prop"))
    expect_true(is_valid_obo("<http://purl.obolibrary.org/obo/so#has_origin>", ns_type = "prop"))
    expect_true(is_valid_obo("obo:so#has_origin", ns_type = "prop"))
    expect_true(is_valid_obo("doid:14566", ns_type = "prop"))
    # only properties allowed
    expect_false(is_valid_obo("http://purl.obolibrary.org/obo/DOID_0001816", ns_type = "prop"))
    expect_false(is_valid_obo("<http://purl.obolibrary.org/obo/CL_0000066>", ns_type = "prop"))
    expect_false(is_valid_obo("obo:DOID_14566", ns_type = "prop"))
    expect_false(is_valid_obo("DOID:14566", ns_type = "prop"))
    expect_false(is_valid_obo("0001816", ns_type = "prop")) # no prefix
    expect_false(is_valid_obo("obo:DOID#14566", ns_type = "prop")) # capitalized namespace
    expect_false(is_valid_obo("obo:doid_14566 ", ns_type = "prop")) # wrong separator
    expect_false(is_valid_obo("obo: doid#14566", ns_type = "prop")) # has space
    # non-character input
    expect_error(is_valid_obo(1L, ns_type = "prop"))
})


# is_valid_doid() tests ---------------------------------------------------

test_that("is_valid_doid() works", {
    expect_true(is_valid_doid("http://purl.obolibrary.org/obo/DOID_0001816"))
    expect_true(is_valid_doid("DOID:4"))
    expect_true(is_valid_doid("obo:DOID_14566"))
    expect_true(is_valid_doid("obo:doid#DO_IEDB_slim"))
    expect_false(is_valid_doid("DOID_0040001")) # ns.lui (non-standard)
    expect_false(is_valid_doid("0001816")) # no prefix
    expect_false(is_valid_doid("obo:DOID:14566")) # wrong separator
    expect_false(is_valid_doid(" DOID_0040001")) # has space
    expect_false(is_valid_doid("DOID _0040001")) # has space
    # non-character input
    expect_error(is_valid_doid(1L))
})

test_that("is_valid_doid() allow arg works", {
    expect_is_valid_doid_format <- function(x, allow) {
        res <- setNames(rep(F, length(x)), names(x))
        if ("standard" %in% allow) {
            allow <- union(
                setdiff(allow, "standard"),
                c("curie", "obo_curie", "uri", "<uri>")
            )
        }
        res[
            names(x) %in% allow &
                stringr::str_detect(x, stringr::coll("doid", ignore_case = TRUE))
        ] <- T
        expect_equal(
            is_valid_doid(x, allow = allow),
            res,
            info = paste0("allow argument: ", paste(allow, collapse = ", "))
        )
    }

    doid <- c(
        ### may pass, according to allow & ns_type ###
        curie = "DOID:14566",             #1
        obo_curie = "obo:DOID_14566",     #2
        uri = "http://purl.obolibrary.org/obo/DOID_0001816",         #3
        "<uri>" = "<http://purl.obolibrary.org/obo/DOID_0000066>",   #4
        ns.lui = "DOID_14566",            #5
        ns.lui = "doid#chebi",            #6
        ### 7-19 should always be FALSE ###
        # non-DOID but valid OBO formats
        curie = "so:has_origin",          #7
        obo_curie = "obo:so#has_origin",  #8
        uri = "http://purl.obolibrary.org/obo/so#has_origin",      #9
        "<uri>" = "<http://purl.obolibrary.org/obo/CL_0000066>",   #10
        ns.lui = "so#has_origin",         #11
        # lui
        "0001816",
        "chebi",
        # wrong separator
        "obo:DOID:14566",
        # bracketed curie
        "<obo:DOID_14566>",
        # spaces
        "obo:DOID_14566 ",
        "obo: DOID_14566",
        # non-OBO
        "foaf:Person",
        "http://xmlns.com/foaf/0.1/Person"
    )

    expect_is_valid_doid_format(doid, allow = "standard")
    expect_is_valid_doid_format(doid, allow = "curie")
    expect_is_valid_doid_format(doid, allow = "obo_curie")
    expect_is_valid_doid_format(doid, allow = "uri")
    expect_is_valid_doid_format(doid, allow = "<uri>")
    expect_is_valid_doid_format(doid, allow = "ns.lui")
    # multiple inputs accepted
    expect_is_valid_doid_format(doid, allow = c("standard", "ns.lui"))
    expect_is_valid_doid_format(doid, allow = c("uri", "<uri>"))
    expect_is_valid_doid_format(doid, allow = c("obo_curie", "ns.lui"))
    expect_is_valid_doid_format(doid, allow = c("curie", "<uri>"))
})

# all_duplicated() tests --------------------------------------------------

na_dup <- c(NA, 1, 1:7, NA)
df_dup <- data.frame(
    x = c(NA, 1, 1:7, NA),
    y = c(NA, 1, 1:7, NA)
)

test_that("all_duplicated() works", {
    expect_equal(all_duplicated(1:10), rep(FALSE, 10))
    expect_equal(
        all_duplicated(c(1, 1:8, 1)),
        c(TRUE, TRUE, rep(FALSE, 7), TRUE)
    )
    expect_equal(
        all_duplicated(na_dup),
        c(rep(TRUE, 3), rep(FALSE, 6), TRUE)
    )
    expect_equal(
        all_duplicated(df_dup),
        c(rep(TRUE, 3), rep(FALSE, 6), TRUE)
    )
})

test_that("all_duplicated(incomparables = NA) works", {
    expect_equal(
        all_duplicated(1:10, incomparables = NA),
        rep(FALSE, 10)
    )
    expect_equal(
        all_duplicated(na_dup, incomparables = NA),
        c(FALSE, rep(TRUE, 2), rep(FALSE, 6), FALSE)
    )
    # not implemented for data.frames
    # expect_equal(
    #     all_duplicated(df_dup, incomparables = NA),
    #     c(FALSE, rep(TRUE, 2), rep(FALSE, 6), FALSE)
    # )
})


# is_invariant() tests ----------------------------------------------------

test_that("is_invariant() default method works (chr, lgl)", {
    .chr <- letters[1:2]
    .chr_invar <- c("a", "a")

    expect_false(is_invariant(.chr))
    expect_true(is_invariant(.chr_invar))

    .chr_na <- c("a", NA_character_)

    expect_false(is_invariant(.chr_na))
    expect_true(is_invariant(.chr_na, na.rm = TRUE))

    .lgl <- c(T, F)
    .lgl_invar <- rep(T, 2)

    expect_false(is_invariant(.lgl))
    expect_true(is_invariant(.lgl_invar))

    .lgl_na <- c(T, NA_character_)

    expect_false(is_invariant(.lgl_na))
    expect_true(is_invariant(.lgl_na, na.rm = TRUE))
})

test_that("is_invariant() list method works", {
    ##### simple lists #####
    .l <- list(1, 2)
    .l_invar <- list("a", "a")
    expect_false(is_invariant(.l))
    expect_true(is_invariant(.l_invar))

    # with names
    .l_nm <- list(a = 1, b = 2)
    .l_nm_only <- list(a = 1, b = 1)
    .l_nm_invar <- list(a = 1, a = 1)
    expect_false(is_invariant(.l_nm))
    expect_false(is_invariant(.l_nm_only))
    expect_true(is_invariant(.l_nm_only, incl_nm = FALSE))
    expect_true(is_invariant(.l_nm_invar))

    ##### 2-level lists ##### fully unnamed
    .ll <- list(list(1), list(2))
    .ll_invar <- list(list(1), list(1))
    expect_false(is_invariant(.ll))
    expect_true(is_invariant(.ll_invar))

    # with names at lvl 2
    .ll_nm <- list(list(a = 1), list(b = 1))
    .ll_nm_invar <- list(list(a = 1), list(a = 1))
    expect_false(is_invariant(.ll_nm))
    expect_false(is_invariant(.ll_nm, incl_nm = FALSE))
    expect_true(is_invariant(.ll_nm_invar))

    # with names at lvl 1
    .l_nm_l <- list(a = list(1), b = list(2))
    .l_nm_only_l <- list(a = list(1), b = list(1))
    .l_nm_l_invar <- list(a = list(1), a = list(1))
    expect_false(is_invariant(.l_nm_l))
    expect_false(is_invariant(.l_nm_only_l))
    expect_true(is_invariant(.l_nm_only_l, incl_nm = FALSE))
    expect_true(is_invariant(.l_nm_l_invar))

    # with names at both lvls
    .l_nm_l_nm <- list(a = list(x = 1), b = list(y = 2))
    .l_nm_l_nm_only <- list(a = list(x = 1), a = list(y = 1))
    .l_nm_only_l_nm <- list(a = list(x = 1), b = list(x = 1))
    .l_nm_l_nm_invar <- list(a = list(x = 1), a = list(x = 1))
    expect_false(is_invariant(.l_nm_l_nm))
    expect_false(is_invariant(.l_nm_l_nm_only))
    expect_false(is_invariant(.l_nm_only_l_nm))
    expect_true(is_invariant(.l_nm_only_l_nm, incl_nm = FALSE))
    expect_true(is_invariant(.l_nm_l_invar))
})

test_that("is_invariant() data.frame method works", {
    df <- datasets::mtcars[1:2, ]
    df_invar <- df
    df_invar[2, ] <- df_invar[1, ]

    expect_false(is_invariant(df))
    expect_true(is_invariant(df_invar))
})

test_that("is_invariant() numeric method works", {
    .int <- 1:2
    .int_invar <- rep(1, 2)

    expect_false(is_invariant(.int))
    expect_true(is_invariant(.int_invar))

    .int_na <- c(1, NA)

    expect_false(is_invariant(.int_na))
    expect_true(is_invariant(.int_na, na.rm = TRUE))

    .num <- c(1, 1 + sqrt(.Machine$double.eps))
    .num_invar <- c(1, 1 + sqrt(.Machine$double.eps) - 1e-14)

    expect_false(is_invariant(.num))
    expect_true(is_invariant(.num_invar))
    expect_true(is_invariant(.num, tol = sqrt(.Machine$double.eps) + 1e-14))
})


# is_curie() tests --------------------------------------------------------

test_that("is_curie(def = 'obo') works", {
    expect_true(is_curie("DOID:0001816", def = "obo"))
    expect_false(is_curie("obo:DOID_4", def = "obo"))
    expect_false(is_curie("obo:so#has_origin", def = "obo"))
    expect_false(is_curie("oboInOwl:hasDbXref", def = "obo"))
    expect_false(is_curie("alfred:LO362836C", def = "obo"))
    expect_false(is_curie("4dn.biosource:4DNSR73BT2A2", def = "obo"))
    expect_false(is_curie("aceview.worm:aap-1", def = "obo"))
    expect_false(is_curie("0001816", def = "obo"))
    expect_false(is_curie(" obo:HP_0000001", def = "obo"))
    expect_false(is_curie("http://purl.obolibrary.org/obo/DOID_0001816", def = "obo"))
})

test_that("is_curie(def = 'obo_generic') works", {
    expect_true(is_curie("DOID:0001816", def = "obo_generic"))
    expect_true(is_curie("obo:DOID_4", def = "obo_generic"))
    expect_true(is_curie("obo:so#has_origin", def = "obo_generic"))
    expect_true(is_curie("oboInOwl:hasDbXref", def = "obo_generic"))
    expect_true(is_curie("alfred:LO362836C", def = "obo_generic"))
    expect_false(is_curie("4dn.biosource:4DNSR73BT2A2", def = "obo_generic"))
    expect_false(is_curie("aceview.worm:aap-1", def = "obo_generic"))
    expect_false(is_curie("0001816", def = "obo_generic"))
    expect_false(is_curie(" obo:HP_0000001", def = "obo_generic"))
    expect_false(is_curie("http://purl.obolibrary.org/obo/DOID_0001816", def = "obo_generic"))
})

test_that("is_curie(def = 'w3c_safe') works", {
    expect_true(is_curie("[DOID:0001816]", def = "w3c_safe"))
    expect_true(is_curie("[obo:so#has_origin]", def = "w3c_safe"))
    expect_true(is_curie("[aceview.worm:aap-1]", def = "w3c_safe"))
    expect_false(is_curie("obo:DOID_4", def = "w3c_safe"))
    expect_false(is_curie("_4dn.biosource:4DNSR73BT2A2", def = "w3c_safe"))
    expect_false(is_curie("4dn.biosource:4DNSR73BT2A2", def = "w3c_safe"))
    expect_false(is_curie("0001816", def = "w3c_safe"))
    expect_false(is_curie(" obo:HP_0000001", def = "w3c_safe"))
    expect_false(
        is_curie(
            "http://purl.obolibrary.org/obo/DOID_0001816",
            def = "w3c_safe"
        )
    )
})

test_that("is_curie(def = 'w3c') works", {
    expect_true(is_curie("DOID:0001816", def = "w3c"))
    expect_true(is_curie("obo:DOID_4", def = "w3c"))
    expect_true(is_curie("obo:so#has_origin", def = "w3c"))
    expect_true(is_curie("oboInOwl:hasDbXref", def = "w3c"))
    expect_true(is_curie("alfred:LO362836C", def = "w3c"))
    expect_true(is_curie("_4dn.biosource:4DNSR73BT2A2", def = "w3c"))
    expect_false(is_curie("4dn.biosource:4DNSR73BT2A2", def = "w3c"))
    expect_true(is_curie("aceview.worm:aap-1", def = "w3c"))
    expect_false(is_curie("0001816", def = "w3c"))
    expect_false(is_curie(" obo:HP_0000001", def = "w3c"))
    expect_true(is_curie("http://purl.obolibrary.org/obo/DOID_0001816", def = "w3c"))
    # expect_true(is_curie("blah:1　2", "w3c")) # non-breaking space U+3000, should work but doesn't
    expect_false(is_curie("blah:1 2", "w3c")) # normal space in LUI
})


# is_uri() tests ----------------------------------------------------------

test_that("is_uri() works", {
    expect_true(is_uri("http://purl.obolibrary.org/obo/DOID_0001816"))
    expect_true(is_uri("https://google.com"))
    expect_true(is_uri("mailto:fake.name@blah.com"))
    expect_true(is_uri("file://"))
    expect_true(is_uri("mailto:"))
    expect_false(is_uri("blah"))
    expect_false(is_uri(""))
    expect_error(is_valid_doid(1L))
})

test_that("is_uri() empty_ok arg works", {
    expect_true(is_uri("https://google.com", empty_ok = FALSE))
    expect_true(is_uri("mailto:fake.name@blah.com", empty_ok = FALSE))
    expect_false(is_uri("ftp://", empty_ok = FALSE))
    expect_false(is_uri("mailto:", empty_ok = FALSE))
    expect_false(is_uri("blah", empty_ok = FALSE))
})


# iff_all_vals() tests ----------------------------------------------------

test_that("iff_all_vals() works", {
    expect_true(iff_all_vals(1:5, 5:1))
    expect_true(iff_all_vals(letters[1:5], letters[5:1]))

    res_missing <- FALSE
    attr(res_missing, "missing") <- 5:2
    expect_equal(iff_all_vals(1, 5:1), res_missing)

    res_extra <- FALSE
    attr(res_extra, "extra") <- 2:5
    expect_false(iff_all_vals(1:5, 1))
})
