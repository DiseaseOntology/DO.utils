test_that("is_valid_obo works", {
    expect_true(is_valid_obo("http://purl.obolibrary.org/obo/DOID_0001816"))
    expect_true(is_valid_obo("<http://purl.obolibrary.org/obo/CL_0000066>"))
    expect_true(is_valid_obo("obo:DOID_14566"))
    expect_true(is_valid_obo("http://purl.obolibrary.org/obo/so#has_origin"))
    expect_true(is_valid_obo("<http://purl.obolibrary.org/obo/so#has_origin>"))
    expect_true(is_valid_obo("obo:so#has_origin"))
    expect_false(is_valid_obo("0001816"))
    expect_false(is_valid_obo("obo:DOID:14566"))
    expect_false(is_valid_obo("<obo:DOID_14566>"))
    expect_false(is_valid_obo("DOID:14566"))
    expect_false(is_valid_obo("obo:DOID_14566 "))
    expect_false(is_valid_obo("obo: DOID_14566"))
    expect_error(is_valid_obo(1L))
})

test_that("is_valid_doid works", {
    expect_true(is_valid_doid("http://purl.obolibrary.org/obo/DOID_0001816"))
    expect_true(is_valid_doid("DOID:4"))
    expect_true(is_valid_doid("obo:DOID_14566"))
    expect_true(is_valid_doid("DOID_0040001"))
    expect_false(is_valid_doid("0001816"))
    expect_false(is_valid_doid("obo:DOID:14566"))
    expect_false(is_valid_doid("obo:doid#DO_IEDB_slim"))
    expect_false(is_valid_doid(" DOID_0040001"))
    expect_false(is_valid_doid("DOID _0040001"))
    expect_error(is_valid_doid(1L))
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
    expect_true(is_curie("4dn.biosource:4DNSR73BT2A2", def = "w3c"))
    expect_true(is_curie("aceview.worm:aap-1", def = "w3c"))
    expect_false(is_curie("0001816", def = "w3c"))
    expect_false(is_curie(" obo:HP_0000001", def = "w3c"))
    expect_true(is_curie("http://purl.obolibrary.org/obo/DOID_0001816", def = "w3c"))
    # expect_true(is_curie("blah:1　2", "w3c")) # non-breaking space U+3000, should work but doesn't
    expect_false(is_curie("blah:1 2", "w3c")) # normal space in LUI
})
