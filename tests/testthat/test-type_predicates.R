
test_that("is_valid_doid works", {
    expect_true(is_valid_doid("http://purl.obolibrary.org/obo/DOID_0001816"))
    expect_true(is_valid_doid("DOID:4"))
    expect_true(is_valid_doid("obo:DOID_14566"))
    expect_true(is_valid_doid("DOID_0040001"))
    expect_false(is_valid_doid("0001816"))
    expect_false(is_valid_doid("obo:DOID:14566"))
    expect_error(is_valid_doid(1L))
})
