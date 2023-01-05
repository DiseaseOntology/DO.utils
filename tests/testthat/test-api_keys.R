test_that("use_scopus_insttoken() works (no envvar)", {
    withr::local_envvar(c(Elsevier_insttoken = NA))

    expect_equal(use_scopus_insttoken("blah"), c("X-ELS-Insttoken" = "blah"))
    expect_equal(
        use_scopus_insttoken(headers = rscopus::inst_token_header("blah")),
        structure(c(`X-ELS-Insttoken` = "blah"), class = "token")
    )
    expect_error(use_scopus_insttoken())
    expect_error(use_scopus_insttoken(""))
    expect_error(
        use_scopus_insttoken("blah", rscopus::inst_token_header("blah"))
    )
})

test_that("use_scopus_insttoken() works (with envvar)", {
    withr::local_envvar(c(Elsevier_insttoken = "blah"))

    expect_equal(use_scopus_insttoken(), c("X-ELS-Insttoken" = "blah"))
    expect_equal(
        use_scopus_insttoken("ABC123"),
        c("X-ELS-Insttoken" = "ABC123")
    )
    expect_equal(
        use_scopus_insttoken(headers = rscopus::inst_token_header("ABC123")),
        structure(c(`X-ELS-Insttoken` = "ABC123"), class = "token")
    )
})

test_that("set_scopus_keys() works", {
    withr::local_envvar(c(Elsevier_API = NA, Elsevier_insttoken = NA))

    expect_equal(set_scopus_keys("ABC123", "blah"), c(TRUE, TRUE))
    expect_equal(
        Sys.getenv(c("Elsevier_API", "Elsevier_insttoken")),
        c(Elsevier_API = "ABC123", Elsevier_insttoken = "blah")
    )

    withr::local_envvar(c(Elsevier_API = NA, Elsevier_insttoken = NA))
    expect_equal(set_scopus_keys("hello"), TRUE)
    expect_equal(
        Sys.getenv(c("Elsevier_API", "Elsevier_insttoken")),
        c(Elsevier_API = "hello", Elsevier_insttoken = "")
    )

    withr::local_envvar(c(Elsevier_API = NA, Elsevier_insttoken = NA))
    expect_equal(set_scopus_keys(insttoken = "goodbye"), TRUE)
    expect_equal(
        Sys.getenv(c("Elsevier_API", "Elsevier_insttoken")),
        c(Elsevier_API = "", Elsevier_insttoken = "goodbye")
    )

    expect_error(set_scopus_keys())
})
