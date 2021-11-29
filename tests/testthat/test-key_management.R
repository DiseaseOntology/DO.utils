# inform_of_loc() ---------------------------------------------------------

# helpers
set_pkg_opt <- function(to) {
    options(DO.utils.key_msg = to)
}
set_indiv_opt <- function(key, value) {
    indiv_nm <- paste("DO.utils.key_msg", key, sep = ".")
    options(
        purrr::set_names(list(value), nm = indiv_nm)
    )
}

# capture pkg-wide option (in case it's set) & unset to avoid test influence
store_opt <- getOption("DO.utils.key_msg")
set_pkg_opt(NULL) # unset to avoid possible side-effects


# individual key option
test_that("individual key options work when TRUE", {
    set_indiv_opt("test_msg", TRUE)
    expect_message(inform_of_loc("test_msg", "keyring"))
})

test_that("individual key options work when FALSE", {
    set_indiv_opt("test_msg", FALSE)
    expect_silent(inform_of_loc("test_msg", "env_var"))
})

test_that("individual key options inform once and only once", {
    set_indiv_opt("test_msg", NULL)
    expect_message(inform_of_loc("test_msg", "env_var"))
    expect_silent(inform_of_loc("test_msg", "keyring"))
})

set_indiv_opt("test_msg", NULL) # unset to avoid side-effects


# pkg-wide key option
test_that("pkg-wide key option = TRUE always produces message", {
    set_pkg_opt(TRUE)
    set_indiv_opt("test_msg", NULL)
    expect_message(inform_of_loc("test_msg", "keyring"))
    expect_message(inform_of_loc("test_msg", "keyring"))
})

test_that("pkg-wide key option = FALSE never produces message", {
    set_pkg_opt(FALSE)
    set_indiv_opt("test_msg", NULL)
    expect_silent(inform_of_loc("test_msg", "keyring"))
    expect_silent(inform_of_loc("test_msg", "keyring"))
})


# unset/restore to avoid side-effects
set_indiv_opt("test_msg", NULL)
set_pkg_opt(store_opt)
