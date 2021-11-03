source("data/lists.R")

# Helper Functions --------------------------------------------------------

round_trip <- function(x) {
    release_list(confine_list(x))
}


# Tests -------------------------------------------------------------------

test_that("produces character of correct length", {
    expect_vector(confine_list(l2), character(), length(l2))
    expect_vector(confine_list(l3), character(), length(l3))
    expect_vector(confine_list(l4), character(), length(l4))
    expect_vector(confine_list(l4_named), character(), length(l4_named))
    expect_vector(
        confine_list(l4_partial_names),
        character(),
        length(l4_partial_names)
    )
    expect_vector(
        confine_list(l4_nonuniq_names),
        character(),
        length(l4_nonuniq_names)
    )
    expect_vector(
        confine_list(l4_part_names_null),
        character(),
        length(l4_part_names_null)
    )
})

test_that("structure is preserved", {
    expect_identical(l2, round_trip(l2))
    expect_identical(l3, round_trip(l3))
    expect_identical(l4, round_trip(l4))
    expect_identical(l4_named, round_trip(l4_named))
    expect_identical(l4_partial_names, round_trip(l4_partial_names))
    expect_identical(l4_nonuniq_names, round_trip(l4_nonuniq_names))
    expect_identical(l4_part_names_null, round_trip(l4_part_names_null))
})
