# Test Data ---------------------------------------------------------------

l2 <- list("a", "b")
l3 <- list("a", "b", list("c", "d"))
l4 <- list("a", "b", list("c", "d"), list(list("e", "f"), list("g", "h")))
l4_named <- list(
    A = "a", B = "b",
    L1 = list(C = "c", D = "d"),
    L2 = list(
        LL1 = list(F = "f", G = "g"),
        LL2 = list(H = "h", I = "i")
    )
)
l4_partial_names <- list(
    "a", "b",
    L1 = list(C = "c", D = "d"),
    L2 = list(
        LL1 = list("F" = "f", G = "g"),
        LL2 = list(H = "h", I = "i")
    )
)

l4_nonuniq_names <- list(
    "a", "b",
    L1 = list(C = "c", D = "d"),
    L1 = list(
        LL1 = list("F" = "f", G = "g"),
        LL1 = list(H = "h", I = "i")
    )
)

# expected character with JSON data
c2 <- c("[\"a\"]", "[\"b\"]")
c3 <- c("[\"a\"]", "[\"b\"]", "[[\"c\"],[\"d\"]]")
c4 <- c("[\"a\"]", "[\"b\"]", "[[\"c\"],[\"d\"]]",
        "[[[\"e\"],[\"f\"]],[[\"g\"],[\"h\"]]]")
c4_named <- c(
    A = "[\"a\"]",
    B = "[\"b\"]",
    L1 = "{\"C\":[\"c\"],\"D\":[\"d\"]}",
    L2 = "{\"LL1\":{\"F\":[\"f\"],\"G\":[\"g\"]},\"LL2\":{\"H\":[\"h\"],\"I\":[\"i\"]}}"
)
c4_partial_names <- c(
    "[\"a\"]", "[\"b\"]",
    L1 = "{\"C\":[\"c\"],\"D\":[\"d\"]}",
    L2 = "{\"LL1\":{\"F\":[\"f\"],\"G\":[\"g\"]},\"LL2\":{\"H\":[\"h\"],\"I\":[\"i\"]}}"
)
c4_nonuniq_names <- c(
    "[\"a\"]", "[\"b\"]",
    L1 = "{\"C\":[\"c\"],\"D\":[\"d\"]}",
    L1 = "{\"LL1\":{\"F\":[\"f\"],\"G\":[\"g\"]},\"LL1\":{\"H\":[\"h\"],\"I\":[\"i\"]}}"
)

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
})

test_that("output is correct", {
    expect_identical(confine_list(l2), c2)
    expect_identical(confine_list(l3), c3)
    expect_identical(confine_list(l4), c4)
    expect_identical(confine_list(l4_named), c4_named)
    expect_identical(confine_list(l4_partial_names), c4_partial_names)
    expect_identical(confine_list(l4_nonuniq_names), c4_nonuniq_names)
})

test_that("structure is preserved", {
    expect_identical(l2, round_trip(l2))
    expect_identical(l3, round_trip(l3))
    expect_identical(l4, round_trip(l4))
    expect_identical(l4_named, round_trip(l4_named))
    expect_identical(l4_partial_names, round_trip(l4_partial_names))
    expect_identical(l4_nonuniq_names, round_trip(l4_nonuniq_names))
})
