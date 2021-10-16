# lists for testing

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
l4_part_names_null <- list(
    "a", "b",
    # NULL no name
    L1 = list(NULL, D = "d"),
    L2 = list(
        LL1 = list(F = "f", G = "g"),
        # NULL with name
        LL2 = list(H = "h", I = NULL)
    )
)
l4_part_names_na <- list(
    "a", "b",
    # NA no name
    L1 = list(NA, D = "d"),
    L2 = list(
        LL1 = list(F = "f", G = "g"),
        # NA with name
        LL2 = list(H = "h", I = NA)
    )
)
l4_part_names_na_null <- list(
    "a", "b", NA, NULL, X = NA, Y = NULL,
    L1 = list(NA, D = "d", NULL),
    L2 = list(
        LL1 = list("F" = "f", G = "g", Z = NULL),
        LL2 = list(H = "h", I = NA)
    )
)
