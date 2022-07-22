library(microbenchmark)
devtools::load_all()

# Data --------------------------------------------------------------------

x <- c(1:2, 8:6, 4, -1:-2, 20:37, 4, 40, 43, 45)
x_long <- sample.int(1500, 1000)
xx_long <- sample.int(15000, 10000)

doid <- paste0("DOID:", x)
doid_long <- paste0("DOID:", x_long)
doid_xlong <- paste0("DOID:", xx_long)

to_range(
    doid,
    ~ as.integer(stringr::str_remove(.x, "DOID:")),
    end_rm = "DOID:"
)


# Reference functions -----------------------------------------------------
# From https://stackoverflow.com/questions/16911773/collapse-runs-of-consecutive-numbers-to-ranges

collapseConsecutive <- function(s){
    x <- sort(unique(s))

    x_0 <- x[1]
    out <- toString(x[1])
    hasDash <- FALSE

    for(i in 2:length(x)) {
        x_1 <- x[i]
        x_2 <- x[i+1]

        if((x_0 + 1) == x_1 && !is.na(x_2) && (x_1 + 1) == x_2) {
            if(!hasDash) {
                out <- c(out, "-")
                hasDash <- TRUE
            }
        } else {
            if(hasDash) {
                hasDash <- FALSE
            } else {
                out <- c(out, ",")
            }
            out <- c(out, x_1)
            hasDash <- FALSE
        }
        x_0 <- x_1
    }
    outString <- paste(out, collapse="")
    outString
}

findIntRuns <- function(run){
    run <- sort(unique(run))
    rundiff <- c(1, diff(run))
    difflist <- split(run, cumsum(rundiff!=1))
    out_vctr <- unlist(lapply(difflist, function(x){
        if(length(x) %in% 1:2) as.character(x) else paste0(x[1], "-", x[length(x)])
    }), use.names=FALSE)
    paste(out_vctr, collapse = ",")
}

contractSeqs <-  function(x) {
    in.seq <- function(x) {
        # returns TRUE for elments within ascending sequences
        (c(diff(x, 1), NA) == 1 & c(NA, diff(x,2), NA) == 2)
    }
    x <- sort(unique(x))
    # returns string formatted with contracted sequences
    x[in.seq(x)] <- ""
    gsub(",{2,}", "-", paste(x, collapse=","), perl=TRUE)
}


# Benchmarking ------------------------------------------------------------

# Relative to references
## Short input
microbenchmark(
    collapseConsecutive(x),
    findIntRuns(x),
    contractSeqs(x),
    to_range(x),
    times = 500,
    check = 'equal'
)

## Medium input
microbenchmark(
    collapseConsecutive(x_long),
    findIntRuns(x_long),
    contractSeqs(x_long),
    to_range(x_long),
    times = 100,
    check = 'equal'
)

## Long input
microbenchmark(
    collapseConsecutive(xx_long),
    findIntRuns(xx_long),
    contractSeqs(xx_long),
    to_range(xx_long),
    times = 100,
    check = 'equal'
)


# Comparing arguments

microbenchmark(
    to_range(x),
    to_range(doid, ~ as.integer(stringr::str_remove(.x, "DOID:"))),
    to_range(doid, ~ as.integer(stringr::str_remove(.x, "DOID:")), end_rm = "DOID:"),
    times = 100
)

microbenchmark(
    to_range(x_long),
    to_range(doid_long, ~ as.integer(stringr::str_remove(.x, "DOID:"))),
    to_range(doid_long, ~ as.integer(stringr::str_remove(.x, "DOID:")), end_rm = "DOID:"),
    times = 100
)

microbenchmark(
    to_range(xx_long),
    to_range(doid_xlong, ~ as.integer(stringr::str_remove(.x, "DOID:"))),
    to_range(doid_xlong, ~ as.integer(stringr::str_remove(.x, "DOID:")), end_rm = "DOID:"),
    times = 20
)
