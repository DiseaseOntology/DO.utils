# Notes on Decisions


# httr vs httr2 (URL audit)

To complete an audit of URLs for DO, the `httr` package was used instead of the newer `httr2`. `httr2` was still in development (as it remains at the time of this writing), while `httr` is stable and likely to persist. This worked well until an `httr` **internal error was discovered.**

## Error

After testing many URLs I came across a situation where a request would be successful but `httr` would fail to parse it and the result would be lost. The specific situation was a GET request for "rarediseases.org/robots.txt", which succeeded (2022-04-22) but the response did not include the scheme (http vs https) in the URL causing `httr` to fail, even though it could definitely parse the response. This problem was noted in httr's issue tracker ([#607](https://github.com/r-lib/httr/issues/607)) in Aug 2019 with a pull request ([#620](https://github.com/r-lib/httr/pull/620)) issued shortly thereafter, which has still not been released and it would seem there are [no plans for a release](https://github.com/r-lib/httr/pull/620#issuecomment-622575877).

## Problem

This could cause a major problem during an audit because it _could_ block a whole domain or prevent the results of an entire domain of URLs from being saved and it cannot be easily remedied outside of an audit as a standalone because the initial request and parsing code relies on `httr`, which would always fail.

## Fix

There are two possible fixes:

1. Fork `httr` on Github and implement pull request [#620](https://github.com/r-lib/httr/pull/620). This solution is less than ideal but one I've had to use before (e.g. `rentrez`). In that case, there was no alternative code in development.
2. Migrate everything to `httr2`. There are some benefits to using `httr2` over `httr` but nothing that appeared to supersede the initial concern about it's stability and, like `httr`, it did not appear to support multiple requests. After further searching, `httr2` does appear to support multiple requests in a specific form via [`multi_req_perform()`](https://httr2.r-lib.org/reference/multi_req_perform.html). It's not clear if this should be implemented or not.

### Additional Commentary

[2022-04-22] It would appear that `httr2` suffers from a [similar problem](https://github.com/r-lib/httr2/issues/127) to `httr` and a number of other issues that are [not likely to be fixed in the near future](https://github.com/r-lib/httr2/issues/118#issuecomment-1086115636).

## DECISION

At this time, `httr` remains more stable and tested, making it the better choice.
