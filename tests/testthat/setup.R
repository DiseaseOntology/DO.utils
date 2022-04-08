check_internet <- function() {
    if(!curl::has_internet()) {
        skip("No internet connection.")
    }
}
