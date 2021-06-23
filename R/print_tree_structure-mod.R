# copied from xml2, 2021-06-23 - https://github.com/r-lib/xml2/blob/e7e87ecb1f3d7ef0f5ce50148644a860ea8125c3/R/xml_structure.R
# MIT license - https://github.com/r-lib/xml2/blob/master/LICENSE.md

# iteratively walks through each node and cat's one line at a time
# change it by:
#   1) saving all elements @ top level,
#   2) count unique elements (and add count of each unique value),
#   3) if only one unique element, step deeper; if more than one, step deeper
#       into unique groups
#   4) for each level, check if each unique node is present in all parent nodes
#       of the same type,
#   3) use * to identify nodes that are are NOT available in each parent node
#       and/or add count of nodes at each level

# rename to print_condensed_xml_structure
print_xml_structure <- function(x, prefix = 0, indent = 2, html = FALSE, file = "") {
    padding <- paste(rep(" ", prefix), collapse = "")
    type <- xml_type(x)

    if (type == "element") {

        attr <- xml_attrs(x)
        if (html) {
            html_attrs <- list()
            if ("id" %in% names(attr)) {
                html_attrs$id <- paste0("#", attr[["id"]])
                attr <- attr[setdiff(names(attr), "id")]
            }

            if ("class" %in% names(attr)) {
                html_attrs$class <- paste0(".", gsub(" ", ".", attr[["class"]]))
                attr <- attr[setdiff(names(attr), "class")]
            }

            attr_str <- paste(unlist(html_attrs), collapse = " ")
        } else {
            attr_str <- ""
        }

        if (length(attr) > 0) {
            attr_str <- paste0(attr_str, " [", paste0(names(attr), collapse = ", "), "]")
        }

        node <- paste0("<", xml_name(x), attr_str, ">")

        cat(padding, node, "\n", sep = "", file = file, append = TRUE)
        lapply(xml_contents(x), print_xml_structure, prefix = prefix + indent,
               indent = indent, html = html, file = file)
    } else {
        cat(padding, "{", type, "}\n", sep = "", file = file, append = TRUE)
    }
}
