# RDF

# W3C standards

## CURIEs

- Standard = https://www.w3.org/TR/2010/NOTE-curie-20101216/
- Defined format
    - TL;DR
        - Prefix = [[:alnum:].-_]+
        - Reference = [[:alnum:]-._~!$&'()*+,;=]+
            - appears to include just about any normal letter, number, or symbol (includes registered mark, non-breaking space, etc.; anything not included can be percent encoded)
    - Detailed defined format:
        - curie       :=   [ [ prefix ] ':' ] reference
        - prefix      :=   NCName
            - [NCName](https://www.w3.org/TR/1999/REC-xml-names-19990114/#NT-NCName) 	::= 	(Letter | '_') (NCNameChar)*
                - NCNameChar 	::= 	Letter | Digit | '.' | '-' | '_' | CombiningChar | Extender
                - [Letter](https://www.w3.org/TR/REC-xml/#NT-Letter)	   ::=   	BaseChar | Ideographic
                    - BaseChar includes all Basic latin upper & lower case letters,  (international version
        - reference   :=   irelative-ref (as defined in [IRI](https://www.ietf.org/rfc/rfc3987.txt))
            - irelative-ref  = irelative-part [ "?" iquery ] [ "#" ifragment ]
                - irelative-part = "//" iauthority ipath-abempty OR / ipath-absolute
                    - all basically depend on isegment
                    - isegment = *ipchar
                        - ipchar = iunreserved / pct-encoded / sub-delims / ":" / "@"
                            - iunreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
                                - ucschar        = %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD / %xD0000-DFFFD / %xE1000-EFFFD
                            - pct-encoded = "%" HEXDIG HEXDIG
                            - sub-delims     = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
        - [W3C charsets](https://www.w3.org/Style/XSL/TestSuite/results/4/XEP/charsets.pdf), for broader reference see https://en.wikipedia.org/wiki/List_of_Unicode_characters.

# SPARQL support in DO.utils

The [rdflib](https://github.com/ropensci/rdflib) R package provides SPARQL queries by wrapping the [redlands](https://cran.r-project.org/web/packages/redland/index.html) package but cannot be used because it does not support FILTER NOT EXISTS statements.

Two possible alternatives that both support the complete set of SPARQL operations are [ROBOT](robot.obolibrary.org/) and python's [RDFLib](https://rdflib.readthedocs.io/en/stable/#).

- ROBOT requires installation of JDK 11 and _always_ writes output to a file.

- RDFLib requires installation of python.

**RDFLib** seems like the best option. The RStudio team has been working on making Python and R more interoperable by creating the [reticulate](https://rstudio.github.io/reticulate/) package, and it presumably can automatically download and install the needed Python packages when installing an R package (see https://rstudio.github.io/reticulate/articles/package.html).
