# parse_omim_name() -------------------------------------------------------
# Test data: tests/testthat/data/omim/entry_names.tsv
#   - Max 4 examples per number of comma-separated qualifiers (0–5+)
#   - 'name' and 'abbreviation' columns are manually curated expected values
#
# Note on eponyms: parse_omim_name() defaults to eponyms = disease_eponyms.
# Tests that check for uncorrected proper nouns pass NULL explicitly so they
# are not sensitive to the contents of disease_eponyms at test time.


# ---- helpers ------------------------------------------------------------

omim_data <- readr::read_tsv(
    test_path("data", "omim", "entry_names.tsv"),
    col_types = readr::cols(.default = "c"),
    na = ""
)


# ---- abbreviation extraction --------------------------------------------

test_that("parse_omim_name() extracts abbreviations correctly", {
    result <- parse_omim_name(omim_data$entry)
    expect_equal(result$abbreviation, omim_data$abbreviation)
})


# ---- names with no comma (0 qualifiers) ---------------------------------

test_that("parse_omim_name() lowercases simple entries with no comma", {
    expect_equal(
        parse_omim_name("SCHIZOPHRENIA 12")$name,
        "schizophrenia 12"
    )
    expect_equal(
        parse_omim_name("DYSTONIA 12; DYT12")$name,
        "dystonia 12"
    )
    # Proper nouns not corrected when not in disease_eponyms
    expect_equal(
        parse_omim_name("HURIEZ SYNDROME; HRZ", eponyms = NULL)$name,
        "huriez syndrome"
    )
    # Multi-word proper noun with internal lowercase particles
    expect_equal(
        parse_omim_name("DEN HOED-DE BOER-VOISIN SYNDROME; DHDBV", eponyms = NULL)$name,
        "den hoed-de boer-voisin syndrome"
    )
})


# ---- one comma (1 qualifier) --------------------------------------------

test_that("parse_omim_name() rearranges a single pre-qualifier", {
    expect_equal(
        parse_omim_name("SPASTIC PARAPLEGIA 14, AUTOSOMAL RECESSIVE; SPG14")$name,
        "autosomal recessive spastic paraplegia 14"
    )
    expect_equal(
        parse_omim_name("SCHWANNOMATOSIS, VESTIBULAR; SWNV")$name,
        "vestibular schwannomatosis"
    )
    # Number embedded in qualifier token
    expect_equal(
        parse_omim_name("DEAFNESS, AUTOSOMAL RECESSIVE 117; DFNB117")$name,
        "autosomal recessive deafness 117"
    )
})

test_that("parse_omim_name() handles TYPE as a post-qualifier", {
    expect_equal(
        parse_omim_name("OSTEOGENESIS IMPERFECTA, TYPE XI; OI11")$name,
        "osteogenesis imperfecta type XI"
    )
})


# ---- two commas (2 qualifiers) ------------------------------------------

test_that("parse_omim_name() leaves feature-list entries unchanged", {
    # No forcing qualifier → keep in original comma order
    expect_equal(
        parse_omim_name("DEVELOPMENTAL DELAY, IMPAIRED SPEECH, AND BEHAVIORAL ABNORMALITIES; DDISBA")$name,
        "developmental delay, impaired speech, and behavioral abnormalities"
    )
    expect_equal(
        parse_omim_name("SPASTIC TETRAPLEGIA, THIN CORPUS CALLOSUM, AND PROGRESSIVE MICROCEPHALY; SPATCCM")$name,
        "spastic tetraplegia, thin corpus callosum, and progressive microcephaly"
    )
})

test_that("parse_omim_name() rearranges two-qualifier entries with CONGENITAL", {
    expect_equal(
        parse_omim_name("DEAFNESS, CONGENITAL, WITH INNER EAR AGENESIS, MICROTIA, AND MICRODONTIA")$name,
        "congenital deafness with inner ear agenesis, microtia, and microdontia"
    )
    # Trailing phrase part of no-abbreviation entry
    expect_equal(
        parse_omim_name("LIPODYSTROPHY, GENERALIZED, WITH IMPAIRED INTELLECTUAL DEVELOPMENT, DEAFNESS, SHORT STATURE, AND SLENDER BONES")$name,
        "generalized lipodystrophy with impaired intellectual development, deafness, short stature, and slender bones"
    )
})

test_that("parse_omim_name() handles SUSCEPTIBILITY TO prefix", {
    expect_equal(
        parse_omim_name("SCOLIOSIS, ISOLATED, SUSCEPTIBILITY TO, 1; IS1")$name,
        "susceptibility to isolated scoliosis 1"
    )
})


# ---- three commas (3 qualifiers) ----------------------------------------

test_that("parse_omim_name() rearranges 3-qualifier entries", {
    expect_equal(
        parse_omim_name("EPILEPSY, PROGRESSIVE MYOCLONIC, 4, WITH OR WITHOUT RENAL FAILURE; EPM4")$name,
        "progressive myoclonic epilepsy 4 with or without renal failure"
    )
    expect_equal(
        parse_omim_name("MACROTHROMBOCYTOPENIA, ISOLATED, 1, AUTOSOMAL DOMINANT; MACTHC1")$name,
        "autosomal dominant isolated macrothrombocytopenia 1"
    )
    expect_equal(
        parse_omim_name("NEURODEGENERATION, CHILDHOOD-ONSET, WITH ATAXIA, TREMOR, OPTIC ATROPHY, AND COGNITIVE DECLINE; CONATOC")$name,
        "childhood-onset neurodegeneration with ataxia, tremor, optic atrophy, and cognitive decline"
    )
    # Proper nouns not capitalized without eponyms
    expect_equal(
        parse_omim_name("PERIPHERAL DEMYELINATING NEUROPATHY, CENTRAL DYSMYELINATION, WAARDENBURG SYNDROME, AND HIRSCHSPRUNG DISEASE; PCWH",
                        eponyms = NULL)$name,
        "peripheral demyelinating neuropathy, central dysmyelination, waardenburg syndrome, and hirschsprung disease"
    )
})


# ---- four commas (4 qualifiers) -----------------------------------------

test_that("parse_omim_name() rearranges 4-qualifier entries", {
    expect_equal(
        parse_omim_name("LEUKODYSTROPHY, HYPOMYELINATING, 23, WITH ATAXIA, DEAFNESS, LIVER DYSFUNCTION, AND DILATED CARDIOMYOPATHY; HLD23")$name,
        "hypomyelinating leukodystrophy 23 with ataxia, deafness, liver dysfunction, and dilated cardiomyopathy"
    )
    expect_equal(
        parse_omim_name("NEUROPATHY, HEREDITARY MOTOR AND SENSORY, WITH DEAFNESS, IMPAIRED INTELLECTUAL DEVELOPMENT, AND ABSENT SENSORY LARGE MYELINATED FIBERS")$name,
        "hereditary motor and sensory neuropathy with deafness, impaired intellectual development, and absent sensory large myelinated fibers"
    )
    # Feature list (no forcing) → kept in original order
    expect_equal(
        parse_omim_name("FETAL AKINESIA, RESPIRATORY INSUFFICIENCY, MICROCEPHALY, POLYMICROGYRIA, AND DYSMORPHIC FACIES; FARIMPD")$name,
        "fetal akinesia, respiratory insufficiency, microcephaly, polymicrogyria, and dysmorphic facies"
    )
    # Feature list (no forcing) → kept in original order
    expect_equal(
        parse_omim_name("TELANGIECTASIA, IMPAIRED INTELLECTUAL DEVELOPMENT, MICROCEPHALY, METAPHYSEAL DYSPLASIA, EYE ABNORMALITIES, AND SHORT STATURE; TIMES")$name,
        "telangiectasia, impaired intellectual development, microcephaly, metaphyseal dysplasia, eye abnormalities, and short stature"
    )
})


# ---- five-plus commas (5+ qualifiers) -----------------------------------

test_that("parse_omim_name() rearranges complex multi-qualifier entries", {
    # Definitive pre-qualifier listed AFTER a trailing-phrase AND-token
    expect_equal(
        parse_omim_name("MYASTHENIC SYNDROME, CONGENITAL, 7A, PRESYNAPTIC, AND DISTAL MOTOR NEUROPATHY, AUTOSOMAL DOMINANT; CMS7A")$name,
        "autosomal dominant presynaptic congenital myasthenic syndrome-7A and distal motor neuropathy"
    )
    # Feature list (no forcing) → kept in original order
    expect_equal(
        parse_omim_name("NEURODEVELOPMENTAL DISORDER WITH GROWTH IMPAIRMENT, QUADRIPARESIS, AND POOR OR ABSENT SPEECH; NEDGQS")$name,
        "neurodevelopmental disorder with growth impairment, quadriparesis, and poor or absent speech"
    )
    expect_equal(
        parse_omim_name("NEURODEVELOPMENTAL DISORDER WITH HYPOTONIA, SPEECH DELAY, AND DYSMORPHIC FACIES; NEDHSF")$name,
        "neurodevelopmental disorder with hypotonia, speech delay, and dysmorphic facies"
    )
})


# ---- capitalization fixes -----------------------------------------------

test_that("parse_omim_name() uppercases Roman numerals after TYPE", {
    expect_equal(
        parse_omim_name("AMELOGENESIS IMPERFECTA, TYPE IB")$name,
        "amelogenesis imperfecta type IB"
    )
    expect_match(
        parse_omim_name("SOME DISEASE, TYPE IIIA")$name,
        "type IIIA"
    )
})

test_that("parse_omim_name() uppercases alphanumeric subtype codes", {
    # 7A is a trailing number extracted from a comma qualifier
    expect_match(
        parse_omim_name("MYASTHENIC SYNDROME, CONGENITAL, 7A, PRESYNAPTIC, AND DISTAL MOTOR NEUROPATHY, AUTOSOMAL DOMINANT; CMS7A")$name,
        "syndrome-7A"
    )
})

test_that("parse_omim_name() applies immunoglobulin capitalization", {
    # HYPER-IGE → hyper-IgE
    expect_match(
        parse_omim_name("ERYTHRODERMA, CONGENITAL, WITH PALMOPLANTAR KERATODERMA, HYPOTRICHOSIS, AND HYPER-IGE; EPKHE")$name,
        "hyper-IgE"
    )
})


# ---- eponyms argument ---------------------------------------------------

test_that("parse_omim_name() applies eponym corrections", {
    result <- parse_omim_name(
        "PERIPHERAL DEMYELINATING NEUROPATHY, CENTRAL DYSMYELINATION, WAARDENBURG SYNDROME, AND HIRSCHSPRUNG DISEASE; PCWH",
        eponyms = c("waardenburg" = "Waardenburg", "hirschsprung" = "Hirschsprung")
    )
    expect_match(result$name, "Waardenburg")
    expect_match(result$name, "Hirschsprung")
})


# ---- patterns argument --------------------------------------------------

test_that("parse_omim_name() applies phrase patterns", {
    result <- parse_omim_name(
        "SHORT SYNDROME; SHORTSYN",
        patterns = c("short syndrome" = "SHORT syndrome")
    )
    expect_equal(result$name, "SHORT syndrome")
    expect_equal(result$abbreviation, "SHORTSYN")
})

test_that("parse_omim_name() patterns override word-level eponyms", {
    # eponyms would capitalise "Short" → "Short syndrome",
    # but the phrase pattern should win and produce "SHORT syndrome"
    result <- parse_omim_name(
        "SHORT SYNDROME; SHORTSYN",
        eponyms  = c("short" = "Short"),
        patterns = c("short syndrome" = "SHORT syndrome")
    )
    expect_equal(result$name, "SHORT syndrome")
})

test_that("parse_omim_name() longer patterns take priority over shorter ones", {
    # "short" pattern would produce "SHORT syndrome"; full-phrase pattern wins
    result <- parse_omim_name(
        "SHORT SYNDROME; SHORTSYN",
        patterns = c(
            "short"           = "SHORT",
            "short syndrome"  = "SHORT syndrome"
        )
    )
    expect_equal(result$name, "SHORT syndrome")
})

test_that("parse_omim_name() NULL patterns disables phrase replacement", {
    # Without patterns the word remains plain lowercase
    result <- parse_omim_name("SHORT SYNDROME; SHORTSYN", patterns = NULL)
    expect_equal(result$name, "short syndrome")
})


# ---- mixed-case input ---------------------------------------------------

test_that("parse_omim_name() accepts mixed-case input identically to all-caps", {
    expect_equal(
        parse_omim_name("Spastic Paraplegia 14, Autosomal Recessive; SPG14")$name,
        parse_omim_name("SPASTIC PARAPLEGIA 14, AUTOSOMAL RECESSIVE; SPG14")$name
    )
    expect_equal(
        parse_omim_name("Osteogenesis Imperfecta, Type XI; OI11")$name,
        parse_omim_name("OSTEOGENESIS IMPERFECTA, TYPE XI; OI11")$name
    )
})

test_that("parse_omim_name() preserves abbreviation case from input", {
    # Abbreviation is returned verbatim; only the name portion is uppercased
    # internally for parsing.
    expect_equal(
        parse_omim_name("Short syndrome; shortsyn")$abbreviation,
        "shortsyn"
    )
    expect_equal(
        parse_omim_name("SHORT SYNDROME; SHORTSYN")$abbreviation,
        "SHORTSYN"
    )
})


# ---- data frame interface -----------------------------------------------

test_that("parse_omim_name() works on data frames", {
    df <- data.frame(
        entry = c("SCHWANNOMATOSIS, VESTIBULAR; SWNV", "SCHIZOPHRENIA 12"),
        stringsAsFactors = FALSE
    )
    result <- parse_omim_name(df, col = "entry")
    expect_s3_class(result, "data.frame")
    expect_named(result, c("entry", "name", "abbreviation"))
    expect_equal(result$name, c("vestibular schwannomatosis", "schizophrenia 12"))
    expect_equal(result$abbreviation, c("SWNV", NA))
})

test_that("parse_omim_name() errors informatively for data frames", {
    df <- data.frame(entry = "SCHIZOPHRENIA 12", stringsAsFactors = FALSE)
    expect_error(parse_omim_name(df), "`col` must be specified")
    expect_error(parse_omim_name(df, col = "wrong"), 'Column "wrong" not found')
})
