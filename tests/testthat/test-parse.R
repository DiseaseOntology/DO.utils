# parse_omim_name() -------------------------------------------------------
# Test data: tests/testthat/data/omim/entry_names.tsv
#   - Max 4 examples per number of comma-separated qualifiers (0–5+)
#   - 'name' and 'abbreviation' columns are manually curated expected values
omim_data <- readr::read_tsv(
  test_path("data", "omim", "entry_names.tsv"),
  col_types = readr::cols(.default = "c"),
  na = ""
)

test_that("parse_omim_name() preserves abbreviations correctly", {
  result <- parse_omim_name(omim_data$entry)
  expect_equal(
    stringr::str_extract(result, ";.*"),
    stringr::str_extract(omim_data$entry, ";.*")
  )
})


# names with no comma (0 qualifiers) --------------------------------------

test_that("parse_omim_name() lowercases simple entries with no comma", {
  expect_equal(parse_omim_name("SCHIZOPHRENIA 12"), "schizophrenia 12")
  expect_equal(parse_omim_name("DYSTONIA 12; DYT12"), "dystonia 12; DYT12")
  # Proper nouns not corrected when not in disease_eponyms
  expect_equal(
    parse_omim_name("HURIEZ SYNDROME; HRZ", eponyms = NULL),
    "huriez syndrome; HRZ"
  )
  # Multi-word proper noun with internal lowercase particles
  expect_equal(
    parse_omim_name("DEN HOED-DE BOER-VOISIN SYNDROME; DHDBV", eponyms = NULL),
    "den hoed-de boer-voisin syndrome; DHDBV"
  )
})


# 1 comma ---------------------------------------------------------------

test_that("parse_omim_name() rearranges a single pre-qualifier", {
  expect_equal(
    parse_omim_name("SPASTIC PARAPLEGIA 14, AUTOSOMAL RECESSIVE; SPG14"),
    "autosomal recessive spastic paraplegia 14; SPG14"
  )
  expect_equal(
    parse_omim_name("SCHWANNOMATOSIS, VESTIBULAR; SWNV"),
    "vestibular schwannomatosis; SWNV"
  )
  # Number embedded in qualifier token
  expect_equal(
    parse_omim_name("DEAFNESS, AUTOSOMAL RECESSIVE 117; DFNB117"),
    "autosomal recessive deafness 117; DFNB117"
  )
})

test_that("parse_omim_name() handles TYPE as a post-qualifier", {
  expect_equal(
    parse_omim_name("OSTEOGENESIS IMPERFECTA, TYPE XI; OI11"),
    "osteogenesis imperfecta type XI; OI11"
  )
})


# 2 commas --------------------------------------------------------------

test_that("parse_omim_name() leaves feature-list entries unchanged", {
  # No forcing qualifier = keep in original comma order
  expect_equal(
    parse_omim_name(
      "DEVELOPMENTAL DELAY, IMPAIRED SPEECH, AND BEHAVIORAL ABNORMALITIES; DDISBA" # nolint: line_length_linter.
    ),
    "developmental delay, impaired speech, and behavioral abnormalities; DDISBA"
  )
  expect_equal(
    parse_omim_name(
      "SPASTIC TETRAPLEGIA, THIN CORPUS CALLOSUM, AND PROGRESSIVE MICROCEPHALY; SPATCCM" # nolint: line_length_linter.
    ),
    "spastic tetraplegia, thin corpus callosum, and progressive microcephaly; SPATCCM" # nolint: line_length_linter.
  )
})

test_that("parse_omim_name() rearranges two-qualifier entries with CONGENITAL", { # nolint: line_length_linter.
  expect_equal(
    parse_omim_name(
      "DEAFNESS, CONGENITAL, WITH INNER EAR AGENESIS, MICROTIA, AND MICRODONTIA"
    ),
    "congenital deafness with inner ear agenesis, microtia, and microdontia"
  )
  # Trailing phrase part of no-abbreviation entry
  expect_equal(
    parse_omim_name(
      "LIPODYSTROPHY, GENERALIZED, WITH IMPAIRED INTELLECTUAL DEVELOPMENT, DEAFNESS, SHORT STATURE, AND SLENDER BONES" # nolint: line_length_linter.
    ),
    "generalized lipodystrophy with impaired intellectual development, deafness, short stature, and slender bones" # nolint: line_length_linter.
  )
})

test_that("parse_omim_name() handles SUSCEPTIBILITY TO prefix", {
  expect_equal(
    parse_omim_name("SCOLIOSIS, ISOLATED, SUSCEPTIBILITY TO, 1; IS1"),
    "susceptibility to isolated scoliosis 1; IS1"
  )
})


# 3 commas --------------------------------------------------------------

test_that("parse_omim_name() rearranges 3-qualifier entries", {
  expect_equal(
    parse_omim_name(
      "EPILEPSY, PROGRESSIVE MYOCLONIC, 4, WITH OR WITHOUT RENAL FAILURE; EPM4"
    ),
    "progressive myoclonic epilepsy 4 with or without renal failure; EPM4"
  )
  expect_equal(
    parse_omim_name(
      "MACROTHROMBOCYTOPENIA, ISOLATED, 1, AUTOSOMAL DOMINANT; MACTHC1"
    ),
    "autosomal dominant isolated macrothrombocytopenia 1; MACTHC1"
  )
  expect_equal(
    parse_omim_name(
      "NEURODEGENERATION, CHILDHOOD-ONSET, WITH ATAXIA, TREMOR, OPTIC ATROPHY, AND COGNITIVE DECLINE; CONATOC" # nolint: line_length_linter.
    ),
    "childhood-onset neurodegeneration with ataxia, tremor, optic atrophy, and cognitive decline; CONATOC" # nolint: line_length_linter.
  )
  # Proper nouns not capitalized without eponyms
  expect_equal(
    parse_omim_name(
      "PERIPHERAL DEMYELINATING NEUROPATHY, CENTRAL DYSMYELINATION, WAARDENBURG SYNDROME, AND HIRSCHSPRUNG DISEASE; PCWH", # nolint: line_length_linter.
      eponyms = NULL
    ),
    "peripheral demyelinating neuropathy, central dysmyelination, waardenburg syndrome, and hirschsprung disease; PCWH" # nolint: line_length_linter.
  )
})


# 4 commas --------------------------------------------------------------

test_that("parse_omim_name() rearranges 4-qualifier entries", {
  expect_equal(
    parse_omim_name(
      "LEUKODYSTROPHY, HYPOMYELINATING, 23, WITH ATAXIA, DEAFNESS, LIVER DYSFUNCTION, AND DILATED CARDIOMYOPATHY; HLD23" # nolint: line_length_linter.
    ),
    "hypomyelinating leukodystrophy 23 with ataxia, deafness, liver dysfunction, and dilated cardiomyopathy; HLD23" # nolint: line_length_linter.
  )
  expect_equal(
    parse_omim_name(
      "NEUROPATHY, HEREDITARY MOTOR AND SENSORY, WITH DEAFNESS, IMPAIRED INTELLECTUAL DEVELOPMENT, AND ABSENT SENSORY LARGE MYELINATED FIBERS" # nolint: line_length_linter.
    ),
    "hereditary motor and sensory neuropathy with deafness, impaired intellectual development, and absent sensory large myelinated fibers" # nolint: line_length_linter.
  )
  # Feature list (no forcing) → kept in original order
  expect_equal(
    parse_omim_name(
      "FETAL AKINESIA, RESPIRATORY INSUFFICIENCY, MICROCEPHALY, POLYMICROGYRIA, AND DYSMORPHIC FACIES; FARIMPD" # nolint: line_length_linter.
    ),
    "fetal akinesia, respiratory insufficiency, microcephaly, polymicrogyria, and dysmorphic facies; FARIMPD" # nolint: line_length_linter.
  )
  # Feature list (no forcing) → kept in original order
  expect_equal(
    parse_omim_name(
      "TELANGIECTASIA, IMPAIRED INTELLECTUAL DEVELOPMENT, MICROCEPHALY, METAPHYSEAL DYSPLASIA, EYE ABNORMALITIES, AND SHORT STATURE; TIMES" # nolint: line_length_linter.
    ),
    "telangiectasia, impaired intellectual development, microcephaly, metaphyseal dysplasia, eye abnormalities, and short stature; TIMES" # nolint: line_length_linter.
  )
})


# 5+ commas --------------------------------------------------------------

test_that("parse_omim_name() rearranges complex multi-qualifier entries", {
  # Definitive pre-qualifier listed AFTER a trailing-phrase AND-token
  expect_equal(
    parse_omim_name(
      "MYASTHENIC SYNDROME, CONGENITAL, 7A, PRESYNAPTIC, AND DISTAL MOTOR NEUROPATHY, AUTOSOMAL DOMINANT; CMS7A" # nolint: line_length_linter.
    ),
    "autosomal dominant presynaptic congenital myasthenic syndrome 7A and distal motor neuropathy; CMS7A" # nolint: line_length_linter.
  )
  # Feature list (no forcing) → kept in original order
  expect_equal(
    parse_omim_name(
      "NEURODEVELOPMENTAL DISORDER WITH GROWTH IMPAIRMENT, QUADRIPARESIS, AND POOR OR ABSENT SPEECH; NEDGQS" # nolint: line_length_linter.
    ),
    "neurodevelopmental disorder with growth impairment, quadriparesis, and poor or absent speech; NEDGQS" # nolint: line_length_linter.
  )
  expect_equal(
    parse_omim_name(
      "NEURODEVELOPMENTAL DISORDER WITH HYPOTONIA, SPEECH DELAY, AND DYSMORPHIC FACIES; NEDHSF" # nolint: line_length_linter.
    ),
    "neurodevelopmental disorder with hypotonia, speech delay, and dysmorphic facies; NEDHSF" # nolint: line_length_linter.
  )
})


# capitalization fixes ---------------------------------------------------

test_that("parse_omim_name() uppercases Roman numerals after TYPE", {
  expect_equal(
    parse_omim_name("AMELOGENESIS IMPERFECTA, TYPE IB"),
    "amelogenesis imperfecta type IB"
  )
  expect_match(parse_omim_name("SOME DISEASE, TYPE IIIA"), "type IIIA")
})

test_that("parse_omim_name() uppercases alphanumeric subtype codes", {
  # 7A is a trailing number extracted from a comma qualifier
  expect_match(
    parse_omim_name(
      "MYASTHENIC SYNDROME, CONGENITAL, 7A, PRESYNAPTIC, AND DISTAL MOTOR NEUROPATHY, AUTOSOMAL DOMINANT; CMS7A" # nolint: line_length_linter.
    ),
    "syndrome 7A"
  )
})

test_that("parse_omim_name() applies immunoglobulin capitalization", {
  # HYPER-IGE = hyper-IgE
  expect_match(
    parse_omim_name(
      "ERYTHRODERMA, CONGENITAL, WITH PALMOPLANTAR KERATODERMA, HYPOTRICHOSIS, AND HYPER-IGE; EPKHE" # nolint: line_length_linter.
    ),
    "hyper-IgE"
  )
})


# eponyms argument ------------------------------------------------------

test_that("parse_omim_name() applies eponym corrections", {
  result <- parse_omim_name(
    "PERIPHERAL DEMYELINATING NEUROPATHY, CENTRAL DYSMYELINATION, WAARDENBURG SYNDROME, AND HIRSCHSPRUNG DISEASE; PCWH", # nolint: line_length_linter.
    eponyms = c("waardenburg" = "Waardenburg", "hirschsprung" = "Hirschsprung")
  )
  expect_match(result, "Waardenburg")
  expect_match(result, "Hirschsprung")
})


# patterns argument -----------------------------------------------------

test_that("parse_omim_name() applies phrase patterns", {
  result <- parse_omim_name(
    "SHORT SYNDROME; SHORTSYN",
    patterns = c("short syndrome" = "SHORT syndrome")
  )
  expect_equal(result, "SHORT syndrome; SHORTSYN")
})

test_that("parse_omim_name() patterns override word-level eponyms", {
  # eponyms would capitalise "Short" = "Short syndrome",
  # but the phrase pattern should win and produce "SHORT syndrome"
  result <- parse_omim_name(
    "SHORT SYNDROME; SHORTSYN",
    eponyms = c("short" = "Short"),
    patterns = c("short syndrome" = "SHORT syndrome")
  )
  expect_equal(result, "SHORT syndrome; SHORTSYN")
})

test_that("parse_omim_name() longer patterns take priority over shorter ones", {
  # "short" pattern would produce "SHORT syndrome"; full-phrase pattern wins
  result <- parse_omim_name(
    "SHORT SYNDROME; SHORTSYN",
    patterns = c(
      "short" = "SHORT",
      "short syndrome" = "SHORT syndrome"
    )
  )
  expect_equal(result, "SHORT syndrome; SHORTSYN")
})

test_that("parse_omim_name() NULL patterns disables phrase replacement", {
  # Without patterns the word remains plain lowercase
  result <- parse_omim_name("SHORT SYNDROME; SHORTSYN", patterns = NULL)
  expect_equal(result, "short syndrome; SHORTSYN")
})


# mixed-case input ------------------------------------------------------

test_that("parse_omim_name() accepts mixed-case input identically to all-caps", { # nolint: line_length_linter.
  expect_equal(
    parse_omim_name("Spastic Paraplegia 14, Autosomal Recessive; SPG14"),
    parse_omim_name("SPASTIC PARAPLEGIA 14, AUTOSOMAL RECESSIVE; SPG14")
  )
  expect_equal(
    parse_omim_name("Osteogenesis Imperfecta, Type XI; OI11"),
    parse_omim_name("OSTEOGENESIS IMPERFECTA, TYPE XI; OI11")
  )
})

test_that("parse_omim_name() preserves abbreviation case from input", {
  # Abbreviation is returned verbatim; only the name portion is uppercased
  # internally for parsing.
  expect_match(
    parse_omim_name("Short syndrome; shortsyn"),
    "shortsyn"
  )
  expect_match(
    parse_omim_name("SHORT SYNDROME; SHORTSYN"),
    "SHORTSYN"
  )
})
