# Build the omim_qualifiers dataset
#
# omim_qualifiers is a fully hand-curated character vector of OMIM
# adjective/onset qualifier tokens that trigger inversion rearrangement in
# parse_omim_name(). These are qualifiers that OMIM places as comma-separated
# tokens after the primary disease term but that belong before it in natural-
# language order (e.g. "DEAFNESS, CONGENITAL" → "congenital deafness").
#
# NOTE: Structural qualifiers — pure numbers, TYPE/MULTIPLE TYPES, and
# definitive inheritance terms such as AUTOSOMAL RECESSIVE or X-LINKED — are
# handled by hardcoded rules in omim_has_forcing() and are NOT included here.
#
# WORKFLOW:
#   1. Add new qualifier tokens (uppercase, exactly as they appear in OMIM
#      comma-separated qualifier position) to the vector below.
#   2. Source this file (or run devtools::load_all() + usethis::use_data()).
#   3. Rebuild package documentation: devtools::document().

omim_qualifiers <- c(
  "BILATERAL",
  "CHILDHOOD-ONSET",
  "CONGENITAL",
  "EARLY-ONSET",
  "FAMILIAL",
  "FOCAL",
  "GENERALIZED",
  "HEREDITARY",
  "HYPOMYELINATING",
  "ISOLATED",
  "JUVENILE",
  "LATE-ONSET",
  "NEONATAL",
  "NONISOLATED",
  "NONPHOTOSENSITIVE",
  "NONSYNDROMIC",
  "PHOTOSENSITIVE",
  "POSTSYNAPTIC",
  "PRESYNAPTIC",
  "PROGRESSIVE",
  "SUSCEPTIBILITY TO",
  "SYNDROMIC",
  "UNILATERAL",
  "VESTIBULAR"
)

usethis::use_data(omim_qualifiers, overwrite = TRUE)
