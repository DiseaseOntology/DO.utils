# Build the disease_cap_patterns dataset
#
# disease_cap_patterns is a fully hand-curated named character vector of
# phrase-level regex substitutions applied to lowercased disease entry names by
# parse_omim_name(). Patterns run AFTER (and therefore OVERRIDE) word-level
# disease_eponyms replacements.
#
# KEY DESIGN NOTES:
#   * Names  : case-insensitive regex patterns matched against the full
#              lowercased name string.
#   * Values : replacement strings (may include backreferences, e.g. "\\1").
#   * Order  : longer patterns are applied before shorter ones so more
#              specific patterns take precedence—no manual ordering needed.
#   * These  : override any conflicting disease_eponyms word-level decisions.
#
# WORKFLOW:
#   1. Add entries to disease_cap_patterns below.
#   2. Source this file (or run devtools::load_all() + usethis::use_data()).
#   3. Rebuild package documentation: devtools::document().

disease_cap_patterns <- c(
  "^adult syndrome$" = "ADULT syndrome",
  "^arts syndrome$" = "Arts syndrome",
  "-boycott-" = "-Boycott-",
  "baraitser-winter" = "Baraitser-Winter",
  "barber-say" = "Barber-Say",
  "burn-mckeown" = "Burn-McKeown",
  "^cousin syndrome$" = "Cousin syndrome",
  "^dent disease" = "Dent disease",
  "(disorder.+glycosylation.+)([ivx]+)([a-z]{1,2})$" = "\\1\\U\\2\\L\\3",
  "even-plus" = "EVEN-plus",
  "gjb2/gjb3" = "GJB2/GJB3",
  "gpr98/pdzd7" = "GPR98/PDZD7",
  "^glass syndrome$" = "Glass syndrome",
  "^gracile syndrome$" = "GRACILE syndrome",
  "^image(.{0,4})syndrome" = "IMAGE\\1syndrome",
  "johanson-blizzard" = "Johanson-Blizzard",
  "lowry-wood" = "Lowry-Wood",
  "melnick-needles" = "Melnick-Needles",
  "northern epilepsy" = "Northern epilepsy",
  "senior-loken" = "Senior-Loken",
  "short syndrome" = "SHORT syndrome",
  "tan-almurshedi" = "Tan-Almurshedi",
  "west nile" = "West Nile",
  "(-w)(hite)|(w)(hite-)|(w)(hite)( syndrome)" = "\\U\\1\\L\\2\\3"
)

usethis::use_data(disease_cap_patterns, overwrite = TRUE)
