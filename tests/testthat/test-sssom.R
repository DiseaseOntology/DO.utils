# === 1. BASIC FUNCTIONALITY ===

test_that("recode simple rule column", {
  df <- tibble::tibble(
    curation_rule = c("not_disease", "hierarchy (sc)", "not_disease")
  )
  
  result <- recode_sssom_rules(df, rule_cols = "curation_rule")
  expect_equal(result$curation_rule, c("not-disease", "hierarchy-primary", "not-disease"))
})

test_that("recode simple comment column with single rule", {
  df <- tibble::tibble(
    comment = c("Rules: not_disease.")
  )
  
  result <- recode_sssom_rules(df, comment_cols = "comment")
  expect_equal(result$comment, "Rules: not-disease.")
})

test_that("recode both rule and comment columns together", {
  df <- tibble::tibble(
    curation_rule = c("not_disease", "hierarchy (sc)"),
    comment = c("Rules: not_disease.", "Rules: hierarchy (sc), other_map.")
  )
  
  result <- recode_sssom_rules(
    df,
    rule_cols = "curation_rule",
    comment_cols = "comment"
  )
  
  expect_equal(result$curation_rule, c("not-disease", "hierarchy-primary"))
  expect_equal(result$comment, c("Rules: not-disease.", "Rules: hierarchy-primary, other-map."))
})

test_that("recode comment with multiple rules", {
  df <- tibble::tibble(
    comment = c("Rules: not_disease, hierarchy (sc), other_map.")
  )
  
  result <- recode_sssom_rules(df, comment_cols = "comment")
  expect_equal(result$comment, "Rules: not-disease, hierarchy-primary, other-map.")
})


# === 2. RULE COLUMN EDGE CASES ===

test_that("handle NA values in rule column", {
  df <- tibble::tibble(
    curation_rule = c("not_disease", NA, "hierarchy (sc)")
  )
  
  result <- recode_sssom_rules(df, rule_cols = "curation_rule")
  expect_equal(result$curation_rule, c("not-disease", NA, "hierarchy-primary"))
})

test_that("handle unmapped values in rule column (use .default)", {
  df <- tibble::tibble(
    curation_rule = c("not_disease", "unmapped_rule", "hierarchy (sc)")
  )
  
  result <- recode_sssom_rules(df, rule_cols = "curation_rule")
  expect_equal(result$curation_rule, c("not-disease", "unmapped_rule", "hierarchy-primary"))
})

test_that("recode multiple rule columns", {
  df <- tibble::tibble(
    rule_col1 = c("not_disease", "hierarchy (sc)"),
    rule_col2 = c("other_map", "not_disease")
  )
  
  result <- recode_sssom_rules(
    df,
    rule_cols = c("rule_col1", "rule_col2")
  )
  
  expect_equal(result$rule_col1, c("not-disease", "hierarchy-primary"))
  expect_equal(result$rule_col2, c("other-map", "not-disease"))
})


# === 3. COMMENT COLUMN EDGE CASES ===

test_that("preserve qualifiers in parentheses", {
  df <- tibble::tibble(
    comment = c("Rules: not_disease (from NCI:00000), hierarchy (sc) (from UMLS:12345).")
  )
  
  result <- recode_sssom_rules(df, comment_cols = "comment")
  expect_equal(result$comment, "Rules: not-disease (from NCI:00000), hierarchy-primary (from UMLS:12345).")
})

test_that("handle mixed qualifiers (some rules have them, some don't)", {
  df <- tibble::tibble(
    comment = c("Rules: not_disease (from NCI:00000), hierarchy (sc), other_map (from MIM:618131).")
  )
  
  result <- recode_sssom_rules(df, comment_cols = "comment")
  expect_equal(result$comment, "Rules: not-disease (from NCI:00000), hierarchy-primary, other-map (from MIM:618131).")
})

test_that("handle ending with period", {
  df <- tibble::tibble(
    comment = c("Rules: not_disease, hierarchy (sc).")
  )
  
  result <- recode_sssom_rules(df, comment_cols = "comment")
  expect_equal(result$comment, "Rules: not-disease, hierarchy-primary.")
})

test_that("handle ending with semicolon", {
  df <- tibble::tibble(
    comment = c("Rules: not_disease, hierarchy (sc);")
  )
  
  result <- recode_sssom_rules(df, comment_cols = "comment")
  expect_equal(result$comment, "Rules: not-disease, hierarchy-primary.")
})

test_that("handle no ending punctuation", {
  df <- tibble::tibble(
    comment = c("Rules: not_disease, hierarchy (sc)")
  )
  
  result <- recode_sssom_rules(df, comment_cols = "comment")
  expect_equal(result$comment, "Rules: not-disease, hierarchy-primary.")
})

test_that("handle Rules pattern in middle of comment", {
  df <- tibble::tibble(
    comment = c("Some text before. Rules: not_disease, hierarchy (sc). Some text after.")
  )
  
  result <- recode_sssom_rules(df, comment_cols = "comment")
  expect_equal(result$comment, "Some text before. Rules: not-disease, hierarchy-primary. Some text after.")
})

test_that("handle NA values in comment column", {
  df <- tibble::tibble(
    comment = c("Rules: not_disease.", NA, "Rules: hierarchy (sc).")
  )
  
  result <- recode_sssom_rules(df, comment_cols = "comment")
  expect_equal(result$comment, c("Rules: not-disease.", NA, "Rules: hierarchy-primary."))
})

test_that("recode multiple comment columns", {
  df <- tibble::tibble(
    comment1 = c("Rules: not_disease.", "Rules: hierarchy (sc)."),
    comment2 = c("Rules: other_map.", "Rules: not_disease, hierarchy (sc).")
  )
  
  result <- recode_sssom_rules(
    df,
    comment_cols = c("comment1", "comment2")
  )
  
  expect_equal(result$comment1, c("Rules: not-disease.", "Rules: hierarchy-primary."))
  expect_equal(result$comment2, c("Rules: other-map.", "Rules: not-disease, hierarchy-primary."))
})

test_that("leave text outside Rules pattern unchanged", {
  df <- tibble::tibble(
    comment = c("Some important context. Rules: not_disease. This also matters.")
  )
  
  result <- recode_sssom_rules(df, comment_cols = "comment")
  expect_equal(result$comment, "Some important context. Rules: not-disease. This also matters.")
})

test_that("handle comments with no Rules pattern", {
  df <- tibble::tibble(
    comment = c("Just some free text with no rules.")
  )
  
  result <- recode_sssom_rules(df, comment_cols = "comment")
  expect_equal(result$comment, "Just some free text with no rules.")
})

test_that("handle case-insensitive Rules keyword", {
  df <- tibble::tibble(
    comment = c("rules: not_disease.", "RULES: hierarchy (sc).")
  )
  
  result <- recode_sssom_rules(df, comment_cols = "comment")
  expect_equal(result$comment, c("Rules: not-disease.", "Rules: hierarchy-primary."))
})


# cannot handle periods or semicolons inside qualifiers
# test_that("handle periods inside qualifiers", {
#   df <- tibble::tibble(
#     comment = c("Rules: not_disease (from U.S. source), hierarchy (sc).")
#   )
#
#   result <- recode_sssom_rules(df, comment_cols = "comment")
#   expect_equal(result$comment, "Rules: not-disease (from U.S. source), hierarchy-primary.")
# })