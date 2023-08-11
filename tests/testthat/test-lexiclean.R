test_chr <- c(
    "hypercalcemic sarcoidosis",
    "X-linked exudative vitreoretinopathy 2", # dash
    # capitalized
    "Jalili syndrome", "hepatitis D",
    # have number + letters
    "dilated cardiomyopathy 1FF", "chromosome 15q25 deletion syndrome",
    "spinocerebellar ataxia type 19/22",
    # have punctuation
    "Addison's disease", "arthrogryposis, renal dysfunction, and cholestasis 1",
    # roman numerals
    "trichorhinophalangeal syndrome type II", "nuclear type mitochondrial complex I deficiency 22",
    # complex
    "B-lymphoblastic leukemia/lymphoma, BCR-ABL1–like"
)

test_that("lexiclean.character() works", {
    expect_equal(
        lexiclean(test_chr),
        c(
            "hypercalcemicsarcoidosis",
            "xlinkedexudativevitreoretinopathyii",
            "jalilisyndrome", "hepatitisd",
            "dilatedcardiomyopathyiff", "chromosomexvqxxvdeletionsyndrome",
            "spinocerebellarataxiatypexixxxii",
            "addisonsdisease", "arthrogryposisrenaldysfunctionandcholestasisi",
            "trichorhinophalangealsyndrometypeii", "nucleartypemitochondrialcomplexideficiencyxxii",
            "blymphoblasticleukemialymphomabcrablilike"
        )
    )
})

test_that("lexiclean.character() individual modifications work", {
    expect_equal(
        lexiclean(test_chr, mod = "lc"),
        c(
            "hypercalcemic sarcoidosis",
            "x-linked exudative vitreoretinopathy 2",
            "jalili syndrome", "hepatitis d",
            "dilated cardiomyopathy 1ff", "chromosome 15q25 deletion syndrome",
            "spinocerebellar ataxia type 19/22",
            "addison's disease", "arthrogryposis, renal dysfunction, and cholestasis 1",
            "trichorhinophalangeal syndrome type ii", "nuclear type mitochondrial complex i deficiency 22",
            "b-lymphoblastic leukemia/lymphoma, bcr-abl1–like"
        )
    )
    expect_equal(
        lexiclean(test_chr, mod = "roman"),
        c(
            "hypercalcemic sarcoidosis",
            "X-linked exudative vitreoretinopathy II",
            "Jalili syndrome", "hepatitis D",
            "dilated cardiomyopathy IFF", "chromosome XVqXXV deletion syndrome",
            "spinocerebellar ataxia type XIX/XXII",
            "Addison's disease", "arthrogryposis, renal dysfunction, and cholestasis I",
            "trichorhinophalangeal syndrome type II", "nuclear type mitochondrial complex I deficiency XXII",
            "B-lymphoblastic leukemia/lymphoma, BCR-ABLI–like"
        )
    )
    expect_equal(
        lexiclean(test_chr, mod = "rm_space"),
        c(
            "hypercalcemicsarcoidosis",
            "X-linkedexudativevitreoretinopathy2",
            "Jalilisyndrome", "hepatitisD",
            "dilatedcardiomyopathy1FF", "chromosome15q25deletionsyndrome",
            "spinocerebellarataxiatype19/22",
            "Addison'sdisease", "arthrogryposis,renaldysfunction,andcholestasis1",
            "trichorhinophalangealsyndrometypeII", "nucleartypemitochondrialcomplexIdeficiency22",
            "B-lymphoblasticleukemia/lymphoma,BCR-ABL1–like"
        )
    )
    expect_equal(
        lexiclean(test_chr, mod = "rm_punct"),
        c(
            "hypercalcemic sarcoidosis",
            "Xlinked exudative vitreoretinopathy 2",
            "Jalili syndrome", "hepatitis D",
            "dilated cardiomyopathy 1FF", "chromosome 15q25 deletion syndrome",
            "spinocerebellar ataxia type 1922",
            "Addisons disease", "arthrogryposis renal dysfunction and cholestasis 1",
            "trichorhinophalangeal syndrome type II", "nuclear type mitochondrial complex I deficiency 22",
            "Blymphoblastic leukemialymphoma BCRABL1like"
        )
    )
})
