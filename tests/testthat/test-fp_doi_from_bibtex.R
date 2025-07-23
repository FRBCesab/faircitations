# Example BibTeX
filename <- system.file(
  file.path("extdata", "references.bib"),
  package = "fairpub"
)

test_that("Test fp_doi_from_bibtex() for error", {
  # Argument missing
  expect_error(
    fp_doi_from_bibtex(),
    "Argument 'bibtex' is required",
    fixed = TRUE
  )

  expect_error(
    fp_doi_from_bibtex(NULL),
    "Argument 'bibtex' is required",
    fixed = TRUE
  )

  # Not a file name
  expect_error(
    fp_doi_from_bibtex(data.frame()),
    "Argument 'bibtex' must be a character (BibTeX file name)",
    fixed = TRUE
  )

  expect_error(
    fp_doi_from_bibtex(matrix()),
    "Argument 'bibtex' must be a character (BibTeX file name)",
    fixed = TRUE
  )

  expect_error(
    fp_doi_from_bibtex(numeric()),
    "Argument 'bibtex' must be a character (BibTeX file name)",
    fixed = TRUE
  )

  expect_error(
    fp_doi_from_bibtex(logical()),
    "Argument 'bibtex' must be a character (BibTeX file name)",
    fixed = TRUE
  )

  # Wrong length
  expect_error(
    fp_doi_from_bibtex(rep(filename, 2)),
    "Argument 'bibtex' must be of length 1 (one BibTeX file)",
    fixed = TRUE
  )

  # File not found
  expect_error(
    fp_doi_from_bibtex("./wrong_path.bib"),
    "The file './wrong_path.bib' does not exist",
    fixed = TRUE
  )
})


test_that("Test fp_doi_from_bibtex() for success", {
  expect_silent(dois <- fp_doi_from_bibtex(filename))

  dois <- fp_doi_from_bibtex(filename)

  # Class
  expect_true(inherits(dois, "character"))
  expect_equal(length(dois), 38L)
  expect_true(any(is.na(dois)))

  # Cleaned DOI
  pos <- grep("^doi:", dois)
  expect_equal(length(pos), 0L)

  pos <- grep("^http", dois)
  expect_equal(length(pos), 0L)

  pos <- grep("[A-Z]", dois)
  expect_equal(length(pos), 0L)
})
