# Test datasets
journals <- c(
  "Science",
  "Science of Nature",
  "The Science of Nature",
  "Ecology Letters"
)

journal_na <- c("string", NA)
journal_na <- journal_na[-1]


test_that("Test fp_journal_fairness() for error", {
  # Argument missing
  expect_error(
    fp_journal_fairness(),
    "Argument 'journal' is required",
    fixed = TRUE
  )

  expect_error(
    fp_journal_fairness(NULL),
    "Argument 'journal' is required",
    fixed = TRUE
  )

  # Not a string
  expect_error(
    fp_journal_fairness(data.frame()),
    "Argument 'journal' must be character",
    fixed = TRUE
  )

  expect_error(
    fp_journal_fairness(matrix()),
    "Argument 'journal' must be character",
    fixed = TRUE
  )

  expect_error(
    fp_journal_fairness(numeric()),
    "Argument 'journal' must be character",
    fixed = TRUE
  )

  expect_error(
    fp_journal_fairness(logical()),
    "Argument 'journal' must be character",
    fixed = TRUE
  )

  # Wrong length
  expect_error(
    fp_journal_fairness(journals),
    "Argument 'journal' must be of length 1",
    fixed = TRUE
  )

  # Only NA in DOI
  expect_error(
    fp_journal_fairness(journal_na),
    "Argument 'journal' cannot be NA",
    fixed = TRUE
  )
})

test_that("Test fp_journal_fairness() for success", {
  # Test for exact match
  expect_silent(res <- fp_journal_fairness(journals[1]))

  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2L)
  expect_equal(nrow(res), 1L)

  expect_true("journal" %in% colnames(res))
  expect_true("fairness" %in% colnames(res))

  expect_equal(res[1, "journal"], journals[1])
  expect_equal(res[1, "fairness"], "Non-profit and academic friendly")

  # Test for fuzzy suggestions
  expect_invisible(suppressMessages(fp_journal_fairness(journals[2])))
  expect_message(res <- fp_journal_fairness(journals[2]))

  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2L)
  expect_equal(nrow(res), 1L)

  expect_true("journal" %in% colnames(res))
  expect_true("fairness" %in% colnames(res))

  expect_true(is.na(res[1, "journal"]))
  expect_true(is.na(res[1, "fairness"]))

  # Test for suggested match
  expect_silent(res <- fp_journal_fairness(journals[3]))

  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2L)
  expect_equal(nrow(res), 1L)

  expect_true("journal" %in% colnames(res))
  expect_true("fairness" %in% colnames(res))

  expect_equal(res[1, "journal"], journals[3])
  expect_equal(res[1, "fairness"], "For-profit and non-academic friendly")

  # Test for the third fairness category
  expect_silent(res <- fp_journal_fairness(journals[4]))

  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2L)
  expect_equal(nrow(res), 1L)

  expect_true("journal" %in% colnames(res))
  expect_true("fairness" %in% colnames(res))

  expect_equal(res[1, "journal"], journals[4])
  expect_equal(res[1, "fairness"], "For-profit and academic friendly")
})
