# Test datasets
dois <- c(
  "10.1126/science.162.3859.1243",
  "https://doi.org/10.1111/j.1461-0248.2005.00792.x",
  "10.1038/35002501",
  "https://doi.org/10.xxxx/xxxx",
  "10.21105/joss.05753"
)

doi_na <- c("string", NA)
doi_na <- doi_na[-1]

doi_not_in_oa <- "https://doi.org/10.xxxx/xxxx"

doi_not_in_dafnee <- "10.5281/zenodo.7390791"

test_that("Test fp_article_fairness() for error", {
  # Argument missing
  expect_error(
    fp_article_fairness(),
    "Argument 'doi' is required",
    fixed = TRUE
  )

  expect_error(
    fp_article_fairness(NULL),
    "Argument 'doi' is required",
    fixed = TRUE
  )

  # Not a string
  expect_error(
    fp_article_fairness(data.frame()),
    "Argument 'doi' must be character",
    fixed = TRUE
  )

  expect_error(
    fp_article_fairness(matrix()),
    "Argument 'doi' must be character",
    fixed = TRUE
  )

  expect_error(
    fp_article_fairness(numeric()),
    "Argument 'doi' must be character",
    fixed = TRUE
  )

  expect_error(
    fp_article_fairness(logical()),
    "Argument 'doi' must be character",
    fixed = TRUE
  )

  # Wrong length
  expect_error(
    fp_article_fairness(dois),
    "Argument 'doi' must be of length 1",
    fixed = TRUE
  )

  # API credentials
  expect_error(
    fp_article_fairness(dois[1]),
    paste0(
      "Be polite with OpenAlex API and run: ",
      "`options(openalexR.mailto = 'your_email')`"
    ),
    fixed = TRUE
  )

  withr::local_options(
    list("openalexR.mailto" = NULL)
  )

  expect_error(
    fp_article_fairness(dois[1]),
    paste0(
      "Be polite with OpenAlex API and run: ",
      "`options(openalexR.mailto = 'your_email')`"
    ),
    fixed = TRUE
  )

  # Set API email
  withr::local_options(
    list("openalexR.mailto" = "anonymous@mail.com")
  )

  # Only NA in DOI
  expect_error(
    fp_article_fairness(doi_na),
    "Argument 'doi' cannot be NA",
    fixed = TRUE
  )
})

test_that("Test fp_article_fairness() for success", {
  needs_api()

  # Set API email
  withr::local_options(
    list("openalexR.mailto" = "anonymous@mail.com")
  )

  # Test for NP journal
  expect_silent(res <- fp_article_fairness(dois[1]))

  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2L)
  expect_equal(nrow(res), 1L)

  expect_true("journal" %in% colnames(res))
  expect_true("fairness" %in% colnames(res))

  expect_equal(res[1, "journal"], "Science")
  expect_equal(res[1, "fairness"], "Non-profit and academic friendly")

  # Test for FP & academic journal
  expect_silent(res <- fp_article_fairness(dois[2]))

  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2L)
  expect_equal(nrow(res), 1L)

  expect_true("journal" %in% colnames(res))
  expect_true("fairness" %in% colnames(res))

  expect_equal(res[1, "journal"], "Ecology Letters")
  expect_equal(res[1, "fairness"], "For-profit and academic friendly")

  # Test for FP & non-academic journal
  expect_silent(res <- fp_article_fairness(dois[3]))

  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2L)
  expect_equal(nrow(res), 1L)

  expect_true("journal" %in% colnames(res))
  expect_true("fairness" %in% colnames(res))

  expect_equal(res[1, "journal"], "Nature")
  expect_equal(res[1, "fairness"], "For-profit and non-academic friendly")

  # Test for not found in OpenAlex
  expect_silent(res <- fp_article_fairness(dois[4]))

  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2L)
  expect_equal(nrow(res), 1L)

  expect_true("journal" %in% colnames(res))
  expect_true("fairness" %in% colnames(res))

  expect_true(is.na(res[1, "journal"]))
  expect_equal(res[1, "fairness"], "Record not found in OpenAlex")

  # Test for not found in OpenAlex
  expect_silent(res <- fp_article_fairness(dois[5]))

  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2L)
  expect_equal(nrow(res), 1L)

  expect_true("journal" %in% colnames(res))
  expect_true("fairness" %in% colnames(res))

  expect_equal(res[1, "journal"], "The Journal of Open Source Software")
  expect_equal(res[1, "fairness"], "Record not found in DAFNEE database")
})
