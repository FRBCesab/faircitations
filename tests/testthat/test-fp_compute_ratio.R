# Test datasets
dois <- c(
  "10.1126/science.162.3859.1243",
  "10.1038/35002501",
  "https://doi.org/10.1111/j.1461-0248.2005.00792.x",
  "https://doi.org/10.xxxx/xxxx",
  "10.5281/zenodo.7390791",
  NA
)

doi_na <- c("string", rep(NA, 3))
doi_na <- doi_na[-1]

doi_not_in_oa <- c("https://doi.org/10.xxxx/xxxx")

doi_not_in_dafnee <- c("10.5281/zenodo.7390791")

test_that("Test fp_compute_ratio() for error (no API query)", {
  # Argument missing
  expect_error(
    fp_compute_ratio(),
    "Argument 'doi' is required",
    fixed = TRUE
  )

  expect_error(
    fp_compute_ratio(NULL),
    "Argument 'doi' is required",
    fixed = TRUE
  )

  # Not a string
  expect_error(
    fp_compute_ratio(data.frame()),
    "Argument 'doi' must be character",
    fixed = TRUE
  )

  expect_error(
    fp_compute_ratio(matrix()),
    "Argument 'doi' must be character",
    fixed = TRUE
  )

  expect_error(
    fp_compute_ratio(numeric()),
    "Argument 'doi' must be character",
    fixed = TRUE
  )

  expect_error(
    fp_compute_ratio(logical()),
    "Argument 'doi' must be character",
    fixed = TRUE
  )

  # Wrong length
  expect_error(
    fp_compute_ratio(character()),
    "Argument 'doi' must be of length > 0",
    fixed = TRUE
  )

  # API credentials
  expect_error(
    fp_compute_ratio(dois),
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
    fp_compute_ratio(dois),
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
    fp_compute_ratio(doi_na),
    "No valid DOI provided (missing value)",
    fixed = TRUE
  )
})

test_that("Test fp_compute_ratio() for error (w/ API queries)", {
  needs_api()

  # Set API email
  withr::local_options(
    list("openalexR.mailto" = "anonymous@mail.com")
  )

  expect_error(
    fp_compute_ratio(doi_not_in_oa),
    "No record found in OpenAlex",
    fixed = TRUE
  )

  expect_error(
    fp_compute_ratio(doi_not_in_dafnee),
    "No record found in DAFNEE database",
    fixed = TRUE
  )
})

test_that("Test fp_compute_ratio() for success", {
  needs_api()

  # Set API email
  withr::local_options(
    list("openalexR.mailto" = "anonymous@mail.com")
  )

  expect_silent(ratios <- fp_compute_ratio(dois))

  # Check list
  expect_true(inherits(ratios, "list"))
  expect_equal(length(ratios), 2L)

  expect_true("summary" %in% names(ratios))
  expect_true("ratios" %in% names(ratios))

  # Check summary data.frame
  expect_true(inherits(ratios$"summary", "data.frame"))
  expect_equal(ncol(ratios$"summary"), 2L)
  expect_equal(nrow(ratios$"summary"), 8L)

  expect_true("metric" %in% colnames(ratios$"summary"))
  expect_true("value" %in% colnames(ratios$"summary"))

  expect_equal(ratios$"summary"[1, 2], length(dois))
  expect_equal(ratios$"summary"[2, 2], sum(!is.na(dois)))
  expect_equal(ratios$"summary"[3, 2], ratios$"summary"[2, 2])
  expect_equal(ratios$"summary"[4, 2], 4L)
  expect_equal(ratios$"summary"[5, 2], 3L)
  expect_equal(ratios$"summary"[6, 2], 1L)
  expect_equal(ratios$"summary"[7, 2], 1L)
  expect_equal(ratios$"summary"[8, 2], 1L)

  # Check ratios vector
  expect_true(inherits(ratios$"ratios", "numeric"))
  expect_equal(length(ratios$"ratios"), 3L)

  expect_true(
    "Non-profit and academic friendly" %in% names(ratios$"ratios")
  )
  expect_true(
    "For-profit and academic friendly" %in% names(ratios$"ratios")
  )
  expect_true(
    "For-profit and non-academic friendly" %in% names(ratios$"ratios")
  )

  expect_equal(ratios$"ratios"[[1]], 0.33)
  expect_equal(ratios$"ratios"[[2]], 0.33)
  expect_equal(ratios$"ratios"[[3]], 0.33)
})
