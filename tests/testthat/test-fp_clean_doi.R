# Test dataset
x <- c(
  "10.1098/rsos.160384",
  "10.1098/RSOS.160384",
  "doi:10.1098/rsos.160384",
  "doi: 10.1098/rsos.160384",
  "http://doi.org/10.1098/rsos.160384",
  "http://dx.doi.org/10.1098/rsos.160384",
  "https://doi.org/10.1098/rsos.160384",
  "HTTPS://DOI.ORG/10.1098/RSOS.160384",
  "https://dx.doi.org/10.1098/rsos.160384",
  "HTTPS://DX.DOI.ORG/10.1098/RSOS.160384",
  " https://dx.doi.org/10.1098/rsos.160384",
  "https://dx.doi.org/10.1098/rsos.160384 ",
  " https://dx.doi.org/10.1098/rsos.160384 ",
  NA
)

test_that("Test fp_clean_doi() for success", {
  expect_silent(dois <- fp_clean_doi(x))

  dois <- fp_clean_doi(x)

  # Class
  expect_true(inherits(dois, "character"))
  expect_equal(length(dois), length(x))
  expect_true(any(is.na(dois)))

  # Cleaned DOI
  pos <- grep("^doi:", dois)
  expect_equal(length(pos), 0L)

  pos <- grep("^http", dois)
  expect_equal(length(pos), 0L)

  pos <- grep("[A-Z]", dois)
  expect_equal(length(pos), 0L)

  pos <- grep("\\s", dois)
  expect_equal(length(pos), 0L)

  unique_values <- unique(dois)
  expect_true(length(unique_values) == 2L)
  expect_true("10.1098/rsos.160384" %in% unique_values)
  expect_true(NA %in% unique_values)
})
