test_that("Test fp_list_dafnee_journals() for success", {
  x <- fp_list_dafnee_journals()

  # Class
  expect_true(inherits(x, "data.frame"))
  expect_equal(ncol(x), 3L)
  expect_true(nrow(x) > 0L)

  # Names
  expect_true("journal" %in% colnames(x))
  expect_true("business_model" %in% colnames(x))
  expect_true("academic_friendly" %in% colnames(x))

  # Journal
  expect_false(any(is.na(x$"journal")))
  expect_false(any(duplicated(x$"journal")))

  # Business model
  expect_false(any(is.na(x$"business_model")))

  unique_values <- unique(x$"business_model")
  expect_true(length(unique_values) == 2L)
  expect_true("Non-profit" %in% unique_values)
  expect_true("For-profit" %in% unique_values)

  # Academic friendly
  expect_false(any(is.na(x$"academic_friendly")))

  unique_values <- unique(x$"academic_friendly")
  expect_true(length(unique_values) == 2L)
  expect_true("Yes" %in% unique_values)
  expect_true("No" %in% unique_values)
})
