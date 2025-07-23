test_that("Test fp_check_mailto() for error", {
  expect_error(
    fp_check_mailto(),
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
    fp_check_mailto(),
    paste0(
      "Be polite with OpenAlex API and run: ",
      "`options(openalexR.mailto = 'your_email')`"
    ),
    fixed = TRUE
  )
})

test_that("Test fp_check_mailto() for success", {
  withr::local_options(
    list("openalexR.mailto" = "anonymous@mail.com")
  )

  expect_invisible(fp_check_mailto())
})
