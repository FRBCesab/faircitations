# Do not run tests that contain this function

needs_api <- function() {
  skip_if_offline()
  skip_on_cran()
}
