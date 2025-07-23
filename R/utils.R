#' @noRd

fp_clean_doi <- function(doi) {
  doi <- gsub("\\s", "", doi)
  doi <- tolower(doi)
  doi <- gsub("http(s)?://(dx.)?doi.org/", "", doi)
  doi <- gsub("doi:", "", doi)

  doi
}

#' @noRd

fp_check_mailto <- function() {
  if (is.null(options()$"openalexR.mailto")) {
    stop(
      "Be polite with OpenAlex API and run: ",
      "`options(openalexR.mailto = 'your_email')`"
    )
  }

  invisible(NULL)
}
