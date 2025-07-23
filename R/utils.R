#' @noRd

fp_clean_doi <- function(doi) {
  doi <- gsub("\\s", "", doi)
  doi <- tolower(doi)
  doi <- gsub("http(s)?://(dx.)?doi.org/", "", doi)
  doi <- gsub("doi:", "", doi)

  doi
}
