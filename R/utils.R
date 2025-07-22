#' @noRd

fp_clean_doi <- function(doi) {
  doi <- gsub("https://doi.org/", "", doi)
  doi <- gsub("doi:", "", doi)
  doi <- gsub("\\s", "", doi)
  doi <- tolower(doi)

  doi
}
