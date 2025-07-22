#' Extract DOI from a BibTeX file
#'
#' @description
#' This function reads a BibTex file and extracts the DOI of references (if
#' originally present in the file).
#'
#' @param bibtex a `character` of length 1. The (absolute or relative) path to
#'   the BibTeX file to open.
#'
#' @return
#' A `character` vector with extracted DOI. Some values can be `NA` in case of
#' books, chapters, etc. or if references are malformed.
#'
#' @export
#'
#' @examples
#' # Path to the BibTeX provided by <fairpub> ----
#' filename <- system.file(
#'   file.path("extdata", "references.bib"),
#'   package = "fairpub"
#' )
#'
#' # Extract DOI from BibTeX ----
#' fp_doi_from_bibtex(filename)

fp_doi_from_bibtex <- function(bibtex) {
  ## Check args ----

  if (missing(bibtex)) {
    stop("Argument 'bibtex' is required")
  }

  if (is.null(bibtex)) {
    stop("Argument 'bibtex' is required")
  }

  if (!is.character(bibtex)) {
    stop("Argument 'bibtex' must be a character (BibTeX file name)")
  }

  if (length(bibtex) != 1) {
    stop("Argument 'bibtex' must be of length 1 (one BibTeX file)")
  }

  if (!file.exists(bibtex)) {
    stop("The file '", bibtex, "' does not exist")
  }

  ## Open BibTeX file ----

  refs <- RefManageR::ReadBib(bibtex)

  ## Extract DOI ----

  dois <- refs$"doi"

  dois <- lapply(dois, function(x) {
    if (is.null(x)) {
      return(NA)
    } else {
      return(x)
    }
  })

  dois <- unlist(dois)
  names(dois) <- NULL

  ## Clean DOI ----

  fp_clean_doi(dois)
}
