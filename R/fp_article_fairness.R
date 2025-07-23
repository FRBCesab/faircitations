#' Get the fairness status of an article
#'
#' @description
#' By querying the OpenAlex bibliographic database (<https://openalex.org>) and
#' the DAFNEE database (<https://dafnee.isem-evolution.fr/>), this function
#' returns the business model and the academic friendly status of an article
#' (more precisely the fairness status of the journal).
#'
#' @param doi a `character` of length 1. The Digital Object Identifiers (DOI)
#'   of the article.
#'
#' @return A `data.frame` with two columns: `journal`, the journal name, and
#' `fairness`, the fairness status with the following possible values:
#'   - Non-profit and academic friendly
#'   - For-profit and academic friendly
#'   - For-profit and non academic friendly
#'   - Record not found in OpenAlex
#'   - Record not found in DAFNEE database
#'
#' @export
#'
#' @examples
#' # Be polite and send your email to OpenAlex API ----
#' options(openalexR.mailto = 'anonymous@mail.com')
#'
#' # Fairness status ----
#' fp_article_fairness(doi = "10.1126/science.162.3859.1243")
#' fp_article_fairness(doi = "10.1111/j.1461-0248.2005.00792.x")
#' fp_article_fairness(doi = "10.1038/35002501")
#' fp_article_fairness(doi = "10.xxxx/xxxx")
#' fp_article_fairness(doi = "10.21105/joss.05753")

fp_article_fairness <- function(doi) {
  ## Check args ----

  if (missing(doi)) {
    stop("Argument 'doi' is required")
  }

  if (is.null(doi)) {
    stop("Argument 'doi' is required")
  }

  if (!is.character(doi)) {
    stop("Argument 'doi' must be character")
  }

  if (length(doi) != 1L) {
    stop("Argument 'doi' must be of length 1")
  }

  if (is.na(doi)) {
    stop("Argument 'doi' cannot be NA")
  }

  ## Check if user is polite ----

  fp_check_mailto()

  ## Clean DOI ----

  doi <- fp_clean_doi(doi)

  ## Query OpenAlex ----

  works <- suppressWarnings(openalexR::oa_fetch(entity = "work", doi = doi))
  works <- as.data.frame(works)

  if (nrow(works) == 0) {
    return(
      data.frame(
        "journal" = NA,
        "fairness" = "Record not found in OpenAlex"
      )
    )
  }

  works <- works[, c("doi", "source_id", "source_display_name")]
  colnames(works)[2] <- "oa_source_id"

  ## Query Dafnee (internal dataset) ----

  works_dafnee <- merge(works, dafnee, by = "oa_source_id", all = FALSE)

  if (nrow(works_dafnee) == 0) {
    return(
      data.frame(
        "journal" = works[1, "source_display_name"],
        "fairness" = "Record not found in DAFNEE database"
      )
    )
  }

  if (works_dafnee$"business_model" == "NP") {
    return(
      data.frame(
        "journal" = works_dafnee[1, "source_display_name"],
        "fairness" = "Non-profit and academic friendly"
      )
    )
  }

  if (works_dafnee$"business_model" == "FP") {
    if (works_dafnee$"academic_friendly" == "yes") {
      return(
        data.frame(
          "journal" = works_dafnee[1, "source_display_name"],
          "fairness" = "For-profit and academic friendly"
        )
      )
    }

    if (works_dafnee$"academic_friendly" == "no") {
      return(
        data.frame(
          "journal" = works_dafnee[1, "source_display_name"],
          "fairness" = "For-profit and non-academic friendly"
        )
      )
    }
  }
}
