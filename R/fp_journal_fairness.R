#' Get the fairness status of a journal
#'
#' @description
#' By querying the OpenAlex bibliographic database (<https://openalex.org>) and
#' the DAFNEE database (<https://dafnee.isem-evolution.fr/>), this function
#' returns the business model and the academic friendly status of a journal.
#'
#' @param journal a `character` of length 1. The name of the journal. Do not use
#'   journal abbreviation.
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
#' fp_journal_fairness("Science")
#' fp_journal_fairness("Science of Nature")
#' fp_journal_fairness("The Science of Nature")

fp_journal_fairness <- function(journal) {
  ## Check args ----

  if (missing(journal)) {
    stop("Argument 'journal' is required")
  }

  if (is.null(journal)) {
    stop("Argument 'journal' is required")
  }

  if (!is.character(journal)) {
    stop("Argument 'journal' must be character")
  }

  if (length(journal) != 1L) {
    stop("Argument 'journal' must be of length 1")
  }

  if (is.na(journal)) {
    stop("Argument 'journal' cannot be NA")
  }

  ## Clean journal name ----

  journal <- tolower(journal)

  ## Query Dafnee (internal dataset) ----

  dafnee$"journal" <- dafnee$"oa_source_name"
  dafnee$"journal" <- tolower(dafnee$"journal")

  ## Exact match ----

  results <- dafnee[dafnee$"journal" == journal, ]

  if (nrow(results) > 0) {
    results <- results[1, ]

    if (results$"business_model" == "NP") {
      return(
        data.frame(
          "journal" = results[1, "oa_source_name"],
          "fairness" = "Non-profit and academic friendly"
        )
      )
    }

    if (results$"business_model" == "FP") {
      if (results$"academic_friendly" == "yes") {
        return(
          data.frame(
            "journal" = results[1, "oa_source_name"],
            "fairness" = "For-profit and academic friendly"
          )
        )
      }

      if (results$"academic_friendly" == "no") {
        return(
          data.frame(
            "journal" = results[1, "oa_source_name"],
            "fairness" = "For-profit and non-academic friendly"
          )
        )
      }
    }
  }

  ## Fuzzy search ----

  fuzzy <- stringdist::stringdist(journal, dafnee$"journal")
  suggestions <- dafnee[order(fuzzy, decreasing = FALSE), "oa_source_name"]
  suggestions <- suggestions[1:3]

  ## Print message ----

  msg <- paste0("  '", paste0(suggestions, collapse = "'\n  '"), "'")

  message(
    paste0(
      "No exact match found!\nThe fuzzy search returns these three best ",
      "candidates:\n",
      msg
    )
  )

  invisible(
    data.frame(
      "journal" = NA,
      "fairness" = NA
    )
  )
}
