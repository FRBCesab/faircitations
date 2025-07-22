#' List DAFNEE journals
#'
#' @description
#' The DAFNEE database (Database of Academia‑Friendly Journals in Ecology and
#' Evolution, <https://dafnee.isem-evolution.fr/>) provides the business model
#' and the academic friendly status of several journals in the field of Ecology
#' and Evolution.
#'
#' The `faircitations` package provides a selection of 291 DAFNEE journals and
#' this function returns information about these journals
#'
#' @return A `data.frame` with three columns:
#'   - `journal`, the name of the journal
#'   - `business_model`, the business model of the journal (non-profit or
#'     for-profit)
#'   - `academic_friendly`, the academic friendly status of the journal (yes or
#'     no)
#'
#' @export
#'
#' @examples
#' # List DAFNEE journals in faircitations ----
#' journals <- fp_get_dafnee_journals()
#'
#' # Number of journals ----
#' nrow(journals)
#'
#' # Preview of the outputs ----
#' head(journals)

fp_get_dafnee_journals <- function() {
  data <- dafnee[, -1]
  colnames(data)[1] <- "journal"

  data$"business_model" <- gsub("^NP$", "Non-profit", data$"business_model")
  data$"business_model" <- gsub("^FP$", "For-profit", data$"business_model")

  data$"academic_friendly" <- gsub("^yes$", "Yes", data$"academic_friendly")
  data$"academic_friendly" <- gsub("^no$", "No", data$"academic_friendly")

  data
}
