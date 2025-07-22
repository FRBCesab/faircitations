#' Non profit & academic friendly ratio of citations
#'
#' @description
#' Scientific journals operate over a broad spectrum of publishing strategies,
#' from strictly for-profit, to non-profit, and in-between business models
#' (e.g. for-profit but academic friendly journals).
#'
#' From a list of references, this function computes three citation ratios:
#' the proportion of non-profit citations, the proportion of for-profit and
#' academic friendly citations, and the proportion of for-profit and
#' non-academic friendly citations (Beck et al. 2025).
#'
#' It uses the OpenAlex bibliographic database (<https://openalex.org>) to
#' retrieve journal names from article DOI and the DAFNEE database
#' (<https://dafnee.isem-evolution.fr/>) to get the business model and the
#' academic friendly status of journals.
#'
#' @param doi a `character` vector of Digital Object Identifiers (DOI). Can
#'   contain `NA` (book, book chapter, etc.).
#'
#' @return A `list` of two elements:
#'
#'   - `summary`, a `data.frame` with two columns (`metric` and `value`)
#'   reporting the following statistics:
#'     - number of total references (length of `doi` argument)
#'     - number of references with DOI
#'     - number of deduplicated references
#'     - number of references found in the OpenAlex database
#'     - number of references whose journal is indexed in the DAFNEE database
#'     - number of non-profit and academic friendly references
#'     - number of for-profit and academic friendly references
#'     - number of for-profit and non academic friendly references
#'
#'   - `ratios`, a vector of three ratios:
#'     - non-profit and academic friendly ratio
#'     - for-profit and academic friendly ratio
#'     - for-profit and non academic friendly ratio
#'
#' @export
#'
#' @references Beck M et al. (2025) Strategic citations for a fairer academic
#'   landscape. Submitted to Proc B - Biological Science Practices.
#'
#' @examples
#' # Import a BiBTex file (provided example) ----
#' filename <- system.file(
#'   file.path("extdata", "references.bib"),
#'   package = "faircitations"
#' )
#'
#' refs <- RefManageR::ReadBib(filename)
#'
#' length(refs)
#' refs[1:2]
#'
#' # Extract DOI ----
#' doi_list <- unlist(refs$"doi")
#' names(doi_list) <- NULL
#' doi_list
#'
#' # Compute citation ratios ----
#' np_ratio(doi_list)

np_ratio <- function(doi) {
  ## Check args ----

  if (missing(doi)) {
    stop("Argument 'doi' is required")
  }

  if (!is.character(doi)) {
    stop("Argument 'doi' must be character")
  }

  ## Check if user is polite ----

  # if (is.null(options()$"openalexR.mailto")) {
  #   stop(
  #     "Be polite with OpenAlex API and run: ",
  #     "`options(openalexR.mailto = 'your_email')`"
  #   )
  # }

  ## Clean references ----

  n_original_refs <- length(doi)

  doi <- doi[!is.na(doi)]
  n_refs_without_na <- length(doi)

  doi <- gsub("https://doi.org/", "", doi)
  doi <- tolower(doi)

  doi <- doi[!duplicated(doi)]
  n_refs_without_dups <- length(doi)

  ## Get OpenAlex metadata ----

  works <- openalexR::oa_fetch(entity = "work", doi = doi) |>
    as.data.frame()
  works <- works[, c("doi", "source_id")]
  colnames(works)[2] <- "oa_source_id"

  ## Prepare data ----

  works$"doi" <- tolower(works$"doi")
  works$"doi" <- gsub("https://doi.org/", "", works$"doi")

  doi <- data.frame("doi" = doi)

  works <- merge(doi, works, by = "doi", all = TRUE)

  n_refs_in_openalex <- sum(!is.na(works$"oa_source_id"))

  ## Add Dafnee metadata ----

  data_for_ratio <- merge(works, dafnee, by = "oa_source_id", all = FALSE)

  ## Compute ratios ----

  n_refs_in_dafnee <- nrow(data_for_ratio)

  n_refs_np <- length(which(data_for_ratio$"business_model" == "NP"))
  n_refs_fp_acad <- length(which(
    data_for_ratio$"business_model" == "FP" &
      data_for_ratio$"academic_friendly" == "yes"
  ))

  n_refs_fp_nonacad <- length(which(
    data_for_ratio$"business_model" == "FP" &
      data_for_ratio$"academic_friendly" == "no"
  ))

  ## Outputs ----

  data.frame(
    "metric" = c(
      "N. of total refs",
      "N. of refs w/ DOI",
      "N. of refs w/o duplicate",
      "N. of refs found in OpenAlex",
      "N. of refs found in Dafnee",
      "N. of Non-Profit refs",
      "N. of For-Profit refs (academic friendly)",
      "N. of For-Profit refs (non-academic friendly)",
      "Non-Profit ratio"
    ),
    "value" = c(
      n_original_refs,
      n_refs_without_na,
      n_refs_without_dups,
      n_refs_in_openalex,
      n_refs_in_dafnee,
      n_refs_np,
      n_refs_fp_acad,
      n_refs_fp_nonacad,
      n_refs_np / nrow(data_for_ratio)
    )
  )
}
