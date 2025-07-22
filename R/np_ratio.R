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

  if (is.null(doi)) {
    stop("Argument 'doi' is required")
  }

  if (!is.character(doi)) {
    stop("Argument 'doi' must be character")
  }

  if (length(doi) == 0) {
    stop("Argument 'doi' must be of length > 0")
  }

  ## Check if user is polite ----

  # if (is.null(options()$"openalexR.mailto")) {
  #   stop(
  #     "Be polite with OpenAlex API and run: ",
  #     "`options(openalexR.mailto = 'your_email')`"
  #   )
  # }

  ## Fill summary output ----

  report <- data.frame()
  report <- rbind(
    report,
    data.frame(
      "metric" = "Total references",
      "value" = length(doi)
    )
  )

  ## Remove missing DOI ----

  doi <- doi[!is.na(doi)]

  report <- rbind(
    report,
    data.frame(
      "metric" = "References with DOI",
      "value" = length(doi)
    )
  )

  if (length(doi) == 0) {
    stop("No valid DOI provided (missing value)")
  }

  ## Clean DOI ----

  doi <- gsub("https://doi.org/", "", doi)
  doi <- tolower(doi)

  ## Remove duplicated references ----

  doi <- doi[!duplicated(doi)]

  report <- rbind(
    report,
    data.frame(
      "metric" = "Deduplicated references",
      "value" = length(doi)
    )
  )

  ## Get OpenAlex metadata ----

  works <- openalexR::oa_fetch(entity = "work", doi = doi)
  works <- as.data.frame(works)

  works <- works[, c("doi", "source_id")]
  colnames(works)[2] <- "oa_source_id"

  ## Prepare data ----

  works$"doi" <- tolower(works$"doi")
  works$"doi" <- gsub("https://doi.org/", "", works$"doi")

  doi <- data.frame("doi" = doi)

  works <- merge(doi, works, by = "doi", all = TRUE)

  ## Remove references absent from OA ----

  works <- works[!is.na(works$"oa_source_id"), ]

  report <- rbind(
    report,
    data.frame(
      "metric" = "References found in OpenAlex",
      "value" = nrow(works)
    )
  )

  if (nrow(works) == 0) {
    stop("No reference found in OpenAlex")
  }

  ## Add Dafnee metadata (internal dataset) ----

  data_for_ratio <- merge(works, dafnee, by = "oa_source_id", all = FALSE)

  report <- rbind(
    report,
    data.frame(
      "metric" = "References found in DAFNEE",
      "value" = nrow(data_for_ratio)
    )
  )

  if (nrow(data_for_ratio) == 0) {
    stop("No reference journal found in DAFNEE database")
  }

  ## Compute ratios ----

  n_refs_in_dafnee <- nrow(data_for_ratio)

  n_refs_np <- length(
    which(
      data_for_ratio$"business_model" == "NP"
    )
  )

  report <- rbind(
    report,
    data.frame(
      "metric" = "Non-profit and academic friendly references",
      "value" = n_refs_np
    )
  )

  n_refs_fp_acad <- length(
    which(
      data_for_ratio$"business_model" == "FP" &
        data_for_ratio$"academic_friendly" == "yes"
    )
  )

  report <- rbind(
    report,
    data.frame(
      "metric" = "For-profit and academic friendly references",
      "value" = n_refs_fp_acad
    )
  )

  n_refs_fp_nonacad <- length(
    which(
      data_for_ratio$"business_model" == "FP" &
        data_for_ratio$"academic_friendly" == "no"
    )
  )

  report <- rbind(
    report,
    data.frame(
      "metric" = "For-profit and non-academic friendly references",
      "value" = n_refs_fp_nonacad
    )
  )

  ## Outputs ----

  ratios <- c(
    "Non-profit and academic friendly" = round(
      n_refs_np / n_refs_in_dafnee,
      2
    ),
    "For-profit and academic friendly" = round(
      n_refs_fp_acad / n_refs_in_dafnee,
      2
    ),
    "For-profit and non-academic friendly" = round(
      n_refs_fp_nonacad / n_refs_in_dafnee,
      2
    )
  )

  list("summary" = report, "ratios" = ratios)
}
