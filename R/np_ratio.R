#' Compute the non-profit ratio on a list of references
#'
#' @description
#' ...
#'
#' @param doi a `character` vector of Digital Object Identifiers (DOI)
#'
#' @return A `data.frame` with two columns: `metric` and `value` reporting
#' different statistics.
#'
#' @examples
#' ## ADD AN EXAMPLE

np_ratio <- function(doi) {
  ## Check args ----

  if (missing(doi)) {
    stop("Argument 'doi' is required")
  }

  if (!is.character(doi)) {
    stop("Argument 'doi' must be character")
  }

  ## Check if user is polite ----

  if (is.null(options()$"openalexR.mailto")) {
    stop(
      "Be polite with OpenAlex API and run: ",
      "`options(openalexR.mailto = 'your_email')`"
    )
  }

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

  dafnee <- utils::read.csv(file.path(
    "data",
    "derived-data",
    "final_list_of_journals.csv"
  ))

  # dafnee <- dafnee[, c("oa_source_id", "business_model", "is_dafnee")]

  ## Clean Dafnee data ----

  dafnee$"business_model" <- gsub(
    "For-profit",
    "FP",
    dafnee$"business_model"
  )

  dafnee$"business_model" <- gsub(
    "Non-profit",
    "NP",
    dafnee$"business_model"
  )

  dafnee$"business_model" <- gsub(
    "University Press",
    "NP",
    dafnee$"business_model"
  )

  dafnee[which(dafnee$business_model == "NP"), "is_dafnee"] <- TRUE

  ## Merge datasets ----

  all_data <- merge(
    works,
    dafnee,
    by = "oa_source_id",
    all.x = TRUE,
    all.y = FALSE
  )

  data_for_ratio <- merge(works, dafnee, by = "oa_source_id", all = FALSE)

  ## Compute ratios ----

  n_refs_in_dafnee <- nrow(data_for_ratio)

  n_refs_np <- length(which(data_for_ratio$"business_model" == "NP"))
  n_refs_fp_acad <- length(which(
    data_for_ratio$"business_model" == "FP" & data_for_ratio$"is_dafnee" == TRUE
  ))

  n_refs_fp_nonacad <- length(which(
    data_for_ratio$"business_model" == "FP" &
      data_for_ratio$"is_dafnee" == FALSE
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
