#' @title Read ADaM datasets from Pentara's file structure
#'
#' @description Automatically fill in file paths based on Pentara SOP file
#' structures to make reading ADaM datasets into scripts easier.
#'
#' @param name The name of the SAS dataset.
#'
#' @return invisibly returns NULL
#'
#' @examples
#' \dontrun{adam()}
#' @noRd

adam <- function() {
  # Check if the user is on Rstudio
  if (Sys.getenv("RSTUDIO") == "1") {
    rstudioapi::getActiveDocumentContext()$path
    haven::read_sas()
  } else {
    haven::read_sas()
  }

  return(dataset)
}
