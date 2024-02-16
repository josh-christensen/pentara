#' @title Read ADaM datasets from Pentara's file structure
#'
#' @description Automatically fill in file paths based on Pentara SOP file
#' structures to make reading ADaM datasets into scripts easier.
#'
#' @param name The name of a SAS dataset in the ADaM directory.
#'
#' @return invisibly returns NULL
#'
#' @examples
#' \dontrun{adam("adsl")}
#'
#' @export

read_dataset <- function(name, type, client, protocol) {
  file_name <- paste0(name, ".sas7bdat")

  # Branch between
  if (Sys.getenv("RSTUDIO") == "1") {
    current_document_path <- rstudioapi::getActiveDocumentContext()$path
    protocol_root <- stringr::str_extract(current_document_path, "([^/]*/){4}")
    protocol_root <- stringr::str_remove(protocol_root, "/$")
    full_path <- file.path(protocol_root, "Data", "Production", "ADaM", file_name)
    dataset <- haven::read_sas(full_path)
  } else {
    stop("Method for users not on Rstudio not yet implemented")
  }

  return(dataset)
}
