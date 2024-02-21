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

read_dataset <- function(name, type, client, protocol, production = TRUE) {
  # TODO Check if the function is being run from the P: drive
  # TODO Check that type is populated
  # TODO Implement argument matching for type

  file_name <- paste0(name, ".sas7bdat")

  # Branch between RStudio response and regular version
  if (Sys.getenv("RSTUDIO") == "1") {
    # Get the current document path
    current_document_path <- rstudioapi::getActiveDocumentContext()$path
    # Get the protocol root directory
    protocol_root <- stringr::str_extract(current_document_path, "([^/]*/){4}")
    protocol_root <- stringr::str_remove(protocol_root, "/$")
    # Create the full path to the dataset
    full_path <- file.path(protocol_root, "Data", "Production", "ADaM", file_name)
  } else {
    stop("Method for users not on Rstudio not yet implemented")
    # TODO Check if client, protocol and type are all populated
    # TODO make client searchable from protocol
  }

  # TODO Check if the file path requested exists

  # Read The dataset
  dataset <- haven::read_sas(full_path)

  return(dataset)
}
