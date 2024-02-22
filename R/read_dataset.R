#' @title Read SAS datasets from Pentara's file structure
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' Automatically fill in file paths based on Pentara SOP file
#' structures to make reading SAS datasets into scripts easier.
#'
#' @param name The name of a SAS dataset in the ADaM directory.
#' @param type One of orig, vorig, sdtm, vsdtm, adam or vadam
#' corresponding to original, SDTM, and ADaM datasets in production
#' and validation.
#' @param protocol The protocol directory. Only required if the user
#' is not using RStudio.
#' @param client The client directory. Only required if the user is
#' not using RStudio.
#'
#' @return invisibly returns NULL
#'
#' @examples
#' \dontrun{
#'   read_dataset("adsl", "adam")
#'   read_dataset("ex", "sdtm", "HOPE-9", "cerebrocognaid")
#' }
#'
#' @export

read_dataset <- function(name, type, protocol, client) {
  # Check if the function has access to the P: drive
  if (!dir.exists("P:/")) {
    stop("The P: drive is not accessible by the R session.\nIf you are a user outside of Pentara, please read in the dataset using the haven package read_sas function.\nIf you are a Pentara employee please contact IT.")
  }

  # Check that type is supplied
  if (missing(type)) {
    stop("No type argument provided. Choose one of sdtm, adam, orig, vsdtm, vadam and vorig.")
  }

  # Argument matching for type
  lower_type <- tolower(type)
  acceptable_types <- c("orig", "sdtm", "adam", "vorig", "vsdtm", "vadam")
  type_index <- grepl(lower_type, acceptable_types)
  if (!any(type_index)) {
    stop("Invalid type argument provided. Choose one of sdtm, adam, orig, vsdtm, vadam and vorig.")
  }
  type_path <- switch(
    lower_type,
    orig = "Production/Original/Current",
    vorig = "Validation/Original/Current",
    sdtm = "Production/SDTM",
    vsdtm = "Validation/SDTM",
    adam = "Production/ADaM",
    vadam = "Validation/ADaM"
  )

  # Append the SAS dataset file extension to the dataset name
  file_name <- paste0(name, ".sas7bdat")

  # Branch depending on availability of RStudio functionality
  if (Sys.getenv("RSTUDIO") == "1") {

    # Get the current document path
    current_document_path <- rstudioapi::getActiveDocumentContext()$path

    # Get the protocol root directory
    protocol_root <- stringr::str_extract(current_document_path, "([^/]*/){4}")
    protocol_root <- stringr::str_remove(protocol_root, "/$")
  } else {
    # TODO Make client searchable from protocol
    # Check if client and protocol are both supplied
    if (missing(client)) {
      stop("RStudio is not available and no client argument was provided.")
    }
    if (missing(protocol)) {
      stop("RStudio is not available and no protocol argument was provided.")
    }

    # Get the protocol root directory
    protocol_root <- file.path("P:/Clients", client, protocol)
  }

  # Create the full path to the dataset
  full_path <- file.path(protocol_root, "Data", type_path, file_name)

  # Check if the requested dataset exists
  if (!file.exists(full_path)) {
    stop("The requested dataset does not exist.")
  }

  # Read the dataset
  dataset <- haven::read_sas(full_path)

  return(dataset)
}
