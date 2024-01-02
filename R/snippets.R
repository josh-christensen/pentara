#' @title Import package snippets
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' `snippets` copies all (missing) snippet definitions
#' in `inst/rstudio/Rsnippets.txt` to the RStudio user snippet file.
#'
#' @return boolean invisible(FALSE) if nothing was added, invisible(TRUE) if snippet definitions were added
#' @export
#'
#' @examples \dontrun{snippets()}
snippets <- function() {

  added <- FALSE

  # if not on RStudio or RStudioServer exit
  #
  if (!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) {
    return(NULL)
  }

  # Name of files containing snippet code to copy
  #
  pckgSnippetsFiles <- c("Rsnippets.txt")#, "Rmdsnippets.txt")

  # Name of files to copy into. Order has to be the same
  # as in 'pckgSnippetsFiles'
  #
  rstudioSnippetsFiles <- c("r.snippets")#, "markdown.snippets")

  # Path to directory for RStudios user files depends on OS
  #
  if (rstudioapi::versionInfo()$version < "1.3") {
    rstudioSnippetsPathBase <- file.path(path.expand('~'),".R", "snippets")
  } else {
    if (.Platform$OS.type == "windows") {
      rstudioSnippetsPathBase <- file.path(Sys.getenv("APPDATA"), "RStudio", "snippets")
    } else {
      rstudioSnippetsPathBase <- file.path(path.expand('~'), ".config/rstudio", "snippets")
    }
  }

  # Read each file in pckgSnippetsFiles and add its contents
  #
  for (i in seq_along(pckgSnippetsFiles)) {

    # Try to get template, if template is not found skip it
    #
    pckgSnippetsFilesPath <- system.file("rstudio", pckgSnippetsFiles[i], package = "pentara")
    if (pckgSnippetsFilesPath == "") {
      next
    }

    # load package snippets definitions
    #
    pckgSnippetsFileContent <- readLines(pckgSnippetsFilesPath)

    # Extract names of package snippets
    #
    pckgSnippetsFileDefinitions <- pckgSnippetsFileContent[grepl("^snippet (.*)", pckgSnippetsFileContent)]


    # Construct path for destination file
    #
    rstudioSnippetsFilePath <- file.path(rstudioSnippetsPathBase, rstudioSnippetsFiles[i])

    # If targeted RStudios user file does not exist create it (Rstudio defaults
    # are included in package snippets so they won't be lost)
    if (!file.exists(rstudioSnippetsFilePath)) {
      file.create(rstudioSnippetsFilePath)
    }

    # Extract 'names' of already existing snippets
    #
    rstudioSnippetsFileContent <- readLines(rstudioSnippetsFilePath)
    rstudioSnippetDefinitions <- rstudioSnippetsFileContent[grepl("^snippet (.*)", rstudioSnippetsFileContent)]

    # replace two spaces with tab, ONLY at beginning of string
    #
    pckgSnippetsFileContentSanitized <- gsub("(?:^ {2})|\\G {2}|\\G\t", "\t", pckgSnippetsFileContent, perl = TRUE)

    # find defintions appearing in packageSnippets but not in rstudioSnippets
    # if no snippets are missing go to next file
    #
    snippetsToCopy <- setdiff(trimws(pckgSnippetsFileDefinitions), trimws(rstudioSnippetDefinitions))
    snippetsNotToCopy <- intersect(trimws(pckgSnippetsFileDefinitions), trimws(rstudioSnippetDefinitions))
    if (length(snippetsToCopy) == 0) {
      cat(paste0("(\nFollowing snippets will NOT be added because there is already a snippet with that name: ",
                 paste0(snippetsNotToCopy, collapse=", ") ,")"))
      next
    }

    # Create list of line numbers where snippet definitons start
    # This list is used to determine the end of each definition block
    #
    allPckgSnippetDefinitonStarts <- grep("^snippet .*", pckgSnippetsFileContentSanitized)

    for (s in snippetsToCopy) {
      startLine <- grep(paste0("^", s, ".*"), pckgSnippetsFileContentSanitized)

      # Find last line of snippet definition:
      # First find start of next defintion and return
      # previous line number or lastline if already in last definiton
      #
      endLine <- allPckgSnippetDefinitonStarts[allPckgSnippetDefinitonStarts > startLine][1] -1
      if (is.na(endLine)) {
        endLine <- length(pckgSnippetsFileContentSanitized)
      }

      snippetText <- paste0(pckgSnippetsFileContentSanitized[startLine:endLine], collapse = "\n")

      # Make sure there is at least one empty line between entries
      #
      if(!length(readLines(rstudioSnippetsFilePath)) == 0) {
        if (utils::tail(readLines(rstudioSnippetsFilePath), n=1) != "") {
          snippetText <- paste0("\n", snippetText)
        }
      }

      # Append snippet block, print message
      #
      cat(paste0(snippetText, "\n"), file = rstudioSnippetsFilePath, append = TRUE)
      # TODO Change this to make sure it individually checks all of the
      # snippets, not just the last
      added <- TRUE
    }

    # Inform user about changes
    #
    if (interactive()) {
      cat(paste0("The following ", length(snippetsToCopy),
                 " snippets were added to '", rstudioSnippetsFilePath, "':\n",
                 paste0(paste0("-", gsub("snippet", "", snippetsToCopy)), collapse="\n")))
      if (length(snippetsNotToCopy) > 0) {
        cat(paste0("\n(The following snippets were NOT added because there was already a snippet with that name:\n",
                   paste0(snippetsNotToCopy, collapse=", ") ,")"))
      }
    }
  }

  # Ensure that snippets are immediately available for use
  suppressMessages(usethis::edit_rstudio_snippets(type = "r"))
  rstudioapi::documentSave()
  rstudioapi::documentClose()

  return(invisible(added))
}
