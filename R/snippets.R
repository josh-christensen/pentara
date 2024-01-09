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
    stop("Snippets are a feature of the Rstudio IDE")
  }

  # Name of files containing snippet code to copy
  #
  pckgSnippetsFiles <- c("default_r_snippets.txt", "r_snippets.txt")#, "Rmdsnippets.txt")

  # Name of files to copy into. Order has to be the same
  # as in 'pckgSnippetsFiles'
  #
  rstudioSnippetsFiles <- c("r.snippets", "r.snippets")#, "markdown.snippets")

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
    current_snippet_file <- pckgSnippetsFiles[i]
    is_default_file <- grepl("^default", current_snippet_file)
    pckgSnippetsFilesPath <- system.file("rstudio", current_snippet_file, package = "pentara")
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
    current_rstudio_snippet_file <- rstudioSnippetsFiles[i]
    rstudioSnippetsFilePath <- file.path(rstudioSnippetsPathBase, current_rstudio_snippet_file)

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
    if(is_default_file) {
      # For default files
      # Copy anything that should be present and isn't
      snippetsToCopy <- setdiff(trimws(pckgSnippetsFileDefinitions), trimws(rstudioSnippetDefinitions))
      # Otherwise don't copy it at all
      snippetsNotToCopy <- intersect(trimws(pckgSnippetsFileDefinitions), trimws(rstudioSnippetDefinitions))
      # If there is nothing to copy from this file we are done
      if (length(snippetsToCopy) == 0) {
        next
      }
    } else {
      # For non default files find pckg snippets that aren't present
      pckg_snippets_not_in_rstudio <- setdiff(trimws(pckgSnippetsFileDefinitions), trimws(rstudioSnippetDefinitions))
      # And snippets that are already present
      shared_snippets <- intersect(trimws(pckgSnippetsFileDefinitions), trimws(rstudioSnippetDefinitions))

      # For those that are already present check if they match
      matching_shared_snippets <- character()
      nonmatching_shared_snippets <- character()
      rstudio_refresh_indices <- numeric()

      pckg_snippet_cutoffs <- c(grep("^snippet", pckgSnippetsFileContent), length(pckgSnippetsFileContent) + 1)
      rstudio_snippet_cutoffs <- c(grep("^snippet", rstudioSnippetsFileContent), length(rstudioSnippetsFileContent) + 1)

      for (current_snippet in shared_snippets) {
        pckg_current_snippet_start <- grep(current_snippet, pckgSnippetsFileContent)
        pckg_current_snippet_end <- pckg_snippet_cutoffs[which(pckg_snippet_cutoffs == pckg_current_snippet_start) + 1] - 1

        rstudio_current_snippet_start <- grep(current_snippet, rstudioSnippetsFileContent)
        rstudio_current_snippet_end <- rstudio_snippet_cutoffs[which(rstudio_snippet_cutoffs == rstudio_current_snippet_start) + 1] - 1

        pckg_current_snippet_text <- pckgSnippetsFileContent[pckg_current_snippet_start:pckg_current_snippet_end]
        rstudio_current_snippet_text <- rstudioSnippetsFileContent[rstudio_current_snippet_start:rstudio_current_snippet_end]

        snippets_are_equal <- identical(pckg_current_snippet_text, rstudio_current_snippet_text)
        if (snippets_are_equal) {
          matching_shared_snippets <- append(matching_shared_snippets, current_snippet)
        } else {
          nonmatching_shared_snippets <- append(nonmatching_shared_snippets, current_snippet)
          rstudio_refresh_indices <- append(rstudio_refresh_indices, rstudio_current_snippet_start:rstudio_current_snippet_end)
        }
      }

      # Rename to match preexisting code
      snippetsToCopy <- pckg_snippets_not_in_rstudio
      snippetsNotToCopy <- matching_shared_snippets
      snippetsToRefresh <- nonmatching_shared_snippets

      # Remove the snippets that need to be refreshed from the original file and
      # mark them for addition
      if (length(snippetsToRefresh) != 0 & interactive()) {
        cat(
          "The following snippets will be overwritten:\n",
          paste0("- ", snippetsToRefresh, collapse = "\n")
        )
        user_input <- readline("Proceed? (y/n): ")
        while(!(user_input %in% c("y", "n"))) {
          user_input <- readline("Invalid input. Please enter 'y' or 'n': ")
        }
        if(user_input == "y") {
          snippetsToCopy <- c(snippetsToCopy, snippetsToRefresh)
          rstudio_file_without_refresh <- rstudioSnippetsFileContent[-rstudio_refresh_indices]
          cat(paste0(rstudio_file_without_refresh, collapse = "\n"), file = rstudioSnippetsFilePath)
        }
      }

      if (length(c(pckg_snippets_not_in_rstudio, nonmatching_shared_snippets)) == 0) {
        cat(
          paste0(
            "All snippets in package file ",
            current_snippet_file,
            " are present in Rstudio file ",
            current_rstudio_snippet_file,
            " and have identical definitions."
          )
        )
        next
      }
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
                 paste0(paste0("-", gsub("snippet", "", snippetsToCopy)), collapse="\n")), "\n")
      # if (length(snippetsNotToCopy) > 0) {
      #   cat(paste0("\nThe following ", length(snippetsNotToCopy), " snippets were NOT added because there was already a snippet with that name:\n",
      #              paste0("- ", snippetsNotToCopy, collapse="\n")))
      # }
    }
  }

  # Ensure that snippets are immediately available for use
  suppressMessages(usethis::edit_rstudio_snippets(type = "r"))
  rstudioapi::documentSave()
  rstudioapi::documentClose()

  return(invisible(added))
}
