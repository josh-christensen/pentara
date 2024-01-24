.onLoad <- function(libname, pkgname) {
  # Check if ggplot2 is present in the file structure
  if(nzchar(system.file(package = "ggplot2"))) {
    # If it is present change the default theme to theme_bw
    ggplot2::theme_set(ggplot2::theme_bw())
  }
}
