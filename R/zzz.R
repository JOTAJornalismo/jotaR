.onAttach <- function(libname, pkgname) {

  if (.Platform$OS.type == "windows")  { # nocov start
    if (interactive()) packageStartupMessage("Registering Windows fonts with R")
    extrafont::loadfonts("win", quiet = TRUE)
  }

  if (getOption("rJOTA.loadfonts", default = FALSE)) {
    if (interactive()) packageStartupMessage("Registering PDF & PostScript fonts with R")
    extrafont::loadfonts("pdf", quiet = TRUE)
    extrafont::loadfonts("postscript", quiet = TRUE)
  }

  fnt <- extrafont::fonttable()
  if (!any(grepl("Arial|Arial[ ]Narrow|Roboto[ ]Regular", fnt$FamilyName))) {
    packageStartupMessage("NOTE: Arial, Arial Narrow or Roboto fonts are required to use these themes.")
    packageStartupMessage("      Please use rJOTA::import_roboto() to install Roboto Regular and")
    packageStartupMessage("      if Arial Narrow is not on your system, please install it.")
  } # nocov end

}
