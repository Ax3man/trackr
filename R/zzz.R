.onAttach <- function(...) {
  packageStartupMessage(strwrap(
    "Welcome to trackr. Please note that the package is currently in development, and code may break in the future. Please file issues and feature suggestions on the GitHub page (github.com/Ax3man/trackr/issues).",
    indent = 0, exdent = 0))
}

if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# Set some globals to deal with R CMD check and multidplyr.
globalVariables(c('X', 'Y', 'frame', 'animal', 'trial', 'n', '.name'))
