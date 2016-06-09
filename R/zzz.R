.onAttach <- function(...) {
  sapply(strwrap(
    "Welcome to trackr. \n\nPlease note that the package is currently in development, and code may break at any time. Please file issues and feature suggestions on the GitHub page (github.com/Ax3man/trackr/issues).",
    indent = 0, exdent = 1), packageStartupMessage)
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", 'X', 'Y', 'frame', 'animal', 'trial',
                           'X1', 'Y1', 'X2', 'Y2', 'orientation1', 'orientation2',
                           'minor_axis1', 'minor_axis2', 'major_axis1', 'major_axis2',
                           'n', '.name'))
}
