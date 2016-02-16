.onAttach <- function(...) {
  packageStartupMessage(strwrap("Welcome to trackr. Please note that the package
    is currently in development, and code may break in the future. Please file
    issues and suggestions on GitHub."))
}

if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))
