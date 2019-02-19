#' \code{motrack} package
#'
#' Multiple Object Tracking utilities
#'
#' @docType package
#' @name motrack
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
# as in https://github.com/STAT545-UBC/Discussion/issues/451 but not working
if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'