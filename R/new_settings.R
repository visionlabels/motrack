#' Creates a custom copy of settings
#'
#' @param .from optional parent from which defaults are inherited
#' @param ... named list of properties to override default settings.
#'
#' @return Updated list of settings
#' @export
#'
#' @seealso [`default_settings()`]
#'
#' @examples
#' s1 <- new_settings(xlim = c(0, 10), ylim = c(0, 10))
#' s2 <- new_settings(.from = s1, show_labels = TRUE)
new_settings <- function(.from = NULL, ...) {
  # what names are valid?
  default_settings_list <- default_settings()
  optional_parameters <- c("speed", "arena_inside_radius")
  valid_parameter_names <- union(
    names(default_settings_list),
    optional_parameters
  )
  # is parent provided?
  if (is.null(.from)) {
    parent <- default_settings()
  } else {
    parent <- .from
    stopifnot(all(names(parent) %in% valid_parameter_names))
  }
  dots <- list(...)
  for (key in names(dots)) {
    if (!key %in% valid_parameter_names) {
      stop("Unknown parameter: ", key)
    }
    parent[[key]] <- dots[[key]]
  }
  parent
}
