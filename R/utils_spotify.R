#' Converting Spaces in a String to '%20'
#'
#' @param string A String.
#'
#' @return A String.
#' @export
convert_spaces <- function(string) {
  if (!is.null(string) && nchar(string) > 0 && is.character(string)) {
    usethis::ui_info("Converting provided string...")
    converted_string <- string %>% stringr::str_replace_all(" ", "%20")
    return(converted_string)
  } else {
    return(NULL)
  }
}
