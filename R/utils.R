#' cerate Coordinates from numeric parameters
#'
#'
get_session_cookie <- function(url) {

  res <- httr::GET(url)

  httr::cookies(res)$value %>%
    purrr::set_names(httr::cookies(res)$name)

}
