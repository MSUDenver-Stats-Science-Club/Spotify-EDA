#' Title
#'
#' @param track_name
#' @param artist_name
#' @param album_name
#'
#' @return A Data Frame.
#' @export
spotify_search <- function(
  track_name = NULL,
  artist_name = NULL,
  album_name = NULL
) {

  ## Validate provided values
  track_name_c <- convert_spaces(track_name)
  artist_name_c <- convert_spaces(artist_name)
  album_name_c <- convert_spaces(album_name)

  if (!is.null(track_name_c)) {
    track_param <- glue::glue("track:{track_name_c}")
  } else {
    track_param <- NULL
  }

  if (!is.null(artist_name_c)) {
    artist_param <- glue::glue("artist:{artist_name_c}")
  } else {
    artist_param <- NULL
  }

  if (!is.null(album_name_c)) {
    album_param <- glue::glue("album:{album_name_c}")
  } else {
    album_param <- NULL
  }

  ## Defining our base url
  api_url <- "https://api.spotify.com/v1/search"

  ## Create search query
  s_params <- paste(track_param, artist_param, album_param, sep = "+") %>%
    stringr::str_remove("\\+$")

  types <- stringr::str_extract_all(s_params, "track|artist|album") %>%
    unlist() %>%
    paste(collapse = ",")

  search_query <- paste0(
    api_url,
    "?",
    glue::glue("q={s_params}&type={types}&limit=20")
  )

  ## Make our request
  request <- httr::GET(
    url = search_query,
    httr::content_type_json()
  )

  if (request$status_code == 200) {

    request_list <- request %>%
      httr::content()

    request_list %>%
      jsonlite::write_json("C:/Users/jason/Downloads/Datasets/test.json", pretty = TRUE)

    # track_results <- request_list %>%
    #   purrr::map_dfr(
    #     .f = ~{
    #
    #         # id <- .x %>% purrr::pluck("id")
    #         # link <- .x %>% purrr::pluck("href")
    #         # name <- .x %>% purrr::pluck("name")
    #         # popularity <- .x %>% purrr::pluck("popularity")
    #         #
    #         # track_df <- data.frame(
    #         #   name,
    #         #   link,
    #         #   id
    #         # )
    #
    #     }
    #   )



  } else if (request$status_code == 401) {

    usethis::ui_info("Token expired. Re-authenticating...")
    authenticate()

    request <- httr::GET(
      url = search_query,
      httr::content_type_json()
    )

    if (request$status_code == 200) {



    } else {

      rlang::abort("Token had expired. Subsequent requests could not be completed...")

    }

  }

  return(search_results)

}
