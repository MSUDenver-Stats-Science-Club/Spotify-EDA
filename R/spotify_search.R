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
  album_name_c <- convert_spaces(ablum_name)

  ## Defining our base url
  api_url <- "https://api.spotify.com/v1/search"

  ## Create search query
  # types <- paste(type, collapse = ",")
  # convert_name <- stringr::str_replace_all(name, " ", "%20")
  s_params <- glue::glue("track:{track_name_c}+artist:{artist_name_c}+album:{album_name_c}")

  types <-

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
      httr::content() %>%
      purrr::pluck("tracks", "items")

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
