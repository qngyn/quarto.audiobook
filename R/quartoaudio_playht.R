#' Convert  a quarto file to audio using play.ht API (https://docs.play.ht/reference/api-generate-audio)
#' @param input The text to generate into audio
#' @param user_id
#' @param api_key
#' @param voice the unique ID for a playHT or cloned voice
#' @export

quartoaudio_playht <- function(input,
                               user_id = get_playht_user_id(),
                               api_key = get_playht_api_key(),
                               voice = "s3://voice-cloning-zero-shot/d9ff78ba-d016-47f6-b0ef-dd630f59414e/female-cs/manifest.json",
                               ...) {
  if (is.na(user_id) || is.null(user.id) || user.id == "") {
    stop("No user id is provided. Please try again!")
  }

  if(is.na(api_key) || is.null(api_key) || api_key == "") {
    stop("No key is provided. Please try again!")
  }

  ENDPOINT <- "https://api.play.ht/api/v2/tts"

  headers <-  list(
    `AUTHORIZATION` = api_key,
    `X-USER-ID` = user_id,
    `Content-Type` = "application/json",
    `Accept` = "text/event-stream"
  )

  payload <- list(
    text = input,
    voice = voice
  )

  other_information <- list(...)

  payload <- c(payload, other_information)

  request <-  httr2::request(ENDPOINT) |> httr2::req_headers(!!!headers) |> httr2::req_body_json(payload)

  response <- request |> httr2::req_perform()
}


get_playht_user_id <- function() {
  user_id <- Sys.getenv("PLAYHT_USER_ID")
  if (identical(user_id, "")) {
    stop("No PLAYHT_USER_ID is found, please supply with `user_id` arguments or with PLAYHT_USER_ID env var")
  }
  user_id
}

get_playht_api_key <- function() {
  key <- Sys.getenv("PLAYHT_API_KEY")
  if (identical(key, "")) {
    stop("No PLAYHT_API_KEY is found, please supply with `api_key` arguments or with PLAYHT_API_KEY env var")
  }
  key
}


# quartoaudio_playht <- function(input, user_id, secret_key,...) {
#   library(httr)
#   if (is.na(user_id) || is.null(user_id) || user_id == "") {
#     stop("No user id is provided. Please try again!")
#   }
#
#   if(is.na(secret_key) || is.null(secret_key) || secret_key == "") {
#     stop("No key is provided. Please try again!")
#   }
#
#   ENDPOINT <- "https://api.play.ht/api/v2/tts"
#
#   headers <-  c(
#     `AUTHORIZATION` = secret_key,
#     `X-USER-ID` = user_id,
#     `Content-Type` = "application/json"
#   )
#
#   payload <- list(
#     text = input,
#     voice = "s3://voice-cloning-zero-shot/d9ff78ba-d016-47f6-b0ef-dd630f59414e/female-cs/manifest.json"
#   )
#
#   other_information <- list(...)
#   payload <- c(payload, other_information)
#
#   response <- POST(ENDPOINT,
#                    body = payload,
#                    add_headers(.headers = headers),
#                    content_type("application/json"),
#                    accept("text/event-stream"),
#                    encode = "json")
#   content(response, "text")
# }
