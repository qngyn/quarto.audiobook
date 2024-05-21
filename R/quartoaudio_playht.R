#' Convert  a quarto file to audio using openAI API
#' @param input The text to generate into audio
#' @export

quartoaudio_playht <- function(input, user_id, secret_key,...) {
  library(httr)
  if (is.na(user_id) || is.null(user.id) || user.id == "") {
    stop("No user id is provided. Please try again!")
  }

  if(is.na(secret_key) || is.null(secret_key) || secret_key == "") {
    stop("No key is provided. Please try again!")
  }

  ENDPOINT <- "https://api.play.ht/api/v2/tts"

  headers <-  c(
    `AUTHORIZATION` = secret_key,
    `X-USER-ID` = user_id,
    `Content-Type` = "application/json"
  )

  payload <- list(
    text = input,
    voice = "s3://voice-cloning-zero-shot/d9ff78ba-d016-47f6-b0ef-dd630f59414e/female-cs/manifest.json"
  )

  other_information <- list(...)
  payload <- c(payload, other_information)

  response <- POST(ENDPOINT,
                   body = payload,
                   add_headers(.headers = headers),
                   content_type("application/json"),
                   accept("text/event-stream"),
                   encode = "json")
  content(response, "text")
}
