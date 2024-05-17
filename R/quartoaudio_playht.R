#' Convert  a quarto file to audio using openAI API
#' @param input The text to generate into audio
#' @export

quartoaudio_playht <- function(input, user_id, secret_key, output_format="mp3",...) {
  library(httr)

  ENDPOINT <- "https://api.play.ht/api/v2/tts"

  headers <-  c(
    `AUTHORIZATION` = secret_key,
    `X-USER-ID` = user_id,
    `Content-Type` = "application/json"
  )

  payload <- list(
    text = input,
    voice = "s3://voice-cloning-zero-shot/d9ff78ba-d016-47f6-b0ef-dd630f59414e/female-cs/manifest.json",
    output_format = output_format,
    voice_engine = "PlayHT2.0"
  )

  response <- POST(ENDPOINT,
                   body = payload,
                   add_headers(.headers = headers),
                   content_type("application/json"),
                   accept("text/event-stream"),
                   encode = "json")
  content(response, "text")
}
