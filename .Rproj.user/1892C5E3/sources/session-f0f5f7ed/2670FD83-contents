#'convert a quarto file to audio using openAI API
#'@param api_key API key
#'@param model Available openAI TTS model: `tts-1`, `tts-1-hd`
#'@param voice The voice use when generating the audio. Supported voice are `alloy`, `echo`, `fable`, `onyx`, `nova`, `shimmer`
#'@param input The text to generate into audio
#'@param reponse_format The format to audio in. Defaults to mp3. Supported format are `mp3`, `opus`, `aac`, `flac`, `wav`, `pcm`
#'@param speed The speed of generated audio. Defaults to 1. Values are from `0.25` to `4.0`
#'@export

quartoaudio_openai <- function(api_key, model="tts-1", voice="alloy", input, ...) {
  library(httr)
  library(magrittr)

  if (is.na(api_key) || is.null(api_key) || api_key == "") {
    stop("Please provide a valid key")
  }
  if (model != "tts-1" && model != "tts-1-hd") {
    stop("Invalid model input. Please check the reference again: https://platform.openai.com/docs/api-reference/audio/createSpeech#audio-createspeech-voice")
  }

  voiceList <- c("alloy", "echo", "fable", "onyx", "nova", "shimmer")
  if (!(voice %in% voiceList)) {
    stop ("Invalid supported voice. Please check the refernce again: https://platform.openai.com/docs/api-reference/audio/createSpeech#audio-createspeech-voice")
  }

  body <- list(
    model = model,
    voice = voice,
    input = input
  )

  headers <- add_headers(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  )

  ENDPOINT <- "https://api.openai.com/v1/audio/speech"

  response <- POST(ENDPOINT, body = body, encode = "json", headers = headers)

  if (status_code(response) == 200) {
    # Save the audio response to the output file
    writeBin(content(response, "raw"), output_file)
    message("Audio saved successfully to ", output_file)
  } else {
    # If request failed, print error message
    stop(paste("Error:", content(response, "text")))
  }
}
