#' Convert Quarto file into audio using VoiceRSS API (https://www.voicerss.org/api/)
#' @param input quarto file
#' @param hl convert textual content into the desired language. Defaults to English (US). More language can be found on VoiceRSS API website
#' @param v convert textual content into following voices. Defaults to Linda (English(US)). More voices can be found on VoiceRSS API website
#' @param c convert textual content to audio codecs. Defaults to MP3. More audio codecs can be found on VoiceRSS API website
#' @param f convert textual to audio formats. Defaults to 44khz_16bit_stereo. More audio format can be found on VoiceRSS API website
#' @export

quartoaudio_voiceRSS <- function(input, api_key, hl="en-us", c="MP3", f="44khz_16bit_stereo",...){
  library(httr)

  ENDPOINT <- "http://api.voicerss.org/"

  payload <- list(
    key = api_key,
    src = input,
    hl = hl,
    c = c,
    f = f
    )

  other_information <- list(...)

  payload <- c(payload, other_information)
  response <- POST(ENDPOINT, body = payload)

  if (http_error(response)) {
    stop("Error:", content(response, "text"))
  }

  response_content <- content(response, "text")
  if (grepl("ERROR:", response_content)) {
    response_content <- (unlist(strsplit(response_content, ":")[1]))[2] #will need to modify this line later
    stop(response_content)
  }

}
