#' Convert Quarto file into audio using VoiceRSS API (https://www.voicerss.org/api/)
#' @param input quarto file
#' @param language convert textual content into the desired language. Defaults to English (US). More language can be found on VoiceRSS API website
#' @param voices convert textual content into following voices. Defaults to Linda (English(US)). More voices can be found on VoiceRSS API website
#' @param audio_codecs convert textual content to audio codecs. Defaults to MP3. More audio codecs can be found on VoiceRSS API website
#' @param audio_format convert textual to audio formats. Defaults to 44khz_16bit_stereo. More audio format can be found on VoiceRSS API website
#' @export

quartoaudio_voiceRSS <- function(input, api_key, language="en-us", voices="Linda", audio_codecs="MP3", audio_format="44khz_16bit_stereo"){
  ENDPOINT <- "http://api.voicerss.org/"
  params <- list(key = api_key, src = input, hl = language, c = "MP3", f = audio_format)
  response <- POST(ENDPOINT, body = params)

  if (http_error(response)) {
    stop("Error:", content(response, "text"))
  }

  response_content <- content(response, "text")
  if (grepl("ERROR:", response_content)) {
    response_content <- (unlist(strsplit(response_content, ":")[1]))[2] #will need to modify this line later
    stop(response_content)
  }
}
