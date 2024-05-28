#' Convert Quarto file into audio using VoiceRSS API (https://www.voicerss.org/api/)
#' @param input quarto file
#' @param hl convert textual content into the desired language. Defaults to English (US). More language can be found on VoiceRSS API website
#' @param v convert textual content into following voices. Defaults to Linda (English(US)). More voices can be found on VoiceRSS API website
#' @param c convert textual content to audio codecs. Defaults to MP3. More audio codecs can be found on VoiceRSS API website
#' @param f convert textual to audio formats. Defaults to 44khz_16bit_stereo. More audio format can be found on VoiceRSS API website
#' @export

quartoaudio_voiceRSS <- function(input,
                                 api_key = get_voicerss_api_key(),
                                 hl="en-us",
                                 c="MP3",
                                 f="44khz_16bit_stereo",
                                 ...){

  if (is.na(api_key) || is.null(api_key) || api_key == "") {
    stop("Please provide a key")
  }

  #Base URL
  ENDPOINT <- "http://api.voicerss.org/"

  #Request payload
  payload <- list(
    key = api_key,
    src = input,
    hl = hl,
    c = c,
    f = f
    )

  other_information <- list(...)

  payload <- c(payload, other_information)
  # response <- POST(ENDPOINT, body = payload)

  request <- httr2::request(ENDPOINT) |> httr2::req_body_form(!!!payload)
  response <- request |> httr2::req_perform()

  if (httr2::resp_is_error(response)) {
    stop("Error:", httr2::resp_body_string(response))
  }

  response_content <- httr2::resp_body_string(response)
  if (grepl("ERROR:", response_content)) {
    response_content <- (unlist(strsplit(response_content, ":")[1]))[2] #will need to modify this line later
    stop(response_content)
  }

  if (httr2::resp_status(response) == 200) {
    output_file <- "voiceRSStesting.mp3" #temporary hold
    # Save the audio response to the specified output file
    writeBin(httr2::resp_body_raw(response), output_file)
    message("Audio saved successfully to ", output_file)
  } else {
    # If request failed, print error message
    stop(paste("Error:", httr2::resp_body_string(response)))
  }
}

get_voicerss_api_key <- function() {
  key <- Sys.getenv("VOICERSS_API_KEY")
  if (identical(key, "")) {
    stop("No VOICERSS_API_KEY is found, please supply with `api_key` arguments or with VOICERSS_API_KEY env var")
  }
  key
}
