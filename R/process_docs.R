# currentScript <- ""
# totalLength <- 0
# MAX_LENGTH <- 4096
# for (idx in seq_along(testing)) {
#   #PROCESSING DOCUMENTS
#   currentLine <- testing[idx]
#   if (startsWith(currentLine, '#')) {
#     #trimming '#' from the header
#     currentLine <- gsub("^#+", "", currentLine)
#   }
#   #trimming leading space
#   currentLine <- gsub("^\\s+", "", currentLine)
#   testing[idx] <- currentLine
#
#
#   #API CALL
#   numChar <- nchar(currentLine)
#   if (totalLength + numChar <= 4096) {
#     totalLength <- totalLength + numChar
#     currentScript <- paste(currentScript, currentLine, sep = "\n")
#     print(currentScript)
#   } else {
#     ##calling API call
#     totalLength <- numChar
#   }
#
#   print(testing[idx])
# }
# ------------------------------


#pass a quarto file here
#select everything exepct for code chunk
rmd <- parsermd::parse_rmd() |> parsermd::rmd_select(!(parsermd::has_type("rmd_chunk"))) |> parsermd::as_document()
total_chars <- 0 #counting amount of current characters
MAX_LENGTH <- 4096 #the maximum characters per API call
current_script <- ""
idx <- 1
quote_flag <- FALSE
while (idx <= seq_along(sentence)) {
  if (sentence[idx] == "") {
    idx <- idx + 1
    next
  }

  current_line <- sentence[idx]


  #trimming off the '#'
  if (detect_heading(current_line)) {
    current_line <- trimws(gsub("^#+", "", currentLine))

  } else if (dectect_list(current_line)) {
    list_post_process <- list_process(sentence, idx)
    list_chars <- list_post_process[[1]]
    list_info <- list_post_process[[2]]
    idx <- list_post_process[[3]]
  }
}

detect_heading <- function(str) {
  return (grepl("^\\s*#{1,6}\\s+", str))
}

#Detect a list
detect_list <- function(str) {
  return (grepl("^\\s*-\\s+", str))
}


#Processing a list
list_process <- function(str_list, idx) {
  list_chars <- 0
  list_info <- ""
  while (idx <= seq_along(str_list) && detect_list(sentence[idx])) {
    current_line <- trimws(gsub("^\\s*-\\s*", "", current_line))
    list_chars <- nchar(current_line) + list_chars + 3
    list_info <- paste(list_info, current_line, sep = ". ")
    idx <- idx + 1
  }

  return (list(list_chars, list_info, idx - 1))
}

#--------------------------------------------

#PROCESSING DOCUMENTS

#Detect if a string has/is a hyperlink
#If it has/is a hyperlink -> remove the link and only keep the link's name
detect_and_process_hyperlink <- function(str) {
  pattern <- "\\[([^\\]]+)\\]\\(([^\\)]+)\\)"
  if (stringr::str_detect(str, pattern)) {
    cleaned_text <- stringr::str_replace_all(str, pattern, "\\1")
    return (cleaned_text)
  }
  return (str)
}

#Detect if a string is bold or not.
#If it is bold, remove all asterisks
detect_and_process_bold_text <- function(str) {
  pattern <- "\\*\\*(.*?)\\*\\*|\\_\\_(.*?)\\_\\_"
  if (stringr::str_detect(str, pattern)) {
    cleaned_text <- stringr::str_replace_all(str, pattern, "\\1\\2")
    return(cleaned_text)
  }
  return (str)
}

#Detect if a string is italics or not
#If it is italics, remove all asterisks
detect_and_process_italics_text <- function(str) {
  pattern <- "\\_(.*?)\\_|\\*(.*?)\\*"
  if (stringr::str_detect(str, pattern)) {
    cleaned_text <- stringr::str_replace_all(str, pattern, "\\1\\2")
    return(cleaned_text)
  }
  return (str)
}

#Detect if a string has equations or not
#If it is, remove the equation and notify as equation is detected.
detect_and_process_equations <- function(str) {
  pattern <- "\\$\\$(.*?)\\$\\$|\\$(.*?)\\$"
  if (stringr::str_detect(str, pattern)) {
    cleaned_text <- stringr::str_replace_all(str, pattern, "equation defined")
    return (cleaned_text)
  }

  return (str)
}
'
1. limiting the characters into 4096 characters only. This also includes spaces, punctations, and special characters.
2. When it is a list/quote:
  - do not need to indicate whether this is a list/quote
  - sub for each levels
  - indicate when list/quote end

  - also, if this adding list/quote would exceed limited characters -> using this a breakpoint as well?
3. how to mark the audio file in order to attach it into the right quarto file
4. Skip code chunk, and table as well
5. Detect a link in a string as well
6. screen reader of tables (across cols/rows)
7. anything shrinks the chars should be happened first
--------
1. creating the text to spend into the API
2. splititng it into portions to send into the API
  cleaning the text
3. API calls
4. Inserting audio back to the quarto file

## tracking audio file
putting these quartofile into folder
/audio/quartofile_name
quarofile-1

#Chapter notation at the beginning of the audio file. "Chapter Title" by _quarto.yml
#detect if the file has saved
'
