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
rmd <- parsermd::parse_rmd() |>
  parsermd::rmd_select(!(parsermd::has_type("rmd_chunk"))) |>
  parsermd::as_document()

total_chars <- 0
MAX_LENGTH <- 4096
idx < - 1
while (idx <= seq_along(rmd)) {
  if (rmd[idx] == "") {
    idx = idx + 1
    next
  } else if (detect_table(rmd[idx])) {
  }
}

process_docs <- function(file_name) {
  rmd <- parsermd::parse_rmd(file_name) |>
    parsermd::rmd_select(!(parsermd::has_type("rmd_chunk"))) |>
    parsermd::as_document()
  total_chars <- 0
  MAX_LENGTH <- 4096
  idx <- 1
  current_script <- ""
  script <- list()

  while (idx <= seq_along(rmd)) {
    if (detect_heading(rmd[idx])) {
      heading <- clean_heading(rmd[idx])
    } else if (detect_table(rmd[idx])) {
      idx <- process_table(idx) + 1
      next
    } else if (detect_unordered_list(rmd[idx])) {

    } else if (detect_ordered_list(rmd[idx])) {

    } else {

    }
  }
}

# #Detect a list
# detect_list <- function(str) {
#   return (grepl("^\\s*-\\s+", str))
# }
#
#
# #Processing a list
# list_process <- function(str_list, idx) {
#   list_chars <- 0
#   list_info <- ""
#   while (idx <= seq_along(str_list) && detect_list(sentence[idx])) {
#     current_line <- trimws(gsub("^\\s*-\\s*", "", current_line))
#     list_chars <- nchar(current_line) + list_chars + 3
#     list_info <- paste(list_info, current_line, sep = ". ")
#     idx <- idx + 1
#   }
#
#   return (list(list_chars, list_info, idx - 1))
# }

#--------------------------------------------
#PROCESSING DOCUMENTS

#TEXT DECORATION
#Headings
clean_text <- function(str) {
  str <- clean_strikethrough(str)
  str <- clean_subscript(str)
  str <- clean_superscript(str)
  str <- clean_bold_text(str)
  str <- clean_italics_text(str)
  str <- clean_inline_code(str)
  str <- clean_hyperlink(str)
  str <- clean_image(str)
  str <- clean_equation(str)
  return(str)
}

detect_heading <- function(str) {
  return (stringr::str_detect(str, "^#+\\s*"))
}

clean_heading <- function(str) {
  pattern <- "^#+\\s*"
  level <- stringr::str_count(str, '#')
  str <- stringr::str_replace(str, pattern, "")
  if (level == 1) {
    str <- paste("Title", str)
  } else {
    prefix <- paste(rep("Sub", level - 1), collapse = " ")
    str <- sprintf("%s Heading %s", prefix, str)
  }
  return (str)
}

#Strikethrough
clean_strikethrough <- function(str) {
  pattern <- "~~(.*?)~~"
  if (stringr::str_detect(str, pattern)) {
    str <- stringr::str_replace(str, pattern, "")
  }
  return(str)
}

clean_superscript <- function(str) {
  pattern <- "\\^(.*?)\\^"
  if (stringr::str_detect(str, pattern)) {
    str <- stringr::str_replace(str, pattern, function(text) {
      superscript_text <- stringr::str_match(str, pattern)[2]
      return(paste(" superscript", superscript_text, sep = " "))
    })
  }
  return(str)
}


clean_subscript <- function(str) {
  pattern <- "~(.*?)~"
  if (stringr::str_detect(str, pattern)) {
    str <- stringr::str_replace(str, pattern, function(text) {
      subscript_text <- stringr::str_match(str, pattern)[2]
      return(paste(" subscript", subscript_text, sep = " "))
    })
  }
  return(str)
}

#Bold text
clean_bold_text <- function(str) {
  pattern <- "\\*\\*(.*?)\\*\\*|\\_\\_(.*?)\\_\\_"
  if (stringr::str_detect(str, pattern)) {
    str <- stringr::str_replace_all(str, pattern, "\\1\\2")
  }
  return (str)
}

#Italics
clean_italics_text <- function(str) {
  pattern <- "\\_(.*?)\\_|\\*(.*?)\\*"
  if (stringr::str_detect(str, pattern)) {
    str <- stringr::str_replace_all(str, pattern, "\\1\\2")
  }
  return (str)
}

# Code chunks
#Inline code
clean_inline_code <- function(str) {
  pattern <- "`(.*?)`"
  pattern_code <- "`.*\\{[^}]+\\}.*`"

  if (stringr::str_detect(str, pattern_code)) {
    str <- stringr::str_replace(str, pattern_code, "")
  }
  else if (stringr::str_detect(str, pattern)) {
    str <- stringr::str_replace_all(str, pattern, "\\1")
  }
  return (str)
}

#LINKS
clean_hyperlink <- function(str) {
  pattern <- "\\[([^\\]]+)\\]\\(([^\\)]+)\\)"
  if (stringr::str_detect(str, pattern)) {
    str <- stringr::str_replace_all(str, pattern, "\\1") #fix this
  }
  return (str)
}

#IMAGES
clean_image <- function(str) {
  pattern <-  "!\\[(.*?)\\]\\((.*?)\\)"
  if (stringr::str_detect(str, pattern)) {
    str <- stringr::str_replace_all(str, pattern, function(text) {
      caption <- stringr::str_match(text, pattern)[2]
      if (caption == "") {
        return("An image goes here")
      } else {
        return(paste("An image with ", caption, sep = " ")) #fix this
      }
    })
  }
  return(str)
}

#EQUATION
clean_equation <- function(str) {
  pattern <- "\\$\\$(.*?)\\$\\$|\\$(.*?)\\$"
  if (stringr::str_detect(str, pattern)) {
    str <- stringr::str_replace_all(str, pattern, "equation define") #fix this
  }
  return (str)
}

#PARAGRAPH
split_paragraph <- function(str) {
  sentences <- unlist(strsplit(str, ".", fixed = TRUE))
  sentences <- sentences[sentences != "" & sentences != " "]
  return(sentences)
}

#LIST
#CHECKED/UNCHECKED ITEM
clean_task_list <- function(text) {
  unchecked_item <- "^\\[ \\] (.+)"
  checked_item <- "^\\[x\\] (.+)"

  if (stringr::str_detect(str, unchecked_item)) {
    str <- stringr::str_match(str, unchecked_item)[2]
    str <- paste("Unchecked item", str, sep =" ")
  } else if (stringr::str_detect(str, checked_item)) {
    str <- stringr::str_match(str, checked_item)[2]
    str <- paste("Checked Item", str, sep = " ")
  }

  return(str)
}

#LIST
detect_unordered_list <- function(str) {
  return (stringr::str_detect(str, "^\\s*-\\s*") && (nchar(str) != stringr::str_count(str, "-")))
}

clean_unorderd_list <- function(rmd, idx) {
  prev_indent <- 0
  list_str <- "list start"
  while (detect_unordered_list(rmd[idx]) || rmd[idx] == "") {
    if (rmd[idx] != "") {

      curr_indent <- nchar(rmd[idx]) - nchar(gsub("^\\s+", "", rmd[idx]))
      # print(paste("curr_indent", as.character(curr_indent)))
      # print(paste("prev_indent", as.character(prev_indent)))
      # print(paste("level", as.character(level)))
      if (curr_indent < prev_indent) {
        level <- round(curr_indent / 4)
        sub_level <- paste(rep("sub", level - 1), collapse = " ")
        list_str <- paste(list_str, sprintf("%s start", sub_level), sep = ". ")
      } else if (curr_indent > prev_indent) {
        level <- round(prev_indent / 4)
        sub_level <- paste(rep("sub", level - 1), collapse = " ")
        list_str <- paste(list_str, sprintf("%s end", sub_level), sep = ". ")
      }

      rmd[idx] <- clean_text(sub("^\\s*-\\s*", "", rmd[idx]))
      list_str <- paste(list_str, rmd[idx], sep = ". ")
      prev_indent <- curr_indent
    }
    idx <- idx + 1
  }

  list_str <- paste(list_str, "list end", sep = ". ")
  return (c(list_str, idx))
}


#TABLE
detect_table <- function(str) {
  pipes_count <- stringr::str_count(str, "\\|")
  column_count <- length(stringr::str_split(str, "\\|", simplify = TRUE))
  return(pipes_count != 0 && pipes_count + 1 == column_count)
}

process_table_helper <- function(rmd, tables, idx) {
  columns <- stringr::str_split((rmd[idx]), "\\|", simplify = TRUE)
  right_idx <- length(columns) - 1
  tables <- append(tables, list(columns[2: right_idx]))
  return(tables)
}

detect_table_separator <- function(str) {
  pattern <- "^\\|?[-:|]+\\|?$"
  return (stringr::str_detect(str, pattern))
}

process_table <- function(rmd, idx) {
  tables <- list()
  tables <- process_table_helper(rmd, tables, idx)
  while (detect_table(rmd[idx + 1])) {
    if (!(detect_table_separator(rmd[idx + 1]))) {
      tables <- process_table_helper(rmd, tables, idx + 1)
    }
    idx = idx + 1
  }
  return(idx)
}

#PAGE BREAK
detect_break_line <- function(str) {
  pattern <- "^[-*]+$"
  return (stringr::str_detect(str, pattern) && (stringr::str_count(str, "[-*]")))
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
