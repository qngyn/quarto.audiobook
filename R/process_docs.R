process_docs <- function(file_name) {
  rmd <- parsermd::parse_rmd(file_name) |>
    parsermd::rmd_select(!(parsermd::has_type("rmd_chunk"))) |>
    parsermd::as_document()
  total_chars <- 0
  MAX_LENGTH <- 4096
  idx <- 1
  current_script <- ""
  script <- list()

  while (idx <= length(rmd)) {
    if (detect_table(rmd[idx])) {
      idx <- process_table(rmd, idx)
      next
    } else if (detect_unordered_list(rmd[idx])) {
      unordered_list <- clean_unorderd_list(rmd, 35)
      script <- append(script, unordered_list[1])
      idx <- unordered_list[2]
      next
    }

    if (rmd[idx] != "") {
      rmd[idx] <- clean_text(rmd[idx])
      if (detect_heading(rmd[idx])) {
        rmd[idx] <- clean_heading(rmd[idx])
      }
      script <- append(script, split_paragraph(rmd[idx]))
    }

    idx <- idx + 1
  }

  return (unlist(script))

}


#--------------------------------------------
#PROCESSING DOCUMENTS

#TEXT DECORATION
#Headings
clean_text <- function(str) {
  str <- clean_strikethrough(str)
  str <- clean_subscript(str)
  str <- clean_superscript(str)
  str <- clean_hyperlink(str)
  str <- clean_bold_text(str)
  str <- clean_italics_text(str)
  str <- clean_inline_code(str)
  str <- clean_image(str)
  str <- clean_equation(str)
  str <- clean_extra_spaces(str)
  str <- clean_task_list(str)
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
detect_strikethrough <- function(str) {
  return (stringr::str_detect(str, "~~(.*?)~~"))
}
clean_strikethrough <- function(str) {
  pattern <- "~~(.*?)~~"
  str <- stringr::str_replace(str, pattern, "")
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

#' cleaning extra white space(s) (including leading, trailing, and spaces in between)
#' @param str input string
#' @returns return string after removing extra white space(s)
clean_extra_spaces <- function(str) {
  str <- trimws(str, "both")
  str <- gsub("\\s+"," ", str)
  return(str)
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
detect_hyperlink <- function(str) {
  return (stringr::str_detect(str, "\\[([^\\]]+)\\]\\(([^\\)]+)\\)"))
}
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
        return(paste("An image with a caption as", caption, sep = " ")) #fix this
      }
    })
  }
  return(str)
}

#EQUATION
clean_equation <- function(str) {
  pattern <- "\\$\\$(.*?)\\$\\$|\\$(.*?)\\$"
  if (stringr::str_detect(str, pattern)) {
    str <- stringr::str_replace_all(str, pattern, "an equation goes here") #fix this
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
clean_task_list <- function(str) {
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

#' helper function to calculate the indent level
#' @param str an element in the list
#' @returns return the indent level
indent_level <- function(str) {
  return (nchar(str) - nchar(gsub("^\\s+", "", str)))
}

#' helper function to generate sub list/end
#' @param command indicates if it is start or end
#' @param level indicates level of the list
#' return string that with sub repeates to certain level start/end
indicate_list <- function(command, level) {
  sub_level <- paste(rep("sub", level), collapse = " ")
  sprintf("%s list %s", sub_level, command)
}

clean_unorderd_list <- function(rmd, idx) {
  list_str <- ""
  stack <- vector()

  while (detect_unordered_list(rmd[idx]) || rmd[idx] == "") {
    if (rmd[idx] != "") {
      curr_indent <- indent_level(rmd[idx])
      if (list_str == "") {
        list_str <- "list start"
      } else if (tail(stack, 1) < curr_indent) {
        level <- round(curr_indent / 4)
        list_str <- paste(list_str, indicate_list("start", level), sep = ". ")
      } else if (tail(stack, 1) > curr_indent) {
        while (length(stack) != 0 && tail(stack, 1) > curr_indent) {
          level <- round(stack[length(stack)] / 4)
          list_str <- paste(list_str, indicate_list("end", level), sep = ". ")
          stack <- stack[-length(stack)]
        }
      }

      if (length(stack) == 0 || curr_indent != stack[length(stack)]) {
        stack <- append(stack, curr_indent)
      }

      rmd[idx] <- clean_text(sub("^\\s*-\\s*", "", rmd[idx]))
      list_str <- paste(list_str, rmd[idx], sep = ". ")
    }
    idx <- idx + 1
  }
  while (length(stack) > 1) {
    level <- round(stack[length(stack)] / 4)
    list_str <- paste(list_str, indicate_list("end", level), sep = ". ")
    stack <- stack[-length(stack)]
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
    # if (!(detect_table_separator(rmd[idx + 1]))) {
    #   tables <- process_table_helper(rmd, tables, idx + 1)
    # }
    idx = idx + 1
  }
  return(idx + 1)
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
1. creating the text to feed into the API
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

- keep .md files
-
'
