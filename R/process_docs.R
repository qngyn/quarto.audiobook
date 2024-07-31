#' Process R markdown Document
#' @description
#' This function processes an R Markdown (or quarto) file by parsing its contents to return a clean and structured version before putting these information for TTS APIs
#' @param file_name the R Markdown/Quarto file name
#' @returns list of clean text element from the file
#'

process_docs <- function(file_name) {
  #Parse the R Markdown file and filter out specific block types (code)
  rmd <- parsermd::parse_rmd(file_name) |>
    parsermd::rmd_select(!(parsermd::has_type(c("rmd_code_block", "rmd_raw_attr_chunk", "rmd_fenced_div_open", "rmd_raw_chunk", "rmd_fenced_div_close")))) |>
    parsermd::as_document()

  main_scripts <- list()
  current_script <- ""
  MAX_CHAR <- 4096
  idx <- 1

  while (idx <= length(rmd)) {
    if (detect_break_line(rmd[idx]) || rmd[idx] == ""){
      idx <- idx + 1
      next
    } else if (detect_block_list(rmd[idx])) {
      block_list <- clean_block_list(rmd, idx)
      scripts1 <- group_string(main_scripts, current_script, block_list[1], MAX_CHAR)
      main_scripts <- scripts1[1]
      current_script <- scripts1[2]
      idx <- as.numeric(block_list[2])
      next
    } else if (detect_table(rmd[idx])) {
      idx <- process_table(rmd, idx)
      next
    } else if (detect_unorder_list(rmd[idx])) {
      unorder_list <- clean_unorder_list(rmd, idx)
      scripts2 <- group_string(main_scripts, current_script, unorder_list[1], MAX_CHAR)
      main_scripts <- scripts2[1]
      current_script <- scripts2[2]
      idx <- as.numeric(unorder_list[2])
      next
    } else if (detect_quotes(rmd[idx])) {
      quotes <- clean_quotes(rmd, idx)
      scripts3 <- group_string(main_scripts, current_script, quotes[1], MAX_CHAR)
      main_scripts <- scripts3[1]
      current_script <- scripts3[2]
      idx <- as.numeric(quotes[2])
      next
    } else {
      rmd[idx] <- clean_text(rmd[idx])
      if (detect_heading(rmd[idx])) {
        rmd[idx] <- clean_heading(rmd[idx])
        if (nchar(current_script) != 0) {
          main_scripts <- append(main_scripts, current_script)
          current_script <- ""
        }
        current_script <- rmd[idx]
      } else {
        if (nchar(current_script) + nchar(rmd[idx]) + 2 <= MAX_CHAR) {
          if (nchar(current_script) == 0) {
            current_script <- rmd[idx]
          } else {
            current_script <- paste(current_script, rmd[idx], sep = "\n")
          }
        } else {
          scripts4 <- split_paragraph(main_scripts, current_script, rmd[idx], MAX_CHAR)
          main_scripts <- scripts4[1]
          current_script <- scripts4[2]
        }
      }
    }

    idx <- idx + 1
  }

  if (nchar(current_script) > 0) {
    main_scripts <- append(main_scripts, current_script)
  }

  return(unlist(main_scripts))
}

#--------------------------------------------
#PROCESSING DOCUMENTS

#TEXT DECORATION
#' Clean various text decorations in the input string
#' @param str an input string
#' @returns a clean string w text denotations but no decorations
clean_text <- function(str) {
  str <- clean_strikethrough(str)
  str <- clean_subscript(str)
  str <- clean_superscript(str)
  str <- clean_hyperlink(str)
  str <- clean_image(str)
  str <- clean_bold_text(str)
  str <- clean_italics_text(str)
  str <- clean_inline_code(str)
  str <- clean_equation(str)
  str <- clean_extra_spaces(str)
  str <- clean_task_list(str)
  return(str)
}

#' Group string based on maximum character limit (4096)
#' @param str the list of main script
#' @param current_script the current script that is being processed
#' @param script_to_add the script need to process
#' @param MAX_CHAR the maximum character limit (default as 4096)
#' @returns a list containing updated main_script and current_script by script_to_add
group_string <- function(main_script, current_script, script_to_add, MAX_CHAR = 4096) {
  if (nchar(current_script) + nchar(script_to_add) + 2 <= MAX_CHAR) {
    current_script <- paste(current_script, script_to_add, sep = "\n")
  } else {
    main_script <- append(main_sript, current_script)
    current_script <- ""
  }
  return(list(main_script, current_script))
}

#Headings
#' Detect if the current string is a header (`# text`) syntax
#' @param str an input string
#' @return a boolean value if the current string is a header or not
detect_heading <- function(str) {
  return (stringr::str_detect(str, "^#+\\s*"))
}

#' Clean the R Markdown header
#' @param str a string represents an R Markdown header
#' @returns a clean version of a header without '#'. If the levels of the heading is more than 1, it will also indicates the number of sub levels.
#' @examples
#' clean_heading("## Level 2 Heading")
#' Returns "Sub Heading Level 2 Heading"
clean_heading <- function(str) {
  attribute_pattern <- "\\{\\.[a-zA-Z0-9_-]+\\}"

  hash_pattern <- "^#+\\s*"

  level <- stringr::str_count(str, '#')

  cleaned_str <- stringr::str_remove_all(str, hash_pattern)

  cleaned_str <- stringr::str_remove_all(cleaned_str, attribute_pattern)

  cleaned_str <- stringr::str_trim(cleaned_str)

  if (level == 1) {
    cleaned_str <- paste("Title", cleaned_str)
  } else {
    prefix <- paste(rep("Sub", level - 1), collapse = " ")
    cleaned_str <- sprintf("%s Heading %s", prefix, cleaned_str)
  }

  return(cleaned_str)
}

#Strikethrough
clean_strikethrough <- function(str) {
  pattern <- "~~(.*?)~~"
  if (stringr::str_detect(str, "~~(.*?)~~")) {
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
#' function to remove bold text syntax
#' @param str a string may have bold text (**text**)
#' @returns a string without moving bold text syntax
clean_bold_text <- function(str) {
  pattern <- "\\*\\*(.*?)\\*\\*|\\_\\_(.*?)\\_\\_"
  if (stringr::str_detect(str, pattern)) {
    str <- stringr::str_replace_all(str, pattern, "\\1\\2")
  }
  return (str)
}

#Italics
#' function to remove italics text syntax
#' @param str a string may have italics text (**text**)
#' @returns a string without moving italics text syntax
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

# detect_hyperlink <- function(str) {
#   return (stringr::str_detect(str, "\\[([^\\]]+)\\]\\(([^\\)]+)\\)"))
# }

#' Remove Markdown hyperlinks (denotes as `[text](url)`) from a string
#' @param str an input string
#' @returns an input string with all Markdown hyperlinks replace by their link text
#' @examples
#' clean_hyperlink("# [Summary](https://www.example.com)")
#' Returns "# Summary"
#' clean_hyperlink("link to this [package](https://github.com/qngyn/quarto.audiobook) and [the program](https://summerofcode.withgoogle.com/programs/2024/projects)")
#' Returns "link to this package and the program"
clean_hyperlink <- function(str) {
  pattern <- "\\[([^\\]]+)\\]\\(([^\\)]+)\\)"
  if (stringr::str_detect(str, pattern)) {
    str <- stringr::str_replace_all(str, pattern, "\\1")
  }
  return (str)
}

#IMAGES
#' extract the name of an image (text between square bracket) in image syntax in markdown
#' @param str input string
#' @returns return the string with the name of the image only
clean_image <- function(str) {
  pattern <-  "!\\[(.*?)\\]\\((.*?)\\)"
  if (stringr::str_detect(str, pattern)) {
    str <- stringr::str_replace_all(str, pattern, function(text) {
      caption <- stringr::str_match(text, pattern)[2]
      if (caption == "") {
        return("An image goes here")
      } else {
        return(paste("An image with a caption as", caption, sep = " "))
      }
    })
  }
  return(str)
}

#EQUATION
#' Mark a string if it contains an equation
#' @param str input string
#' @returns return the input string without the equation but with a denotation
#' @examples
#' clean_equation("This is a string with an equation $x = y + 1$")
#' Returns "This is a string with an equation an equation goes here"
#' clean_equation("$$ x = y + 1 $$")
#' Returns "an equation goes here
clean_equation <- function(str) {
  pattern <- "\\$\\$(.*?)\\$\\$|\\$(.*?)\\$"
  if (stringr::str_detect(str, pattern)) {
    str <- stringr::str_replace_all(str, pattern, "an equation goes here")
  }
  return (str)
}

#PARAGRAPH

# split_paragraph <- function(str) {
#   sentences <- unlist(strsplit(str, ".", fixed = TRUE))
#   sentences <- sentences[sentences != "" & sentences != " "]
#   return(sentences)
# }

#'
split_paragraph <- function(main_scripts, current_script, text_to_add, MAX_CHAR = 4096) {
  sentences <- unlist(strsplit(text_to_add, ".", fixed = TRUE))
  sentences <- sentences[sentences != "" & sentences != " "]
  for (sentence in sentences) {
    if (nchar(sentence) + nchar(current_script) + 2 <= MAX_CHAR) {
      current_script <- paste(current_script, sentence, sep = "/n")
    } else {
      main_scripts <- append(main_scripts, current_script)
      current_script <- sentence
    }
  }
  return (list(main_scripts, current_script))
}

#LIST
#CHECKED/UNCHECKED ITEM
#'
#'@param str an input string that may have checked/unchecked syntax
#'@returns an input string w/o checked/uncheked syntax but with "unchecked/checked" identification
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
#' helper function to detect if this is a part of an unorder list or not
#' @param str an input string
#' @returns a boolean value as if this is a list or not
detect_unorder_list <- function(str) {
  return (stringr::str_detect(str, "^\\s*-\\s*") && (nchar(str) != stringr::str_count(str, "-")))
}

#' helper function to calculate the indent level
#' @param str an element in the list
#' @returns return the indent level of the current element in a list
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

#' cleaning unorder list
#' @param rmd the parse markdown information
#' @param idx the start index where first detects a list
#' @returns a flatten list which indicates where a list, and a sub list start/end (w/ text message to indicate if a sublist of level three is happened) and the last index where the list end
clean_unorder_list <- function(rmd, idx) {
  list_str <- ""
  stack <- vector()

  while (detect_unorder_list(rmd[idx]) || rmd[idx] == "") {
    if (rmd[idx] != "") {
      curr_indent <- indent_level(rmd[idx])
      if (list_str == "") {
        list_str <- "list start"
      } else if (tail(stack, 1) < curr_indent) {
        level <- round(curr_indent / 4)
        if (length(stack) == 1) {
          list_str <- paste(list_str, indicate_list("start", level), sep = ". ")
        } else {
          message("a list of level three of more is indicated")
        }

      } else if (tail(stack, 1) > curr_indent) {
        while (length(stack) > 2 && tail(stack, 1) > curr_indent) {
          stack <- stack[-length(stack)]
        }
        level <- round(stack[length(stack)] / 4)
        list_str <- paste(list_str, indicate_list("end", level), sep = ". ")
        stack <- stack[-length(stack)]
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


#QUOTES
#' helper function to detect if this is a part of a quote list or not
#' @param str an input string
#' @returns a boolean value as if this is a quote or not
detect_quotes <- function(str) {
  return (stringr::str_detect(str, "^\\s*>\\s*"))
}

#' helper function to calculate the indent level of the quote
#' @param str an element in the list
#' @returns return the indent level of the current element in the quotes
detect_quote_level <- function(str) {
  return (stringr::str_count(str, ">"))
}

#' cleaning the quote list
#' @param rmd the parse markdown information
#' @param idx the start index where first detects a quote
#' @returns a flatten list which indicates where a quote list, and a sub quote list start/end (w/ text message to indicate if a sub quote list of level three is happened) and the last index where the quote list end
clean_quotes <- function(rmd, idx) {
  list_str <- ""
  stack <- vector()

  while (detect_quotes(rmd[idx])) {
    level <- detect_quote_level(rmd[idx])
    if (list_str == "") {
      list_str <- "quote starts"
      stack <- append(stack, level)
    } else if (tail(stack, 1) < level) {
      if (length(stack) == 1) {
        sub_level <- paste(rep("sub", level), collapse = " ")
        list_str <- paste(list_str, sprintf("%s quote %s", sub_level, "start"), sep = ". ")
      } else {
        message("a quote of level three of more is started")
      }
      stack <- append(stack, level)
    } else if (tail(stack, 1) > level) {
      while (length(stack) > 2 && tail(stack, 1) > level) {
        stack <- stack[-length(stack)]
      }
      sub_level <- paste(rep("sub", level), collapse = " ")
      list_str <- paste(list_str, sprintf("%s quote %s", sub_level, "end"), sep = ". ")
      stack <- stack[-length(stack)]
    }

    rmd[idx] <- clean_text(gsub("^([> ]+)", "", rmd[idx]))
    if (rmd[idx] != "") {
      list_str <- paste(list_str, rmd[idx], sep = ". ")
    }
    idx <- idx + 1
  }
  while (length(stack) > 1) {
    sub_level <- paste(rep("sub", level), collapse = " ")
    list_str <- paste(list_str, sprintf("%s quote %s", sub_level, "end"), sep = ". ")
    stack <- stack[-length(stack)]
  }

  list_str <- paste(list_str, "quote end", sep = ". ")
  return (c(list_str, idx))
}

#BLOCK LIST
detect_block_list <- function(str) {
  pattern <- "^\\|[^|]*$"
  return(stringr::str_detect(str, pattern))
}

clean_block_list <- function(rmd, idx) {
  list_str <- ""
  while (detect_block_list(rmd[idx])) {
    clean_string <- stringr::str_replace(rmd[idx], "^\\|\\s*", "")
    list_str <- paste(list_str, clean_string)
    idx <- idx + 1
  }
  return (c(list_str, idx))
}

#TABLE (not fully developed)
# the following fuctions replace to table is not fully developed yet ~ will need to improve in the future
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
#' detect if the current string is a line break syntax
#' @param str an input string
#' @return a boolean value if the input string is a line break or not
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
