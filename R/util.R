# Content Editing Functions ----

# Reorganize library calls to the beginning of the file
reorganize_libraries <- function(editor_content) {
  if (is.null(editor_content)) {
    return(NULL)
  }

  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }

  # Extract all library calls
  library_calls <- extract_library_calls(editor_content)

  # If no library calls found, return original content
  if (length(library_calls) == 0) {
    return(paste(editor_content, collapse = "\n"))
  }

  # Remove library calls from their current locations
  cleaned_content <- remove_library_calls_from_content(editor_content)

  # Add consolidated library chunk at the beginning
  final_content <- add_library_chunk_to_beginning(
    cleaned_content,
    library_calls
  )

  return(paste(final_content, collapse = "\n"))
}

# Extract all library() calls from the content
extract_library_calls <- function(editor_content) {
  library_calls <- character(0)
  in_r_chunk <- FALSE

  for (i in seq_along(editor_content)) {
    line <- editor_content[i]

    if (grepl("^```\\{r\\}", line)) {
      in_r_chunk <- TRUE
    } else if (in_r_chunk && grepl("^```$", line)) {
      in_r_chunk <- FALSE
    } else if (in_r_chunk) {
      # Check for library calls in this line
      if (grepl("^\\s*library\\s*\\(", line, perl = TRUE)) {
        library_calls <- c(library_calls, trimws(line))
      }
    }
  }

  # Remove duplicates while preserving order
  return(unique(library_calls))
}

# Remove library calls from their current locations
remove_library_calls_from_content <- function(editor_content) {
  result <- character(0)
  in_r_chunk <- FALSE
  current_chunk_lines <- character(0)
  chunk_start_line <- ""

  i <- 1
  while (i <= length(editor_content)) {
    line <- editor_content[i]

    if (grepl("^```\\{r\\}", line)) {
      # Start of R chunk
      in_r_chunk <- TRUE
      current_chunk_lines <- character(0)
      chunk_start_line <- line
    } else if (in_r_chunk && grepl("^```$", line)) {
      # End of R chunk - process the accumulated lines
      in_r_chunk <- FALSE

      # Filter out library calls from chunk content
      non_library_lines <- character(0)
      for (chunk_line in current_chunk_lines) {
        if (!grepl("^\\s*library\\s*\\(", chunk_line, perl = TRUE)) {
          non_library_lines <- c(non_library_lines, chunk_line)
        }
      }

      # Only add the chunk if it has non-library content
      if (length(non_library_lines) > 0) {
        # Check if all remaining lines are just whitespace
        non_empty_lines <- non_library_lines[trimws(non_library_lines) != ""]
        if (length(non_empty_lines) > 0) {
          result <- c(result, chunk_start_line, non_library_lines, line)
        }
      }

      current_chunk_lines <- character(0)
    } else if (in_r_chunk) {
      # Inside R chunk - collect lines
      current_chunk_lines <- c(current_chunk_lines, line)
    } else {
      # Outside R chunk - add line as-is
      result <- c(result, line)
    }

    i <- i + 1
  }

  return(result)
}

# Add consolidated library chunk to the beginning
add_library_chunk_to_beginning <- function(content_lines, library_calls) {
  if (length(library_calls) == 0) {
    return(content_lines)
  }

  # Find where to insert the library chunk (after YAML header)
  yaml_start_line <- 0
  yaml_end_line <- 0
  in_yaml <- FALSE

  for (i in seq_along(content_lines)) {
    line <- content_lines[i]

    # Look for YAML start (first non-empty line that starts with ---)
    if (!in_yaml && grepl("^---\\s*$", line)) {
      # For the first line, always consider it as YAML start if it matches
      if (i == 1) {
        yaml_start_line <- i
        in_yaml <- TRUE
      } else {
        # For other lines, check if there's substantial content before it
        preceding_content <- content_lines[1:(i - 1)]
        has_substantial_content <- any(
          grepl("\\S", preceding_content) &
            !grepl("^\\s*$", preceding_content)
        )

        if (!has_substantial_content) {
          yaml_start_line <- i
          in_yaml <- TRUE
        }
      }
    } else if (in_yaml && grepl("^---\\s*$", line)) {
      yaml_end_line <- i
      break
    }
  }

  # Create the library chunk
  library_chunk <- c(
    "```{r}",
    library_calls,
    "```"
  )

  # Insert after YAML header or at the very beginning
  if (yaml_end_line > 0) {
    result <- c(
      content_lines[1:yaml_end_line],
      "",
      library_chunk,
      content_lines[(yaml_end_line + 1):length(content_lines)]
    )
  } else {
    result <- c(
      library_chunk,
      content_lines
    )
  }

  return(result)
}

# Insert a new page into the survey.qmd file
insert_page_into_survey <- function(page_id, editor_content) {
  if (is.null(editor_content)) {
    return(NULL)
  }

  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }

  # Find the last page marker (lines starting with ---)
  page_markers <- grep("^---\\s+[a-zA-Z0-9_]+", editor_content, perl = TRUE)

  if (length(page_markers) == 0) {
    # No pages yet, insert at the end
    last_page_end <- length(editor_content)
  } else {
    # Insert after the last page
    last_page_end <- length(editor_content)
  }

  page_template <- generate_page_template(page_id)
  result <- c(
    editor_content[1:last_page_end],
    page_template,
    if (last_page_end < length(editor_content)) {
      editor_content[(last_page_end + 1):length(editor_content)]
    } else {
      NULL
    }
  )

  return(paste(result, collapse = "\n"))
}

# Insert a new page below a specific existing page
insert_page_below_specific_page <- function(
  new_page_id,
  below_page_id,
  editor_content
) {
  if (
    is.null(editor_content) || is.null(new_page_id) || is.null(below_page_id)
  ) {
    return(NULL)
  }

  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }

  # Find the page with the new format: --- below_page_id
  page_start_pattern <- paste0("^---\\s+", below_page_id, "\\s*$")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)

  if (length(page_start_lines) == 0) {
    return(insert_page_into_survey(new_page_id, editor_content))
  }

  page_start_line <- page_start_lines[1]

  # Find where this page ends (start of next page or end of file)
  all_page_markers <- grep("^---\\s+[a-zA-Z0-9_]+", editor_content, perl = TRUE)
  next_page_markers <- all_page_markers[all_page_markers > page_start_line]

  if (length(next_page_markers) > 0) {
    # Insert before the next page
    page_end_line <- next_page_markers[1] - 1
  } else {
    # This is the last page, insert at the end
    page_end_line <- length(editor_content)
  }

  page_template <- generate_page_template(new_page_id)

  result <- c(
    editor_content[1:page_end_line],
    page_template,
    if (page_end_line < length(editor_content)) {
      editor_content[(page_end_line + 1):length(editor_content)]
    } else {
      NULL
    }
  )

  return(paste(result, collapse = "\n"))
}

# Generate a page template based on page ID
generate_page_template <- function(page_id) {
  if (page_id == "end") {
    return(c(
      paste0("--- ", page_id),
      "",
      "## Thanks for taking our survey!",
      "",
      "```{r}",
      "sd_close()",
      "```",
      ""
    ))
  } else {
    return(c(
      paste0("--- ", page_id),
      "",
      ""
    ))
  }
}

# Insert a question into a specific page
insert_question_into_survey <- function(
  page_id,
  question_type,
  question_id,
  question_label,
  editor_content,
  options_text = NULL,
  min_val = NULL,
  max_val = NULL,
  is_range = FALSE
) {
  if (is.null(editor_content) || is.null(page_id)) {
    return(NULL)
  }

  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }

  # Find the page with new format: --- page_id
  page_start_pattern <- paste0("^---\\s+", page_id, "\\s*$")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)

  if (length(page_start_lines) == 0) {
    return(NULL)
  }

  page_start_line <- page_start_lines[1]

  # Find where this page ends (start of next page or end of file)
  all_page_markers <- grep("^---\\s+[a-zA-Z0-9_]+", editor_content, perl = TRUE)
  next_page_markers <- all_page_markers[all_page_markers > page_start_line]

  if (length(next_page_markers) > 0) {
    page_end_line <- next_page_markers[1] - 1
  } else {
    page_end_line <- length(editor_content)
  }

  insertion_point <- find_insertion_point(
    editor_content,
    page_start_line,
    page_end_line
  )
  question_code <- generate_question_code(
    question_type,
    question_id,
    question_label,
    options_text,
    min_val,
    max_val,
    is_range
  )
  question_chunk <- c(
    "```{r}",
    question_code,
    "```"
  )

  result <- c(
    editor_content[1:(insertion_point - 1)],
    question_chunk,
    editor_content[insertion_point:length(editor_content)]
  )

  return(paste(result, collapse = "\n"))
}

# Insert text into a specific page
insert_text_into_survey <- function(page_id, text_content, editor_content) {
  if (is.null(editor_content) || is.null(page_id)) {
    return(NULL)
  }

  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }

  # Find the page with new format: --- page_id
  page_start_pattern <- paste0("^---\\s+", page_id, "\\s*$")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)

  if (length(page_start_lines) == 0) {
    return(NULL)
  }

  page_start_line <- page_start_lines[1]

  # Find where this page ends (start of next page or end of file)
  all_page_markers <- grep("^---\\s+[a-zA-Z0-9_]+", editor_content, perl = TRUE)
  next_page_markers <- all_page_markers[all_page_markers > page_start_line]

  if (length(next_page_markers) > 0) {
    page_end_line <- next_page_markers[1] - 1
  } else {
    page_end_line <- length(editor_content)
  }

  insertion_point <- find_insertion_point(
    editor_content,
    page_start_line,
    page_end_line
  )
  formatted_text <- strsplit(text_content, "\n")[[1]]
  result <- c(
    editor_content[1:(insertion_point - 1)],
    formatted_text,
    "",
    editor_content[insertion_point:length(editor_content)]
  )

  return(paste(result, collapse = "\n"))
}

# Find the best insertion point for a new question
find_insertion_point <- function(
  editor_content,
  page_start_line,
  page_end_line
) {
  for (i in page_start_line:page_end_line) {
    if (grepl("^```\\{r\\}", editor_content[i])) {
      chunk_start <- i
      chunk_end <- NULL

      for (j in (i + 1):page_end_line) {
        if (grepl("^```$", editor_content[j])) {
          chunk_end <- j
          break
        }
      }

      if (!is.null(chunk_end)) {
        chunk_content <- editor_content[(chunk_start + 1):(chunk_end - 1)]
        # Updated pattern to include sd_nav(), sd_close() and sd_prev() as navigation functions
        if (
          any(grepl(
            "sd_nav\\(|sd_next\\(|sd_prev\\(|sd_close\\(",
            chunk_content,
            perl = TRUE
          ))
        ) {
          return(chunk_start)
        }
      }
    }
  }

  return(page_end_line)
}

# Reorder pages in the survey file
reorder_pages <- function(new_order, editor_content) {
  if (is.null(editor_content) || length(new_order) == 0) {
    return(NULL)
  }

  # Ensure new_order is a character vector
  if (is.list(new_order)) {
    new_order <- unlist(new_order)
  }
  new_order <- as.character(new_order)

  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }

  # Find all page markers
  all_page_markers <- grep("^---\\s+[a-zA-Z0-9_]+", editor_content, perl = TRUE)

  page_blocks <- list()

  for (page_id in new_order) {
    # Find this specific page
    page_start_pattern <- paste0("^---\\s+", page_id, "\\s*$")
    page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)

    if (length(page_start_lines) == 0) {
      next
    }

    page_start_line <- page_start_lines[1]

    # Find where this page ends (start of next page or end of file)
    next_page_markers <- all_page_markers[all_page_markers > page_start_line]

    if (length(next_page_markers) > 0) {
      page_end_line <- next_page_markers[1] - 1
    } else {
      page_end_line <- length(editor_content)
    }

    page_blocks[[page_id]] <- list(
      start = page_start_line,
      end = page_end_line,
      content = editor_content[page_start_line:page_end_line]
    )
  }

  if (length(page_blocks) != length(new_order)) {
    return(NULL)
  }

  first_page_start <- min(sapply(page_blocks, function(block) block$start))
  last_page_end <- max(sapply(page_blocks, function(block) block$end))

  # Build page content with blank lines between pages
  page_content_list <- list()
  for (i in seq_along(new_order)) {
    page_id <- new_order[i]
    page_content_list[[length(page_content_list) + 1]] <- page_blocks[[
      page_id
    ]]$content

    # Add blank line after each page except the last one
    if (i < length(new_order)) {
      page_content_list[[length(page_content_list) + 1]] <- ""
    }
  }

  result <- c(
    editor_content[1:(first_page_start - 1)],
    unlist(page_content_list),
    if (last_page_end < length(editor_content)) {
      editor_content[(last_page_end + 1):length(editor_content)]
    } else {
      NULL
    }
  )

  return(paste(result, collapse = "\n"))
}

# Reorder content within a page
reorder_page_content <- function(page_id, new_content_order, editor_content) {
  if (
    is.null(editor_content) ||
      is.null(page_id) ||
      length(new_content_order) == 0
  ) {
    return(NULL)
  }

  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }

  # Find the page with new format: --- page_id
  page_start_pattern <- paste0("^---\\s+", page_id, "\\s*$")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)

  if (length(page_start_lines) == 0) {
    return(NULL)
  }

  page_start_line <- page_start_lines[1]

  # Find where this page ends (start of next page or end of file)
  all_page_markers <- grep("^---\\s+[a-zA-Z0-9_]+", editor_content, perl = TRUE)
  next_page_markers <- all_page_markers[all_page_markers > page_start_line]

  if (length(next_page_markers) > 0) {
    page_end_line <- next_page_markers[1] - 1
  } else {
    page_end_line <- length(editor_content)
  }

  original_page_content <- editor_content[page_start_line:page_end_line]
  navigation_chunks <- extract_navigation_chunks(original_page_content)
  survey_structure <- parse_survey_structure()

  if (
    is.null(survey_structure) ||
      !("pages" %in% names(survey_structure)) ||
      !(page_id %in% names(survey_structure$pages))
  ) {
    return(NULL)
  }

  page_items <- survey_structure$pages[[page_id]]
  new_page_content <- c(
    editor_content[page_start_line],
    ""
  )

  for (item in new_content_order) {
    if (
      !is.list(item) ||
        !all(c("type", "id") %in% names(item)) ||
        !(item$id %in% names(page_items))
    ) {
      next
    }

    current_item <- page_items[[item$id]]

    if (!is.list(current_item) || !("is_question" %in% names(current_item))) {
      next
    }

    if (current_item$is_question) {
      if ("raw" %in% names(current_item)) {
        r_chunk_lines <- strsplit(current_item$raw, "\n")[[1]]
        new_page_content <- c(new_page_content, r_chunk_lines, "")
      }
    } else {
      if ("content" %in% names(current_item)) {
        text_lines <- strsplit(current_item$content, "\n")[[1]]
        new_page_content <- c(new_page_content, text_lines, "")
      }
    }
  }

  if (length(navigation_chunks) > 0) {
    for (chunk in navigation_chunks) {
      new_page_content <- c(new_page_content, chunk$lines, "")
    }
  }

  # No closing delimiter needed with new format

  result <- c(
    editor_content[1:(page_start_line - 1)],
    new_page_content,
    if (page_end_line < length(editor_content)) {
      editor_content[(page_end_line + 1):length(editor_content)]
    } else {
      NULL
    }
  )

  return(paste(result, collapse = "\n"))
}

# Extract navigation chunks from page content
extract_navigation_chunks <- function(page_content) {
  navigation_chunks <- list()
  in_chunk <- FALSE
  chunk_start <- NULL

  for (i in seq_along(page_content)) {
    line <- page_content[i]

    if (grepl("^```\\{r\\}", line)) {
      in_chunk <- TRUE
      chunk_start <- i
    } else if (in_chunk && grepl("^```$", line)) {
      in_chunk <- FALSE
      chunk_content <- page_content[(chunk_start + 1):(i - 1)]

      if (any(grepl("sd_nav\\(|sd_next\\(|sd_prev\\(|sd_close\\(", chunk_content))) {
        navigation_chunks[[length(navigation_chunks) + 1]] <- list(
          start = chunk_start,
          end = i,
          lines = page_content[chunk_start:i]
        )
      }
    }
  }

  return(navigation_chunks)
}

# Separate multiple sd_* function calls into individual R chunks AND clean up content
r_chunk_separation <- function(editor_content) {
  if (is.null(editor_content)) {
    return(NULL)
  }

  if (length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }

  result <- character(0)
  i <- 1

  while (i <= length(editor_content)) {
    if (grepl("^```\\{r\\}", editor_content[i])) {
      chunk_start <- i
      chunk_end <- NULL

      for (j in (i + 1):length(editor_content)) {
        if (grepl("^```$", editor_content[j])) {
          chunk_end <- j
          break
        }
      }

      if (!is.null(chunk_end)) {
        chunk_content <- editor_content[(chunk_start + 1):(chunk_end - 1)]

        # Check for top-level sd_ function calls (not nested)
        sd_start_indices <- find_top_level_sd_calls(chunk_content)

        # If multiple top-level sd_ calls, split the chunk
        if (length(sd_start_indices) > 1) {
          for (start_idx in sd_start_indices) {
            call_end <- find_function_call_end(chunk_content, start_idx)

            # Add this call as a separate chunk
            result <- c(
              result,
              "```{r}",
              chunk_content[start_idx:call_end],
              "```",
              ""
            )
          }

          # Skip past the original chunk
          i <- chunk_end + 1
        } else {
          # No multiple sd_ calls, keep the chunk as is
          result <- c(
            result,
            editor_content[chunk_start:chunk_end]
          )
          i <- chunk_end + 1
        }
      } else {
        # If no end of chunk found, add this line and continue
        result <- c(result, editor_content[i])
        i <- i + 1
      }
    } else {
      # Not an R chunk start, add this line and continue
      result <- c(result, editor_content[i])
      i <- i + 1
    }
  }

  # Remove trailing empty line if present
  if (length(result) > 0 && result[length(result)] == "") {
    result <- result[-length(result)]
  }

  # Convert to string for processing
  content_string <- paste(result, collapse = "\n")

  # Apply library reorganization
  content_string <- reorganize_libraries(content_string)

  # Apply empty line cleanup
  content_string <- clean_multiple_empty_lines(content_string)

  return(content_string)
}

# Find top-level sd_ function calls (not nested inside other sd_ calls)
find_top_level_sd_calls <- function(chunk_content) {
  # Find all sd_ function call positions
  all_sd_indices <- grep("\\bsd_[a-zA-Z0-9_]+\\s*\\(", chunk_content)

  if (length(all_sd_indices) <= 1) {
    return(all_sd_indices)
  }

  top_level_indices <- c()

  for (i in seq_along(all_sd_indices)) {
    current_idx <- all_sd_indices[i]
    is_top_level <- TRUE

    # Check if this sd_ call is nested inside any previous sd_ call
    for (j in seq_along(all_sd_indices)) {
      if (j >= i) {
        break
      } # Only check previous calls

      prev_idx <- all_sd_indices[j]
      prev_end <- find_function_call_end(chunk_content, prev_idx)

      # If current call is within the range of a previous call, it's nested
      if (current_idx > prev_idx && current_idx <= prev_end) {
        is_top_level <- FALSE
        break
      }
    }

    if (is_top_level) {
      top_level_indices <- c(top_level_indices, current_idx)
    }
  }

  return(top_level_indices)
}

# Find the end of a function call by tracking parentheses
find_function_call_end <- function(chunk_content, start_idx) {
  call_end <- start_idx
  paren_count <- 0

  # Count initial opening parentheses in the first line
  opening <- gregexpr("\\(", chunk_content[start_idx])[[1]]
  if (opening[1] > 0) {
    paren_count <- length(opening)
  }

  # Count initial closing parentheses in the first line
  closing <- gregexpr("\\)", chunk_content[start_idx])[[1]]
  if (closing[1] > 0) {
    paren_count <- paren_count - length(closing)
  }

  # If parentheses not balanced in first line, find the end
  if (paren_count > 0) {
    for (k in (start_idx + 1):length(chunk_content)) {
      # Update parenthesis count
      opening <- gregexpr("\\(", chunk_content[k])[[1]]
      if (opening[1] > 0) {
        paren_count <- paren_count + length(opening)
      }

      closing <- gregexpr("\\)", chunk_content[k])[[1]]
      if (closing[1] > 0) {
        paren_count <- paren_count - length(closing)
      }

      # If balanced, we found the end
      if (paren_count <= 0) {
        call_end <- k
        break
      }
    }
  }

  return(call_end)
}

# Generate question code based on type
generate_question_code <- function(
  type,
  id,
  label,
  options_text = NULL,
  min_val = NULL,
  max_val = NULL,
  is_range = FALSE
) {
  # Ensure we have valid inputs
  if (is.null(id) || id == "") {
    id <- paste0(type, "_id")
  }
  if (is.null(label) || label == "") {
    label <- paste0(type, "_label")
  }

  # Generate appropriate code based on question type
  if (
    type %in%
      c(
        "mc",
        "mc_buttons",
        "mc_multiple",
        "mc_multiple_buttons",
        "select",
        "slider"
      )
  ) {
    # Parse options input
    options_vector <- parse_options_input(options_text)

    # Generate option lines
    option_lines <- character(0)
    for (i in seq_along(options_vector)) {
      option_label <- names(options_vector)[i]
      option_value <- options_vector[i]

      if (i == 1) {
        option_lines <- c(
          option_lines,
          paste0("    \"", option_label, "\" = \"", option_value, "\"")
        )
      } else if (i == length(options_vector)) {
        option_lines <- c(
          option_lines,
          paste0("    \"", option_label, "\" = \"", option_value, "\"")
        )
      } else {
        option_lines <- c(
          option_lines,
          paste0("    \"", option_label, "\" = \"", option_value, "\",")
        )
      }
    }

    # Add comma to all but last option
    if (length(option_lines) > 1) {
      for (i in 1:(length(option_lines) - 1)) {
        if (!grepl(",$", option_lines[i])) {
          option_lines[i] <- paste0(option_lines[i], ",")
        }
      }
    }

    return(c(
      paste0("sd_question("),
      paste0("  type   = \"", type, "\","),
      paste0("  id     = \"", id, "\","),
      paste0("  label  = \"", label, "\","),
      paste0("  option = c("),
      option_lines,
      paste0("  )"),
      paste0(")")
    ))
  } else if (type == "slider_numeric") {
    # Set default values if not provided
    if (is.null(min_val)) {
      min_val <- 0
    }
    if (is.null(max_val)) {
      max_val <- 10
    }

    # Generate the option sequence
    option_seq <- paste0("seq(", min_val, ", ", max_val, ", 1)")

    result <- c(
      paste0("sd_question("),
      paste0("  type   = \"", type, "\","),
      paste0("  id     = \"", id, "\","),
      paste0("  label  = \"", label, "\",")
    )

    # Add option parameter with comma if range mode is enabled
    if (is_range) {
      result <- c(result, paste0("  option = ", option_seq, ","))

      # Calculate 25% and 75% quantiles rounded to integers
      range_vals <- seq(min_val, max_val, 1)
      q25 <- round(quantile(range_vals, 0.25))
      q75 <- round(quantile(range_vals, 0.75))

      result <- c(result, paste0("  default = c(", q25, ", ", q75, ")"))
    } else {
      result <- c(result, paste0("  option = ", option_seq))
    }

    result <- c(result, paste0(")"))
    return(result)
  } else {
    # Simple questions (text, textarea, numeric, date, daterange)
    return(c(
      paste0("sd_question("),
      paste0("  type  = \"", type, "\","),
      paste0("  id    = \"", id, "\","),
      paste0("  label = \"", label, "\""),
      paste0(")")
    ))
  }
}

# Content Editing Helper Functions ----

# Clean up multiple consecutive empty lines - allow up to two empty lines
clean_multiple_empty_lines <- function(content) {
  if (is.null(content)) {
    return(NULL)
  }

  if (is.character(content) && length(content) == 1) {
    content <- strsplit(content, "\n")[[1]]
  }

  result <- character(0)
  empty_line_count <- 0

  for (line in content) {
    if (trimws(line) == "") {
      empty_line_count <- empty_line_count + 1
      # Allow up to two empty lines, remove anything beyond that
      if (empty_line_count <= 2) {
        result <- c(result, line)
      }
    } else {
      # Non-empty line, reset counter
      empty_line_count <- 0
      result <- c(result, line)
    }
  }

  return(paste(result, collapse = "\n"))
}

# Extract options from raw question code
extract_options_from_raw <- function(raw_code) {
  if (is.null(raw_code) || raw_code == "") {
    return("")
  }

  # Look for option = c(...) pattern
  option_pattern <- "option\\s*=\\s*c\\s*\\(([^)]+)\\)"
  option_match <- regexpr(option_pattern, raw_code, perl = TRUE)

  if (option_match > 0) {
    option_content <- regmatches(raw_code, option_match)
    # Extract just the content inside c(...)
    inner_pattern <- "c\\s*\\(([^)]+)\\)"
    inner_match <- regexpr(inner_pattern, option_content, perl = TRUE)

    if (inner_match > 0) {
      start_pos <- attr(inner_match, "capture.start")[1]
      length_val <- attr(inner_match, "capture.length")[1]
      options_text <- substr(
        option_content,
        start_pos,
        start_pos + length_val - 1
      )

      # Parse the options to extract just the labels
      # Remove quotes and extract labels (before =)
      options_pairs <- strsplit(options_text, ",")[[1]]
      labels <- character(0)

      for (pair in options_pairs) {
        pair <- trimws(pair)
        if (grepl("=", pair)) {
          label_part <- strsplit(pair, "=")[[1]][1]
          label_part <- trimws(label_part)
          label_part <- gsub("^[\"']|[\"']$", "", label_part) # Remove quotes
          labels <- c(labels, label_part)
        }
      }

      return(paste(labels, collapse = ", "))
    }
  }

  return("")
}

# Convert option label to snake case value
label_to_snake_case <- function(label) {
  # Convert to lowercase, replace spaces and special chars with underscores
  snake_case <- tolower(label)
  snake_case <- gsub("[^a-z0-9]+", "_", snake_case)
  # Remove leading/trailing underscores
  snake_case <- gsub("^_+|_+$", "", snake_case)
  # Replace multiple underscores with single
  snake_case <- gsub("_+", "_", snake_case)
  return(snake_case)
}

# Parse options input string into named vector
parse_options_input <- function(options_text) {
  if (is.null(options_text) || trimws(options_text) == "") {
    return(c("Option A" = "option_a", "Option B" = "option_b"))
  }

  # First, replace line breaks with commas to normalize separators
  # Handle different line break types (Windows: \r\n, Unix: \n, Mac: \r)
  options_text <- gsub("\r\n", "\n", options_text) # Normalize Windows line breaks
  options_text <- gsub("\r", "\n", options_text) # Normalize Mac line breaks

  # Replace line breaks with commas, but be careful not to double up commas
  options_text <- gsub("\n+", ",", options_text) # Replace one or more line breaks with comma

  # Split by comma, handling various spacing
  options_raw <- strsplit(options_text, ",")[[1]]
  options_raw <- trimws(options_raw)
  options_raw <- options_raw[options_raw != ""] # Remove empty entries

  if (length(options_raw) == 0) {
    return(c("Option A" = "option_a", "Option B" = "option_b"))
  }

  # Create named vector
  option_values <- sapply(options_raw, label_to_snake_case, USE.NAMES = FALSE)
  options_vector <- setNames(option_values, options_raw)

  return(options_vector)
}

# Parse survey structure from survey.qmd file
parse_survey_structure <- function() {
  # Read the survey content
  if (
    exists("input", envir = parent.frame()) &&
      !is.null(get("input", envir = parent.frame())$survey_editor)
  ) {
    survey_content <- get("input", envir = parent.frame())$survey_editor
    survey_content <- paste(survey_content, collapse = "\n")
  } else if (file.exists("survey.qmd")) {
    survey_content <- readLines("survey.qmd", warn = FALSE)
    survey_content <- paste(survey_content, collapse = "\n")
  } else {
    return(list(error = "survey.qmd file not found!"))
  }

  # Convert to lines for easier processing
  survey_lines <- strsplit(survey_content, "\n")[[1]]

  # Extract pages using new format: --- page_id
  page_pattern <- "^---\\s+([a-zA-Z0-9_]+)\\s*$"

  # Find all page markers
  page_line_numbers <- grep(page_pattern, survey_lines, perl = TRUE)

  if (length(page_line_numbers) == 0) {
    return(list(error = "No pages found in survey.qmd!"))
  }

  # Extract page IDs from the matched lines
  page_ids <- character(length(page_line_numbers))
  for (i in seq_along(page_line_numbers)) {
    line <- survey_lines[page_line_numbers[i]]
    # Extract the page ID (everything after "--- ")
    page_ids[i] <- trimws(sub("^---\\s+", "", line))
  }

  # Process each page
  pages <- list()
  for (i in seq_along(page_ids)) {
    page_id <- page_ids[i]
    page_start <- page_line_numbers[i] + 1  # Start after the --- page_id line

    # Find where this page ends
    if (i < length(page_line_numbers)) {
      page_end <- page_line_numbers[i + 1] - 1
    } else {
      page_end <- length(survey_lines)
    }

    # Extract page content
    if (page_end >= page_start) {
      page_content <- paste(survey_lines[page_start:page_end], collapse = "\n")
      pages[[page_id]] <- extract_page_items(page_content, page_id)
    } else {
      pages[[page_id]] <- list()
    }
  }

  return(list(pages = pages, page_ids = page_ids))
}

# Clean consecutive empty lines in a text string or vector
clean_consecutive_empty_lines <- function(content) {
  if (is.character(content) && length(content) == 1) {
    content <- strsplit(content, "\n")[[1]]
  }

  result <- character(0)
  prev_empty <- FALSE

  for (line in content) {
    current_empty <- trimws(line) == ""

    # Only add empty line if previous wasn't empty
    if (!(prev_empty && current_empty)) {
      result <- c(result, line)
    }

    prev_empty <- current_empty
  }

  return(paste(result, collapse = "\n"))
}

# Handle moving content between pages
handle_cross_page_content_move <- function(
  from_page_id,
  to_page_id,
  target_content_list,
  editor_content
) {
  if (is.null(editor_content) || is.null(from_page_id) || is.null(to_page_id)) {
    return(NULL)
  }

  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }

  survey_structure <- parse_survey_structure()
  if (is.null(survey_structure) || !("pages" %in% names(survey_structure))) {
    return(NULL)
  }

  # Get content items that need to be moved
  content_to_move <- list()
  for (item in target_content_list) {
    if (item$id %in% names(survey_structure$pages[[from_page_id]])) {
      content_to_move[[item$id]] <- survey_structure$pages[[from_page_id]][[
        item$id
      ]]
    }
  }

  if (length(content_to_move) == 0) {
    return(NULL)
  }

  # Remove content from source page
  temp_content <- editor_content
  for (content_id in names(content_to_move)) {
    content_item <- content_to_move[[content_id]]
    content_type <- if (content_item$is_question) "question" else "text"
    temp_content <- delete_content_from_survey_lines(
      from_page_id,
      content_id,
      content_type,
      temp_content
    )
    if (is.null(temp_content)) {
      return(NULL)
    }
  }

  # Add content to target page in the specified order
  result_content <- add_content_to_target_page(
    to_page_id,
    content_to_move,
    target_content_list,
    temp_content
  )

  return(paste(result_content, collapse = "\n"))
}

# Delete content from survey (working with lines array)
delete_content_from_survey_lines <- function(
  page_id,
  content_id,
  content_type,
  editor_lines
) {
  # Similar to delete_content_from_survey but works with lines array
  # Implementation needed based on your existing delete_content_from_survey function
  editor_content_str <- paste(editor_lines, collapse = "\n")
  result_str <- delete_content_from_survey(
    page_id,
    content_id,
    content_type,
    editor_content_str
  )
  if (is.null(result_str)) {
    return(NULL)
  }
  return(strsplit(result_str, "\n")[[1]])
}

# Add content to target page
add_content_to_target_page <- function(
  page_id,
  content_items,
  target_order,
  editor_lines
) {
  # Find target page boundaries with new format: --- page_id
  page_start_pattern <- paste0("^---\\s+", page_id, "\\s*$")
  page_start_lines <- grep(page_start_pattern, editor_lines, perl = TRUE)

  if (length(page_start_lines) == 0) {
    return(NULL)
  }

  page_start_line <- page_start_lines[1]

  # Find where this page ends (start of next page or end of file)
  all_page_markers <- grep("^---\\s+[a-zA-Z0-9_]+", editor_lines, perl = TRUE)
  next_page_markers <- all_page_markers[all_page_markers > page_start_line]

  if (length(next_page_markers) > 0) {
    page_end_line <- next_page_markers[1] - 1
  } else {
    page_end_line <- length(editor_lines)
  }

  # Get current page structure to identify existing content and navigation
  original_page_content <- editor_lines[page_start_line:page_end_line]
  navigation_chunks <- extract_navigation_chunks(original_page_content)

  # Get existing content from target page (excluding moved items)
  survey_structure <- parse_survey_structure()
  if (is.null(survey_structure) || !("pages" %in% names(survey_structure))) {
    return(NULL)
  }

  existing_page_items <- survey_structure$pages[[page_id]]
  moved_item_ids <- names(content_items)

  # Filter out moved items from existing content
  remaining_existing_items <- existing_page_items[
    !names(existing_page_items) %in% moved_item_ids
  ]

  # Rebuild page content in the exact order specified by target_order
  new_page_content <- c(
    editor_lines[page_start_line], # Page opening
    ""
  )

  # Process each item in the target order
  for (item in target_order) {
    item_id <- item$id

    # Check if it's a moved item
    if (item_id %in% names(content_items)) {
      content_item <- content_items[[item_id]]
      if (content_item$is_question && "raw" %in% names(content_item)) {
        content_lines <- strsplit(content_item$raw, "\n")[[1]]
        new_page_content <- c(new_page_content, content_lines, "")
      } else if (
        !content_item$is_question && "content" %in% names(content_item)
      ) {
        text_lines <- strsplit(content_item$content, "\n")[[1]]
        new_page_content <- c(new_page_content, text_lines, "")
      }
    } else if (item_id %in% names(remaining_existing_items)) {
      # Check if it's an existing item that stayed in the page
      existing_item <- remaining_existing_items[[item_id]]
      if (existing_item$is_question && "raw" %in% names(existing_item)) {
        content_lines <- strsplit(existing_item$raw, "\n")[[1]]
        new_page_content <- c(new_page_content, content_lines, "")
      } else if (
        !existing_item$is_question && "content" %in% names(existing_item)
      ) {
        text_lines <- strsplit(existing_item$content, "\n")[[1]]
        new_page_content <- c(new_page_content, text_lines, "")
      }
    }
  }

  # Add navigation chunks at the end
  if (length(navigation_chunks) > 0) {
    for (chunk in navigation_chunks) {
      new_page_content <- c(new_page_content, chunk$lines, "")
    }
  }

  # No closing delimiter needed with new format

  # Rebuild the entire file
  result <- c(
    editor_lines[1:(page_start_line - 1)],
    new_page_content,
    if (page_end_line < length(editor_lines)) {
      editor_lines[(page_end_line + 1):length(editor_lines)]
    } else {
      NULL
    }
  )

  return(result)
}

# Function to render the survey structure UI
render_survey_structure <- function(survey_structure, page_states = NULL) {
  shiny::div(
    id = "pages-container",
    # Render all pages
    lapply(survey_structure$page_ids, function(page_id) {
      page_items <- survey_structure$pages[[page_id]]

      # Sort items by position
      if (length(page_items) > 0) {
        positions <- sapply(page_items, function(item) item$position)
        sorted_items <- page_items[order(positions)]
      } else {
        sorted_items <- list()
      }

      # Check if this page should be expanded or collapsed
      is_expanded <- TRUE # default
      if (!is.null(page_states) && page_id %in% names(page_states)) {
        is_expanded <- page_states[[page_id]]
      }

      shiny::div(
        class = "page-wrapper",
        `data-page-id` = page_id,

        # Page header with toggle and drag handle
        shiny::div(
          class = "page-header",
          shiny::div(
            class = "page-drag-handle drag-handle",
            shiny::icon("grip-vertical")
          ),
          shiny::div(
            paste0("Page: ", page_id),
            style = "margin: 0; font-weight: bold;"
          ),

          shiny::div(
            class = "page-actions",
            style = "display: flex; gap: 5px; align-items: center;",
            # Add Text button
            shiny::actionButton(
              inputId = "add_text_btn_ui",
              label = "+T",
              icon = NULL,
              class = "btn-sm btn-outline-success add-text-btn",
              style = "font-weight: bold; font-family: Arial, sans-serif; width: 32px; height: 32px; padding: 0; display: flex; align-items: center; justify-content: center;",
              title = "Add text to this page",
              `data-page-id` = page_id
            ),
            # Add Question button
            shiny::actionButton(
              inputId = "add_question_btn_ui",
              label = "+Q",
              icon = NULL,
              class = "btn-sm btn-outline-info add-question-btn",
              style = "font-weight: bold; font-family: Arial, sans-serif; width: 32px; height: 32px; padding: 0; display: flex; align-items: center; justify-content: center;",
              title = "Add question to this page",
              `data-page-id` = page_id
            ),
            # Separator
            shiny::div(
              "|",
              style = "color: #666; margin: 0 3px; font-weight: normal;"
            ),
            # Modify page button
            shiny::actionButton(
              inputId = "modify_page_btn_ui",
              label = NULL,
              icon = shiny::icon("edit"),
              class = "btn-sm btn-outline-primary modify-page-btn",
              title = "Modify page ID",
              `data-page-id` = page_id
            ),
            # Delete page button
            shiny::actionButton(
              inputId = "delete_page_btn_ui",
              label = NULL,
              icon = shiny::icon("trash-alt"),
              class = "btn-sm btn-outline-danger delete-page-btn",
              title = "Delete page",
              `data-page-id` = page_id
            )
          ),

          shiny::div(
            class = "toggle-icon",
            # Update icon based on state
            shiny::icon(if (is_expanded) "chevron-down" else "chevron-right")
          )
        ),

        # Content container - set display based on state
        shiny::div(
          class = "questions-container",
          id = paste0("page-", page_id, "-content"),
          `data-page-id` = page_id,
          style = if (is_expanded) "display: block;" else "display: none;",

          # Add content items in their order
          if (length(sorted_items) > 0) {
            lapply(names(sorted_items), function(item_id) {
              render_content_item(sorted_items[[item_id]])
            })
          }
        )
      )
    }),

    # Add A New Page button - follows the page pattern
    shiny::div(
      class = "page-wrapper",
      style = "margin-top: 10px;",
      shiny::actionButton(
        "add_page_btn",
        "Add A New Page",
        class = "btn-success",
        style = "width: 100%; padding: 10px; font-weight: bold; background-color: #ffe0b2 !important; border-color: #ffcc80 !important; color: #f57c00 !important;"
      )
    )
  )
}

# Render a single content item (question or text)
render_content_item <- function(item) {
  if (item$is_question) {
    # Question item
    shiny::div(
      class = "question-item",
      `data-question-id` = item$id,
      `data-content-type` = "question",
      `data-position` = item$position,

      # Question drag handle
      shiny::div(
        class = "question-drag-handle drag-handle",
        shiny::div(
          "Q",
          style = "width: 24px; height: 24px; background-color: #a6d4f0; color: #1e7e93; border-radius: 2px; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 14px; font-family: Arial, sans-serif;"
        )
      ),

      # Content wrapper - keeps the vertical layout
      shiny::div(
        class = "content-wrapper",
        style = "flex-grow: 1;",

        # Question content - ID and Type on first line, Label on second line
        shiny::div(
          shiny::HTML(paste0("<strong>", item$id, "</strong> | ", item$type))
        ),
        shiny::div(
          shiny::HTML(paste0("<em>", item$label, "</em>"))
        )
      ),

      # Content delete button
      shiny::div(
        class = "content-actions",
        style = "margin-left: auto; display: flex; gap: 5px;",
        # Modify question button
        shiny::actionButton(
          inputId = "modify_content_btn_ui",
          label = NULL,
          icon = shiny::icon("edit"),
          class = "btn-sm btn-outline-success modify-content-btn",
          title = "Modify question",
          `data-content-id` = item$id,
          `data-page-id` = item$page_id,
          `data-content-type` = "question"
        ),
        # Delete question button
        shiny::actionButton(
          inputId = "delete_content_btn_ui",
          label = NULL,
          icon = shiny::icon("trash-alt"),
          class = "btn-sm btn-outline-danger delete-content-btn",
          title = "Delete question",
          `data-content-id` = item$id,
          `data-page-id` = item$page_id
        )
      )
    )
  } else {
    # Text item
    shiny::div(
      class = "text-item",
      `data-text-id` = item$id,
      `data-content-type` = "text",
      `data-position` = item$position,

      # Text drag handle
      shiny::div(
        class = "text-drag-handle drag-handle",
        shiny::div(
          "T",
          style = "width: 24px; height: 24px; background-color: #94d3a2; color: #1a5928; border-radius: 2px; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 14px; font-family: Arial, sans-serif;"
        )
      ),

      # Text content wrapper - keeps vertical layout if needed
      shiny::div(
        class = "content-wrapper",
        style = "flex-grow: 1;",

        # Text content preview
        shiny::div(
          shiny::HTML(item$preview)
        )
      ),

      # Content delete button
      shiny::div(
        class = "content-actions",
        style = "margin-left: auto; display: flex; gap: 5px;",
        # Modify text button
        shiny::actionButton(
          inputId = "modify_content_btn_ui",
          label = NULL,
          icon = shiny::icon("edit"),
          class = "btn-sm btn-outline-success modify-content-btn",
          title = "Modify text",
          `data-content-id` = item$id,
          `data-page-id` = item$page_id,
          `data-content-type` = "text"
        ),
        # Delete text button
        shiny::actionButton(
          inputId = "delete_content_btn_ui",
          label = NULL,
          icon = shiny::icon("trash-alt"),
          class = "btn-sm btn-outline-danger delete-content-btn",
          title = "Delete text",
          onclick = paste0(
            "Shiny.setInputValue('delete_content_btn', { pageId: '",
            item$page_id,
            "', contentId: '",
            item$id,
            "', contentType: 'text' });"
          ),
          `data-content-id` = item$id,
          `data-page-id` = item$page_id
        )
      )
    )
  }
}

# Extract items (questions and text) from page content
extract_page_items <- function(page_content, page_id) {
  # Find all R code blocks
  r_block_pattern <- "```\\{r\\}([\\s\\S]*?)```"
  r_blocks <- gregexpr(r_block_pattern, page_content, perl = TRUE)

  content_items <- list()
  item_index <- 1

  # If no R blocks, add entire content as text
  if (r_blocks[[1]][1] == -1) {
    if (nchar(trimws(page_content)) > 0) {
      content_items[[paste0("text_", page_id, "_1")]] <- create_text_item(
        paste0("text_", page_id, "_1"),
        trimws(page_content),
        1,
        page_id
      )
    }
    return(content_items)
  }

  # Get positions and lengths of R blocks
  r_positions <- r_blocks[[1]]
  r_lengths <- attr(r_blocks[[1]], "match.length")
  r_matches <- regmatches(page_content, r_blocks)[[1]]

  # Create a list to track all content segments
  segments <- list()

  # Add text before first R block if exists
  if (r_positions[1] > 1) {
    text_content <- substr(page_content, 1, r_positions[1] - 1)
    if (nchar(trimws(text_content)) > 0) {
      segments[[length(segments) + 1]] <- list(
        type = "text",
        start = 1,
        end = r_positions[1] - 1,
        content = trimws(text_content)
      )
    }
  }

  # Process each R block and text between blocks
  for (j in seq_along(r_positions)) {
    # Add the R block
    block_start <- r_positions[j]
    block_end <- block_start + r_lengths[j] - 1
    r_block <- r_matches[j]

    # Extract code content
    code_content <- gsub("```\\{r\\}", "", r_block)
    code_content <- gsub("```$", "", code_content)
    code_content <- trimws(code_content)

    # Check if it's a question
    is_question <- grepl("sd_question\\s*\\(", code_content, perl = TRUE)

    segments[[length(segments) + 1]] <- list(
      type = if (is_question) "question" else "r_block",
      start = block_start,
      end = block_end,
      content = r_block,
      code = code_content
    )

    # Add text after this block if not the last block
    if (j < length(r_positions)) {
      next_start <- r_positions[j + 1]
      text_start <- block_end + 1
      text_end <- next_start - 1

      if (text_end >= text_start) {
        text_content <- substr(page_content, text_start, text_end)
        if (nchar(trimws(text_content)) > 0) {
          segments[[length(segments) + 1]] <- list(
            type = "text",
            start = text_start,
            end = text_end,
            content = trimws(text_content)
          )
        }
      }
    } else if (block_end < nchar(page_content)) {
      # Add text after last block
      text_content <- substr(page_content, block_end + 1, nchar(page_content))
      if (nchar(trimws(text_content)) > 0) {
        segments[[length(segments) + 1]] <- list(
          type = "text",
          start = block_end + 1,
          end = nchar(page_content),
          content = trimws(text_content)
        )
      }
    }
  }

  # Process segments into content items
  text_counter <- 1
  for (segment in segments) {
    if (segment$type == "text") {
      text_id <- paste0("text_", page_id, "_", text_counter)
      text_counter <- text_counter + 1

      content_items[[text_id]] <- create_text_item(
        text_id,
        segment$content,
        item_index,
        page_id
      )
      item_index <- item_index + 1
    } else if (segment$type == "question") {
      # Extract question parameters
      question_params <- extract_question_params(segment$code)

      if (question_params$id != "unknown") {
        content_items[[question_params$id]] <- create_question_item(
          question_params$type,
          question_params$id,
          question_params$label,
          segment$content,
          item_index,
          page_id
        )
        item_index <- item_index + 1
      }
    }
  }

  return(content_items)
}

# Create a question item
create_question_item <- function(
  type,
  id,
  label,
  raw_content,
  position,
  page_id
) {
  list(
    type = type,
    id = id,
    label = label,
    raw = raw_content,
    position = position,
    is_question = TRUE,
    page_id = page_id
  )
}

# Create a text item
create_text_item <- function(id, content, position, page_id) {
  # Generate preview (first 5 words)
  words <- strsplit(content, "\\s+")[[1]]
  preview <- paste(head(words, 5), collapse = " ")
  if (length(words) > 5) {
    preview <- paste0(preview, "...")
  }

  list(
    id = id,
    preview = preview,
    content = content,
    position = position,
    is_question = FALSE,
    page_id = page_id
  )
}

# Modify page ID in the survey.qmd file
modify_page_id <- function(old_page_id, new_page_id, editor_content) {
  if (is.null(editor_content) || is.null(old_page_id) || is.null(new_page_id)) {
    return(NULL)
  }

  # Ensure editor_content is in lines
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }

  # Find and replace the page ID using new format: --- old_page_id
  page_pattern <- paste0("^---\\s+", old_page_id, "\\s*$")

  replaced <- FALSE
  for (i in seq_along(editor_content)) {
    if (grepl(page_pattern, editor_content[i], perl = TRUE)) {
      editor_content[i] <- paste0("--- ", new_page_id)
      replaced <- TRUE
      break  # Only replace the first occurrence
    }
  }

  if (!replaced) {
    return(NULL)
  }

  return(paste(editor_content, collapse = "\n"))
}

# Modify question content in the survey
modify_question_content <- function(
  page_id,
  old_question_id,
  new_type,
  new_id,
  new_label,
  editor_content,
  options_text = NULL,
  min_val = NULL,
  max_val = NULL,
  is_range = FALSE
) {
  if (is.null(editor_content) || is.null(page_id) || is.null(old_question_id)) {
    return(NULL)
  }

  # Ensure editor_content is in lines
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }

  # Find the page with new format: --- page_id
  page_start_pattern <- paste0("^---\\s+", page_id, "\\s*$")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)

  if (length(page_start_lines) == 0) {
    return(NULL)
  }

  page_start_line <- page_start_lines[1]

  # Find where this page ends (start of next page or end of file)
  all_page_markers <- grep("^---\\s+[a-zA-Z0-9_]+", editor_content, perl = TRUE)
  next_page_markers <- all_page_markers[all_page_markers > page_start_line]

  if (length(next_page_markers) > 0) {
    page_end_line <- next_page_markers[1] - 1
  } else {
    page_end_line <- length(editor_content)
  }

  # Find the R chunk containing the old question
  for (i in page_start_line:page_end_line) {
    if (grepl("^```\\{r\\}", editor_content[i])) {
      chunk_start <- i
      chunk_end <- NULL

      for (j in (i + 1):page_end_line) {
        if (grepl("^```$", editor_content[j])) {
          chunk_end <- j
          break
        }
      }

      if (!is.null(chunk_end)) {
        chunk_content <- paste(
          editor_content[(chunk_start + 1):(chunk_end - 1)],
          collapse = "\n"
        )

        # Check if this chunk contains the question we want to modify
        if (
          grepl(
            paste0('id\\s*=\\s*["\']', old_question_id, '["\']'),
            chunk_content,
            perl = TRUE
          )
        ) {
          # Generate new question code
          new_question_code <- generate_question_code(
            new_type,
            new_id,
            new_label,
            options_text,
            min_val,
            max_val,
            is_range
          )

          # Replace the chunk content
          result <- c(
            editor_content[1:(chunk_start)],
            new_question_code,
            editor_content[chunk_end:length(editor_content)]
          )

          return(paste(result, collapse = "\n"))
        }
      }
    }
  }

  return(NULL)
}

# Modify text content in the survey
modify_text_content <- function(
  page_id,
  old_text_id,
  new_text,
  editor_content
) {
  if (is.null(editor_content) || is.null(page_id) || is.null(old_text_id)) {
    return(NULL)
  }

  # Get the current survey structure
  survey_structure <- parse_survey_structure()

  if (
    is.null(survey_structure) ||
      !("pages" %in% names(survey_structure)) ||
      !(page_id %in% names(survey_structure$pages)) ||
      !(old_text_id %in% names(survey_structure$pages[[page_id]]))
  ) {
    return(NULL)
  }

  # Get the old content
  old_text_item <- survey_structure$pages[[page_id]][[old_text_id]]

  if (!("content" %in% names(old_text_item))) {
    return(NULL)
  }

  old_text <- old_text_item$content

  # Ensure editor_content is in lines
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }

  # Find and replace the old text with new text
  editor_content_str <- paste(editor_content, collapse = "\n")

  # Escape special characters for regex
  escaped_old_text <- gsub(
    "([\\$\\^\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|])",
    "\\\\\\1",
    old_text
  )

  # Replace the text
  updated_content_str <- gsub(
    escaped_old_text,
    new_text,
    editor_content_str,
    fixed = TRUE
  )

  # Check if replacement was successful
  if (identical(editor_content_str, updated_content_str)) {
    return(NULL)
  }

  return(updated_content_str)
}

# Delete a page from the survey.qmd file
delete_page_from_survey <- function(page_id, editor_content) {
  if (is.null(editor_content) || is.null(page_id)) {
    return(NULL)
  }

  # Ensure editor_content is in lines
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }

  # Find the page with new format: --- page_id
  page_start_pattern <- paste0("^---\\s+", page_id, "\\s*$")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)

  if (length(page_start_lines) == 0) {
    return(NULL)
  }

  # Use the first match
  page_start_line <- page_start_lines[1]

  # Find where this page ends (start of next page or end of file)
  all_page_markers <- grep("^---\\s+[a-zA-Z0-9_]+", editor_content, perl = TRUE)
  next_page_markers <- all_page_markers[all_page_markers > page_start_line]

  if (length(next_page_markers) > 0) {
    page_end_line <- next_page_markers[1] - 1
  } else {
    page_end_line <- length(editor_content)
  }

  # Remove the page (including the --- page_id line)
  result <- c(
    editor_content[1:(page_start_line - 1)],
    if (page_end_line < length(editor_content)) {
      editor_content[(page_end_line + 1):length(editor_content)]
    } else {
      NULL
    }
  )

  # Return the updated content
  return(paste(result, collapse = "\n"))
}

# Delete content from a page in the survey
delete_content_from_survey <- function(
  page_id,
  content_id,
  content_type,
  editor_content
) {
  if (is.null(editor_content) || is.null(page_id) || is.null(content_id)) {
    return(NULL)
  }

  # Ensure editor_content is in lines
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }

  # Find the page with new format: --- page_id
  page_start_pattern <- paste0("^---\\s+", page_id, "\\s*$")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)

  if (length(page_start_lines) == 0) {
    return(NULL)
  }

  # Use the first match
  page_start_line <- page_start_lines[1]

  # Find where this page ends (start of next page or end of file)
  all_page_markers <- grep("^---\\s+[a-zA-Z0-9_]+", editor_content, perl = TRUE)
  next_page_markers <- all_page_markers[all_page_markers > page_start_line]

  if (length(next_page_markers) > 0) {
    page_end_line <- next_page_markers[1] - 1
  } else {
    page_end_line <- length(editor_content)
  }

  # Get the page content
  page_content <- editor_content[page_start_line:page_end_line]

  # Parse the structure to find the content
  survey_structure <- parse_survey_structure()

  if (
    is.null(survey_structure) ||
      !("pages" %in% names(survey_structure)) ||
      !(page_id %in% names(survey_structure$pages)) ||
      !(content_id %in% names(survey_structure$pages[[page_id]]))
  ) {
    return(NULL)
  }

  # Get the content item
  content_item <- survey_structure$pages[[page_id]][[content_id]]

  # Depending on content type, find and remove it
  if (content_type == "question" && content_item$is_question) {
    # Remove the R chunk containing the question
    r_block_pattern <- "```\\{r\\}([\\s\\S]*?)```"
    r_blocks <- gregexpr(
      r_block_pattern,
      paste(page_content, collapse = "\n"),
      perl = TRUE
    )
    r_matches <- regmatches(paste(page_content, collapse = "\n"), r_blocks)[[1]]

    for (r_block in r_matches) {
      # Look for the question ID in the block
      if (
        grepl(
          paste0('id\\s*=\\s*["\']', content_id, '["\']'),
          r_block,
          perl = TRUE
        )
      ) {
        # Remove this block from page content
        updated_page_content <- gsub(
          gsub(
            "([\\$\\^\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|])",
            "\\\\\\1",
            r_block
          ),
          "",
          paste(page_content, collapse = "\n"),
          fixed = TRUE
        )

        # Rebuild the document
        result <- c(
          editor_content[1:(page_start_line - 1)],
          strsplit(updated_page_content, "\n")[[1]],
          if (page_end_line < length(editor_content)) {
            editor_content[(page_end_line + 1):length(editor_content)]
          } else {
            NULL
          }
        )

        # Clean up any consecutive blank lines
        result <- clean_consecutive_blank_lines(result)

        return(paste(result, collapse = "\n"))
      }
    }
  } else if (content_type == "text" && !content_item$is_question) {
    # For text items, we need to find the exact text and remove it
    if ("content" %in% names(content_item)) {
      text_content <- content_item$content

      # Escape special characters for regex
      escaped_text <- gsub(
        "([\\$\\^\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|])",
        "\\\\\\1",
        text_content
      )

      # Find and remove the text from page content
      updated_page_content <- gsub(
        escaped_text,
        "",
        paste(page_content, collapse = "\n"),
        fixed = TRUE
      )

      # Rebuild the document
      result <- c(
        editor_content[1:(page_start_line - 1)],
        strsplit(updated_page_content, "\n")[[1]],
        if (page_end_line < length(editor_content)) {
          editor_content[(page_end_line + 1):length(editor_content)]
        } else {
          NULL
        }
      )

      # Clean up any consecutive blank lines
      result <- clean_consecutive_blank_lines(result)

      return(paste(result, collapse = "\n"))
    }
  }

  # If we get here, deletion failed
  return(NULL)
}

# Helper function to clean up consecutive blank lines
clean_consecutive_blank_lines <- function(lines) {
  # Remove consecutive blank lines
  result <- character(0)
  prev_blank <- FALSE

  for (line in lines) {
    current_blank <- trimws(line) == ""

    if (!(prev_blank && current_blank)) {
      result <- c(result, line)
    }

    prev_blank <- current_blank
  }

  return(result)
}

# Extract question parameters
extract_question_params <- function(code_text) {
  extract_param <- function(param_name, text) {
    # Try single quotes
    pattern <- paste0(param_name, "\\s*=\\s*'([^']*)'")
    match <- regexpr(pattern, text, perl = TRUE)

    if (
      match > 0 &&
        !is.null(attr(match, "capture.start")) &&
        attr(match, "capture.start")[1] > 0
    ) {
      start_pos <- attr(match, "capture.start")[1]
      length_val <- attr(match, "capture.length")[1]
      return(substr(text, start_pos, start_pos + length_val - 1))
    }

    # Try double quotes
    pattern <- paste0(param_name, '\\s*=\\s*"([^"]*)"')
    match <- regexpr(pattern, text, perl = TRUE)

    if (
      match > 0 &&
        !is.null(attr(match, "capture.start")) &&
        attr(match, "capture.start")[1] > 0
    ) {
      start_pos <- attr(match, "capture.start")[1]
      length_val <- attr(match, "capture.length")[1]
      return(substr(text, start_pos, start_pos + length_val - 1))
    }

    return("unknown")
  }

  return(list(
    type = extract_param("type", code_text),
    id = extract_param("id", code_text),
    label = extract_param("label", code_text)
  ))
}

# Process content order array
process_content_order <- function(flat_order) {
  content_list <- list()

  if (is.vector(flat_order) || is.list(flat_order)) {
    # Ensure we have a character vector
    if (!is.character(flat_order)) {
      flat_order <- as.character(unlist(flat_order))
    }

    # Process pairs of type and id
    if (length(flat_order) >= 2) {
      for (i in seq(1, length(flat_order), by = 2)) {
        if (i + 1 <= length(flat_order)) {
          content_list[[length(content_list) + 1]] <- list(
            type = flat_order[i],
            id = flat_order[i + 1]
          )
        }
      }
    } else {
      shiny::showNotification(
        "Order data too short. Needed pairs of type/id values.",
        type = "error"
      )
      return(list())
    }
  } else {
    shiny::showNotification("Invalid content order format type", type = "error")
    return(list())
  }

  return(content_list)
}

# Check and separate content if necessary
check_and_separate_content <- function(
  page_id,
  content_list,
  current_content,
  session
) {
  page_structure <- parse_survey_structure()

  if (
    !is.null(page_structure) &&
      !is.null(page_structure$pages) &&
      page_id %in% names(page_structure$pages)
  ) {
    # Check if any dragged item has multiple sd_* calls
    needs_separation <- FALSE
    for (item in content_list) {
      if (
        item$type == "question" &&
          item$id %in% names(page_structure$pages[[page_id]])
      ) {
        question_item <- page_structure$pages[[page_id]][[item$id]]
        if ("raw" %in% names(question_item)) {
          # Count sd_ function calls in this chunk
          raw_content <- question_item$raw
          sd_calls <- gregexpr("\\bsd_[a-zA-Z0-9_]+\\s*\\(", raw_content)
          if (sd_calls[[1]][1] > 0 && length(sd_calls[[1]]) > 1) {
            needs_separation <- TRUE
            break
          }
        }
      }
    }

    # If we need to separate content
    if (needs_separation) {
      current_content <- r_chunk_separation(current_content)
      shinyAce::updateAceEditor(
        session,
        "survey_editor",
        value = current_content
      )
      shiny::showNotification(
        "Separated multiple functions in dragged content",
        type = "message",
        duration = 2
      )
    }
  }
}

# Launch preview server
launch_preview_server <- function(port) {
  # Create a temporary R script to run the app
  temp_script <- tempfile(fileext = ".R")
  writeLines(
    paste0(
      "library(shiny)\n",
      "port <- ",
      port,
      "\n",
      "setwd('",
      getwd(),
      "')\n",
      "source('app.R')\n",
      "options(shiny.port = port)\n",
      "options(shiny.host = '127.0.0.1')\n",
      "shiny::runApp(launch.browser = FALSE)\n"
    ),
    temp_script
  )

  # Run the temp script in a separate R process
  r_path <- file.path(R.home("bin"), "R")
  system2(
    r_path,
    c("--vanilla", "-f", temp_script),
    wait = FALSE,
    stdout = NULL,
    stderr = NULL
  )
}
