test_that("label_to_snake_case converts labels correctly", {
  expect_equal(label_to_snake_case("Option A"), "option_a")
  expect_equal(label_to_snake_case("My Cool Option"), "my_cool_option")
  expect_equal(label_to_snake_case("Test-Option 123"), "test_option_123")
  expect_equal(label_to_snake_case("Multiple   Spaces"), "multiple_spaces")
  expect_equal(label_to_snake_case("__Leading__Trailing__"), "leading_trailing")
})

test_that("parse_options_input parses options correctly", {
  result <- parse_options_input("Option A, Option B, Option C")
  expect_type(result, "character")
  expect_equal(length(result), 3)
  expect_equal(names(result), c("Option A", "Option B", "Option C"))
  expect_equal(as.vector(result), c("option_a", "option_b", "option_c"))

  # Test with newlines
  result2 <- parse_options_input("Option A\nOption B\nOption C")
  expect_equal(length(result2), 3)

  # Test empty input
  result3 <- parse_options_input("")
  expect_equal(length(result3), 2) # Returns default options
})

test_that("clean_consecutive_empty_lines removes multiple empty lines", {
  content <- "Line 1\n\n\nLine 2\n\n\n\nLine 3"
  result <- clean_consecutive_empty_lines(content)
  expect_true(grepl("Line 1\n\nLine 2\n\nLine 3", result))
  expect_false(grepl("\n\n\n", result))
})

test_that("generate_page_template creates correct templates", {
  # Test regular page - should NOT contain navigation functions (new surveydown API)
  result <- generate_page_template("my_page")
  expect_true(any(grepl("--- my_page", result)))
  expect_false(any(grepl("sd_next\\(\\)", result)))
  expect_false(any(grepl("sd_nav\\(\\)", result)))

  # Test end page - should contain sd_close()
  result_end <- generate_page_template("end")
  expect_true(any(grepl("--- end", result_end)))
  expect_true(any(grepl("sd_close\\(\\)", result_end)))
  expect_true(any(grepl("Thanks for taking our survey", result_end)))
})

test_that("clean_multiple_empty_lines reduces consecutive blanks", {
  content <- c("line1", "", "", "", "line2", "", "line3")
  result <- clean_multiple_empty_lines(content)
  lines <- strsplit(result, "\n")[[1]]

  # Should not have more than 2 consecutive empty lines
  empty_count <- 0
  max_empty <- 0
  for (line in lines) {
    if (trimws(line) == "") {
      empty_count <- empty_count + 1
      max_empty <- max(max_empty, empty_count)
    } else {
      empty_count <- 0
    }
  }
  expect_lte(max_empty, 2)
})

test_that("parse_survey_structure handles basic survey", {
  # Create a temporary survey file
  temp_file <- tempfile(fileext = ".qmd")
  on.exit(unlink(temp_file))

  survey_content <- "---
title: Test Survey
---

```{r}
library(surveydown)
```

--- welcome

# Welcome

--- end

## Thanks!

```{r}
sd_close()
```
"
  writeLines(survey_content, temp_file)

  # Save current directory and change to temp directory
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(dirname(temp_file))
  file.copy(temp_file, "survey.qmd", overwrite = TRUE)
  on.exit(unlink("survey.qmd"), add = TRUE)

  result <- parse_survey_structure()

  expect_true("pages" %in% names(result))
  expect_true("page_ids" %in% names(result))
  expect_equal(result$page_ids, c("welcome", "end"))
})
