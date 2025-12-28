# Supabase Storage Integration for sdstudio
#
# This module provides functions to interact with Supabase Storage for
# storing and retrieving surveydown survey files in online deployment mode.

# Check if Supabase is configured -----------------------------------------

#' Check if Supabase credentials are configured
#'
#' Checks for required environment variables: SUPABASE_URL and SUPABASE_SERVICE_KEY
#'
#' @return Logical indicating if Supabase is configured
#' @keywords internal
supabase_configured <- function() {
  url <- Sys.getenv("SUPABASE_URL", "")
  service_key <- Sys.getenv("SUPABASE_SERVICE_KEY", "")
  bucket <- Sys.getenv("SUPABASE_BUCKET", "studio")

  # Only require URL and service key
  !is.null(url) && url != "" &&
  !is.null(service_key) && service_key != "" &&
  !is.null(bucket) && bucket != ""
}

# Initialize Supabase connection ------------------------------------------

#' Initialize Supabase Storage connection
#'
#' Creates a connection object with Supabase credentials from environment.
#' Only requires SUPABASE_URL and SUPABASE_SERVICE_KEY.
#'
#' @return List with connection details or NULL if not configured
#' @keywords internal
supabase_connect <- function() {
  if (!supabase_configured()) {
    warning("Supabase credentials not found. Please set SUPABASE_URL, SUPABASE_SERVICE_KEY, and SUPABASE_BUCKET in your .env file.")
    return(NULL)
  }

  connection <- list(
    url = Sys.getenv("SUPABASE_URL"),
    service_key = Sys.getenv("SUPABASE_SERVICE_KEY"),
    bucket = Sys.getenv("SUPABASE_BUCKET", "studio"),
    base_path = Sys.getenv("SUPABASE_BASE_PATH", "surveys")
  )

  class(connection) <- c("supabase_connection", "list")
  return(connection)
}

# Helper: Build storage API URL -------------------------------------------

#' Build Supabase Storage API URL
#'
#' @param conn Supabase connection object
#' @param path Path within the bucket
#' @return Full URL for storage API
#' @keywords internal
build_storage_url <- function(conn, path = "") {
  base_url <- paste0(conn$url, "/storage/v1/object/", conn$bucket)
  if (path != "") {
    return(paste0(base_url, "/", path))
  }
  return(base_url)
}

#' Build Supabase Storage List API URL
#'
#' @param conn Supabase connection object
#' @param path Path within the bucket
#' @return Full URL for list API
#' @keywords internal
build_list_url <- function(conn, path = "") {
  paste0(conn$url, "/storage/v1/object/list/", conn$bucket)
}

# List surveys in Supabase Storage ----------------------------------------

#' List available surveys in Supabase Storage
#'
#' @param conn Supabase connection object from supabase_connect()
#' @return Character vector of survey names, or NULL on error
#' @keywords internal
supabase_list_surveys <- function(conn = NULL) {
  if (is.null(conn)) {
    conn <- supabase_connect()
  }

  if (is.null(conn)) {
    return(NULL)
  }

  tryCatch({
    # Build request to list objects
    url <- build_list_url(conn)

    # Make POST request to list files (using service key for full access)
    response <- httr2::request(url) |>
      httr2::req_headers(
        "Authorization" = paste("Bearer", conn$service_key),
        "apikey" = conn$service_key
      ) |>
      httr2::req_body_json(list(
        prefix = paste0(conn$base_path, "/"),
        limit = 1000,
        offset = 0
      )) |>
      httr2::req_perform()

    # Parse response
    content <- httr2::resp_body_json(response)

    if (length(content) == 0) {
      return(character(0))
    }

    # Extract survey folder names (first level directories)
    all_paths <- vapply(content, function(x) x$name, character(1))

    # Get unique survey names (folder names after base_path)
    survey_paths <- all_paths[grepl(paste0("^", conn$base_path, "/[^/]+/"), all_paths)]
    survey_names <- unique(
      gsub(paste0("^", conn$base_path, "/([^/]+)/.*"), "\\1", survey_paths)
    )

    return(survey_names)
  }, error = function(e) {
    warning("Failed to list surveys from Supabase: ", e$message)
    return(NULL)
  })
}

# Download survey from Supabase Storage -----------------------------------

#' Download a survey folder from Supabase Storage
#'
#' Downloads all files from a survey folder in Supabase Storage to a local directory
#'
#' @param survey_name Name of the survey folder in Supabase
#' @param dest_path Local destination path (default: temp directory)
#' @param conn Supabase connection object (optional)
#' @return Path to downloaded survey folder, or NULL on error
#' @keywords internal
supabase_download_survey <- function(survey_name, dest_path = NULL, conn = NULL) {
  if (is.null(conn)) {
    conn <- supabase_connect()
  }

  if (is.null(conn)) {
    return(NULL)
  }

  # Create destination directory
  if (is.null(dest_path)) {
    dest_path <- file.path(tempdir(), survey_name)
  }

  if (!dir.exists(dest_path)) {
    dir.create(dest_path, recursive = TRUE)
  }

  tryCatch({
    # List all files in the survey folder
    url <- build_list_url(conn)

    response <- httr2::request(url) |>
      httr2::req_headers(
        "Authorization" = paste("Bearer", conn$service_key),
        "apikey" = conn$service_key
      ) |>
      httr2::req_body_json(list(
        prefix = paste0(conn$base_path, "/", survey_name, "/"),
        limit = 1000,
        offset = 0
      )) |>
      httr2::req_perform()

    content <- httr2::resp_body_json(response)

    if (length(content) == 0) {
      warning("Survey '", survey_name, "' not found or is empty")
      return(NULL)
    }

    # Download each file
    for (item in content) {
      file_path <- item$name

      # Skip if it's a directory (no extension or ends with /)
      if (grepl("/$", file_path)) {
        next
      }

      # Build local file path
      relative_path <- gsub(paste0("^", conn$base_path, "/", survey_name, "/"), "", file_path)
      local_file <- file.path(dest_path, relative_path)

      # Create directory if needed
      local_dir <- dirname(local_file)
      if (!dir.exists(local_dir)) {
        dir.create(local_dir, recursive = TRUE)
      }

      # Download file
      file_url <- build_storage_url(conn, file_path)

      file_response <- httr2::request(file_url) |>
        httr2::req_headers(
          "Authorization" = paste("Bearer", conn$service_key),
          "apikey" = conn$service_key
        ) |>
        httr2::req_perform()

      # Write file
      writeBin(httr2::resp_body_raw(file_response), local_file)
    }

    message("Successfully downloaded survey '", survey_name, "' to ", dest_path)
    return(dest_path)

  }, error = function(e) {
    warning("Failed to download survey from Supabase: ", e$message)
    return(NULL)
  })
}

# Upload survey to Supabase Storage ---------------------------------------

#' Upload a survey folder to Supabase Storage
#'
#' Uploads all files from a local survey folder to Supabase Storage
#'
#' @param survey_path Local path to survey folder
#' @param survey_name Name to use for the survey in Supabase (default: folder name)
#' @param conn Supabase connection object (optional)
#' @param overwrite Logical, overwrite existing files (default: TRUE)
#' @return Logical indicating success
#' @keywords internal
supabase_upload_survey <- function(survey_path, survey_name = NULL, conn = NULL, overwrite = TRUE) {
  if (is.null(conn)) {
    conn <- supabase_connect()
  }

  if (is.null(conn)) {
    return(FALSE)
  }

  if (!dir.exists(survey_path)) {
    warning("Survey path does not exist: ", survey_path)
    return(FALSE)
  }

  # Default survey name to folder name
  if (is.null(survey_name)) {
    survey_name <- basename(survey_path)
  }

  tryCatch({
    # Get all files in the survey folder
    all_files <- list.files(survey_path, recursive = TRUE, full.names = TRUE)

    if (length(all_files) == 0) {
      warning("No files found in survey folder: ", survey_path)
      return(FALSE)
    }

    # Upload each file
    upload_success <- TRUE
    for (file_path in all_files) {
      # Skip directories
      if (dir.exists(file_path)) {
        next
      }

      # Build relative path
      relative_path <- gsub(paste0("^", survey_path, "/?"), "", file_path)

      # Skip temporary Quarto/knitr files
      if (grepl("\\.(knit|utf8)\\.md$|surveydown\\.lua$", relative_path)) {
        next
      }

      # Skip survey.html at root level (it gets moved to _survey/ by surveydown)
      # Only upload the final version in _survey/survey.html
      if (relative_path == "survey.html") {
        next
      }

      # Skip if file no longer exists (temporary files deleted during rendering)
      if (!file.exists(file_path)) {
        next
      }

      # Build storage path
      storage_path <- paste0(conn$base_path, "/", survey_name, "/", relative_path)

      # Upload file
      file_url <- build_storage_url(conn, storage_path)

      # Read file content (with additional check)
      file_info <- file.info(file_path)
      if (is.na(file_info$size)) {
        next  # File disappeared between checks
      }

      # Try to read file content (may fail if file deleted during rendering)
      file_content <- tryCatch({
        readBin(file_path, "raw", file_info$size)
      }, error = function(e) {
        # File was deleted between list and read - skip it
        return(NULL)
      })

      if (is.null(file_content)) {
        next
      }

      # Determine content type
      content_type <- guess_content_type(file_path)

      # Upload request
      result <- tryCatch({
        httr2::request(file_url) |>
          httr2::req_headers(
            "Authorization" = paste("Bearer", conn$service_key),
            "apikey" = conn$service_key,
            "Content-Type" = content_type
          ) |>
          httr2::req_body_raw(file_content) |>
          httr2::req_method("POST") |>
          httr2::req_perform()

        TRUE
      }, error = function(e) {
        # If POST fails, try PUT (for updates)
        if (overwrite) {
          tryCatch({
            httr2::request(file_url) |>
              httr2::req_headers(
                "Authorization" = paste("Bearer", conn$service_key),
                "apikey" = conn$service_key,
                "Content-Type" = content_type
              ) |>
              httr2::req_body_raw(file_content) |>
              httr2::req_method("PUT") |>
              httr2::req_perform()

            TRUE
          }, error = function(e2) {
            warning("Failed to upload file: ", relative_path, " - ", e2$message)
            FALSE
          })
        } else {
          warning("Failed to upload file: ", relative_path, " - ", e$message)
          FALSE
        }
      })

      if (!result) {
        upload_success <- FALSE
      }
    }

    if (upload_success) {
      message("Successfully uploaded survey '", survey_name, "' to Supabase")
    }

    return(upload_success)

  }, error = function(e) {
    warning("Failed to upload survey to Supabase: ", e$message)
    return(FALSE)
  })
}

# Delete survey from Supabase Storage -------------------------------------

#' Delete a survey folder from Supabase Storage
#'
#' @param survey_name Name of the survey to delete
#' @param conn Supabase connection object (optional)
#' @return Logical indicating success
#' @keywords internal
supabase_delete_survey <- function(survey_name, conn = NULL) {
  if (is.null(conn)) {
    conn <- supabase_connect()
  }

  if (is.null(conn)) {
    return(FALSE)
  }

  tryCatch({
    # List all files in the survey
    url <- build_list_url(conn)

    response <- httr2::request(url) |>
      httr2::req_headers(
        "Authorization" = paste("Bearer", conn$service_key),
        "apikey" = conn$service_key
      ) |>
      httr2::req_body_json(list(
        prefix = paste0(conn$base_path, "/", survey_name, "/"),
        limit = 1000,
        offset = 0
      )) |>
      httr2::req_perform()

    content <- httr2::resp_body_json(response)

    if (length(content) == 0) {
      warning("Survey '", survey_name, "' not found")
      return(FALSE)
    }

    # Delete each file
    delete_url <- paste0(conn$url, "/storage/v1/object/", conn$bucket)

    file_paths <- vapply(content, function(x) x$name, character(1))

    httr2::request(delete_url) |>
      httr2::req_headers(
        "Authorization" = paste("Bearer", conn$service_key),
        "apikey" = conn$service_key
      ) |>
      httr2::req_body_json(list(
        prefixes = file_paths
      )) |>
      httr2::req_method("DELETE") |>
      httr2::req_perform()

    message("Successfully deleted survey '", survey_name, "' from Supabase")
    return(TRUE)

  }, error = function(e) {
    warning("Failed to delete survey from Supabase: ", e$message)
    return(FALSE)
  })
}

# Zip survey folder for download ------------------------------------------

#' Create a zip archive of a survey folder
#'
#' @param survey_path Path to the survey folder
#' @param output_path Path for the output zip file (optional)
#' @return Path to the created zip file, or NULL on error
#' @keywords internal
zip_survey_folder <- function(survey_path, output_path = NULL) {
  if (!dir.exists(survey_path)) {
    warning("Survey path does not exist: ", survey_path)
    return(NULL)
  }

  tryCatch({
    # Default output path
    if (is.null(output_path)) {
      survey_name <- basename(survey_path)
      output_path <- file.path(tempdir(), paste0(survey_name, "_", format(Sys.Date(), "%Y%m%d"), ".zip"))
    }

    # Get all files in the survey folder
    current_dir <- getwd()
    setwd(survey_path)

    files <- list.files(recursive = TRUE, all.files = TRUE, no.. = TRUE)

    # Create zip
    zip::zip(
      zipfile = output_path,
      files = files,
      mode = "cherry-pick"
    )

    setwd(current_dir)

    message("Successfully created zip archive: ", output_path)
    return(output_path)

  }, error = function(e) {
    warning("Failed to create zip archive: ", e$message)
    return(NULL)
  })
}

# Helper: Guess content type ----------------------------------------------

#' Guess MIME content type from file extension
#'
#' @param file_path Path to file
#' @return MIME type string
#' @keywords internal
guess_content_type <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))

  content_types <- list(
    "qmd" = "text/markdown",
    "md" = "text/markdown",
    "R" = "text/plain",
    "r" = "text/plain",
    "txt" = "text/plain",
    "csv" = "text/csv",
    "json" = "application/json",
    "yml" = "text/yaml",
    "yaml" = "text/yaml",
    "html" = "text/html",
    "css" = "text/css",
    "js" = "application/javascript",
    "png" = "image/png",
    "jpg" = "image/jpeg",
    "jpeg" = "image/jpeg",
    "gif" = "image/gif",
    "svg" = "image/svg+xml",
    "pdf" = "application/pdf",
    "zip" = "application/zip"
  )

  if (ext %in% names(content_types)) {
    return(content_types[[ext]])
  }

  return("application/octet-stream")
}

# Detect deployment environment -------------------------------------------

#' Detect if running in a deployed environment
#'
#' Checks various indicators to determine if the app is deployed online
#' vs running locally
#'
#' @return Logical indicating if deployed
#' @keywords internal
detect_deployment_environment <- function() {
  # Check for common deployment environment variables
  is_deployed <-
    Sys.getenv("SHINY_SERVER_VERSION", "") != "" ||  # Shiny Server
    Sys.getenv("SHINYAPPS_URL", "") != "" ||         # shinyapps.io
    Sys.getenv("CONNECT_SERVER", "") != "" ||        # Posit Connect
    Sys.getenv("R_CONFIG_ACTIVE", "") == "production"

  # Also check if current directory is writable
  if (!is_deployed) {
    test_file <- file.path(getwd(), ".write_test")
    can_write <- tryCatch({
      writeLines("test", test_file)
      file.remove(test_file)
      TRUE
    }, error = function(e) {
      FALSE
    })

    # If can't write to current directory, likely deployed
    is_deployed <- !can_write
  }

  return(is_deployed)
}
