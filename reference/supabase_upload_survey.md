# Upload a survey folder to Supabase Storage

Uploads all files from a local survey folder to Supabase Storage

## Usage

``` r
supabase_upload_survey(
  survey_path,
  survey_name = NULL,
  conn = NULL,
  overwrite = TRUE
)
```

## Arguments

- survey_path:

  Local path to survey folder

- survey_name:

  Name to use for the survey in Supabase (default: folder name)

- conn:

  Supabase connection object (optional)

- overwrite:

  Logical, overwrite existing files (default: TRUE)

## Value

Logical indicating success
