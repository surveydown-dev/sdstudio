# Download a survey folder from Supabase Storage

Downloads all files from a survey folder in Supabase Storage to a local
directory

## Usage

``` r
supabase_download_survey(survey_name, dest_path = NULL, conn = NULL)
```

## Arguments

- survey_name:

  Name of the survey folder in Supabase

- dest_path:

  Local destination path (default: temp directory)

- conn:

  Supabase connection object (optional)

## Value

Path to downloaded survey folder, or NULL on error
