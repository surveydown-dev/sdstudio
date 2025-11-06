# Launch surveydown Studio

This function launches a Shiny app with 3 tabs: Build, Preview, and
Responses. The Build tab includes a template selection interface for
creating new surveys.

## Usage

``` r
launch(gssencmode = "auto")
```

## Arguments

- gssencmode:

  Character string. The GSS encryption mode for the database connection.
  Defaults to `"auto"`. Options include:

  - `"auto"`: Tries `"prefer"` first, then falls back to `"disable"` if
    GSSAPI fails (recommended)

  - `"prefer"`: Uses GSSAPI encryption if available, plain connection if
    not

  - `"disable"`: Forces plain connection without GSSAPI encryption

## Value

No return value, called for its side effects of launching a Shiny app.

## Examples

``` r
if (interactive()) {
  # Launch studio (uses "auto" mode by default)
  launch()

  # Launch studio with disabled GSS encryption (for VPN connections)
  launch(gssencmode = "disable")

  # Launch studio with prefer mode (no fallback)
  launch(gssencmode = "prefer")
}
```
