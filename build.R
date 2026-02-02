# Load package
devtools::load_all()

# Create the documentation for the package
devtools::document()

# Install the package
devtools::install(force = TRUE)

# Build the pkgdown site
pkgdown::build_site()

# Check package
devtools::check()

# Submit to CRAN
# devtools::submit_cran()
