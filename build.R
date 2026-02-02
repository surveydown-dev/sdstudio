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

# Load the package and view the summary
# library(sdstudio)
# help(package = 'sdstudio')

# Install from github
# pak::pak("surveydown-dev/sdstudio", ask = FALSE)
