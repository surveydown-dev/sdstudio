# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Project Overview

**sdstudio** is an R package providing a Shiny GUI companion for the
[surveydown](https://surveydown.org/) survey platform. It transforms
code-heavy survey creation into a visual drag-and-drop interface with
three main tabs: Build (visual survey creation), Preview (live testing),
and Responses (data analytics).

## Development Commands

### Quick Start

``` r
# Complete build workflow (recommended)
source("build.R")  # Runs: load_all() → document() → install() → build_site() → check()
```

### Individual Development Commands

``` r
devtools::load_all()      # Load package for development/testing
devtools::document()      # Generate roxygen2 documentation
devtools::install()       # Install package locally
pkgdown::build_site()     # Build documentation website (deploys to sdstudio.surveydown.org)
devtools::check()         # Check package compliance (R CMD check)
devtools::test()          # Run test suite (minimal tests currently exist)
```

### Installation Methods

``` r
# From GitHub (user installation)
pak::pak("surveydown-dev/sdstudio", ask = FALSE)

# Local development install
devtools::install(force = TRUE)
```

### Launch Application

``` r
# Launch the studio application
sdstudio::launch()

# With specific database encryption mode
sdstudio::launch(gssencmode = "prefer")  # or "disable"
```

## Architecture Overview

### Core Structure

- **Main Entry**: `R/studio.R` - Contains
  [`launch()`](https://sdstudio.surveydown.org/reference/launch.md)
  function (only exported function)
- **UI Definition**: `R/ui.R` - Shiny UI with floating button system and
  three-tab layout
- **Server Logic**: `R/server.R` - Reactive programming with real-time
  synchronization
- **Utilities**: `R/util.R` - Content processing and helper functions

### Key Features Implementation

- **Template System**: 15 survey templates integrated into Build tab
- **Dual-Pane Editor**: Visual builder + ACE code editor with real-time
  sync
- **Database Integration**: PostgreSQL with GSSAPI encryption and
  connection pooling
- **Responsive Design**: Bootstrap 5 with mobile preview modes

### Asset Files

- **CSS**: `inst/css/sdstudio.css` - Custom styling for drag-and-drop
  and templates
- **JavaScript**: `inst/js/sdstudio.js` - Floating button interactions
- **Citation**: `inst/CITATION` - Package citation information

### Configuration

- **Environment Variables**: Uses `.env` files for database credentials
- **Database Modes**: Auto/prefer/disable GSSAPI encryption modes
- **Connection Pooling**: Handles multiple database connections
  efficiently

## Important Development Notes

### Package Structure

- **Single exported function**:
  [`launch()`](https://sdstudio.surveydown.org/reference/launch.md)
  (only function in NAMESPACE)
- **Roxygen2 documentation**: All R files use roxygen2 for documentation
  generation
- **pkgdown deployment**: Website publishes to
  <https://sdstudio.surveydown.org>
- **Testing**: Minimal test structure exists in `tests/testthat/` - test
  coverage is limited
- **Licensing**: MIT license with standard R package conventions

### Reactive Architecture Patterns

- **Real-time sync**: Visual builder ↔︎ ACE code editor bidirectional
  updates
- **Mode switching**: Local/DB mode affects preview and response
  functionality
- **Template injection**: Survey templates populate both visual and code
  views simultaneously
- **Undo/redo stack**: Maintained in server logic for all editing
  operations

### Database Integration

- **GSSAPI encryption**: Three modes (auto/prefer/disable) for
  PostgreSQL connections
- **Connection pooling**: Uses `pool` package for efficient database
  connections
- **Environment config**: Reads `.env` files for database credentials
- **Fallback behavior**: `auto` mode tries GSSAPI first, falls back to
  plain connection
- **Export functionality**: CSV export from Responses tab

### UI Component Organization

- **Three-tab system**: Build (visual editor) / Preview (live app) /
  Responses (analytics)
- **Floating buttons**: Context-aware visibility based on current tab
  and mode
- **Responsive preview**: Desktop (widescreen) and Mobile (375px)
  viewport modes
- **Bootstrap 5**: Custom CSS in `inst/css/sdstudio.css` for
  drag-and-drop styling
- **JavaScript interactions**: `inst/js/sdstudio.js` handles floating
  button behavior

### Key Dependencies

- **Core**: shiny, bslib, htmltools, surveydown
- **Database**: DBI, RPostgres, pool
- **Editor**: shinyAce for code editing integration
- **Data**: DT for interactive data tables
- **Environment**: dotenv for `.env` file configuration
- **Utilities**: later for delayed execution
