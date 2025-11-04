# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**sdstudio** is an R package providing a Shiny GUI companion for the [surveydown](https://surveydown.org/) survey platform. It transforms code-heavy survey creation into a visual drag-and-drop interface with three main tabs: Build (visual survey creation), Preview (live testing), and Responses (data analytics).

## Development Commands

### Package Development Workflow
```r
# Standard R package development (use build.R script)
devtools::load_all()      # Load package for development/testing
devtools::document()      # Generate roxygen2 documentation
devtools::install()       # Install package locally
pkgdown::build_site()     # Build documentation website
devtools::check()         # Check package compliance
```

### Launch Application
```r
# Launch the studio application
sdstudio::launch()

# With specific database encryption mode
sdstudio::launch(gssencmode = "prefer")  # or "disable"
```

### Testing
```r
# Run tests (minimal test structure currently exists)
devtools::test()
```

## Architecture Overview

### Core Structure
- **Main Entry**: `R/studio.R` - Contains `launch()` function (only exported function)
- **UI Definition**: `R/ui.R` - Shiny UI with floating button system and three-tab layout
- **Server Logic**: `R/server.R` - Reactive programming with real-time synchronization
- **Utilities**: `R/util.R` - Content processing and helper functions

### Key Features Implementation
- **Template System**: 15 survey templates integrated into Build tab
- **Dual-Pane Editor**: Visual builder + ACE code editor with real-time sync
- **Database Integration**: PostgreSQL with GSSAPI encryption and connection pooling
- **Responsive Design**: Bootstrap 5 with mobile preview modes

### Asset Files
- **CSS**: `inst/css/sdstudio.css` - Custom styling for drag-and-drop and templates
- **JavaScript**: `inst/js/sdstudio.js` - Floating button interactions
- **Citation**: `inst/CITATION` - Package citation information

### Configuration
- **Environment Variables**: Uses `.env` files for database credentials
- **Database Modes**: Auto/prefer/disable GSSAPI encryption modes
- **Connection Pooling**: Handles multiple database connections efficiently

## Important Development Notes

### Package Structure
- Single exported function: `launch()` (defined in NAMESPACE)
- Uses roxygen2 for documentation generation
- pkgdown website deployment to https://sdstudio.surveydown.org
- MIT licensed with standard R package conventions

### Database Integration Patterns
- PostgreSQL connectivity with enterprise security
- Environment variable configuration support
- Connection status indicators and error handling
- CSV export functionality for responses

### UI/UX Architecture  
- Three-tab system (Build/Preview/Responses)
- Floating button context-aware visibility
- Real-time preview with desktop/mobile modes
- Undo/redo functionality in visual editor

### Dependencies to Note
- **Core**: shiny, bslib, htmltools, surveydown
- **Database**: DBI, RPostgres, pool
- **Editor**: shinyAce for code editing integration
- **Data**: DT for interactive tables
- **Environment**: dotenv for configuration