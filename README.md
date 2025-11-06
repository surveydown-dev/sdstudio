
<!-- README.md is generated from README.Rmd. Please edit this file -->

## sdstudio

**sdstudio** is a companion package for building, previewing, and
managing surveys built using the [surveydown](https://surveydown.org/)
survey platform through a Graphical User Interface (GUI). The studio
runs as a local Shiny web app and makes it easier to create, preview,
and manage surveys without extensive programming knowledge.

## Installation

The **sdstudio** package is not yet on CRAN, but the development version
can be installed from GitHub:

``` r
# install.packages("pak")
pak::pak("surveydown-dev/sdstudio", ask = FALSE)
```

## Launch the Studio

The **surveydown studio** is the main application in **sdstudio**.
Launch it with:

``` r
sdstudio::launch()
```

## Features

### 🏗️ **Build Tab** - Visual Survey Creation

- **Drag-and-drop interface** for intuitive survey construction
- **14 survey templates** covering basic to advanced use cases
- **Dual-pane editor** with visual builder and code editor
- **Real-time synchronization** between visual interface and underlying
  code
- **Undo/redo functionality** for all editing operations
- **Local/DB mode switching** for local testing and database connection
  workflows

### 👁️ **Preview Tab** - Live Survey Testing

- **Real-time preview** of your survey as you build
- **Responsive preview modes**: Desktop (widescreen) and Mobile (375px)
  views

### 📊 **Responses Tab** - Data Management & Analytics

- **Real-time analytics dashboard** showing summaries of responses and
  local interactive data table
- **PostgreSQL database integration** with connection pooling
- **GSSAPI encryption support** with automatic fallback options
- **CSV export functionality** for data analysis

## Database Configuration

The studio supports flexible database connection modes:

``` r
# Auto mode (recommended) - tries GSSAPI first, falls back if needed
launch()

# Prefer mode - uses GSSAPI if available, plain connection if not  
launch(gssencmode = "prefer")

# Disable mode - forces plain connection (useful for VPN environments)
launch(gssencmode = "disable")
```

Configure your database connection using environment variables or the
built-in Settings interface within the studio.
