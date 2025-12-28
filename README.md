
<!-- README.md is generated from README.Rmd. Please edit this file -->

## sdstudio

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/sdstudio)](https://cran.r-project.org/package=sdstudio)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/grand-total/sdstudio)](https://cran.r-project.org/package=sdstudio)
[![GitHub
stars](https://img.shields.io/github/stars/surveydown-dev/sdstudio?style=social)](https://github.com/surveydown-dev/sdstudio)
<!-- badges: end -->

`sdstudio` is a companion package for building, previewing, and managing
surveys built using the [surveydown](https://surveydown.org/) survey
platform through a Graphical User Interface (GUI). The studio runs as a
local Shiny web app and makes it easier to create, preview, and manage
surveys without extensive programming knowledge.

## Intro Video

<div align="center">

<a href="https://www.youtube.com/watch?v=p00ivfg1B1M">
<img src="https://img.youtube.com/vi/p00ivfg1B1M/maxresdefault.jpg" alt="Watch the sdstudio intro video" width="500"/>
</a>

</div>

## Installation

Installing `sdstudio`:

``` r
# Option 1: install from CRAN
install.packages("sdstudio")

# Option 2: install from GitHub
pak::pak("surveydown-dev/sdstudio", ask = FALSE)
```

Make sure to install `surveydown` as well:

``` r
# Option 1: install from CRAN
install.packages("surveydown")

# Option 2: install from GitHub
pak::pak("surveydown-dev/surveydown", ask = FALSE)
```

## Launch

Simply call the `launch()` function to start the studio:

``` r
sdstudio::launch()
```

## Features

### 🏗️ **Build Tab** - Visual Survey Creation

- **Drag-and-drop interface** for intuitive survey construction
- **16 survey templates** covering basic to advanced use cases
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

## Online Deployment 🌐

`sdstudio` now supports **online deployment** using Supabase Storage! Deploy your survey studio to the cloud and work from anywhere.

**Key Features:**
- ☁️ **Cloud storage** - Store surveys in Supabase Storage
- 🔄 **Auto-sync** - Changes automatically saved every 10 seconds
- ⬇️ **Download** - Export surveys as ZIP files
- ⬆️ **Upload** - Migrate local surveys to cloud
- 🔒 **Secure** - Private buckets with optional user authentication

**Quick Start:**
1. Create a free [Supabase](https://supabase.com) account
2. Set up a storage bucket (see [DEPLOYMENT.md](DEPLOYMENT.md))
3. Configure environment variables
4. Deploy to shinyapps.io, Posit Connect, or Shiny Server

For detailed instructions, see the [Online Deployment Guide](DEPLOYMENT.md).

## Templates

We currently have 16 templates to choose from. The ***default***
template contains a minimum basic structure for a clean start. You might
also try the ***question_type*** template for a showcase of all question
types, or try some advanced features with ***conditional_display***,
***conditional_navigation***, etc. For a full list of templates, go to
the [surveydown-dev GitHub site](https://github.com/surveydown-dev) and
search for any repo starting with “template\_”.

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
