
<!-- README.md is generated from README.Rmd. Please edit this file -->

## sdstudio

**sdstudio** is a companion package that support the
[surveydown](https://surveydown.org/) survey platform. It launches a
Shiny webapp and transforms the typical code-heavy survey creation
workflow into an accessible Graphical User Interface, making it easy to
create, preview, and manage surveys without extensive programming
knowledge.

## Installation

The **sdstudio** package is not yet on CRAN, but the development version
can be installed from GitHub:

``` r
# install.packages("pak")
pak::pak("surveydown-dev/sdstudio", ask = FALSE)
```

## Launch the Studio

The **surveydown Studio** is the main application in **sdstudio** - a
unified Shiny interface for complete survey lifecycle management. Launch
it with:

``` r
sdstudio::launch()
```

### Studio Features

#### 🏗️ **Build Tab** - Visual Survey Creation

- **Drag-and-drop interface** for intuitive survey construction
- **15 survey templates** covering basic to advanced use cases:
  - Basic templates (Default, Question Types, YAML-based)
  - Advanced features (Conditional logic, Random options, Reactive
    questions)
  - Specialized surveys (Conjoint experiments, Interactive maps, Live
    polling)
- **Dual-pane editor** with visual builder and code editor (ACE
  integration)
- **12 question types** including multiple choice, text, numeric,
  sliders, dates
- **Real-time synchronization** between visual interface and underlying
  code
- **Undo/redo functionality** for all editing operations
- **Local/Live mode switching** for development and production workflows

#### 👁️ **Preview Tab** - Live Survey Testing

- **Real-time preview** of your survey as you build
- **Responsive preview modes**: Desktop (widescreen) and Mobile (375px)
  views
- **Automatic refresh** when survey code changes
- **Interactive testing** environment before deployment

#### 📊 **Responses Tab** - Data Management & Analytics

- **PostgreSQL database integration** with connection pooling
- **GSSAPI encryption support** with automatic fallback options
- **Real-time analytics dashboard** showing:
  - Total responses and daily averages
  - Completion rate tracking
  - Cumulative and daily response trends (interactive charts)
- **Interactive data table** with search, filtering, and sorting
- **CSV export functionality** for data analysis
- **Visual connection status** indicators with detailed feedback

### Key Capabilities

**Template System**: Choose from 15 professionally designed templates
ranging from basic surveys to advanced conjoint experiments and
interactive visualizations.

**Visual Survey Builder**: Create surveys using an intuitive
drag-and-drop interface with real-time code synchronization, eliminating
the need for manual coding while maintaining full code access.

**Database Integration**: Seamless PostgreSQL connectivity with
enterprise-grade security (GSSAPI encryption) and automatic connection
management.

**Responsive Design**: Built with Bootstrap 5, providing a modern,
mobile-friendly interface that works across all devices.

**Development Workflow**: Switch between Local (development) and Live
(production) modes with one click, supporting iterative survey
development and testing.

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
