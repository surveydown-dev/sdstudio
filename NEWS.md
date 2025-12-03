# sdstudio (development version)

# sdstudio 0.1.4

## Major Updates

- Updated to match the latest `surveydown` API (>= 1.0.1):
  - `sd_next()` is now deprecated; use `sd_nav()` for explicit navigation
  - Navigation functions are no longer required on intermediate pages
  - Only the final page needs `sd_close()` for proper survey termination
  - Page templates now generate clean pages without automatic navigation code

## Improvements

- Auto-refresh logic now preserves user-added spacing:
  - Up to 2 consecutive empty lines are preserved during content refresh
  - Prevents automatic removal of intentional line breaks
  - Improves visual formatting control in the survey editor

# sdstudio 0.1.3

- Page break compatibility with the new `surveydown` package (>= 0.14.0).

# sdstudio 0.1.2

- Bug fix: The database connection is now fully compatible with `gssencmode = "auto"` when `"disable"` mode is triggered. 

# sdstudio 0.1.1

- Removed the "Questions (YAML)" template since `.yml` files are not yet compatible.
- Improved the Modal UI of the `slider_numeric` question type.

# sdstudio 0.1.0

- Package renamed to **sdstudio** (used to be sdApps).
- Launch the studio with `sdstudio::launch()`.
- 3 tabs: Build, Preview, and Responses.
- Build tab: Survey creation with 15 pre-defined templates, or use your own.
- Preview tab: Live-preview the survey.
- Responses tab: Survey responses management.
- All tabs support local and DB modes. To set up DB mode, input your database credentials on the Responses tab.

# sdstudio 0.0.3

- `launch()` is now compatible with default fence format.

# sdstudio 0.0.2

- `launch()` now has only 2 sections in the Construction tab: Structure and Code. In Structure, the pages and contents are drag-able and each has an editing and deleting button. In Code, the `survey.qmd` and `app.R` files are all live and editable. The Preview tab has a live preview of the survey Shiny app, and has a "Refresh" button to manually refresh the preview. 

# sdstudio 0.0.1

- Initial version.
