# ui.R

# Load UI components from external file
source("ui_elements.R")

ui <- page_sidebar(

  # Background color with transparency
  bg = "rgba(238, 238, 238, 0.6)",
  includeCSS("www/styles.css"), # Load custom CSS styles

  # Sidebar configuration
  sidebar = sidebar(
    width = 200, # Sidebar width in pixels
    tags$img(src = "Logo_blue.png", height = "200px", width = "auto"), # Company logo
    uiOutput("year_selector"), # Dynamic year selector input
    bg = "rgba(0, 0, 0, 0)" # Transparent sidebar background
  ),

  # Set theme using Bootstrap 5 and Cerulean Bootswatch
  theme = bs_theme(
    bootswatch = "cerulean",
    "navbar-bg" = "#f0f0f0",
    version = 5
  ),

  # Main content area with scrollable div
  div(
    style = "overflow-y: auto; max-height: 98vh; padding-right: 10px; position: absolute; top: 2; bottom: 2;",

    # First row: map and regional sales
    layout_column_wrap(
      fill = T,
      ui_elements[["map"]],
      ui_elements[["sales_by_region"]]
    ),

    # Second row: sales by category
    layout_column_wrap(
      fill = T,
      ui_elements[["sales_by_category"]]
    ),

    # Third row: sales by sub-category
    layout_column_wrap(
      fill = T,
      ui_elements[["sales_by_sub_category"]]
    ),

    # Optional reset button or map element (if defined in ui_elements)
    ui_elements[["reset_map"]]
  )
)
