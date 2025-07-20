# Load UI components from external file
source("ui_elements.R")

ui <- page_navbar(

  # Load custom CSS styles
  includeCSS("www/styles.css"),

  # Logo and dashboard title in navbar (aligned left)
  nav_item(
    tags$div(
      style = "display: flex; align-items: center; margin-left: -15px",
      tags$img(
        src = "Logo_white.png",
        height = "70px",
        style = "margin-right: 5px;"
      ),
      tags$span(
        "Superstore Dashboard",
        style = "color: white; font-weight: bold; font-size: 1.1em;"
      )
    )
  ),

  # Push following items to the right
  nav_spacer(),

  # External links and vertical separators
  nav_item(
    tags$a("LinkedIn", href = "https://www.linkedin.com/in/benjamin-wortmann/", target = "_blank")
  ),
  nav_item(
    tags$div(style = "width: 1px; background-color: #ccc; height: 25px; margin: 12px 10px;")
  ),
  nav_item(
    tags$a("GitHub", href = "https://github.com/BenWort86", target = "_blank")
  ),
  nav_item(
    tags$div(style = "width: 1px; background-color: #ccc; height: 25px; margin: 12px 10px;")
  ),

  # Info button triggering modal dialog
  nav_item(
    actionLink("show_info", "Info", style = "color: white; cursor: pointer;")
  ),

  # Sidebar with dynamic year selector
  sidebar = sidebar(
    title = "Filter:",
    uiOutput("year_selector")
  ),

  # Theme customization (Cerulean + dark navbar styles)
  theme = bs_theme(
    bootswatch = "cerulean",
    version = 5,
    "navbar-bg" = "#001ba3",
    "navbar-dark-color" = "white",
    "navbar-dark-hover-color" = "#cccccc",
    "navbar-dark-active-color" = "white"
  ),

  # Main scrollable content area with responsive layout
  div(
    class = "main-scroll",

    layout_column_wrap(
      fill = TRUE,
      ui_elements[["map"]],
      ui_elements[["sales_by_region"]]
    ),

    layout_column_wrap(
      fill = TRUE,
      ui_elements[["sales_by_category"]]
    ),

    layout_column_wrap(
      fill = TRUE,
      ui_elements[["sales_by_sub_category"]]
    ),

    # Optional reset map button
    ui_elements[["reset_map"]]
  )
)
