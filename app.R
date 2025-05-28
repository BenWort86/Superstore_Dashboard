library(shiny)
library(bslib)
library(readxl)
library(janitor)
library(plotly)
library(data.table)
library(shinyWidgets)
library(lubridate)
library(readr)

# Load UI interface from external file
source("ui.R")

# Server logic
server <- function(input, output, session) {
  # Load helper functions from external file
  source("helpers.R", local = T)

  data <- reactive({
    tryCatch(
      {
        sample_superstore <- data.table(
          clean_names(
            read_excel(
              "data/sample_-_superstore.xls",
              col_types = c(
                "text", "text", "date", "date", "text",
                "text", "text", "text", "text", "text",
                "text", "text", "text", "text", "text",
                "text", "text", "numeric", "numeric", "numeric", "numeric"
              )
            )
          )
        )

        us_geocode <- data.table(
          clean_names(
            read_csv(
              "data/US_GeoCode.csv",
              col_types = cols(
                state_code = col_character(),
                name       = col_character(),
                latitude   = col_double(),
                longitude  = col_double()
              )
            )
          )
        )

        sample_superstore <-
          sample_superstore[us_geocode, on = .(state = name), nomatch = 0]
      },
      error = function(e) {
        return(data.table())
      }
    )
  })

  # Reactive expression:
  # Filter data by selected year(s) and aggregate sales by state
  sales_by_state <- reactive({
    data_in <- data()

    # Return empty plot if no data is available
    if (nrow(data_in) == 0) {
      return(data.table())
    }

    # Ensure a year is selected
    req(selected_year())

    # Sum of sales by state + group index
    data_in[
      year(order_date) %in% selected_year()
    ][
      , .(state, sales, state_code, latitude, longitude)
    ][
      , .(sales = sum(sales)),
      by = .(state, state_code, latitude, longitude)
    ][
      , state_idx := .GRP,
      by = state
    ]
  }) |> bindCache(selected_year()) # Cache results based on selected year(s)

  # Reactive expression: Convert input to numeric year(s)
  selected_year <- reactive({
    as.numeric(input$selected_year)
  })

  # Reactive expression: Get previous year(s)
  previous_year <- reactive({
    selected_year() - 1
  })

  # Reactive value: Store currently selected state
  current_state <- reactiveVal()

  # Reset selected state when reset button is clicked
  observeEvent(input$reset, current_state(NULL))

  # Reactive:
  # Filter data for the currently selected state (or all if none selected)
  data_current_state <- reactive({
    if (!length(current_state())) {
      return(data())
    }
    data()[state == current_state()]
  })

  # Observe clicks on the map and update the selected state
  observeEvent(event_data("plotly_click", source = "map"), {
    click_data <- event_data("plotly_click", source = "map")

    if (!is.null(click_data$pointNumber)) {
      clicked_point <- sales_by_state()[state_idx == (click_data$pointNumber + 1)]
      clicked_state <- unique(clicked_point$state)
      current_state(clicked_state) # Update selected state
    }
  })

  # Render the year selector input using shinyWidgets::pickerInput
  output$year_selector <- renderUI({
    pickerInput(
      "selected_year",
      label = div("Year:", style = "color: black;"),
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "None",
        `select-all-text` = "All",
        `none-selected-text` = "No Selection",
        `selected-text-format` = "count",
        `count-selected-text` = "Multiple Selection"
      ),
      choices = unique(year(data()$order_date)),
      selected = c(2014, 2015, 2016, 2017),
      multiple = TRUE
    )
  })


  # Sales By State & Region (First Row) -------------------------------------

  output$o_sales_by_state_map <- renderPlotly({
    # Get filtered data (by selected year)
    data_in <- sales_by_state()

    # Return empty plot if no data is available
    if (nrow(data_in) == 0) {
      return(empty_plot("No Data Available"))
    }

    # Ensure a year is selected
    req(selected_year())

    # Create custom tooltip text
    state_tooltip <- c(text = paste(
      "<span style='font-size:16px;'><b>", data_in$state, "</b></span><br>",
      "<br><b>Sales:</b> $", abbreviate_number(data_in$sales),
      sep = ""
    ))

    # Plot choropleth map of U.S. states
    plot_ly(
      data = data_in,
      type = "choropleth",
      locations = ~state_code,
      locationmode = "USA-states",
      z = ~sales,
      source = "map",
      color = ~sales,
      text = ~state_tooltip,
      colorscale = "Blues",
      reversescale = T,
      hoverinfo = "text",
      showscale = T
    ) |>
      layout(
        geo = list(scope = "usa"),
        margin = list(l = 0, r = 0, t = 10, b = 10, pad = 0),
        dragmode = F,
        autosize = T
      ) |>
      config(displayModeBar = F) |>
      event_register("plotly_click") # Enable click events
  }) |> bindCache(selected_year(), sales_by_state()) # Cache results based on selected year(s) & state


  output$o_sales_by_region <- renderPlotly({
    # Get filtered data (by selected year)
    data_in <- data_current_state()

    # Return empty plot if no data is available
    if (nrow(data_in) == 0) {
      return(empty_plot("No Data Available"))
    }

    # Ensure a year is selected
    req(selected_year())

    # Aggregate sales by region and month
    data_reg_mon <- data_in[
      year(order_date) %in% selected_year()
    ][
      , .(sales = sum(sales)),
      by = .(region, month_of_order = floor_date(order_date, "month"))
    ]

    # Due to the independence of the axis of each
    # chart combination (indicator (KPI) + line), all missing dates need to
    # be filled up

    # Create full grid of region × month to fill missing combinations
    all_combinations <- CJ(
      region = data_reg_mon[, unique(region)],
      month_of_order = seq(data_reg_mon[, min(month_of_order)],
        data_reg_mon[, max(month_of_order)],
        by = "month"
      )
    )

    # Define plotting order by total sales
    region_order <-
      data_reg_mon[, .(total_sales = sum(sales)), by = region][
        order(-total_sales)
      ][
        , .(region, region_idx = .I)
      ]

    # Merge and fill missing values with 0
    data_out <-
      data_reg_mon[
        all_combinations,
        on = .(region, month_of_order)
      ][
        is.na(sales), sales := 0
      ][
        region_order,
        on = .(region)
      ]

    count <- data_out[, uniqueN(region)]

    # Start dynamic plot construction
    p <- plot_ly()
    layout_args <- list()

    for (i in 1:count) {
      data_reg <- data_out[region_idx == i]

      region <- data_reg$region
      month_of_order <- data_reg$month_of_order
      sales_trend <- data_reg$sales
      total_sales <- sum(sales_trend)

      max_idx <- which.max(sales_trend)
      min_idx <- which.min(sales_trend)

      # Custom tooltip per region
      sales_trend_tooltip <- c(text = paste(
        "<span style='font-size:16px;'><b>", region, "</b></span><br>",
        "<br><b>Sales:</b> $", abbreviate_number(sales_trend),
        "<br><b>Date:</b>", month_of_order, "<extra></extra>",
        sep = ""
      ))

      # Positioning for subplot layout
      height <- 1 / count
      top <- 1 - (i - 1) * height
      bottom <- top - height

      # Axis ID
      xaxis_id <- paste0("x", i + 1)
      yaxis_id <- paste0("y", i + 1)

      p <- p |>
        # Add total sales as a number indicator
        add_trace(
          title = list(
            text  = region,
            align = "left",
            font  = list(size = 12, color = "gray")
          ),
          domain = list(x = c(0, 0.15), y = c(bottom, top)),
          type = "indicator",
          mode = "number",
          value = round(total_sales / 1000, 2),
          number = list(
            font = list(
              size   = 30,
              family = "Verdana"
            ),
            valueformat = ".2f",
            suffix = "k",
            prefix = "$"
          ),
          align = "left"
        ) |>
        # Filled line chart showing monthly sales trend
        add_trace(
          x = month_of_order,
          y = sales_trend,
          mode = "lines",
          fill = "tozeroy",
          type = "scatter",
          fillcolor = "rgba(211,211,211,0.4)",
          line = list(color = "rgba(211,211,211,0.6)", shape = "spline"),
          xaxis = xaxis_id,
          yaxis = yaxis_id,
          showlegend = F,
          hovertemplate = sales_trend_tooltip
        ) |>
        # highlight max
        add_trace(
          name = "Maximum",
          x = month_of_order[max_idx],
          y = sales_trend[max_idx],
          type = "scatter",
          mode = "markers",
          marker = list(color = "#5c7af5", size = 5),
          cliponaxis = F,
          xaxis = xaxis_id,
          yaxis = yaxis_id,
          showlegend = F,
          hovertemplate = sales_trend_tooltip
        ) |>
        # highlight min
        add_trace(
          name = "Minimum",
          x = month_of_order[min_idx],
          y = sales_trend[min_idx],
          type = "scatter",
          mode = "markers",
          marker = list(color = "gray", size = 5),
          cliponaxis = F,
          xaxis = xaxis_id,
          yaxis = yaxis_id,
          showlegend = F,
          hovertemplate = sales_trend_tooltip
        )

      # Define x layout domain
      layout_args[[paste0("xaxis", i + 1)]] <- list(
        domain = c(0.2, 1),
        title = "",
        showline = F,
        showticklabels = (i == count),
        showgrid = F,
        zeroline = F,
        visible = T,
        anchor = yaxis_id,
        range = c(
          min(month_of_order) - days(3),
          max(month_of_order) + days(3)
        )
      )

      # Define y layout domain
      layout_args[[paste0("yaxis", i + 1)]] <- list(
        domain         = c(bottom, top),
        title          = "",
        showgrid       = F,
        showline       = F,
        showticklabels = F,
        zeroline       = F,
        anchor         = xaxis_id
      )
    }

    # Apply final layout
    p <- do.call(layout, c(list(p), layout_args, list(margin = list(
      l = 10,
      r = 10,
      t = 10,
      b = 20
    ))))
  }) |> bindCache(selected_year(), data_current_state()) # Cache results based on selected year(s) & state

  # Sales By Category (Second Row) ------------------------------------------

  output$o_sales_by_category <- renderPlotly({
    # Get filtered data (by selected year)
    data_in <- data_current_state()

    # Return empty plot if no data is available
    if (nrow(data_in) == 0) {
      return(empty_plot("No Data Available"))
    }

    # Ensure a year is selected
    req(selected_year())

    # Aggregate by category and month
    data_cat_mon <- data_in[
      year(order_date) %in% selected_year()
    ][
      , .(sales = sum(sales)),
      by = .(category, month_of_order = floor_date(order_date, "month"))
    ]

    # Due to the independence of the axis of each
    # chart combination (indicator + monthly trend), all missing dates need to
    # be filled up

    # Create full grid of region × month to fill missing combinations
    all_combinations <- CJ(
      category = data_cat_mon[, unique(category)],
      month_of_order = seq(data_cat_mon[, min(month_of_order)],
        data_cat_mon[, max(month_of_order)],
        by = "month"
      )
    )

    # Define order of categories by total sales
    cat_order <-
      data_cat_mon[
        , .(total_sales = sum(sales)),
        by = category
      ][
        order(-total_sales)
      ][
        , .(category, category_idx = .I)
      ]

    # Merge and fill missing sales
    data_out <-
      data_cat_mon[
        all_combinations,
        on = .(category, month_of_order)
      ][
        is.na(sales), sales := 0
      ][
        cat_order,
        on = .(category)
      ]

    count <- data_out[, uniqueN(category)]

    # Get max sales for bullet chart scale
    max_total_sales <- data_out[
      , .(total_sales = sum(sales)),
      by = category
    ][
      , max(total_sales)
    ]

    # Start dynamic plot construction
    p <- plot_ly()
    layout_args <- list()

    for (i in 1:count) {
      data_cat <- data_out[category_idx == i]
      category <- data_cat[, unique(category)]

      month_of_order <- data_cat$month_of_order
      sales_trend <- data_cat$sales
      total_sales <- sum(sales_trend)

      # Highlight bar with highest sales
      bar_color_max <-
        fifelse(sales_trend == max(sales_trend), "#5c7af5", "lightgray")

      # Tooltip sales trend
      sales_trend_tooltip <- c(text = paste(
        "<span style='font-size:16px;'><b>", category, "</b></span><br>",
        "<br><b>Sales:</b> $", abbreviate_number(sales_trend),
        "<br><b>Date:</b>", month_of_order, "<extra></extra>",
        sep = ""
      ))

      # Positioning for subplot layout
      height <- 1 / count
      top <- 1 - (i - 1) * height
      bottom <- top - height

      # Axis ID
      xaxis_id <- paste0("x", i + 1)
      yaxis_id <- paste0("y", i + 1)

      # Add bullet chart
      p <- p |>
        # Bullet gauge indicator showing total sales for the category
        add_trace(
          type = "indicator",
          mode = "number+gauge",
          value = total_sales,
          domain = list(x = c(0, 0.35), y = c(bottom, top)),
          gauge = list(
            axis = list(
              range = c(0, max_total_sales),
              showticklabels = F,
              ticks = ""
            ),
            bar = list(color = "#5c7af5", thickness = 0.6),
            shape = "bullet",
            borderwidth = 0
          ),
          number = list(font = list(size = 20), prefix = "$"),
          title = list(text = category, font = list(size = 14))
        ) |>
        # Bar chart showing monthly sales trend for the category
        add_trace(
          type = "bar",
          x = month_of_order,
          y = sales_trend,
          marker = list(color = bar_color_max),
          xaxis = xaxis_id,
          yaxis = yaxis_id,
          showlegend = F,
          hovertemplate = sales_trend_tooltip
        )

      # Define x layout domain
      layout_args[[paste0("xaxis", i + 1)]] <- list(
        domain = c(0.4, 1),
        title = "",
        showline = F,
        showgrid = F,
        zeroline = F,
        showticklabels = (i == count),
        anchor = yaxis_id
      )

      # Define y layout domain
      layout_args[[paste0("yaxis", i + 1)]] <- list(
        domain = c(bottom, top),
        title = "",
        showline = F,
        zeroline = F,
        showgrid = F,
        showticklabels = F,
        anchor = xaxis_id
      )
    }

    # Apply final layout
    p <- do.call(layout, c(list(p), layout_args, list(margin = list(
      l = 130,
      r = 10,
      t = 10,
      b = 20
    ))))
  }) |> bindCache(selected_year(), data_current_state()) # Cache results based on selected year(s) & state

  # Sales By Sub Category (Third Row) ---------------------------------------

  output$o_sales_by_sub_category <- renderPlotly({
    # Get filtered data (by selected year)
    data_in <- data_current_state()

    # Return empty plot if no data is available
    if (nrow(data_in) == 0) {
      return(empty_plot("No Data Available"))
    }

    # Ensure a year is selected
    req(selected_year())

    # Aggregate data by month and sub-category
    data_subcat_mon <- data_in[
      year(order_date) %in% selected_year()
    ][
      , .(sales = sum(sales)),
      by = .(sub_category, month_of_order = floor_date(order_date, "month"))
    ]

    # Due to the independence of the axis of each
    # chart combination (scatter + indicator), all missing dates need to
    # be filled up

    # Generate all combinations to ensure complete time series
    all_combinations <- CJ(
      sub_category = data_subcat_mon[, unique(sub_category)],
      month_of_order = seq(data_subcat_mon[, min(month_of_order)],
        data_subcat_mon[, max(month_of_order)],
        by = "month"
      )
    )

    # Sort sub-categories by total sales
    subcat_order <-
      data_subcat_mon[
        , .(total_sales = sum(sales)),
        by = sub_category
      ][
        order(-total_sales)
      ][
        , .(sub_category, subcat_idx = .I)
      ]

    # Final dataset with missing sales filled with zero
    data_out <-
      data_subcat_mon[
        all_combinations,
        on = .(sub_category, month_of_order)
      ][
        is.na(sales), sales := 0
      ][
        subcat_order,
        on = .(sub_category)
      ]

    count <- data_out[, uniqueN(sub_category)]

    max_total_sales <- data_out[
      , .(total_sales = sum(sales)),
      by = .(sub_category)
    ][
      , max(total_sales)
    ]

    layout_args <- list()
    p <- plot_ly()

    for (i in 1:count) {
      data_subcat <- data_out[subcat_idx == i]

      month_of_order <- data_subcat$month_of_order
      sub_category <- data_subcat$sub_category
      sales <- data_subcat$sales
      total_sales <- sum(sales)

      # Custom tooltip for sales by sub category
      sales_tooltip <- c(text = paste(
        "<span style='font-size:16px;'><b>", sub_category, "</b></span><br>",
        "<br><b>Sales:</b> $", abbreviate_number(sales),
        "<br><b>Date:</b>", month_of_order, "<extra></extra>",
        sep = ""
      ))

      # Positioning for subplot layout
      height <- 1 / count
      top <- 1 - (i - 1) * height
      bottom <- top - height

      # Axis ID
      xaxis_id <- paste0("x", i + 1)
      yaxis_id <- paste0("y", i + 1)

      p <- p |>
        # Scatter plot showing sales over months for the sub-category
        add_trace(
          x = month_of_order,
          y = sub_category,
          type = "scatter",
          mode = "markers",
          xaxis = xaxis_id,
          yaxis = yaxis_id,
          marker = list(
            size = 15,
            color = ~sales,
            colorscale = "Blues",
            reversescale = T,
            colorbar = list(title = "Value"),
            showscale = F
          ),
          showlegend = F,
          hovertemplate = sales_tooltip
        ) |>
        # Bullet gauge indicator showing total sales for the sub-category
        add_trace(
          type = "indicator",
          mode = "number+gauge",
          value = total_sales,
          domain = list(x = c(0.8, 1), y = c(bottom, top)),
          gauge = list(
            axis = list(
              range = c(0, max_total_sales),
              showticklabels = F,
              ticks = ""
            ),
            bar = list(color = "#5c7af5", thickness = 0.6),
            shape = "bullet",
            borderwidth = 0
          ),
          number = list(font = list(size = 15), prefix = "$")
        )

      # Define x layout domain
      layout_args[[paste0("xaxis", i + 1)]] <- list(
        domain = c(0, 0.83),
        title = "",
        showline = F,
        showgrid = F,
        zeroline = F,
        showticklabels = (i == count),
        anchor = yaxis_id,
        range = c(
          min(month_of_order) - days(40),
          max(month_of_order) + days(12)
        )
      )

      # Define y layout domain
      layout_args[[paste0("yaxis", i + 1)]] <- list(
        domain = c(bottom, top),
        title = "",
        showline = F,
        zeroline = F,
        showgrid = F,
        anchor = xaxis_id
      )
    }

    # Apply final layout
    p <- do.call(layout, c(list(p), layout_args, list(margin = list(
      l = 100,
      r = 15,
      t = 10,
      b = 20
    ))))
  }) |> bindCache(selected_year(), data_current_state()) # Cache results based on selected year(s) & state

  # Sales By Segment & Top 10 Manufacturers (Fourth Row) --------------------

  output$o_sales_by_segment_bar <- renderPlotly({
    data <- sales_by_segment()

    count_seg <- length(data$segment)

    max_range <- fifelse(
      !is.na(data$sales_py),
      max(data$sales_cy, data$sales_py),
      max(data$sales_cy)
    )

    p <- plot_ly() |>
      layout(
        xaxis = list(
          title          = "",
          showline       = F,
          showticklabels = F,
          showgrid       = F,
          zeroline       = F,
          visible        = F
        ),
        yaxis = list(
          title    = "",
          showgrid = F,
          showline = F
        ),
        grid = list(
          rows    = count_seg,
          columns = 1,
          pattern = "independent"
        ),
        margin = list(l = 150),
        autosize = T
      ) |>
      config(displayModeBar = F)

    for (i in 1:count_seg) {
      bar_color <- fifelse(i == 1, "b20a1c", "lightgray")

      p <- p |> add_trace(
        title = list(
          text = data[i, ]$segment,
          font = list(size = 15)
        ),
        type = "indicator",
        mode = "number+gauge+delta",
        domain = list(row = i - 1, column = 0),
        value = data[i, ]$sales_cy,
        number = list(font = list(size = 15)),
        gauge = list(
          axis = list(
            tickmode = "array",
            ticks = "",
            showticklabels = F,
            range = c(0, max_range)
          ),
          bar = list(
            color = bar_color,
            thickness = 0.3
          ),
          threshold = list(
            line      = list(color = "black", width = 2),
            thickness = 0.45,
            value     = data[i, ]$sales_py
          ),
          shape = "bullet",
          borderwidth = 0
        ),
        delta = list(
          reference = data[i, ]$sales_py,
          relative = T,
          valueformat = ".2%",
          font = list(size = 10)
        )
      )
    }
    return(p)
  })

  output$o_sales_by_top_10_customers_bar <- renderPlotly({
    data <- sales_by_top_10_customers()

    plot_ly(
      data = data,
      x = ~sales,
      y = ~customer_name,
      type = "bar",
      orientation = "h",
      marker = list(
        color = fifelse(
          data$sales < max(data$sales),
          "lightgray",
          "b20a1c"
        )
      ),
      text = ~ paste0(round(data$sales / 1000, 2), "k"),
      textfont = list(color = "lightgray", size = 14),
      textposition = "outside"
    ) |>
      config(displayModeBar = F) |>
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 150, pad = 10),
        xaxis = list(
          title          = "",
          zeroline       = F,
          visible        = F,
          showline       = F,
          showticklabels = F,
          showgrid       = F,
          range          = c(0, max(data$sales) * 1.15)
        ),
        yaxis = list(
          title          = "",
          showline       = F,
          showticklabels = T,
          tickfont       = list(color = "gray"),
          showgrid       = F,
          categoryorder  = "total ascending"
        )
      )
  })
}
# Run the application
shinyApp(ui = ui, server = server)
