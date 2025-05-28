# ui_elements.R

ui_elements <- list(
  
  # First Row: Sales by State (Map) and Region (Bar)
  
  'map' = card(
    # Header with styled title: "Sales | By State"
    card_header(
      HTML(
        '<div class="text-with-lines">
          <div class="line"></div>
          <span class="text-container">
            <span class="text-black">Sales |</span>
            <span class="text-blue">By State</span>
          </span>
          <div class="line"></div>
        </div>'
      )
    ),
    plotlyOutput('o_sales_by_state_map'),  # Interactive sales map
    actionLink('reset', 'Reset'),          # Reset link for filters/selections
    height = '45vh'
  ), 
  
  'sales_by_region' = card(
    # Header with styled title: "Sales | By Region"
    card_header(
      HTML(
        '<div class="text-with-lines">
          <div class="line"></div>
          <span class="text-container">
            <span class="text-black">Sales |</span>
            <span class="text-blue">By Region</span>
          </span>
          <div class="line"></div>
        </div>'
      )
    ), 
    plotlyOutput('o_sales_by_region'),  # Bar chart for regional sales
    card(card_body(), style = "border: none; overflow: hidden;"), # Empty placeholder card for layout
    height = '45vh'
  ), 
  
  # Second Row: Sales by Category and Trend (duplicated key warning below)
  
  'sales_by_category' = card(
    # Styled header with titles: "Sales | By Category" and "Sales Trend | By Category"
    card_header(
      HTML(
        '<div class="text-with-lines">
          <div class="line"></div>
          <span class="text-container">
            <span class="text-black">Sales |</span>
            <span class="text-blue">By Category</span>
          </span>
          <div class="line"></div>
          <div class="line"></div>
          <span class="text-container">
            <span class="text-black">Sales Trend |</span>
            <span class="text-blue">By Category</span>
          </span>
          <div class="line"></div>
        </div>'
      )
    ),
    plotlyOutput('o_sales_by_category'),  # Chart for sales by category
    height = '30vh'
  ),   

  # Third Row: Sales by Sub-Category
  
  'sales_by_sub_category' = card(
    card_header(
      HTML(
        '<div class="text-with-lines">
          <div class="line"></div>
          <span class="text-container">
            <span class="text-black">Sales |</span>
            <span class="text-blue">By Sub Category</span>
          </span>
          <div class="line"></div>
        </div>'
      )
    ),
    plotlyOutput('o_sales_by_sub_category'),  # Chart for sub-category sales
    height = 'auto'
  ), 
  
  # Fourth Row: Sales by Segment and Top 10 Customers
  
  'sales_by_segment' = card(
    card_header('Sales | By Segment'),           # Header text only
    plotlyOutput('o_sales_by_segment_bar'),      # Bar chart for segment sales
    height = '35vh'
  ),
  
  'sales_by_top_10_customers' = card(
    card_header('Sales | By Top 10 Customers'),  # Header text only
    plotlyOutput('o_sales_by_top_10_customers_bar'),  # Bar chart for top customers
    height = '35vh'
  )
  
)
