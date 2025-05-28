# helpers.R

# Creates an empty placeholder plot with a centered title
empty_plot <- function(title = NULL) {
  p <- plotly_empty(type = "scatter", mode = "markers") %>%
    config(
      displayModeBar = FALSE
    ) %>%
    layout(
      title = list(
        text = title,
        font = list(size = 26, color = "gray"),
        yref = "paper", # Reference frame for vertical position
        y = 0.5 # Vertical position of title (centered)
      )
    )
  return(p)
}

# Formats large numbers with suffixes like k, M, B, T, and appends '€' by default
abbreviate_number <- function(x, digits = 2, suffix = "€") {
  fcase(
    x < 1e3,   as.character(x), # Below 1,000: show as-is
    x < 1e6,   paste0(round(x / 1e3, digits), "k", " ", suffix), # Thousands
    x < 1e9,   paste0(round(x / 1e6, digits), "M", " ", suffix), # Millions
    x < 1e12,  paste0(round(x / 1e9, digits), "B", " ", suffix), # Billions
    x < 1e15,  paste0(round(x / 1e12, digits), "T", " ", suffix) # Trillions
  )
}