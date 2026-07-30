# TX Semiconductor Workforce Dashboard
library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(DT)

enrollment  <- readRDS("supply/data/enrollment.rds")
completions <- readRDS("supply/data/completions.rds")

sectors      <- sort(unique(enrollment$institution_sector))
cip_cats     <- sort(unique(enrollment$cip_category))
year_range   <- range(as.numeric(enrollment$academic_year), na.rm = TRUE)

ui <- page_navbar(
  title = "Texas Semiconductor Workforce Pipeline",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  sidebar = sidebar(
    selectInput("sector", "Institution sector", choices = c("All", sectors), selected = "All"),
    selectInput("cip_cat", "CIP category", choices = c("All", cip_cats), selected = "All"),
    sliderInput("years", "Academic years", min = year_range[1], max = year_range[2],
                value = year_range, sep = "", step = 1)
  ),

  nav_panel("Enrollment trends",
    plotlyOutput("enrollment_plot", height = "500px")
  ),

  nav_panel("Completions trends",
    plotlyOutput("completions_plot", height = "500px")
  ),

  nav_panel("Enrollment vs. completions",
    plotlyOutput("pipeline_plot", height = "500px")
  ),

  nav_panel("Data table",
    downloadButton("download_data", "Download filtered data (CSV)"),
    DTOutput("data_table")
  )
)

server <- function(input, output, session) {

  filt <- function(df) {
    d <- df
    if (input$sector != "All")  d <- filter(d, institution_sector == input$sector)
    if (input$cip_cat != "All") d <- filter(d, cip_category == input$cip_cat)
    d <- filter(d, as.numeric(academic_year) >= input$years[1],
                     as.numeric(academic_year) <= input$years[2])
    d
  }

  output$enrollment_plot <- renderPlotly({
    d <- filt(enrollment) |>
      group_by(academic_year, institution_sector) |>
      summarise(total = sum(count, na.rm = TRUE), .groups = "drop")
    plot_ly(d, x = ~academic_year, y = ~total, color = ~institution_sector,
            type = "scatter", mode = "lines+markers") |>
      layout(yaxis = list(title = "Enrollment (headcount)"), xaxis = list(title = "Academic Year"))
  })

  output$completions_plot <- renderPlotly({
    d <- filt(completions) |>
      group_by(academic_year, institution_sector) |>
      summarise(total = sum(count, na.rm = TRUE), .groups = "drop")
    plot_ly(d, x = ~academic_year, y = ~total, color = ~institution_sector,
            type = "scatter", mode = "lines+markers") |>
      layout(yaxis = list(title = "Degrees & Certificates Awarded"), xaxis = list(title = "Academic Year"))
  })

  output$pipeline_plot <- renderPlotly({
    e <- filt(enrollment)  |> group_by(academic_year) |> summarise(enrolled = sum(count, na.rm = TRUE), .groups = "drop")
    c <- filt(completions) |> group_by(academic_year) |> summarise(completed = sum(count, na.rm = TRUE), .groups = "drop")
    d <- left_join(e, c, by = "academic_year") |> mutate(completion_rate = completed / enrolled)
    plot_ly(d, x = ~academic_year, y = ~completion_rate, type = "bar") |>
      layout(yaxis = list(title = "Completions as % of Enrollment", tickformat = ".0%"),
             xaxis = list(title = "Academic Year"))
  })

  output$data_table <- renderDT({
    datatable(filt(enrollment), options = list(pageLength = 15), filter = "top")
  })

  output$download_data <- downloadHandler(
    filename = function() "tx_semiconductor_enrollment_filtered.csv",
    content = function(file) write.csv(filt(enrollment), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
