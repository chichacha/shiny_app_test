library(shiny)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(bslib)
library(thematic)

# Set default to Monday as start of Week
options("lubridate.week.start" = 1)

# Function to generate the calendar plot
col13 <- ggthemes::tableau_color_pal("Hue Circle")(19)[c(1:12)]
create_calendar_plot <- function(year, calendar_type="445", col_pal = col13,... ) {
  cal <- tibble(
    dt = seq.Date(ymd(paste0(year, "-01-01")), ymd(paste0(year, "-12-31")), by = "day")
  )
  
  wpq <- switch(calendar_type,
                "445" = tibble(
                  wk = 1:53,
                  p = sort(c(rep(1:12, each = 4), c(3, 6, 9, 12, 12))),
                  q = sort(c(rep(1:4, each = 13), c(4)))
                ),
                "454" = tibble(
                  wk = 1:53,
                  p = sort(c(rep(1:12, each=4), c(2, 5, 8, 11, 12))),
                  q = sort(c(rep(1:4, each = 13), c(4)))
                ),
                stop("Invalid calendar type. Choose '445' or '454'.")
  )
  
  cal <- cal |>
    mutate(
      yr = isoyear(dt),
      wk = isoweek(dt),
      wday = wday(dt, label = TRUE)
    ) |>
    left_join(wpq, by = "wk") |>
    mutate(color_num=wk + p + q) |>
    mutate(color_wk = colourvalues::color_values(color_num, 
                                                 palette=farver::decode_colour(col13))) |>
    filter(yr==year)
  
  ggplot(cal, aes(y = wk, x = wday)) +
    geom_tile(color = "white", aes(fill = I(color_wk))) +
    geom_text(aes(label = day(dt)), size = 3) +
    facet_wrap(~p, scales = "free", ncol = 3) +
    scale_y_reverse() +
    scale_x_discrete(labels=~str_sub(.,1L,1L)) +
    labs(x = "", y = "", title = paste(year, calendar_type, "Calendar")) +
    cowplot::theme_minimal_grid() +
    theme(panel.grid.major=element_blank(),
          axis.ticks = element_blank(),
          strip.text = element_text(size=13))
}

#create_calendar_plot(year=2025, calendar_type="454")
#create_calendar_plot(year=2025, calendar_type="445")

# Shiny UI
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("4-4-5 / 4-5-4 Calendar Generator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("year", "Select Year", value = 2025, min = 2000, max = 2100),
      selectInput("calendar_type", "Select Calendar Type", choices = c("445", "454")),
      downloadButton("download", "Download Calendar")
    ),
    mainPanel(
      plotOutput("calendar_plot")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  # Enable thematic to sync ggplot2 with bslib theme
  thematic_shiny()
  
  # Render the plot
  output$calendar_plot <- renderPlot({
    create_calendar_plot(input$year, input$calendar_type)
  }, width="auto", height=750)
  
  
  # Enable downloading the plot
  output$download <- downloadHandler(
    filename = function() {
      paste("calendar_", input$year, "_", input$calendar_type, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, create_calendar_plot(input$year, input$calendar_type), 
             width = 9, height = 9, dpi = 300,  device=ragg::agg_png,
             bg="#fffff3")
    }
  )
}

# Run the app
#thematic_shiny()
shinyApp(ui, server)



