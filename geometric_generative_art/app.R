library(shiny) # Web Application Framework for R
library(shinythemes) # For predefined themes
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(scales) # Scale Functions for Visualization
library(ggforce) # Accelerating 'ggplot2'
library(cowplot) # Streamlined Plot Theme and Plot Annotations for 'ggplot2'
library(showtext) # Using Fonts More Easily in R Graphs


# Your `generate_plot` function
generate_plot <- function(angle=pi/6, sides=3, colors="264653-2a9d8f-e9c46a-f4a261-e76f51",
                          grid_x=16, grid_y=9, r=sqrt(2), mod_p=1.5, yj_p=-1, ...) {
  df_grid <- expand_grid(x = seq(-grid_x, grid_x, by = 1), 
                         y = seq(-grid_y, grid_y, by = 1)) |>
    arrange(y, x) |>
    mutate(
      y_odd = (y %% 2 == 0),
      idx = row_number() - 1,
      x = if_else(y_odd, x + 1, x),
      theta = atan2(y, x)
    )
  
  col_pal <- str_c("#", (str_split(colors, "-")[[1]]))
  
  gr <- (1 + sqrt(5)) / 2
  mod_trans <- modulus_trans(p = mod_p)
  yeo_johnson <- yj_trans(p = yj_p)
  
  gg <- df_grid |>
    ggplot(aes(x0 = x, y0 = y)) +
    geom_regon(aes(sides = sides, r = r, angle = angle, fill = idx %% gr), alpha = 0.4) +
    geom_regon(aes(sides = sides, r = r, angle = -angle, fill = idx %% gr), alpha = 0.4) +
    geom_point(aes(x=x, y=y, color=idx %% gr)) +
    scale_fill_gradientn(colors = col_pal, guide = "none") +
    scale_color_gradientn(colors = col_pal, guide = "none") +
    theme_nothing(font_family = "Roboto Condensed") +
    theme(title = element_text(family="Roboto Condensed", hjust=0, color=col_pal[1])) +
    coord_trans(x = mod_trans, y = yeo_johnson, xlim = c(-grid_x + 2, grid_x - 2), ylim = c(-grid_y, grid_y)) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    #labs(x = "", y = "", title = str_c("x:yeo johnson = ", yj_p, " y:modulus = ", mod_p, " r:", round(r, 2), " angle:", round(angle, 2))) +
    theme(plot.margin=unit(c(0,0,0,0),"mm"),
          plot.background=element_rect(fill=colorspace::lighten(col_pal[2],amount=0.99),
                                       color="#ffffff00"))
  
  return(gg)
}

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"), # Use a predefined theme
  titlePanel(div(
    style = "text-align:center; 
    font-family: 'Roboto Condensed', sans-serif; color: #2a9d8f;",
    "✨ Interactive Plot Generator ✨"
  )),
  sidebarLayout(
    sidebarPanel(
      tags$style(HTML("h4 { color: #2a9d8f; }")), # Custom header style
      h4("Adjust Your Plot Settings:"),
      sliderInput("angle", "Angle (radians):", min = 0, max = 2 * pi, 
                  value = pi / 6, 
                  step = pi/36),
      numericInput("sides", "Number of Sides of Polygons:", value = 3, min = 3, max = 12),
      selectInput(
        "colors",
        "Choose Color Palette:",
        choices = list(
          "Ocean Twilight" = "001219-005f73-0a9396-94d2bd-e9d8a6",
          "Golden Hour" = "ee9b00-ca6702-bb3e03-ae2012-9b2226",
          "Harvest Fields" = "264653-2a9d8f-e9c46a-f4a261-e76f51",
          "Pop Fiesta" = "ff6f61-6b4226-ffbc42-24539a-3caea3",
          "Retro Carnival" = "001219-005f73-0a9396-94d2bd-e9d8a6-ee9b00-ca6702-bb3e03-ae2012-9b2226",
          "Tropical Breeze" = "006d77-83c5be-edf6f9-ffddd2-e29578",
          "Berry Smoothie" = "6a0572-cd58b4-ff85a1-52b788-84a98c",
          "Desert Mirage" = "d4a373-a98467-52796f-354f52-cfdbd5",
          "Neon Glow" = "ff006e-8338ec-3a86ff-06d6a0-f4ff61",
          "Forest Retreat" = "264653-52796f-84a98c-cfbf9e-e4e4e4",
          "Candy Pop" = "f94144-f3722c-f9c74f-90be6d-43aa8b",
          "Aurora Dreams" = "0d1b2a-1b263b-415a77-778da9-e0e1dd",
          "Sunset Glow" = "ff6f61-f4a261-e76f51-6a0572-8338ec",
          "Winter Frost" = "003049-669bbc-d2f0f7-bfd7ea-e1eff6",
          "Cosmic Dawn" = "000814-001d3d-003566-ffc300-ffd60a"
        ),
        selected = "264653-2a9d8f-e9c46a-f4a261-e76f51"
      ),
      numericInput("grid_x", "Number of Objects on X-axis: ", 
                   value = 16, min = 1, max = 50),
      numericInput("grid_y", "Number of Objects on Y-axis:", 
                   value = 9, min = 1, max = 50),
      numericInput("r", "Radius - Size of Polygon:", 
                   value = 0.7, step = 0.1, min=0.01, max=5),
      sliderInput("mod_p", "Modulus Parameter - Controlling Warping on X-axis:", 
                   value = 1, step = 0.1, min=-10, max=10),
      sliderInput("yj_p", "Yeo-Johnson Parameter - Controlling Warping on Y-axis:", 
                   value = 1, step = 0.1, min=-10, max=10)
    ),
    mainPanel(
      h3("Generated Pattern Art:"),
      uiOutput("palette_display"), # Add this for displaying color hex values
      plotOutput("plot", height = "680px", width = "100%") 
      # Set height and width
    )
  )
)

# Define server logic
server <- function(input, output) {
  library(showtext)
  font_add_google("Roboto Condensed", "Roboto Condensed")
  showtext_auto()
  
  # Render selected color palette hex values
  output$palette_display <- renderUI({
    # Split the selected color palette into individual hex codes
    colors <- paste0("c(", paste0('"#', strsplit(input$colors, "-")[[1]], '"', collapse = ", "), ")")
    
    # Display the hex values next to the h3 tag
    tags$p(
      style = "font-family: 'Roboto Condensed'; font-size: 16px; color: #2a9d8f;",
      paste("Color Palette in R format: ", colors)
    )
  })
  
  
  output$plot <- renderPlot({
    generate_plot(
      angle = input$angle,
      sides = input$sides,
      colors = input$colors,
      grid_x = input$grid_x,
      grid_y = input$grid_y,
      r = input$r,
      mod_p = input$mod_p,
      yj_p = input$yj_p
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
