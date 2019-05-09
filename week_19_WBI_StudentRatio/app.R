library(shiny)
library(tidyverse)
WBI_mean_ratio <- readr::read_csv("https://raw.githubusercontent.com/cajablo/tidytues_fun/master/week_19_WBI_mean_ratio.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Student-Teacher Ratios in World Bank Income Groups"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "education_level",
                     label = "Education Level:",
                     choices = c(
                       "Pre-Primary Education" = "Pre-Primary Education", 
                       "Primary Education" = "Primary Education", 
                       "Lower Secondary Education" = "Lower Secondary Education", 
                       "Upper Secondary Education" = "Upper Secondary Education", 
                       "Secondary Education" = "Secondary Education",
                       "All Levels" = "all_levels_mean"
                     ),
                     selected = "all_levels_mean")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     WBI_mean_ratio_filter <- WBI_mean_ratio %>%
       na.omit() %>%
       filter(level == input$education_level)
     
     ggplot(WBI_mean_ratio_filter, aes(x = year, y = mean_ratio, color = country)) +
       geom_line(size = 1) +
       geom_point() +
       labs(
         color = "WB Income Level",
         x = "",
         y = "Mean Student-Teacher Ratio"
       ) +
       theme_minimal() +
       theme(legend.position = "bottom")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

