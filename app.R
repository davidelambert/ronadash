library(shiny)
library(tidyverse)
library(RcppRoll)

st <- c("us", str_to_lower(state.abb), "dc")
names(st) <- c("United States", state.name, "District of Columbia")
which(st == "de")
st <- st[c(1:9, 52, 10:51)]

ptheme <- 
  theme_minimal() +
    theme(
      text = element_text(family = "serif"),
      axis.line.x.bottom = element_line(color = "black", size = .5),
      axis.line.y.left = element_line(color = "black", size = .5),
      axis.ticks = element_line(color = "black", size = .3),
      panel.grid = element_line(color = "grey90"),
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14, color = "black"),
      plot.caption = element_text(size = 10, face = "italic", color = "grey20",
                                  hjust = 0, margin = margin(t = 3)),
      legend.position = "bottom",
      legend.box.spacing = unit(2, units = "pt"),
      legend.box.margin = margin(0),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 14),
    )


## UI ================
ui <- fluidPage(
  
  # Title
  titlePanel("Covid-19 State-Level Data"),
  
  # Overall Layout - inputs in responsive sidebar w/ data in main div
  sidebarLayout(
    
    # Sidebar div
    sidebarPanel(
      selectInput(
        inputId = "state",
        label = "State",
        st
      ),
      selectInput(
        inputId = "measure",
        label = "Measure",
        c("New Cases" = "positiveIncrease",
          "Percent Positive Tests" = "ppos",
          "Hospitalized Currently" = "hospitalizedCurrently",
          "New Deaths" = "deathIncrease",
          "New Tests Conducted" = "totalTestResultsIncrease")
      )
    ),
    
    # main div
    mainPanel(
      plotOutput("ronaTS")
    )
  )
)






## SERVER ========
server <- function(input, output) {
  
  output$ronaTS <- renderPlot({
    
    # pull & clean CovidTracking data
    if (input$state == "us") {
      api <- "https://covidtracking.com/api/v1/us/daily.csv"
    } else {
      api <- paste0("https://covidtracking.com/api/v1/states/",
                    input$state, "/daily.csv")
    }
    
    stdata <- 
      read_csv(
        url(api),
        cols(date = col_date(format = "%Y%m%d")),
        col_names = TRUE
      ) %>% 
      arrange(date) %>% 
      filter(date >= "2020-03-01") %>% 
      mutate(
        ppos = ifelse(
          totalTestResults>30 & positiveIncrease/totalTestResultsIncrease<.5,
          100*positiveIncrease/totalTestResultsIncrease,
          NA_real_
        )
      ) %>% 
      select(date, positive, positiveIncrease, ppos,
             hospitalizedCurrently, death, deathIncrease,
             totalTestResults, totalTestResultsIncrease
      )
    
    # Right-aligned 7-day moving average of input variable
    ma7r <- rollmeanr(stdata[[input$measure]], 7, fill = NA)
    for (i in 6:2) {
    ma7r[which(is.na(ma7r))] <- 
      rollmeanr(stdata[[input$measure]][which(is.na(ma7r))], i, fill = NA)
    }
    

    # plot
    ggplot(stdata[which(!is.na(ma7r)),], aes_(x = ~date)) +
      scale_x_date(date_labels = ("%B")) +
      geom_col(
        aes_(y = as.name(input$measure)),
        fill = "#13294B",
        color = "#13294B"
      ) +
      geom_line(
        aes_(y = ma7r[which(!is.na(ma7r))]),
        size = 1,
        color = "#F29137",
        na.rm = TRUE
      ) +
      labs(
        x="", y="",
        caption = "Data: Covid Tracking Project | Credit: @NeedsMoreChill"
      ) +
      ptheme
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
