# EU High-tech exports, 2007-2018
# Data sources:
#       High-tech exports: https://data.europa.eu/euodp/en/data/dataset/jrc-10113-rio_htech_exports
#       Population: https://data.worldbank.org/indicator/SP.POP.TOTL?locations=EU


library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(tools)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(knitr)
library(tidyr)
library(scales)
library(htmltools)
library(markdown)
library(shinythemes)


# import data ----
exports <- read_csv("EU_high_tech_exports_2007-2018.csv")
countries <- sort(unique(exports$Country))

# Shiny app
# Reference: https://shiny.rstudio.com/gallery/navbar-example.html 
ui <- navbarPage("EU High-tech Exports, 2007-2018",
          #First tab - data on each country
           tabPanel("Country-level Data",
                    fluidRow(column(3, 
                              # select country from dropdown list input ----
                              selectInput(inputId = "country",
                                    label = "Select Country:",
                                    choices = countries,
                                    selected = "Austria",
                                    selectize = FALSE),
                              # select location of exports radio buttons (first page)
                        radioButtons(inputId = "location1",
                                     label = "Select Location:",
                                     choices = c("Inside the EU" = "EU Member States",
                                                 "Outside the EU" = "non-EU Countries",
                                                 "World" = "the World"),
                                     selected = "EU Member States")
                        #Data table of exports (first page showing individual country's data)
                    ), column(9, DT::dataTableOutput(outputId = "exports_table1")
                    )
                    ), fluidRow(
                      column(12,
                             # bar chart plot
                             plotOutput(
                             outputId = "bar"
                             )
                      )
                    )
           ), 
           # Second tab - EU-level data
           tabPanel("EU Data",  fluidRow(
             #slider input to select year
             column(3, sliderInput(inputId = "year",
                                   label = "Select a year between 2007 and 2018:",
                                   min = 2000,
                                   max = 2020,
                                   value = 2007,
                                   sep=""),
                    # select location of exports radio buttons (second page)
                    radioButtons(inputId = "location2",
                                 label = "Select Location:",
                                 choices = c("Inside the EU" = "EU Member States",
                                             "Outside the EU" = "non-EU Countries",
                                             "World" = "the World"),
                                 selected = "EU Member States"),
                              # download button
                              downloadButton(outputId = "download",
                                             label = "Download all data")
                    ),
             column(9,
                    #Data table of exports (second page showing EU-level data)
                      DT::dataTableOutput(outputId = "exports_table2")
                  )
                ),
             fluidRow(
               column(12,
                      # scatter plot output
                 plotOutput(
                   outputId = "scatter"
                 )
               )
             ),
             fluidRow(
               column(12,
                      # treemap plot output
                 plotOutput(
                  outputId = "tree" 
                 )
               )
             )
           )                      
        )


# Define server function ---------
server <- function(input, output, session) {
  
  # download the data
  # Reference: https://shiny.rstudio.com/reference/shiny/1.0.0/downloadButton.html
  output$download <- downloadHandler(
    filename = function() {
      paste('EU_high_tech_exports_2007-2018.csv')
    },
    content = function(con) {
      write_csv(exports, con)
    }
  )
  
  # data set for "Country-level Data" tab
  exportInput <- reactive({
    req(input$country, input$location1)
    if (input$location1 == "the World") {
      subset(exports, Country == input$country & (Partner == "non-EU Countries" | Partner == "EU Member States"))
    } else {
      subset(exports, Country == input$country & Partner == input$location1)
    }
  })
  
  # data set for "EU Data" tab
  export2Input <- reactive({
    req(input$year, input$location2)
    subset(exports, Year == input$year & Partner == input$location2)
  })
  
  # bar chart title ----
  barLegend <- reactive({
    if (input$location1 == "EU Member States") {
      "Inside EU"
    } else {
      "Outside EU"
    }
  })
  
  # Enforce range for "year" slider
  # (Geoffrey and Malvika -- I know this is uninspired, but I couldn't think of something actually useful given my data and inputs without making major changes 
  # to work already completed, so I decided to show understanding this way.)
  
  observe({
    if(input$year < 2007) {
      updateSliderInput(session, 
                        inputId = "year",
                        value = 2007
      )} else if (input$year > 2018) {
        updateSliderInput(session, 
                          inputId = "year",
                          value = 2018)
    }
  })

  # first data table ----
  output$exports_table1 <- 
      DT::renderDataTable(
        DT::datatable(data = exportInput()[, c(1:3,5)],
                      caption = paste("Data Table Showing High-tech Exports from", input$country, "to", input$location1, "from 2007 to 2018."),
                      filter = "top",
                      options = list(pageLength = 4),
                      rownames = FALSE) %>% 
          formatCurrency(c("Value (in Millions)"), currency = "€", interval = 3, mark=",", digits=0) %>% 
          DT::formatRound(c("Population"), digits=0,interval=3, mark=","))
  
  # second data table ----
  output$exports_table2 <- DT::renderDataTable(
    DT::datatable(data = export2Input()[, c(3:5)],
                  caption = paste("Data Table Showing High-tech Exports to", input$location2, "in",input$year),
                  filter = "top",
                  options = list(pageLength = 5),
                  rownames = FALSE) %>% 
      formatCurrency(c("Value (in Millions)"), currency = "€", interval = 3, mark=",", digits=0) %>% 
      DT::formatRound(c("Population"), digits=0,interval=3, mark=","))
  
  # bar chart ----
  output$bar <- renderPlot({
    ggplot(data = exportInput(), aes(x=exportInput()$Year, y=exportInput()$'Value (in Millions)', fill=exportInput()$Partner)) +
      geom_bar(stat = "identity") +
      ylab("High-tech Exports (Millions of €)") +
      theme(legend.title=element_blank(), plot.title = element_text(size = 18)) +
      scale_x_discrete(name = "Year", limits = years) +
      scale_y_continuous(labels=comma) +
      scale_fill_discrete(labels = c(barLegend(), "Inside EU")) +
      ggtitle(paste("High-tech Exports from", input$country, "to", input$location1, "between 2007 and 2018."))
  })

  # scatterplot ----
  output$scatter <- renderPlot({
    ggplot(data = export2Input(), aes(x=export2Input()$Population, y=export2Input()$'Value (in Millions)', label=export2Input()$Country)) +
      geom_point(shape=1, color = "black") +
      ylab("High-tech Exports (Millions of €)") +
      xlab("Population") +
      geom_text(size=3, hjust=0, vjust=-1, check_overlap = TRUE, color = "black") +
      scale_x_log10(labels=comma) +
      scale_y_continuous(labels=comma) +
      annotate(geom="text", x=1500000, y=200000,
               label="Some country labels may not be shown due to overlapping. Note the logarithmic scale on x-axis.", color="red", size=5) +
      theme(legend.title=element_blank(), plot.title = element_text(size = 20)) +
      ggtitle(paste("High-tech Exports to", input$location2, " and Population of EU member states in", input$year, "."))
  }
  )
  
  # treemap plot ----
  output$tree <- renderPlot({
    ggplot(export2Input(), aes(area = export2Input()$'Value (in Millions)', label = export2Input()$Country, subgroup = export2Input()$Partner)) +
      geom_treemap() +
      geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                        grow = TRUE) +
      theme(plot.title = element_text(size = 20)) +
      ggtitle(paste("Treemap showing relative proportion of each country's high-tech exports to total EU high-tech exports to", input$location2, "in", input$year, "."))
  })
}

# Run the application ----
shinyApp(ui = ui, server = server)