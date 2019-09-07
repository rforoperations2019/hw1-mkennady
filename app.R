# EU High-tech exports, 2007-2018
# Data sources:
#       High-tech exports: https://data.europa.eu/euodp/en/data/dataset/jrc-10113-rio_htech_exports
#       Population: https://data.worldbank.org/indicator/SP.POP.TOTL?locations=EU

library(readr)
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

theme_set(theme_classic())


# import data ----
population <- read_csv("World_Population.csv", skip = 4)
exports <- read_csv("RIO_HTECH_EXPORTS-data.csv")

# data cleaning, merging, and subsetting ----

# cleaning data
exports$GEO_DESC <- as.character(exports$GEO_DESC)
exports$GEO_DESC[exports$GEO_DESC == "Czechia"] <- "Czech Republic"
exports$GEO_DESC[exports$GEO_DESC == "Slovakia"] <- "Slovak Republic"
exports <- subset(exports, GEO_DESC != "European Union" & UNIT != "PC_TOT")
exports$GEO <- NULL
exports$PARTNER <- revalue(exports$PARTNER, c("EXT_EU28"="non-EU Countries", "INT_EU28"="EU Member States", "WORLD"="the World"))


# get list of years and countries needed and columns needed from exports dataset
years <- unique(exports$TIME_PERIOD)
exports.cols <- c('TIME_PERIOD', 'PARTNER', 'GEO','OBS_VALUE','GEO_DESC')
countries <- sort(unique(exports$GEO_DESC))


# drop NULL columns from population dataset
#population <- dplyr::select(population, -c(""))
# in population dataset, keep only years and countries that are in exports dataset
# Reference: https://stackoverflow.com/questions/5234117/how-to-drop-columns-by-name-in-a-data-frame
population <- population[ , (colnames(population) %in% years | colnames(population) %in% c('Country Name'))]
population <- subset(population, population$`Country Name` %in% countries)


# gather population data from wide to long
# Reference: http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
population %<>%
  gather(key = "Year", value = "Population", -'Country Name')

# clean and merge exports data
exports <- exports[,colnames(exports) %in% exports.cols] 
population$Year <- as.integer(population$Year)
exports <- left_join(exports, population, by = c("TIME_PERIOD" = "Year", "GEO_DESC" = "Country Name"))
colnames(exports) <- c("Year", "Partner", "Value (in Millions)","Country", "Population")

# Shiny app
# Reference: https://shiny.rstudio.com/gallery/navbar-example.html 
ui <- navbarPage("EU High-tech Exports, 2007-2018", theme = shinytheme("sandstone"),
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