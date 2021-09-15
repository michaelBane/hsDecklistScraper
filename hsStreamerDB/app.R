#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

library(shiny)
library(tidyverse)
library(DT)
library(glue)

data1 <- read_csv('data/videoMetadata.csv')
# data2 <- read_csv('data/deckCodes.csv')
data3 <- read_csv('data/decks.csv') %>%
    mutate(Tribe = coalesce(Tribe, 'NONE'))

classes <- c("DEMONHUNTER", "DRUID", "HUNTER", "MAGE", "PALADIN", "PRIEST",    
             "ROGUE", "SHAMAN", "WARLOCK", "WARRIOR", 'NEUTRAL')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            textInput(inputId = 'titleChoice',
                      label = 'Search Title:',
                      value = '',
                      placeholder = '100% Win-Rate'),
            
            selectInput(inputId = 'classChoice',
                        label = 'Contains Class Cards:',
                        choices = classes,
                        selected = classes,
                        multiple = TRUE),
            
            textInput(inputId = 'cardChoice',
                      label = 'Contains Card:',
                      value = '',
                      placeholder = 'Baku|Genn'),
            
            selectInput(inputId = 'tribeChoice',
                        label = 'Contains Tribes:',
                        choices = sort(unique(data3$Tribe)),
                        selected = sort(unique(data3$Tribe)),
                        multiple = TRUE)
            
            ),

        # Show a plot of the generated distribution
        mainPanel(
           DT::dataTableOutput("decksOutput")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$decksOutput <- DT::renderDataTable({
        
        inner_join(data1, data3,
                   by = 'id') %>%
            mutate(Deck = glue('<a href="https://playhearthstone.com/en-us/deckbuilder?deckcode={deckCode}">link</a>'),
                   Youtube = glue('<a href="{url}">link</a>'),
                   Published = lubridate::as_date(publication_date)) %>%
            filter(str_detect(str_to_upper(title), str_to_upper(input$titleChoice)),
                   str_detect(str_to_upper(Card), str_to_upper(input$cardChoice)),
                   Class %in% c(input$classChoice),
                   Tribe %in% input$tribeChoice) %>%
            arrange(desc(Published)) %>%
            select(Creator = channel_title,
                   Published,
                   Video = title,
                   Youtube,
                   Deck,
                   Code = deckCode) %>%
            distinct() %>%
            datatable(escape = FALSE,
                      rownames = FALSE,
                      options = list(columnDefs = list(list(
                          targets = 5,
                          render = JS(
                              "function(data, type, row, meta) {",
                              "return type === 'display' && data.length > 6 ?",
                              "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                              "}")
                      ))), callback = JS('table.page(3).draw(false);'))
        
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
