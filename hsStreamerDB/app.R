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

# Import HS Data
apiURL <- 'https://api.hearthstonejson.com/v1/91456/enUS/cards.collectible.json'
hsCardData <- fromJSON(apiURL) %>% tibble()

data1 <- read_csv('data/channelVideos_All.csv')
# data2 <- read_csv('data/deckCodes.csv')
data3 <- read_csv('data/decks_All.csv') %>%
    inner_join(hsCardData, by = 'dbfId') %>%
    inner_join(select(hsCardData, dbfId, deckClass = cardClass), by = c('hero' = 'dbfId'))

classes <- sort(unique(data3$deckClass))
creators <- sort(unique(data1$channel_title))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Hearthstone Decklist DB"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput(inputId = 'creatorChoice',
                        label = 'Creators:',
                        choices = creators,
                        selected = creators,
                        multiple = TRUE),
            
            textInput(inputId = 'titleChoice',
                      label = 'Search Title:',
                      value = '',
                      placeholder = 'Sluzzle699'),
            
            selectInput(inputId = 'classChoice',
                        label = 'Deck Classes:',
                        choices = classes,
                        selected = classes,
                        multiple = TRUE),
            
            textInput(inputId = 'cardChoice',
                      label = 'Contains Card:',
                      value = '',
                      placeholder = "Yogg-Saron, Hope's End",
            )
            
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
                   by = c('id' = 'id.x')) %>%
            mutate(Deck = glue('<a href="https://playhearthstone.com/en-us/deckbuilder?deckcode={deckCode}">link</a>'),
                   Youtube = glue('<a href="{url}">link</a>'),
                   Published = lubridate::as_date(publication_date)) %>%
            filter(str_detect(str_to_upper(title), str_to_upper(input$titleChoice)),
                   str_detect(str_to_upper(name), str_to_upper(input$cardChoice)),
                   deckClass %in% input$classChoice,
                   channel_title %in% input$creatorChoice) %>%
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
