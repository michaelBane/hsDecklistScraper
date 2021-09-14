#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)

data1 <- read_csv('data/videoMetadata.csv')
# data2 <- read_csv('data/deckCodes.csv')
data3 <- read_csv('data/decks.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            textInput(inputId = 'cardChoice',
                      label = 'Contains Cards:',
                      value = '',
                      placeholder = 'Baku|Genn'
                      ),
            
            textInput(inputId = 'titleChoice',
                      label = 'Search Title:',
                      value = '',
                      placeholder = '100% Win-Rate'
            ),
            
            selectInput(inputId = 'classChoice',
                        label = 'Contains Class Cards:',
                        choices = unique(data3$Class),
                        selected = unique(data3$Class),
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
            filter(str_detect(str_to_upper(Card), str_to_upper(input$cardChoice)),
                   str_detect(str_to_upper(title), str_to_upper(input$titleChoice)),
                   Class %in% input$classChoice) %>%
            select(channel_title,
                   title,
                   publication_date,
                   deckCode,
                   url) %>%
            mutate(deckLink = paste0('https://playhearthstone.com/en-us/deckbuilder?deckcode=',
                                     deckCode)) %>%
            distinct() %>%
            arrange(desc(publication_date))
        
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
