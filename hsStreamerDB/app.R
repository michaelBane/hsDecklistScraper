#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(glue)
library(markdown)
library(DBI)
library(RSQLite)
library(ggplot2)

#setwd('~/RProjects/hsDecklistScraper/hsStreamerDB/')

# Import HS Data
hsCardData <- read_csv('data/hsCardDataUnnested.csv',
                       col_types = '_c__d__c___________________________') %>%
    distinct()

con <- dbConnect(SQLite(), 
                 dbname = "hearthstoneDB.db")

channelVideos <- dbGetQuery(con, 'select id, title, publication_date, channel_id, channel_title, url from channelvideos') %>% 
    tibble()

decksWithCardMetaData <- dbGetQuery(con, 'select dbfId, id, hero, deckCode from decks') %>%
    inner_join(hsCardData, by = 'dbfId') %>% # join metadata.
    inner_join(select(hsCardData, dbfId, deckClass = cardClass), 
               by = c('hero' = 'dbfId')) %>%
    select(-cardClass) %>%
    tibble() # join hero class.

dbDisconnect(con)

# Main processing on pulled data.
appData <- inner_join(channelVideos, 
                      decksWithCardMetaData,
                      by = 'id') %>%
    mutate(Cards = glue('<a href="https://playhearthstone.com/en-us/deckbuilder?deckcode={deckCode}" target="_blank" rel="noopener noreferrer">link</a>'),
           Video = glue('<a href="{url}" target="_blank" rel="noopener noreferrer">link</a>'),
           Stats = glue('<a href="https://hsreplay.net/decks/{deckCode}" target="_blank" rel="noopener noreferrer">link</a>'),
           Creator = glue('<a href="https://www.youtube.com/channel/{channel_id}" target="_blank" rel="noopener noreferrer">{channel_title}</a>'),
           Published = lubridate::as_date(publication_date)) %>%
    chop(cols = c(name, dbfId))

# Vectors for populating selectors.
classes <- sort(unique(decksWithCardMetaData$deckClass))
creators <- sort(unique(channelVideos$channel_title))
cards <- sort(unique(decksWithCardMetaData$name))
maxDate <- max(appData$Published, na.rm = TRUE)
minDate <- min(appData$Published, na.rm = TRUE)

# For matching color to class in images.
hsPalleteMatch <- tibble(colour = c('#A330C9', '#FF7D0A', '#ABD473', '#69CCF0', '#F58CBA', '#000000', '#FFF569', '#0070DE', '#9482C9', '#C79C6E'),
                         deckClass = classes)

ui <- function(request){
    
    fluidPage(
        
        theme = bs_theme(bootswatch = "united"),
        
        navbarPage("HS Streamer DeckDB: Find something to play!",
                   windowTitle = 'hsDeckDB',
                   tabPanel("Decks",
                            
                            sidebarLayout(
                                
                                sidebarPanel(width = 3,
                                             
                                             selectInput(inputId = 'creatorChoice',
                                                         label = 'Include Creators:',
                                                         choices = creators,
                                                         selected = creators,
                                                         multiple = TRUE),
                                             
                                             textInput(inputId = 'titleChoice',
                                                       label = 'Search Title:',
                                                       value = '',
                                                       placeholder = 'Sluzzle699'),
                                             
                                             selectInput(inputId = 'classChoice',
                                                         label = 'Include Deck Classes:',
                                                         choices = classes,
                                                         selected = classes,
                                                         multiple = TRUE),
                                             
                                             selectInput(inputId = 'cardChoice',
                                                         label = 'Find Cards:',
                                                         choices = cards,
                                                         multiple = TRUE),
                                             
                                             helpText('Multiple inputs return decks with either card, not both'),
                                             
                                             dateRangeInput(inputId = 'dateChoice',
                                                            label = 'Publish Date Range',
                                                            start = minDate,
                                                            end = maxDate,
                                                            min = minDate,
                                                            max = maxDate),
                                             
                                             submitButton(text = "Apply Changes"),
                                             
                                             hr(),
                                             
                                             includeMarkdown('appInfo.md')
                                             
                                ),
                                
                                # Show a plot of the generated distribution
                                mainPanel(
                                    
                                    tabsetPanel(
                                        
                                        tabPanel('Catalogue',
                                                 dataTableOutput("decksOutput")),
                                        
                                        tabPanel('Summaries',
                                                 #plotOutput('timelineOutput'),
                                                 br(),
                                                 plotOutput('commonCardsOutput'),
                                                 br(),
                                                 plotOutput('timelineOutput'),
                                                 br(),
                                                 plotOutput('classDistOutput'),
                                                 br(),
                                                 plotOutput('creatorDistOutput'),
                                                 br(),
                                                 'More coming soon'
                                                 )
                                        
                                    )
                                )
                            )
                            
                   ),

                  tabPanel("Podcast",
                          "Coming Soon")
        ),
        
        # Sidebar with a slider input for number of bins 
        
    )
    
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    plotData <- reactive({
        
        if(is.null(input$cardChoice)){
            
            appData %>%
                filter(str_detect(str_to_upper(title), str_to_upper(input$titleChoice)),
                       deckClass %in% input$classChoice,
                       channel_title %in% input$creatorChoice,
                       between(Published, input$dateChoice[1], input$dateChoice[2]))
            
        } else {
            
            appData %>%
                mutate(containsCards = map_lgl(name, function(x) input$cardChoice %in% x)) %>%
                filter(str_detect(str_to_upper(title), str_to_upper(input$titleChoice)),
                       deckClass %in% input$classChoice,
                       channel_title %in% input$creatorChoice,
                       between(Published, input$dateChoice[1], input$dateChoice[2]),
                       containsCards)
            
        }
        
    })
    
    output$decksOutput <- DT::renderDataTable({
        
        plotData() %>%
            select(Creator,
                   Published,
                   Title = title,
                   Class = deckClass,
                   Video,
                   Cards,
                   Stats,
                   Code = deckCode) %>%
            distinct() %>%
            arrange(desc(Published)) %>%
            datatable(selection = 'none',
                      escape = FALSE,
                      rownames = FALSE,
                      options = list(
                          pageLength = 25,
                          dom = 'tpl',
                          columnDefs = list(
                              list(targets = 2,
                                   render = JS(
                                       "function(data, type, row, meta) {",
                                       "return type === 'display' && data.length > 15 ?",
                                       "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
                                       "}")
                              )
                          )
                      ), 
                      callback = JS('table.page(3).draw(false);'))
        
    })
    
    output$timelineOutput <- renderPlot(

        plotData() %>%
            select(Creator = channel_title,
                   Title = title,
                   deckCode,
                   Published) %>%
            distinct() %>%
            count(Published,
                  name = 'Decks') %>%
            ggplot(aes(x = Published,
                       y = Decks)) +
            geom_histogram(stat = 'identity',
                           color = '#e95420') +
            scale_y_continuous(minor_breaks = NULL) +
            theme_bw() +
            labs(title = 'Decks published by Date.' ,
                 caption = 'Plot respects filters to the left.',
                 y = '')

    )
    
    output$commonCardsOutput <- renderPlot({
        
        plotData() %>%
            select(Creator = channel_title,
                   Title = title,
                   deckCode,
                   Card = name) %>%
            distinct() %>%
            unnest_longer(Card) %>%
            count(Card,
                  name = 'Decks',
                  sort = TRUE) %>%
            head(25) %>%
            ggplot(aes(y = Decks,
                       x = fct_reorder(Card, Decks))) +
            geom_histogram(stat = 'identity',
                           color = '#e95420') +
            scale_y_continuous(minor_breaks = NULL) +
            theme_bw() +
            labs(title = '25 Most popular cards in decks selected.' ,
                 caption = 'Plot respects filters to the left.',
                 x = '') +
            coord_flip()
        
    })
    
    output$classDistOutput <- renderPlot({
        
        palette <- hsPalleteMatch %>%
            inner_join(distinct(plotData(), deckClass),
                       by = "deckClass") %>%
            pull(colour)
        
        plotData() %>%
            select(Creator = channel_title,
                   Title = title,
                   deckCode,
                   Class = deckClass) %>%
            distinct() %>%
            count(Class,
                  name = 'Decks') %>%
            ggplot(aes(y = Decks,
                       x = fct_rev(Class),
                       fill = Class)) +
            geom_histogram(stat = 'identity') +
            scale_y_continuous(minor_breaks = NULL) +
            scale_fill_manual(values = palette,
                              guide = "none") +
            theme_bw() +
            labs(title = 'Number of Decks by Class.',
                 caption = 'Plot respects filters to the left.',
                 x = '') +
            coord_flip()
        
    })

    output$creatorDistOutput <- renderPlot({
        
        plotData() %>%
            select(Creator = channel_title,
                   Title = title,
                   deckCode) %>%
            distinct() %>%
            count(Creator,
                  name = 'Decks') %>%
            ggplot(aes(y = Decks,
                       x = fct_reorder(Creator, Decks))) +
            geom_histogram(stat = 'identity',
                           color = '#e95420') +
            scale_y_continuous(minor_breaks = NULL) +
            scale_fill_manual(guide = "none") +
            theme_bw() +
            labs(title = 'Number of Decks by Creator.',
                 caption = 'Plot respects filters to the left.',
                 x = '') +
            coord_flip()
        
    })
    
}

# Run the application 
shinyApp(ui = ui, 
         server = server, 
         enableBookmarking = "url")












