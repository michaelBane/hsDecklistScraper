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
                       col_types = '_c__d_cc____________________________') %>%
    rename(cardId = id) %>%
    distinct()

hsCalendar <- read_csv('data/hsCalendar.csv')

con <- dbConnect(SQLite(), 
                 dbname = "hearthstoneDB.db")

channelVideos <- dbGetQuery(con, 'select id, title, publication_date, channel_id, channel_title, url from channelvideos') %>% 
    mutate(channel_title = ifelse(channel_title == 'Martian Buu', 'MartianBuu', channel_title)) %>%
    tibble()

decksWithCardMetaData <- dbGetQuery(con, 'select dbfId, id, hero, deckCode from decks') %>%
    inner_join(hsCardData, 
               by = 'dbfId') %>%
    inner_join(select(hsCardData, dbfId, deckClass = cardClass), 
               by = c('hero' = 'dbfId')) %>%
    select(-cardClass) %>%
    tibble() # join hero class.

deckArchetypes <- dbGetQuery(con, 'select * from deckArchetypes')

dbDisconnect(con)

# Main processing on pulled data.
appData <- inner_join(channelVideos, 
                      decksWithCardMetaData,
                      by = 'id') %>%
    left_join(deckArchetypes,
              by = 'deckCode') %>%
    mutate(Cards = glue('<a href="https://playhearthstone.com/en-us/deckbuilder?deckcode={deckCode}" target="_blank" rel="noopener noreferrer">link</a>'),
           Video = glue('<a href="{url}" target="_blank" rel="noopener noreferrer">link</a>'),
           Stats = glue('<a href="https://hsreplay.net/decks/{deckCode}" target="_blank" rel="noopener noreferrer">link</a>'),
           Creator = glue('<a href="https://www.youtube.com/channel/{channel_id}" target="_blank" rel="noopener noreferrer">{channel_title}</a>'),
           Published = lubridate::as_date(publication_date)) %>%
    distinct() %>%
    chop(cols = c(name, dbfId, cardId)) %>%
    mutate(prettyDeck = map_chr(.x = cardId, 
                                .f = function(x) paste(glue('<img style="border:1px solid white" width="50" height="50" src="https://art.hearthstonejson.com/v1/256x/{x}.jpg">'), collapse = '')))

# Vectors for populating selectors.
classes <- sort(unique(decksWithCardMetaData$deckClass))
archetypes <- sort(unique(deckArchetypes$archetype))
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
        
        navbarPage("The Wild Deck Database: Find something to play!",
                   
                   windowTitle = 'Wild Deck DB',
                   
                   tabPanel("Decks",
                            
                            sidebarLayout(
                                
                                sidebarPanel(width = 3,
                                             
                                             textOutput('nOutput'),
                                             
                                             hr(),
                                             
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

                                             # selectInput(inputId = 'archetypeChoice',
                                             #             label = 'Include Archetypes:',
                                             #             choices = archetypes,
                                             #             selected = archetypes,
                                             #             multiple = TRUE),
                                                                                          
                                             selectInput(inputId = 'cardChoice',
                                                         label = 'Includes Cards:',
                                                         choices = cards,
                                                         multiple = TRUE),
                                             
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
                                                 
                                                 dataTableOutput("decksOutput")
                                                 
                                        ),
                                        
                                        tabPanel('Summaries',
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
                   
                   # tabPanel("Archetypes",
                   #          
                   #          tabsetPanel(
                   #              
                   #              tabPanel("Demon Hunter"),
                   #              tabPanel("Druid"),
                   #              tabPanel("Hunter"),
                   #              tabPanel("Mage"),
                   #              tabPanel("Paladin"),
                   #              tabPanel("Priest"),
                   #              tabPanel("Rogue"),
                   #              tabPanel("Shaman"),
                   #              tabPanel("Warlock"),
                   #              tabPanel("Warrior")
                   #              
                   #          )
                   #          
                   # ),
                   
                   tabPanel("Podcast",
                            
                            HTML('<iframe title="Hearthramble" allowtransparency="true" height="480" width="100%" style="border: none; min-width: min(100%, 430px);" scrolling="no" data-name="pb-iframe-player" src="https://www.podbean.com/player-v2/?i=6gabm-cc326f-pbblog-playlist&share=1&download=1&rtl=0&fonts=Arial&skin=ff6d00&font-color=ffffff&order=episodic&limit=20&filter=all&ss=8be91c5e59024d6913c173651b74da5d&btn-skin=1b1b1b&size=480" allowfullscreen=""></iframe>')
                            
                   )
        ),
        
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
                mutate(containsCards = map_lgl(name, function(x) all(input$cardChoice %in% x))) %>%
                filter(str_detect(str_to_upper(title), str_to_upper(input$titleChoice)),
                       deckClass %in% input$classChoice,
                       channel_title %in% input$creatorChoice,
                       between(Published, input$dateChoice[1], input$dateChoice[2]),
                       containsCards)
            
        }
        
    })
    
    output$nOutput <- renderText({
        
        nDecks <- plotData() %>%
            select(Creator,
                   Published,
                   Title = title,
                   Class = deckClass,
                   Video,
                   Cards,
                   Stats,
                   `Pretty Deck` = prettyDeck,
                   Code = deckCode) %>%
            distinct() %>%
            count()
        
        glue('{nDecks} Decks.')
        
        
    })
    
    output$decksOutput <- DT::renderDataTable({
        
        plotData() %>%
            select(Creator,
                   Published,
                   Title = title,
                   Class = deckClass,
                   #archetype,
                   Video,
                   Cards,
                   Stats,
                   `Pretty Deck` = prettyDeck,
                   Code = deckCode) %>%
            distinct() %>%
            arrange(desc(Published)) %>%
            datatable(selection = 'none',
                      escape = FALSE,
                      rownames = FALSE,
                      options = list(
                          pageLength = 15,
                          dom = 'tpl',
                          scrollX = TRUE,
                          autoWidth = TRUE,
                          columnDefs = list(
                              list(width = '1600px', targets = 7),
                              list(width = '500px', targets = 2)
                          ))
            )
    })
    
    output$timelineOutput <- renderPlot({
        
        palette <- hsPalleteMatch %>%
            inner_join(distinct(plotData(), deckClass),
                       by = "deckClass") %>%
            pull(colour)
        
        timlinePlotData <- plotData() %>%
            select(Creator = channel_title,
                   Title = title,
                   Class = deckClass,
                   deckCode,
                   Published) %>%
            distinct() %>%
            count(Published,
                  Class,
                  name = 'Decks') %>%
            group_by(Class) %>%
            arrange(Class, Published) %>%
            mutate(cumSumDecks = cumsum(Decks))
        
        ggplot(timlinePlotData) +
            geom_line(aes(x = Published,
                          y =  cumSumDecks,
                          color = Class)) +
            geom_vline(xintercept = hsCalendar$Released,
                       lty = 2,
                       color = 'grey') +
            geom_text(data = hsCalendar,
                      aes(x = Released,
                          y = max(timlinePlotData$cumSumDecks) + max(timlinePlotData$cumSumDecks) * 0.05,
                          label = Expansion),
                      angle = 18,
                      size = 3,
                      hjust = 0,
                      vjust = 1) +
            scale_x_date(limits = c(min(timlinePlotData$Published),
                                    max(timlinePlotData$Published))) +
            scale_y_continuous(minor_breaks = NULL,
                               limits = c(0, 
                                          max(timlinePlotData$cumSumDecks) + max(timlinePlotData$cumSumDecks) * 0.10)) +
            scale_color_manual(values = palette) +
            theme_bw() +
            labs(title = 'Decks published by Date.' ,
                 caption = 'Plot respects filters to the left.',
                 y = 'Cumulative number of decks')
        
    })
    
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



