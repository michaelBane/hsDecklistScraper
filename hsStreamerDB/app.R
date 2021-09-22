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
library(markdown)

# Import HS Data
hsCardData <- read_csv('data/hsCardDataUnnested.csv',
                       col_types = cols(
                           artist = col_character(),
                           cardClass = col_character(),
                           collectible = col_logical(),
                           cost = col_double(),
                           dbfId = col_double(),
                           flavor = col_character(),
                           id = col_character(),
                           name = col_character(),
                           rarity = col_character(),
                           set = col_character(),
                           spellSchool = col_character(),
                           text = col_character(),
                           type = col_character(),
                           mechanics = col_character(),
                           attack = col_double(),
                           health = col_double(),
                           race = col_character(),
                           elite = col_logical(),
                           targetingArrowText = col_character(),
                           durability = col_double(),
                           overload = col_double(),
                           spellDamage = col_double(),
                           battlegroundsPremiumDbfId = col_double(),
                           howToEarnGolden = col_character(),
                           howToEarn = col_character(),
                           collectionText = col_character(),
                           armor = col_double(),
                           techLevel = col_double(),
                           faction = col_character(),
                           multiClassGroup = col_character(),
                           hideStats = col_logical(),
                           battlegroundsDarkmoonPrizeTurn = col_logical(),
                           questReward = col_character()))

data1 <- read_csv('data/channelVideos_All.csv')
# data2 <- read_csv('data/deckCodes.csv')
data3 <- read_csv('data/decks_All.csv') %>%
    inner_join(hsCardData, by = 'dbfId') %>%
    inner_join(select(hsCardData, dbfId, deckClass = cardClass), by = c('hero' = 'dbfId')) %>%
    mutate(race = coalesce(race, 'NONE'),
           spellSchool = coalesce(spellSchool, 'NONE'),
           mechanics = coalesce(mechanics, 'NONE'))

appData <- inner_join(data1, 
                      data3,
                      by = c('id' = 'id.x')) %>%
    mutate(Deck = glue('<a href="https://playhearthstone.com/en-us/deckbuilder?deckcode={deckCode}">link</a>'),
           Youtube = glue('<a href="{url}">link</a>'),
           HSReplay= glue('<a href="https://hsreplay.net/decks/{deckCode}">link</a>'),
           Published = lubridate::as_date(publication_date))

classes <- sort(unique(data3$deckClass))
creators <- sort(unique(data1$channel_title))
tribes <- sort(unique(data3$race))
spellSchools <- sort(unique(data3$spellSchool))
cards <- sort(unique(data3$name))
#mechanics <- sort(unique(data3$mechanics))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("HS Streamer DeckDB: Find something to play!",
               windowTitle = 'hsDeckDB'),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
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
            
            selectInput(inputId = 'tribeChoice',
                        label = 'Deck Contains Tribes:',
                        choices = tribes,
                        selected = tribes,
                        multiple = TRUE),
            
            selectInput(inputId = 'spellSchoolChoice',
                        label = 'Deck Contains Spell Schools:',
                        choices = spellSchools,
                        selected = spellSchools,
                        multiple = TRUE),
            
            textInput(inputId = 'cardTextChoice',
                      label = 'Find Cards with Text:',
                      value = '',
                      placeholder = "Charrrrrge"),
            
            submitButton(text = "Apply Changes"),
            
            hr(),
            
            includeMarkdown('appInfo.md')
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            
            DT::dataTableOutput("decksOutput")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    plotData <- reactive({
        
        if(is.null(input$cardChoice)){
            
            appData %>%
                filter(str_detect(str_to_upper(title), str_to_upper(input$titleChoice)),
                       #name %in% input$cardChoice,
                       deckClass %in% input$classChoice,
                       channel_title %in% input$creatorChoice,
                       race %in% input$tribeChoice,
                       spellSchool %in% input$spellSchoolChoice,
                       str_detect(str_to_upper(text), str_to_upper(input$cardTextChoice)))
            
        } else {
            
            appData %>%
                filter(str_detect(str_to_upper(title), str_to_upper(input$titleChoice)),
                       name %in% input$cardChoice,
                       deckClass %in% input$classChoice,
                       channel_title %in% input$creatorChoice,
                       race %in% input$tribeChoice,
                       spellSchool %in% input$spellSchoolChoice,
                       str_detect(str_to_upper(text), str_to_upper(input$cardTextChoice)))
            
        }
        
    })
    
    output$decksOutput <- DT::renderDataTable({
        
        plotData() %>%
            select(Creator = channel_title,
                   Published,
                   Video = title,
                   Class = deckClass,
                   Youtube,
                   Deck,
                   HSReplay,
                   Code = deckCode) %>%
            distinct() %>%
            arrange(desc(Published)) %>%
            datatable(escape = FALSE,
                      rownames = FALSE,
                      options = list(
                          pageLength = 25,
                          dom = 'tpl',
                          columnDefs = list(
                              list(targets = 7,
                                   render = JS(
                                       "function(data, type, row, meta) {",
                                       "return type === 'display' && data.length > 6 ?",
                                       "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                                       "}")
                              )
                          )
                      ), 
                      callback = JS('table.page(3).draw(false);'))
        
    })

}
# Run the application 
shinyApp(ui = ui, server = server)
