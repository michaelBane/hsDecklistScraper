library(tidyverse)

# List of files to stitch
channelVideosFiles <- list.files(path = 'hsStreamerDB/data/',
                                 pattern = 'channelVideosBacklog_')

deckCodesFiles <- list.files(path = 'hsStreamerDB/data/',
                             pattern = 'deckCodesBacklog_')

decksFiles <- list.files(path = 'hsStreamerDB/data/',
                         pattern = 'decksBacklog_')

# Stitch channel video metadata
channelVideosAll <- NULL
for(i in channelVideosFiles){
  
  channelData <- read_csv(glue('hsStreamerDB/data/{i}'),
                          col_types = cols(
                            id = col_character(),
                            title = col_character(),
                            publication_date = col_character(),
                            description = col_character(),
                            channel_id = col_character(),
                            channel_title = col_character(),
                            url = col_character(),
                            run_date = col_date(format = "")
                          ))
  
  channelVideosAll <- bind_rows(channelVideosAll, channelData)
  
}
channelVideosAll

# Stitch deckcodes
deckCodesAll <- NULL
for(i in deckCodesFiles){
  
  deckCodeData <- read_csv(glue('hsStreamerDB/data/{i}'))
  
  deckCodesAll <- bind_rows(deckCodesAll, deckCodeData)
  
}
deckCodesAll

# Stitch deck data
decksAll <- NULL
for(i in decksFiles){
  
  deckData <- read_csv(glue('hsStreamerDB/data/{i}'))
  
  decksAll <- bind_rows(decksAll, deckData)
  
}
decksAll

#Collect and unnest HS data.
# Import HS Data
apiURL <- 'https://api.hearthstonejson.com/v1/128654/enUS/cards.collectible.json'
hsCardDataUnnested <- jsonlite::fromJSON(apiURL) %>% 
  tibble() %>%
  unnest_longer(mechanics) %>%
  select(-referencedTags, -classes)


# write_csv(channelVideosAll, 'hsStreamerDB/data/channelVideos_All.csv')
# write_csv(deckCodesAll, 'hsStreamerDB/data/deckCodes_All.csv')
# write_csv(decksAll, 'hsStreamerDB/data/decks_All.csv')
# write_csv(hsCardDataUnnested, 'hsStreamerDB/data/hsCardDataUnnested.csv')

library(RSQLite)
library(DBI)
con <- dbConnect(SQLite(),
                 dbname = "hsStreamerDB/hearthstoneDB.db")

dbWriteTable(con,
             'channelVideosBacklog',
             channelVideosAll)

dbWriteTable(con,
             'deckCodesBacklog',
             deckCodesAll)

dbWriteTable(con,
             'decksBacklog',
             decksAll)

dbDisconnect(con)
