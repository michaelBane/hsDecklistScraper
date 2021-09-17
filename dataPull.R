# Pull YouTube video descriptions and extract deck-lists.
library(tidyverse)
library(tuber)
#library(jsonlite)
library(reticulate)
use_condaenv('hearthstoneDB',
             required = TRUE)
hs <- import('hearthstone')

# Manually curate channel ids
creatorLookup <- read_csv('hsStreamerDB/data/creatorLookup.csv')

n <- 8 # Iterate over this to extract backfills.
creator <- creatorLookup[n, 1]$Creator
channelId <- creatorLookup[n, 2]$channelID
creator
channelId

# Import YT Data.
# Authenticate
# clientID <- '573231105362-p1mtfdnavthjtoq84nivib1scob8ph0n.apps.googleusercontent.com'
# secret <- rstudioapi::askForPassword() 

yt_oauth(clientID, secret)

channelVideos <- get_all_channel_video_stats(channel_id = channelId) #approx 2 mins

# Find all of the initial deck-string patterns
# Remove characters prior to deck codes, then extract all chars from start to first non-alphanumeric. 
# regex: https://www.geeksforgeeks.org/python-program-to-extract-string-till-first-non-alphanumeric-character/

deckCodes <- NULL
for(i in 1:nrow(channelVideos)){
  
  deckCode <- channelVideos$description[i] %>% 
    str_sub(start = str_locate_all(channelVideos$description[i], 'AAEB')[[1]][,'start']) %>%
    str_extract("[\\dA-Za-z\\w+\\w/\\w=]*") %>%
    unique()
  
  id <- channelVideos$id[i]
  
  deckCodes <- bind_rows(deckCodes,
                         tibble(deckCode, id))
  
}
deckCodes

# Parse deckcodes.
decks <- NULL
for(i in 1:nrow(deckCodes)){
  
  parsedDecklist <- hs$deckstrings$parse_deckstring(deckCodes$deckCode[i])
  
  id = deckCodes$id[i]
  deckCode <- deckCodes$deckCode[i]
  
  #Extract clean data from the parsed deck list.
  hero <- parsedDecklist[[2]]
  format <- parsedDecklist[[3]]$name
  
  deckData <- parsedDecklist[[1]] %>% 
    as_tibble(.name_repair = 'unique') %>%
    rownames_to_column() %>%
    pivot_longer(starts_with('..'),
                 names_to = 'card') %>% 
    pivot_wider(names_from = rowname,
                values_from = value) %>%
    unnest(cols = c(`1`, `2`)) %>%
    rename(dbfId = `1`,
           Copies = `2`) %>%
    mutate(fullDeck = sum(Copies) == 30,
           id = id,
           hero = hero,
           format = format,
           deckCode = deckCode)
  
  decks = bind_rows(decks, deckData)

}

View(channelVideos)
View(deckCodes)
View(decks)
#view(hsCardData)

write_csv(channelVideos, glue::glue('hsStreamerdb/data/channelVideos_{creator}.csv'))
write_csv(deckCodes, glue::glue('hsStreamerdb/data/deckCodes_{creator}.csv'))
write_csv(decks, glue::glue('hsStreamerdb/data/decks_{creator}.csv'))



