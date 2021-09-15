# Pull YouTube video descriptions and extract deck-lists.
library(tidyverse)
library(tuber)
library(jsonlite)
library(reticulate)
use_condaenv('hearthstoneDB',
             required = TRUE)
hs <- import('hearthstone')

# Manually curate channel ids
channelId <- 'UCVjzQsZzQ4lmyIrmSKJjiog' #Roffle
channelId <- 'UCTYHKBJgpGBGv8VoEITuVsQ' #Slizzle466

# Import HS Data
apiURL <- 'https://api.hearthstonejson.com/v1/91456/enUS/cards.collectible.json'
hsCardData <- fromJSON(apiURL) %>% tibble()

# Import YT Data.
# Authenticate
clientID <- '573231105362-p1mtfdnavthjtoq84nivib1scob8ph0n.apps.googleusercontent.com'
secret <- rstudioapi::askForPassword() 

yt_oauth(clientID, 
         secret)

channelVideos <- get_all_channel_video_stats(channel_id = channelId) #approx 2 mins

videoMetadata <- channelVideos %>%
  tibble() %>%
  #sample_n(20) %>% #For testing
  select(id, title, publication_date, description, channel_title, url) %>%
  mutate(publication_date = lubridate::as_datetime(publication_date))

# Find all of the initial deck-string patterns
# Remove characters prior to deck codes, then extract all chars from start to first non-alphanumeric. 
# regex: https://www.geeksforgeeks.org/python-program-to-extract-string-till-first-non-alphanumeric-character/

deckCodes <- NULL
for(i in 1:nrow(videoMetadata)){
  
  deckCode <- videoMetadata$description[i] %>% 
    str_sub(start = str_locate_all(videoMetadata$description[i], 'AAEB')[[1]][,'start']) %>%
    str_extract("[\\dA-Za-z\\w+\\w/\\w=]*") %>%
    unique()
  
  id <- videoMetadata$id[i]
  
  deckCodes <- bind_rows(deckCodes,
                         tibble(deckCode, id))
  
}
deckCodes

# Parse deckcodes.
decks <- NULL
for(i in 1:nrow(deckCodes)){
  
  parsedDecklist <- hs$deckstrings$parse_deckstring(deckCodes$deckCode[i])
  id = deckCodes$id[i]
  
  #Extract clean data from the parsed deck list.
  #hero <- parsedDecklist[[2]]
  #format <- parsedDecklist[[3]]
  deckData <- parsedDecklist[[1]] %>% 
    as_tibble(.name_repair = 'unique') %>%
    rownames_to_column() %>%
    pivot_longer(starts_with('..'),
                 names_to = 'card') %>% 
    pivot_wider(names_from = rowname,
                values_from = value) %>%
    unnest(cols = c(`1`, `2`)) %>%
    inner_join(hsCardData,
               by = c(`1` = 'dbfId')) %>%
    select(Card = name,
           Copies = `2`,
           Cost = cost,
           CardType = type,
           Class = cardClass,
           Rarity = rarity,
           Set = set,
           SpellSchool = spellSchool,
           Tribe = race) %>%
    arrange(Cost,
            Card) %>%
    mutate(fullDeck = sum(Copies) == 30,
           id = id,
           deckCode = deckCodes$deckCode[i])
  
  decks = bind_rows(decks, deckData)

}

View(videoMetadata)
View(deckCodes)
View(decks)

# write_csv(videoMetadata, 'hsStreamerdb/data/videoMetadata.csv')
# write_csv(deckCodes, 'hsStreamerdb/data/deckCodes.csv')
# write_csv(decks, 'hsStreamerdb/data/decks.csv')

