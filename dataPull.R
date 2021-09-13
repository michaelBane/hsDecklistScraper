# Pull youtube video descriptions and extract deck-lists.
library(tidyverse)
library(tuber)
library(jsonlite)
library(reticulate)

#Test variables. These will need to be derived. 
videoId <- "m50rm7ZATV0"

# Import HS Data
apiURL <- 'https://api.hearthstonejson.com/v1/91456/enUS/cards.collectible.json'
hsCardData <- fromJSON(apiURL) %>% tibble()

# Import YT Data.
# Authenticate
clientID <- '573231105362-p1mtfdnavthjtoq84nivib1scob8ph0n.apps.googleusercontent.com'
secret <- 'Zs1XgYb-euX9I_FY9BA6BvJO'

yt_oauth(clientID, 
         secret)

# Call API
metaData <- get_video_details(video_id=videoId)
videoDescription <- metaData$items[[1]]$snippet$description

# Find all of the initial deck-string patterns
# Remove characters prior to deck codes, then extract all chars from start to first non-alphanumeric. 
# regex: https://www.geeksforgeeks.org/python-program-to-extract-string-till-first-non-alphanumeric-character/
deckCodes <- videoDescription %>% 
  str_sub(start = str_locate_all(videoDescription, 'AAEB')[[1]][,'start']) %>%
  str_extract("[\\dA-Za-z\\w+\\w/\\w=]*") %>%
  unique()

# Parse deckcodes.
use_condaenv('hearthstoneDB',
             required = TRUE)

hs <- import('hearthstone')

for(i in deckCodes){
  
  print(1)
  
  parsedDecklist <- hs$deckstrings$parse_deckstring(i)
  
  #Extract clean data from the parsed deck list.
  hero <- parsedDecklist[[2]]
  format <- parsedDecklist[[3]]
  
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
            Card)
  
  print(deckData)
  
  # Check 30 cards in deck.  
  print(sum(deckData$Copies) == 30)
  
}

