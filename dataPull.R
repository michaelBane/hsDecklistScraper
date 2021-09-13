# Pull youtube video descriptions and extract deck-lists.

library(tidyverse)
library(tuber)
library(jsonlite)
library(reticulate)

#Test varaibles. These will need to be derived. 
videoId <- "jwna1aEtm4o"
deckCode <- 'AAEBAbTqAwLFuAPo9wMOwAGYxAKfmwP0qwPBuAP30QOF5APQ7APR7AOn9wOu9wOy9wP8ngT9ngQA'

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

# Parse deckcodes.
use_condaenv('hearthstoneDB',
             required = TRUE)

hs <- reticulate::import('hearthstone')

parsedDecklist <- hs$deckstrings$parse_deckstring(deckCode)

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



# Check 30 cards in deck.  
sum(deckData$Copies) == 30
