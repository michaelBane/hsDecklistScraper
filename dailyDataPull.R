# Pull prev 24h of YouTube video descriptions and extract deck-lists.
library(tidyverse)
library(tuber)
library(reticulate)
library(glue)
use_condaenv('hearthstoneDB',
             required = TRUE)
hs <- import('hearthstone')

creatorLookup <- read_csv('hsStreamerDB/data/creatorLookup.csv')

n <- 5 # Iterate over this to extract backfills.
creator <- creatorLookup[n, 1]$Creator
channelId <- creatorLookup[n, 2]$channelID
creator
channelId

yt_oauth(clientID, secret)

# Find recent videos, published in last 24h
# This API call returns truncated descriptions, just need to find the video ids to feed into a better function.
recentVideos <- NULL
for(i in 1:nrow(creatorLookup)){
  
  recentChannelData <- yt_search(term = "", 
                                 published_after = format(lubridate::as_datetime(Sys.time() - 86400), 
                                                          "%Y-%m-%dT%H:%M:%SZ"),
                                 channel_id = creatorLookup$channelID[i],
                                 simplify = FALSE) %>%
    .$video_id %>%
    as.character()

  recentVideos <- c(recentVideos, recentChannelData)
  
}
recentVideos


# Replicate functionality 
channelVideosDailyAll <- NULL
for(i in recentVideos){
  
  videoData <- get_video_details(video_id =i)
  
  videoMetaData <- tibble(id = i,
                          title = videoData$items[[1]]$snippet$title,
                          publication_date = videoData$items[[1]]$snippet$publishedAt,
                          description = videoData$items[[1]]$snippet$description,
                          channel_id = videoData$items[[1]]$snippet$channelId,
                          channel_title = videoData$items[[1]]$snippet$channelTitle,
                          url = glue('https://www.youtube.com/watch?v={i}'))
  
  channelVideosDailyAll <- bind_rows(channelVideosDailyAll,
                                     videoMetaData)
  
}
channelVideosDailyAll

# Find all of the initial deck-string patterns
# Remove characters prior to deck codes, then extract all chars from start to first non-alphanumeric. 
# regex: https://www.geeksforgeeks.org/python-program-to-extract-string-till-first-non-alphanumeric-character/

deckCodesDailyAll <- NULL
for(i in 1:nrow(channelVideosDailyAll)){
  
  deckCode <- channelVideosDailyAll$description[i] %>% 
    str_sub(start = str_locate_all(channelVideosDailyAll$description[i], 'AAEB')[[1]][,'start']) %>%
    str_extract("[\\dA-Za-z\\w+\\w/\\w=]*") %>%
    unique()
  
  id <- channelVideosDailyAll$id[i]
  
  deckCodesDailyAll <- bind_rows(deckCodesDailyAll,
                                 tibble(deckCode, id))
  
}
deckCodesDailyAll

# Parse deckcodes.
decksDailyAll <- NULL
for(i in 1:nrow(deckCodesDailyAll)){
  
  parsedDecklist <- hs$deckstrings$parse_deckstring(deckCodesDailyAll$deckCode[i])
  
  id = deckCodesDailyAll$id[i]
  deckCode <- deckCodesDailyAll$deckCode[i]
  
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
  
  decksDailyAll = bind_rows(decksDailyAll, deckData)
  
}
decksDailyAll

View(channelVideosDailyAll)
View(deckCodesDailyAll)
View(decksDailyAll)

write_csv(channelVideosDailyAll, glue('hsStreamerdb/data/channelVideos_{runDate}.csv', 
                                      runDate = Sys.Date()))
write_csv(deckCodesDailyAll, glue('hsStreamerdb/data/deckCodes_{runDate}.csv', 
                                  runDate = Sys.Date()))
write_csv(decksDailyAll, glue('hsStreamerdb/data/decks_{runDate}.csv', 
                              runDate = Sys.Date()))


