
CREATE VIEW channelVideos AS
SELECT DISTINCT id, title, publication_date, description, channel_id, channel_title, url
FROM (SELECT * FROM channelvideosdaily 
      UNION ALL 
      SELECT * FROM channelvideosbacklog) cv;
      
      
CREATE VIEW deckcodes AS
SELECT DISTINCT deckcode, id
FROM (SELECT * FROM deckcodesdaily 
      UNION ALL 
      SELECT * FROM deckcodesbacklog) dc;
      
CREATE VIEW decks AS
SELECT DISTINCT card, dbfid, copies, fulldeck, id, hero, format, deckcode
FROM (SELECT * FROM decksdaily 
      UNION ALL 
      SELECT * FROM decksbacklog) dc;
