library(twitteR)
library(ROAuth)
library(httr)

#look at trends between albums
# compare to sales and spotify streams

# Set API Keys
api_key <- "0oQ0gLNYn6IciEHNb3SQJzwWZ"
api_secret <- "DHEI3jH0ZxZ6jT9rTPcIJRqSx9su4aBGfya8FyiGQcGttWGkzH"
access_token <- "932401329481293826-4lzYRSuRSN634ScISXITz00Npb1dLae"
access_token_secret <- "mltprIGxVD2BsCfSmd6dAMoCdlEcXNdeL27gzaTPHiRiG"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#search for tweets about tame impala b sides

#tameimpalatweet2=searchTwitter("tame impala b sides",lang="en",n=10000)

tweets.df <- twListToDF(tameimpalatweet2)
users <- lookupUsers(tweets.df$screenName)
users_df <- twListToDF(users)
sorted = sort(table(users_df[, 'location']),decreasing=T)
sorted = data.frame(sorted)
write.csv(sorted, "sorted.csv")
#write.csv(tweets.df, "tweets.df.csv")
#286/500 users had address turned on


library(plyr)
#ti2 = laply(tweets.df, function(t) t$getText())

# Read in dictionary of positive and negative works
yay = scan('positive-words.txt',
           what='character', comment.char=';')
boo = scan('negative-words.txt',
           what='character', comment.char=';')
# Add a few twitter-specific negative phrases
bad_text = c(boo, 'wtf', 'douchebag', 'depressing', 'slow', 'sad')
good_text = c(yay, 'sick', ':)', 'dank', 'fuck', 'listening', 'gold', 'heard', 'fire', 'solved', 'smoke', 'weed', 'weedy', 'dude', 'living', '!!', 'SAY', 'cant', 'stop','woah','new','impressive','grown','shit','insane','insanely','nice', 'god','killing', 'dope','af', 'gold', 'finally', 'better', 'best', 'fave', 'OOO', 'good', 'so good', 'lit', 'untouchable', 'go', 'gift', 'gifted', 'thank')

score.sentiment = function(sentences, good_text, bad_text, .progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, good_text, bad_text) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    #to remove emojis
    sentence <- iconv(sentence, 'UTF-8', 'ASCII')
    sentence = tolower(sentence)        
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, good_text)
    neg.matches = match(words, bad_text)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, good_text, bad_text, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}




# Call the function and return a data frame
tame <- score.sentiment(tweets.df$text, good_text, bad_text, .progress='text')
# Cut the text, just gets in the way
boxplot(tame$score)
#plotdat <- feelthabern[c("name", "score")]
table(tame$score)

d <- density(tame$score)
plot(d)
polygon(d, col="red", border="blue")


################ do a map of location tweet ################ 

#read in the csv with addresses again:
address <- read_csv("C:/Users/spenc/Dropbox/Blog Post Project/Tame Impala/sorted.csv")
#keep only top 10 places
address10 <- address[1:10,]
write.csv(address10, "address10.csv")

library(ggmap)

worldmap = map_data("world")


ggplot() +
  geom_polygon(data=worldmap, aes(x=long,y=lat,group=group), color = "grey", size = 0.4, fill="gainsboro") +
  geom_point(data = address10, mapping = aes(x = Long, y = Lat, size = Freq)) 

myPalette <- colorRampPalette(rev(brewer.pal(6, "RdYlGn")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(2, 25))

ggplot() +
  geom_polygon(data=worldmap, aes(x=long,y=lat,group=group), color = "grey", size = 0.4, fill="gainsboro") +
  geom_point(data = address10, mapping = aes(x = Long, y = Lat, color = Freq), size=.3) + 
  sc +
  ggtitle("MAP")


ggplot() +
  geom_polygon(data=worldmap, aes(x=long,y=lat,group=group), color = "grey", size = 0.4, fill="gainsboro") +
  geom_point(data = address10, mapping = aes(x = Long, y = Lat, size=Freq),alpha=0.4, col="orange") + 
  scale_size_continuous(range=range(2,30)) +
  ggtitle("Map of Tweet Locations") 
# geom_text(aes(label=address10$Location,''))

################  top favorited tweets ################ 
tweets.df$Tweet <- iconv(enc2utf8(tweets.df$text),sub="byte")
tweets.df$TweetLength <- nchar(tweets.df$Tweet)
lapply(dat[,cols], as.numeric)
#tweets.df$favorited <- as.numeric(tweets.df$favorited)
m1 <- glm(tweets.df$favorited~tweets.df$TweetLength)
summary(m1)


################  build a wordplot and wordcloud ################ 

library(tm)
library(wordcloud)
library(RColorBrewer)

mh370_text = sapply(tameimpalatweet2, function(x) x$getText())
tweets <- sapply(mh370_text,function(row) iconv(row, "latin1", "ASCII", sub=""))
mh370_corpus = Corpus(VectorSource(tweets))

tdm = TermDocumentMatrix(
  mh370_corpus,
  control = list(
    removePunctuation = TRUE,
    stopwords = c(stopwords("english")),
    removeNumbers = TRUE, tolower = TRUE)
)

m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing = TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word = names(word_freqs), freq = word_freqs)
#write.csv(dm,'dm.csv')
#remove words like "tame", "tame impala", and anything related to the song names or album name
#read in dataset
dm <- read_csv("C:/Users/spenc/Dropbox/Blog Post Project/Tame Impala/dm.csv")


wordcloud(dm$word, dm$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"), min.freq = 2)

################  graph the word freq ################ 

p <- ggplot(subset(dm, freq>3), aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity") + 
  ggtitle("Top Words, Word Frequency >3") +
  labs(y = "Frequency (Word Count)") +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_bw()
p   


tweets.df <- read_csv("C:/Users/spenc/Dropbox/Blog Post Project/Tame Impala/tweets.df.csv")
tweets.df <- cbind(tweets.df, tame$score)
#write.csv(tweets.df, "UseThis.csv")
############### Sentiment plot ##############
library(ggrepel)
# color palette
cols <- c("#ce472e", "#f05336", "#ffd73e", "#eec73a", "#4ab04a")

set.seed(932)
samp_ind <- sample(c(1:nrow(tweets.df)), nrow(tweets.df) * 0.1) # 10% for labeling

# plotting


ggplot(tweets.df, aes(x = created, y = tame$score, color = tame$score)) +
  theme_minimal() +
  scale_color_gradientn(colors = cols, limits = c(-4, 4),
                        breaks = seq(-4, 4, by = 1),
                        guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_point(aes(color = tame$score), alpha = 0.8) +
  geom_hline(yintercept = 0.65, color = "#4ab04a", size = 1.5, alpha = 0.6, linetype = "longdash") +
  # geom_hline(yintercept = 0.35, color = "#f05336", size = 1.5, alpha = 0.6, linetype = "longdash") +
  geom_smooth(size = 1.2, alpha = 0.2) +
  # geom_label_repel(data = tweets.df[samp_ind, ],
  #                  aes(label = round(tame$score, 2)),
  #                  fontface = 'bold',
  #                  size = 2.5,
  #                  max.iter = 100) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, face = "bold", color = 'black')) +
  ggtitle("Tweets Sentiment Rate (Probability of Positiveness)") +
  xlab("Date") +
  ylab("Sentiment Score") + 
  #guides(fill=guide_legend(title="Sentiment Range")) + 
  labs(color='Sentiment Range') +
  geom_text(aes(label=ifelse(tame$score>2 | tame$score<0,as.character(tame$score),'')),hjust=-1,vjust=0, alpha=0.8)  



## same graph as above, but now will use dataset without neutral points
#remove neutral tweets

tweetsNoNeutral <- tweets.df[apply(tweets.df[c(18)],1,function(z) any(z!=0)),]


ggplot(tweetsNoNeutral, aes(x = created, y = `tame$score`, color =`tame$score`)) +
  theme_minimal() +
  scale_color_gradientn(colors = cols, limits = c(-4, 4),
                        breaks = seq(-4, 4, by = 1),
                        guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_point(aes(color = `tame$score`), alpha = 0.8) +
  geom_hline(yintercept = 0.65, color = "#4ab04a", size = 1.5, alpha = 0.6, linetype = "longdash") +
  # geom_hline(yintercept = 0.35, color = "#f05336", size = 1.5, alpha = 0.6, linetype = "longdash") +
  geom_smooth(size = 1.2, alpha = 0.2) +
  # geom_label_repel(data = tweets.df[samp_ind, ],
  #                  aes(label = round(tame$score, 2)),
  #                  fontface = 'bold',
  #                  size = 2.5,
  #                  max.iter = 100) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, face = "bold", color = 'black')) +
  ggtitle("Tweets Sentiment Rate (Probability of Positiveness)") +
  xlab("Date") +
  ylab("Sentiment Score") + 
  #guides(fill=guide_legend(title="Sentiment Range")) + 
  labs(color='Sentiment Range') 
#  geom_text(aes(label=ifelse(`tame$score`>2 | `tame$score`<0,as.character(tame$score),'')),hjust=-1,vjust=0, alpha=0.8)




library(tidyverse)
library(purrrlyr)
library(text2vec)
library(caret)
library(glmnet)


################  More plotting ################  
ggplot(data = tweets.df, aes(x = created, fill = isRetweet)) +
  geom_histogram() +
  xlab("Time") + ylab("Number of tweets") +
  scale_fill_manual(values = c("midnightblue", "deepskyblue4", "aquamarine3"))

length.tweet <- tweets.df[(tweets.df$TweetLength <= 140),]
ggplot(data = length.tweet, aes(x = TweetLength)) +
  geom_histogram(aes(fill = ..count..), binwidth = 8) +
  theme(legend.position = "none") +
  xlab("Number of Characters") + ylab("Number of Tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") +
  ggtitle("Characters per Tweet")

p <- ggplot(length.tweet, aes(TweetLength, fill = cut(TweetLength, 100))) +
  geom_histogram(show.legend = FALSE) +
  theme_minimal() +
  labs(x = "Number of Characters", y = "# Tweets") +
  ggtitle("Characters Per Tweet")

p + scale_fill_discrete(h = c(180, 360), c = 150, l = 80)

a <- ggplot(length.tweet, aes(x = TweetLength))

a + geom_area(stat = "bin", color = "#00AFBB", fill = "#00AFBB")


a + geom_density(color = "#00AFBB", fill = "#00AFBB", alpha=0.4) +
  ggtitle("Density Plot of Tweet Length") +
  xlab("Tweet Length") +
  ylab("Density")

library(syuzhet)
mySentiment <- get_nrc_sentiment(tweets.df$text)
write.csv(mySentiment, "mySentiment.csv")


#### songs analysis of releases ####
####################Spotify API ##################
library(rlang)
library(tibble)
devtools::install_github('charlie86/spotifyr')
library(devtools)
library(spotifyr)
library(dplyr)
library(httr)
library(purrr)
#library(sentify)
Sys.setenv(SPOTIFY_CLIENT_ID = "3d64d3e77870408caffd90e237e825ab")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "127034756c81480aa8da8f4aadf490af")
spotify_df <- get_artist_audio_features('Tame Impala')
str(spotify_df)
write.csv(spotify_df, "spotify_df.csv")

album_filter <- c('Currents B-Sides & Remixes')
spotify_df1 <- spotify_df[51:55,] #keep just b sides
str(spotify_df)
write.csv(spotify_df1, "Spotify.csv")

spotify_df1 %>% 
  select(valence, track_name) %>%
  arrange(valence) %>% 
  slice(1:10)

library(RColorBrewer)
library(highcharter)

plot_df <- spotify_df1 %>% 
  rowwise %>% 
  mutate(tooltip = paste0('<a style = "margin-right:', max(max(nchar(track_name), nchar(album_name)) * 7, 55), 'px">', # dynamic sizing
                          '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                          '<b>Album:</b> ', album_name,
                          '<br><b>Track:</b> ', track_name)) %>% 
  ungroup

avg_line <- plot_df %>% 
  group_by(album_release_year, album_name, album_img) %>% 
  summarise(avg = mean(valence)) %>% 
  ungroup %>% 
  transmute(x = as.numeric(as.factor(album_release_year)), 
            y = avg,
            tooltip = paste0('<a style = "margin-right:55px">',
                             '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                             '<b>Album:</b> ', album_name,
                             '<br><b>Average Gloom Index:</b> ', round(avg, 2),
                             '</a>'))
plot_track_df <- plot_df %>% 
  mutate(tooltip = paste0(tooltip, '<br><b>Gloom Index:</b> ', valence, '</a>'),
         album_number = as.numeric(as.factor(album_release_year))) %>% 
  ungroup

album_chart <- hchart(plot_track_df, 'scatter', hcaes(x = as.numeric(as.factor(album_release_year)), y = valence, group = album_name)) %>% 
  hc_add_series(data = avg_line, type = 'line') %>%
  hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>% 
  hc_colors(c(sample(brewer.pal(n_distinct(spotify_df1$album_name), 'Paired')), 'black')) %>% 
  hc_xAxis(title = list(text = 'Album'), labels = list(enabled = F)) %>% 
  hc_yAxis(max = 100, title = list(text = 'Gloom Index')) %>% 
  hc_title(text = 'Data Driven Depression') %>% 
  hc_subtitle(text = 'Radiohead song sadness by album') %>% 
  hc_add_theme(hc_theme_smpl())
album_chart$x$hc_opts$series[[10]]$name <- 'Album Averages'
album_chart
