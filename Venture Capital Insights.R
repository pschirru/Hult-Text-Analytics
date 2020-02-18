#Set working directory and import libraries
setwd("/Users/paoloschirru/Desktop/Venture Capital/")
library(dbplyr)
library(tinytex)
library(textreadr)
library(tidytext)
library(tidyverse)
library(twitteR)
library(tm)

#Import data and convert it to a data frame
vc_magazines <- read_document(file="All.docx")
vc_mag_df <- data_frame(text=vc_magazines)

#tibble of words that only make noise in our analysis, They will be removed later
noisy_words <- tibble(word = c("he","rt","accordiing","venturecapital","because","capital","venture","fund"))

#Tokenise and remove stop words/noisy words
vc_mag_token <- vc_mag_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(noisy_words)

#Count and sort words
vc_mag_token %>% 
  count ( word , sort = TRUE ) 

#Set keys to access Twitter
consumer_key <- 'INSERT YOUR TWITTER DEVELOPER KEYS'
consumer_secret <- 'INSERT YOUR TWITTER DEVELOPER KEYS'
access_token <- 'INSERT YOUR TWITTER DEVELOPER KEYS'
access_secret <- 'INSERT YOUR TWITTER DEVELOPER KEYS'

#Load keys
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Search twitter
twitter_search <- twitteR::searchTwitter('#venturecapital', n = 1000, lang = 'en', since = '2019-06-01', retryOnRateLimit = 1e3)
vc_twitter = twitteR::twListToDF(twitter_search)

#remove noise
vc_twitter$text <- gsub("http[^[:space:]]*","",  vc_twitter$text) 
vc_twitter$text <- gsub("http[^[:space:]]*","", vc_twitter$text) 

#tokenize twitter data and remove unnecessary words
vc_tweet_token <- vc_twitter %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(noisy_words)

#save twitter data to excel
#library(openxlsx)
#write.xlsx(vc_tweet_token, 'vc_tweet_tokenss.xlsx')

#obtain frequency to  later plot a corellogram
frequency <- bind_rows(mutate(vc_tweet_token, author="twitter"),
                       mutate(vc_mag_token, author= "magazines")) %>% 
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  spread(author, proportion) %>%
  gather(author, proportion, `magazines`)

library(scales)

#plot frequency in order to get a corellogram
ggplot(frequency, aes(x=proportion, y=`twitter`, 
                      color = abs(`twitter`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "twitter", x=NULL)

#get sentiments for the two datasets

vc_mag_token %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(word) %>% 
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

vc_tweet_token %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(id) %>% 
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

vc_mag_sentiment <- vc_mag_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)

#plot sentiments 
vc_mag_sentiment %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Magazines: sentiment", x=NULL)+
  coord_flip()

vc_twitter_sentiment <- vc_tweet_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)

vc_twitter_sentiment %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Twitter: sentiment", x=NULL)+
  coord_flip()

vc_mag_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=mean(value)) %>%
  mutate(method="AFINN")

vc_tweet_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=mean(value)) %>%
  mutate(method="AFINN")

twitter_words <- lengths(vc_tweet_token)

############################################
## TF-IDF analysis
#############################################
#combine the data
combined_sources <- bind_rows(mutate(vc_twitter, from="twitter"),
                              mutate(vc_mag_df, from= "magazines")
)

#unnest and count words 
twitt_modif <- combined_sources %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(noisy_words) %>%
  count(from, word, sort=TRUE) %>%
  ungroup()

#grouping 
twitt_modif2 <- twitt_modif %>%
  group_by(from) %>%
  summarize(total=sum(n))

#left join the two datasets
sources_leftjoined <- left_join(twitt_modif, twitt_modif2)

tidy_twitt_tfidf <- sources_leftjoined %>%
  bind_tf_idf(word, from, n)

tidy_twitt_tfidf 

#order descending
tidy_twitt_tfidf %>%
  arrange(desc(tf_idf))


#ploting tf-idf
tidy_twitt_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(from) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=from))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~from, ncol=2, scales="free")+
  coord_flip()

#Creating Bigrams 
vc_mag_bigrams <- vc_mag_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

vc_mag_bigrams %>%
  count(bigram, sort = TRUE) 

library(tidyr)

#magazines bigrams
bigrams_mag_separated <- vc_mag_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_mag_filtered <- bigrams_mag_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_mag_counts <- bigrams_mag_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_mag_counts

#twitter bigrams
vc_twitt_bigrams <- vc_twitter %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

vc_twitt_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_twitt_separated <- vc_twitt_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_twitt_filtered <- bigrams_twitt_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_twitt_counts <- bigrams_twitt_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_twitt_counts

#plotting

library(igraph)
mag_graph <- bigram_mag_counts %>%
  filter(n>20) %>%
  graph_from_data_frame()

mag_graph

library(ggraph)
ggraph(mag_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

twitt_graph <- bigram_twitt_counts %>%
  filter(n>20) %>%
  graph_from_data_frame()

ggraph(twitt_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#Bigrams
big_twitt_n <- bigram_twitt_counts %>%
  filter(n > 12)

big_mag_n <- bigram_mag_counts %>%
  filter(n>5)

big_joined <- bigram_twitt_counts %>%
  full_join(bigram_mag_counts) %>%
  filter(n>5)

ggraph(big_twitt_n, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

ggraph(big_mag_n, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

twitt_graph <- bigram_twitt_counts %>%
  filter(n>20) %>%
  graph_from_data_frame()

ggraph(twitt_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

########################################################################################################################
#COVER PAGE
#Wordclouds for the cover page
library(wordcloud)

vc_cloud <- vc_mag_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word)

wordcloud(
  words = vc_cloud$word,
  freq = vc_cloud$n,
  max.words = 45,
  colors = 'blue',
  ordered.colors = TRUE
)

bigram_twitt_counts

wordcloud(
  words = bigram_twitt_counts$word1,
  freq = bigram_twitt_counts$n,
  max.words = 45
)






