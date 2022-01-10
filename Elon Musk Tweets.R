library(twitteR)
library(ROAuth)
library(hms)
library(lubridate) 
library(tidytext)
library(tm)
library(wordcloud)
library(igraph)
library(glue)
library(networkD3)
library(rtweet)
library(stringr)
library(ggplot2)
library(ggeasy)
library(plotly)
library(dplyr)  
library(magrittr)
library(tidyverse)
library(janeaustenr)
library(widyr)
library(ggraph)

tweets_2019 <- tibble(line = 1, tweet = read.csv("2019.csv")[,c('tweet')]) %>% mutate(year = 2019)
tweets_2020 <- tibble(line = 1, tweet = read.csv("2020.csv")[,c('tweet')]) %>% mutate(year = 2020)
tweets_2021 <- tibble(line = 1, tweet = read.csv("2021.csv")[,c('tweet')]) %>% mutate(year = 2021)




tweets<-rbind(tweets_2019,tweets_2020,tweets_2021)


#Number of Tweets per year
plt <- tweets %>% 
  dplyr::count(year) %>% 
  ggplot(mapping = aes(x = year, y = n)) +
  theme_light() +
  geom_line() +
  xlab(label = 'Date') +
  ylab(label = NULL) +
  ggtitle(label = 'Number of Tweets per year')

plt %>% ggplotly()




clean.text = function(x)
{
  # convert to lower case
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  # some other cleaning text
  x = gsub('https://','',x)
  x = gsub('http://','',x)
  x = gsub('[^[:graph:]]', ' ',x)
  x = gsub('[[:punct:]]', '', x)
  x = gsub('[[:cntrl:]]', '', x)
  x = gsub('\\d+', '', x)
  x = str_replace_all(x,"[^[:graph:]]", " ")
  return(x)
}
tweets.txt <- str_replace_all(tweets$tweet,"[^[:graph:]]", " ")

cleanText <- clean.text(tweets.txt)



text_corpus <- Corpus(VectorSource(cleanText))
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removeWords,c("the","will","but","with","this","are","for","that","and","when","their","which"))
tdm <- TermDocumentMatrix(text_corpus)
tdm <- as.matrix(tdm)
tdm <- sort(rowSums(tdm), decreasing = TRUE)
tdm <- data.frame(word = names(tdm), freq = tdm)
set.seed(1234)
wordcloud(text_corpus, min.freq = 1, max.words = 200 , rot.per = 0.32,
          colors=brewer.pal(8, "Dark2"), random.color = T, random.order = F)




#Word frequency

tweets_words <- tweets %>%
  unnest_tokens(word, tweet) %>%
  count(year, word, sort = TRUE) 

stopwords_list <- c(stopwords(), "http" , "t.co", "amp", "re", "will", "can", "just", "am", "https", "yes", "no", "good", "bad", "it's")

tweets_words<- filter(tweets_words, !(word %in%  stopwords_list)) #Removal of stopwords

total_words <- tweets_words %>% 
  group_by(year) %>% 
  summarize(total = sum(n))

tweets_words <- left_join(tweets_words, total_words) %>% mutate(Frequency = n/total) %>% arrange(desc(year))
tweets_words



#Frequency plot
library(forcats)

tweets_words %>%
  group_by(year) %>%
  slice_max(Frequency, n = 15) %>%
  ungroup() %>%
  ggplot(aes(Frequency, fct_reorder(word, Frequency), fill = year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, ncol = 2, scales = "free") +
  labs(x = "Frequency", y = NULL)

# bigrams
tweets_bigrams <- tweets %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)

tweets_bigrams_separated <- tweets_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

tweets_bigrams_filtered <- tweets_bigrams_separated %>%
  filter(!word1 %in% stopwords_list) %>% 
  filter(!word2 %in% stopwords_list)

tweets_bigram_counts <- tweets_bigrams_filtered %>% 
  group_by(year) %>%
  count(word1, word2, sort = TRUE) %>% drop_na()


tweets_bigram_graph_2019 <- tweets_bigram_counts %>% subset(year == 2019,c(-1)) %>%
  filter(n > 10) %>%
  graph_from_data_frame()

tweets_bigram_graph_2020 <- tweets_bigram_counts %>% subset(year == 2020,c(-1)) %>%
  filter(n > 20) %>%
  graph_from_data_frame()

tweets_bigram_graph_2021 <- tweets_bigram_counts %>% subset(year == 2021,c(-1)) %>%
  filter(n > 20) %>%
  graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
par(mfrow=c(1,4))


library("ggpubr")


g_2019<-ggraph(tweets_bigram_graph_2019, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "yellow", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

g_2020<-ggraph(tweets_bigram_graph_2020, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "orange", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

g_2021<-ggraph(tweets_bigram_graph_2021, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "red", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


ggarrange(g_2019,g_2020,g_2021, lables=c("2019","2020","2021"),ncol=2,nrow=2)
