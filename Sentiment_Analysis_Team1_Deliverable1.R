#1
#Team Name : Team 1 Text Mining and Sentiment Analysis
#Team _ members : Eniola Webster-Esho,Alexius Fly & Ozioma Aguegboh

getwd()
install.packages("textdata")
install.packages("fs")
install.packages("wordcloud2")
install.packages("RColorBrewer")
install.packages("forcats")
install.packages("ggrepel")
install.packages("tidylo")
install.packages("stm")
install.packages("quanteda")
install.packages("reshape2")
install.packages("topicmodels")

library(tidylo)
library(ggrepel)
library(forcats)
library(wordcloud2)
library(RColorBrewer)
library(wordcloud)
library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(gutenbergr)
library(tidyr)
library(textdata)
library(scales)
library(fs)
library(stm)
library(quanteda)
library(reshape2)
library(topicmodels)
library(streamR)

#2
nba<- list.files(pattern = "nba.json", recursive = TRUE)
all_tweets_df <- data.frame()
list_of_dfs <- list()
i<- 1
while(i<=length(nba)) {
  list_of_dfs[[i]]<- parseTweets(nba[i])
  i=i+1
}

all_tweets_df <- do.call(rbind, list_of_dfs)



#3 & 4
clean_tweets <- all_tweets_df %>% mutate(text = gsub("https://*|http://*","",text),
                                         created_at = as.POSIXct(created_at, format = "%a %b %d %H:%M:%S +0000 %Y")
)


#5
data(stop_words)

nba_unique_words <- clean_tweets %>%
  dplyr::select(text) %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("rt", "t.co"))

#6a Find Top 20 unique words

nba_unique_words %>%
  count(word, sort=TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Top 20 unique words found nba in tweets")

#6b

nba_nonASCII_words <- nba_unique_words %>%
  
  mutate(word = gsub("[^\x01-\x7F]","",word))

#6c

nba_nonASCII_words %>%
  
  count(word, sort=TRUE) %>%
  
  top_n(20) %>%
  
  mutate(word = reorder(word, n)) %>%
  
  ggplot(aes(x = word, y = n)) +
  
  geom_col() +
  
  xlab(NULL) +
  
  coord_flip() +
  
  labs(x = "Count",
       
       y = "Unique words",
       
       title = "Top 20 non-ASCII rows found in nba tweets")



#7a

nba_unique_words %>%
  anti_join(stop_words) %>%
  count(word) %>%
  filter(n > 100) %>%
  wordcloud2(size = 5)


#7b

nba_nonASCII_words %>%
  count(word, sort = T) %>%
  anti_join(stop_words) %>%
  filter(n > 100) %>%
  wordcloud2(size = 5)


#7c

nba_nonASCII_words %>%
  count(word, sort = T) %>%
  anti_join(stop_words) %>% filter(n > 100) %>%
  wordcloud2(size = 5, color = "random-light",
             backgroundColor = "azure3"
  )

??wordcloud2


#8

afinn <- get_sentiments("afinn")

bing <- get_sentiments("bing")

nrc <- get_sentiments("nrc")



#Show top 15 positive and negative words based on the AFINN sentiment

nba_afinn_word_counts <- nba_unique_words %>%
  
  inner_join(get_sentiments("afinn")) %>%
  
  mutate(sentiment = ifelse (value >=0, "positive","negative")) %>%
  
  count(word, sentiment, sort = TRUE) %>%
  
  ungroup()



nba_afinn_word_counts %>%
  
  group_by(sentiment) %>%
  
  slice_max(n, n = 15) %>%
  
  ungroup() %>%
  
  mutate(word = reorder(word, n)) %>%
  
  ggplot(aes(n, word, fill = sentiment)) +
  
  geom_col(show.legend = FALSE) +
  
  facet_wrap(~sentiment, scales = "free_y") +
  
  labs(x = "Contribution to sentiment",
       
       y = NULL)



#9

#Show top 15 positive and negative words based on the bing sentiment

nba_bing_word_counts <- nba_unique_words %>%
  
  inner_join(get_sentiments("bing")) %>%
  
  count(word, sentiment, sort = TRUE) %>%
  
  ungroup()



nba_bing_word_counts %>%
  
  group_by(sentiment) %>%
  
  filter(sentiment %in% c("positive", "negative")) %>%
  
  slice_max(n, n = 15) %>%
  
  ungroup() %>%
  
  mutate(word = reorder(word, n)) %>%
  
  ggplot(aes(n, word, fill = sentiment)) +
  
  geom_col(show.legend = FALSE) +
  
  facet_wrap(~sentiment, scales = "free_y") +
  
  labs(x = "Contribution to sentiment",
       
       y = NULL)



#10

#Show top 10 emotion based on the NRC sentiment

nba_nrc_word_counts <- nba_unique_words %>%
  
  inner_join(get_sentiments("nrc")) %>%
  
  count(word, sentiment, sort = TRUE) %>%
  
  ungroup()



nba_nrc_word_counts %>%
  
  group_by(sentiment) %>%
  
  slice_max(n, n = 10) %>%
  
  ungroup() %>%
  
  mutate(word = reorder(word, n)) %>%
  
  ggplot(aes(n, word, fill = sentiment)) +
  
  geom_col(show.legend = FALSE) +
  
  facet_wrap(~sentiment, scales = "free_y") +
  
  labs(x = "Contribution to sentiment",
       
       y = NULL)

