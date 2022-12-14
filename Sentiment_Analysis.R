getwd()
install.packages("textdata")
install.packages("fs")
install.packages("wordcloud2")
install.packages("RColorBrewer")
install.packages("forcats")
install.packages("ggrepel")
install.packages("tidylo")
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
library(topicmodels)

?janeaustenr

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text_df <- tibble(line = 1:4, text = text)
text_df



text_df %>%
  unnest_tokens(word, text)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()

View(original_books)

tidy_books <- original_books %>%
  unnest_tokens(word, text)

data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

View(stop_words)

tidy_books %>%
  count(word, sort = TRUE)


tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>%
  count(word, sort = TRUE)

tidy_hgwells %>%
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

#insert code to visualize top words in tidy_hgwells here

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)


frequency <- bind_rows(mutate(tidy_bronte, author = "Bront? Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, values_from = proportion) %>%
  pivot_longer(`Bront? Sisters`:`H.G. Wells`,
               names_to = "author", values_to = "proportion")

frequency
View(frequency)
View(bronte)

ggplot(frequency, aes(x = proportion, y = `Jane Austen`, 
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

cor.test(data = frequency[frequency$author == "Bront? Sisters",],
         ~ proportion + `Jane Austen`)
cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)

################### Sentiment Analysis #################3
afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")

View(afinn)
View(bing)
View(nrc)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
View(tidy_books)
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

hgwells_nrc_joy <- tidy_hgwells %>% 
  inner_join(nrc_joy) %>% 
  count(word, sort = T)

bronte_nrc_joy <- tidy_bronte %>%
  inner_join(nrc_joy) %>%
  count(word, sort = T)

nrc_sadness <- get_sentiments("nrc") %>%
  filter(sentiment == "sadness")

jane_nrc_sadness <- tidy_books %>%
  filter(book == "Sense & Sensibility") %>%
  inner_join(nrc_sadness) %>%
  count(word, sort = TRUE)

jane_nrc_sadness

bronte_nrc_sadness <- tidy_bronte %>%
  inner_join(nrc_sadness) %>%
  count(word, sort = T)

hgwells_nrc_sadness <- tidy_hgwells %>%
  inner_join(nrc_sadness) %>%
  count(word, sort = T)

jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

#Thursday 3rd March
pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

View(pride_prejudice)

afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

View(bing_and_nrc)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)

tidy_books %>%
  inner_join(get_sentiments("bing"))

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

nrc_word_counts <- tidy_books %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

nrc_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

custom_stop_words <- bind_rows(tibble(word = c("miss"),  
                                      lexicon = c("custom")), 
                               stop_words)
custom_stop_words <- bind_rows(tibble(word = c("mother"),  
                                      lexicon = c("custom")), 
                               custom_stop_words)
custom_stop_words <- bind_rows(tibble(word = c("sir"),  
                                      lexicon = c("custom")), 
                               custom_stop_words)
View(custom_stop_words)

#Show top 10 positive and negative words in tidy_books based on the AFINN lexicon
#Show top 10 positive and negative words in tidy_books based on the NRC lexicon
#use custom_stop_words to replace stop_words and re-plot the top 10 words for each sentiment
# in AFINN, NRC, and Bing

#Show top 10 positive and negative words in tidy_books based on the AFINN lexicon
tidy_books <- tidy_books %>%
  anti_join(stop_words)

afinn_word_counts <- tidy_books %>%
  inner_join(get_sentiments("afinn")) %>%
  mutate(sentiment = ifelse (value >=0, "positive","negative")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
  
afinn_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#Show top 10 positive and negative words in tidy_books based on the NRC lexicon
tidy_books <- tidy_books %>%
  anti_join(stop_words)

nrc_word_counts <- tidy_books %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

nrc_word_counts %>%
  group_by(sentiment) %>%
  filter(sentiment %in% c("positive", "negative")) %>% 
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)


#custom_stop_words for afinn

tidy_books <- tidy_books %>%
  anti_join(custom_stop_words)

afinn_word_counts <- tidy_books %>%
  inner_join(get_sentiments("afinn")) %>%
  mutate(sentiment = ifelse (value >=0, "positive","negative")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

afinn_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#custom_stop_words for nrc

tidy_books <- tidy_books %>%
  anti_join(custom_stop_words)

nrc_word_counts <- tidy_books %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

nrc_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#custom_stop_words for bing

tidy_books <- tidy_books %>%
  anti_join(custom_stop_words)

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#Thursday17
tidy_books %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,
                 ordered.colors = F, scale =c(2.5, .2)))


tidy_books %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,
                 random.order=F, scale =c(1, .5),
                 colors = brewer.pal(8, "Dark2")))


tidy_books %>%
  count(word, sort = T) %>%
  anti_join(stop_words) %>% filter(n > 25) %>%
  wordcloud2(size = 2,shape = 'star',
             color = "random-light",
             backgroundColor = "grey",
             minRotation = -pi/2,
             maxRotation = -pi/2,
             rotateRatio = .6,
  )



?RColorBrewer
??wordcloud2

#Thursday24
#TF-IDF
book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)

book_tf_idf

#idf and tf_idf 0 means it is a common word or insignificant



book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

gender_words <- tribble(
  ~Men, ~Women,
  "he", "she",
  "his", "her",
  "man", "woman",
  "men", "women",
  "boy", "girl",
  "himself", "herself"
)

ordered_words <- austen_books() %>% 
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>% 
  pull(word)

ordered_words

gender_words <- gender_words %>% 
  mutate(male_rank_log10 = match(Men, ordered_words) %>% log10(),
         female_rank_log10 = match(Women, ordered_words) %>% log10(),
         rank_diff_log10 = male_rank_log10 - female_rank_log10) %>% 
  pivot_longer(male_rank_log10:female_rank_log10, 
               names_to = "index", 
               values_to = "rank") %>% 
  mutate(label = if_else(index == "male_rank_log10", Men, Women)) %>%
  mutate(index = fct_recode(index,
                            "male" = "male_rank_log10",
                            "female" = "female_rank_log10"))

#find out how you would determine the topic of a book

limits <-  max(abs(gender_words$rank_diff_log10)) * c(-1, 1)

library(ggrepel)
gender_words %>%
  ggplot(aes(index, rank, group = Men)) + 
  geom_line(aes(color = rank_diff_log10), show.legend = FALSE) + 
  geom_text_repel(aes(label = label)) + 
  scale_y_reverse(label = function(x) 10 ^ x, breaks = scales::breaks_pretty(n = 3)) +
  scale_color_fermenter(type = "div", palette = "Spectral", limits = limits) + 
  theme_minimal()

facet_bar <- function(df, y, x, by, nrow = 2, ncol = 2, scales = "free") {
  mapping <- aes(y = reorder_within({{ y }}, {{ x }}, {{ by }}), 
                 x = {{ x }}, 
                 fill = {{ by }})
  
  facet <- facet_wrap(vars({{ by }}), 
                      nrow = nrow, 
                      ncol = ncol,
                      scales = scales) 
  
  ggplot(df, mapping = mapping) + 
    geom_col(show.legend = FALSE) + 
    scale_y_reordered() + 
    facet + 
    ylab("")
}

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>% 
  add_count(book, name = "total_words") %>%
  group_by(book, total_words) %>% 
  count(word, sort = TRUE) %>% 
  ungroup()

book_words <- book_words %>% 
  select(-total_words) %>%
  bind_tf_idf(term = word, document = book, n = n)

book_words %>% arrange(desc(tf_idf))

book_words %>% 
  group_by(book) %>% 
  top_n(10) %>%
  ungroup() %>%
  facet_bar(y = word, 
            x = tf_idf, 
            by = book, 
            nrow = 3)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>% 
  add_count(book, name = "total_words") %>%
  group_by(book, total_words) %>% 
  count(word, sort = TRUE) %>% 
  ungroup()

ggplot(book_words) + 
  geom_histogram(aes(n/total_words, fill = book), show.legend = FALSE) +
  xlim(NA, 0.0009) + 
  facet_wrap(~ book, nrow = 3, scales = "free_y")

tidy_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

bigram_counts <- tidy_bigrams %>%
  count(book, bigram, sort = TRUE)

bigram_counts

bigram_log_odds <- bigram_counts %>%
  bind_log_odds(book, bigram, n) 

bigram_log_odds %>%
  arrange(-log_odds_weighted)

bigram_log_odds %>%
  group_by(book) %>%
  slice_max(log_odds_weighted, n = 10) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, log_odds_weighted)) %>%
  ggplot(aes(log_odds_weighted, bigram, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(book), scales = "free") +
  labs(y = NULL)

View(austen_books())

#tidy_hgwells

write.csv(hgwells,"hgwells.csv",row.names = F)

hgwellsimported <- read.csv("hgwells.csv")


hgwells_bigrams <- hgwellsimported %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

hgwells_bigram_counts <- hgwells_bigrams %>%
  count(gutenberg_id, bigram, sort = TRUE)

hgwells_bigram_counts


