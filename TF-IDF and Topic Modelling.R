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
?janeaustenr

##################    TF-IDF  ###########
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

#####idf and tf_idf 0 means it is a common word or insignificant

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

###########hgwells_bigrams########
getwd()
write.csv(hgwells,"hgwells.csv",row.names = F)

hgwellsimported <- read.csv("hgwells.csv")


hgwells_bigrams <- hgwellsimported %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

hgwells_bigram_counts <- hgwells_bigrams %>%
  count(gutenberg_id, bigram, sort = TRUE)

hgwells_bigram_counts

hgwells_bigram_log_odds <- hgwells_bigram_counts %>%
  bind_log_odds(gutenberg_id, bigram, n) 

hgwells_bigram_log_odds %>%
  arrange(-log_odds_weighted)

hgwells_bigram_log_odds %>%
  group_by(gutenberg_id) %>%
  slice_max(log_odds_weighted, n = 10) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, log_odds_weighted)) %>%
  ggplot(aes(log_odds_weighted, bigram, fill = gutenberg_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(gutenberg_id), scales = "free") +
  labs(y = NULL)

##########bronte_bigrams########
bronteimported <- read.csv("bronte.csv")

bronte_bigrams <- bronteimported %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

bronte_bigram_counts <- bronte_bigrams %>%
  count(gutenberg_id, bigram, sort = TRUE)

bronte_bigram_counts

bronte_bigram_log_odds <- bronte_bigram_counts %>%
  bind_log_odds(gutenberg_id, bigram, n) 

bronte_bigram_log_odds %>%
  arrange(-log_odds_weighted)

bronte_bigram_log_odds %>%
  group_by(gutenberg_id) %>%
  slice_max(log_odds_weighted, n = 10) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, log_odds_weighted)) %>%
  ggplot(aes(log_odds_weighted, bigram, fill = gutenberg_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(gutenberg_id), scales = "free") +
  labs(y = NULL)

############Topic Modelling #########

austen_book_words <- book_words %>%
  mutate(word=str_extract(word,"[a-z']+")) %>%
  anti_join(stop_words)

austen_book_words

austen_dfm <- austen_book_words %>% 
  count(book, word, sort = T) %>% 
  cast_dfm(book, word, n)

austen_dfm

austen_topic_model <- stm(austen_dfm, K=6, init.type = "Spectral")

summary(austen_topic_model)

austen_beta <- tidy(austen_topic_model)

austen_beta

austen_beta %>%
  group_by(topic) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(term=reorder(term,beta)) %>%
  ggplot(aes(term,beta,fill=topic))+
           geom_col(show.legend = F)+
           facet_wrap(~topic, scales = "free")+
           coord_flip()

austen_gamma <- tidy(austen_topic_model, matrix = "gamma",
                     documents_names = rownames(austen_dfm))

ggplot(austen_gamma,aes(gamma,fill=as.factor(topic)))+
  geom_histogram(show.legend = F)+
  facet_wrap(~topic,ncol=3)

austen_topic_model2 <- stm(austen_dfm, K=12, init.type = "Spectral")
summary(austen_topic_model2)

austen_beta2 <- tidy(austen_topic_model2, matrix="beta")

austen_beta2 %>%
  group_by(topic) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(term=reorder(term,beta)) %>%
  ggplot(aes(term,beta,fill=topic))+
  geom_col(show.legend = F)+
  facet_wrap(~topic, scales = "free")+
  coord_flip()

austen_gamma2 <- tidy(austen_topic_model2, matrix = "gamma",
                     documents_names = rownames(austen_dfm))

ggplot(austen_gamma2,aes(gamma,fill=as.factor(topic)))+
  geom_histogram(show.legend = F)+
  facet_wrap(~topic,ncol=4)

###############  AssociatedPress Thursday7 ##############

data("AssociatedPress")
AssociatedPress
ap_lda <- LDA(AssociatedPress, k=2, control = list(seed=1234))

ap_topics <- tidy(ap_lda, matrix="beta")
ap_topics

ap_top_terms <- ap_topics %>% 
  group_by(topic) %>% 
  slice_max(beta, n=10) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

ap_top_terms

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide

beta_wide %>%
  group_by(direction = log_ratio > 0) %>%
  slice_max(abs(log_ratio), n = 10) %>% 
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(log_ratio, term)) +
  geom_col() +
  labs(x = "Log2 ratio of beta in topic 2 / topic 1", y = NULL)

#document-topic probabilities

ap_documents <- tidy(ap_lda, matrix="gamma")
ap_documents

tidy(AssociatedPress) %>% 
  filter(document==6) %>% 
  arrange(desc(count))

#https://www.gutenberg.org/ebooks/7074,https://www.gutenberg.org/ebooks/67789,https://www.gutenberg.org/ebooks/18857,https://www.gutenberg.org/ebooks/14012

###############Example##########

titles <- c("Beauty and the Beast", "The Queen's Advocate",
            "A Journey to the Centre of the Earth",
            "Ice-Caves of France and Switzerland")
my_books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

my_books_by_chapter <- my_books %>% 
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(
    text, regex("^chapter ", ignore_case = TRUE)
  ))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

my_books_by_chapter_by_word <- my_books_by_chapter %>%
  unnest_tokens(word, text)
