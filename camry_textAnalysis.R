library(tm)
library(quanteda)
library(readtext)
library(tidytext)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(igraph)
library(ggraph)

# Load the text documents into R
myCorpus <- readtext("C:/Users/cory/Desktop/text")
myCorpus

# Create a Corpus
myCorpus <- corpus(myCorpus)
summary(myCorpus)
# myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
# texts(myCorpus)[1] # read individual text

# create a Document/Frequency Matrix
myDFM <- dfm(myCorpus, tolower = T, remove = stopwords("english"), remove_punct = T)
myDFM[, 1:10]
head(dfm_sort(myDFM, margin = 'features'))
topfeatures(myDFM)
freq <- textstat_frequency(myDFM, group = myDFM@Dimnames$docs)

# plot frequency
freq %>% 
  group_by(group) %>%
  top_n(10, wt = frequency) %>%
  ggplot() +
  geom_point(aes(x = reorder(feature, frequency), y = frequency)) +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  labs(x = NULL, y = "Frequency")

# wordcloud with comparison by document
textplot_wordcloud(myDFM, comparison = T, max.words = 80)

# tokenize
myTokens <- tokens(myCorpus, what = 'word',
                   remove_punct = T,
                   remove_symbols = T
                   )

# locate keywords in context
kwic(myTokens, 'shift')

# expressions and fixed-length collactions
txStat <- textstat_collocations(myCorpus, 
                      method = "lambda", 
                      size = 3, 
                      min_count = 2,
                      tolower = TRUE)

txStat %>%
  arrange(desc(count)) -> txCounts
head(txCounts)

# 

# tidytext
docTidy = tidy(myCorpus)
str(docTidy)

docTidy %>%
  unnest_tokens(word, text, token = 'words') %>%
  anti_join(stop_words, by = 'word') -> tokenTidy

str(tokenTidy)

tokenTidy %>%
  count(word, sort = T)

tokenTidy %>%
  count(doc_id, word) %>%
  bind_tf_idf(word, doc_id, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) -> tfIDF
tfIDF

tfIDF %>%
  top_n(10) %>%
  ggplot(aes(word, tf_idf, fill = doc_id)) +
  geom_col() +
  coord_flip() +
  theme_few() +
  scale_fill_brewer(palette = 'Dark2')

tfIDF %>% 
  group_by(doc_id) %>% 
  top_n(5) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = doc_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~ doc_id, ncol = 2, scales = "free") +
  coord_flip()

# join to sentiment scores
tokenTidy %>%
  inner_join(get_sentiments("afinn")) -> senTidy

glimpse(senTidy)

senTidy %>% 
  group_by(doc_id) %>%
  summarize(sentiment_score = sum(score)) -> senScore

senScore$category <- c('Interior', 'Quality', 'Mileage', 'Transmission')

senScore$color <- ifelse(senScore$sentiment_score >= 0, 'darkgreen', 'red')
senScore <- transform(senScore, category = reorder(category, sentiment_score))

# plot sentiment
senScore %>%
  ggplot(aes(category, sentiment_score, fill = color)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(y = "Sentiment Score by Category",
       x = NULL) +
  theme_hc() +
  scale_fill_manual(values = c("darkgreen" = "darkgreen", "red" = "red")) +
  guides(fill = F)

# common word counts
tokenTidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() -> bingCounts

glimpse(bingCounts)

bingCounts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() +
  theme_few() +
  scale_fill_manual(values = c('red', 'darkgreen'))

docTidy %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) -> gramTidy
gramTidy

## bi-grams
gramTidy %>%
  count(bigram, sort = T)

# account for stop words
gramTidy %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) -> gramFilter

gramFilter %>%
  count(word1, sort = T)

gramFilter %>% 
  count(word1, word2, sort = TRUE) -> gramCount

gramCount 

gramFilter %>%
  unite(bigram, word1, word2, sep = " ") -> gramUnited

gramUnited

gramUnited %>%
  count(doc_id, bigram) %>%
  bind_tf_idf(bigram, doc_id, n) %>%
  arrange(desc(tf_idf)) -> gramDFIDF

gramDFIDF

gramCount %>%
  filter(n > 2) %>%
  graph_from_data_frame() -> gramGraph

gramGraph

ggraph(gramGraph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = -0.5, hjust = 0.5)

#### END

