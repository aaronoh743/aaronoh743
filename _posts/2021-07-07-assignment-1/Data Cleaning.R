
#Preparing Packages
packages = c('tidytext','widyr','wordcloud',
             'DT','ggwordcloud','textplot',
             'lubridate', 'hms', 'tidyverse',
             'tidygraph', 'ggraph', 'igraph','stringr','tidyr', 'ggplot2',
             'visNetwork', 'topicmodels')

for (p in packages) {
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}


list_of_files <- "C:/Users/aaron/Documents/Aaron Documents/Semester 3/VA/Assignment/Raw Data/News Articles"

read_folder <- function (infolder) {
  tibble(file = dir(infolder,
                    full.names = TRUE)) %>%
    mutate(text = map(file,
                      read_lines)) %>%
    transmute(id=basename(file),
              text) %>%
    unnest(text)
}



raw_text <- tibble(folder =
                     dir(list_of_files,
                         full.names=TRUE)) %>%
  mutate(folder_out = map(folder,
                          read_folder)) %>%
  unnest(cols = c(folder_out)) %>%
  transmute(newsgroup = basename(folder),
            id, text)
write_rds(raw_text, "C:/Users/aaron/Documents/Aaron Documents/Semester 3/VA/Assignment/Raw Data/News Articles/allfiles.rds")

for (col in colnames(raw_text)){
  Encoding(raw_text[[col]]) <- "latin1"}

raw_text %>% 
  group_by(newsgroup) %>%
  summarize(messages = n_distinct(id)) %>%
  ggplot(aes(messages, newsgroup)) +
  geom_col(fill = "lightblue") +
  labs(y=NULL)

raw_text<- raw_text %>%
  #filter(raw_text$text != "") %>%
  filter(raw_text$text != " ")


sep_text <- raw_text %>%
  separate(text, c('Type', 'entry'), "(?<=LOCATION):|(?<=PUBLISHED):|(?<=SOURCE):|(?<=TITLE):", remove=FALSE)


sep_text$entry[is.na(sep_text$entry)] <- sep_text$Type[is.na(sep_text$entry)] 
sep_text <- sep_text %>%
  mutate(Type = ifelse(entry==Type, "TEXT", Type))

sep_text <- sep_text %>%
  group_by(newsgroup,id,Type) %>%
  summarise (entry = paste(entry, collapse = " "))

sep_text <- sep_text %>%
  unite("newsgroup_id", newsgroup:id, sep = "_")

wide_text <- sep_text[,c("newsgroup_id","Type","entry")]
wide_text <- pivot_wider(wide_text,
                         names_from = Type,
                         values_from = entry)

wide_text_cleaned <- data.frame(lapply(wide_text,trimws))

wide_text_cleaned$PUBLISHED <- parse_date_time(wide_text_cleaned$PUBLISHED, orders=c("ymd", "dmy HM","mdy","dmy"))
wide_text_cleaned <- data.frame(wide_text_cleaned)

#write.xlsx(wide_text_cleaned , file = "test.xlsx")



#Visualising Networks to derive primary and derivative source

usenet_words <- wide_text_cleaned %>%
  unnest_tokens(word, TEXT) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

words_by_newsgroup <- usenet_words %>%
  count(newsgroup_id, word, sort=TRUE) %>%
  separate (newsgroup_id, c('newsgroup','id'), '_', remove=FALSE)

news_cors <- words_by_newsgroup %>%
  pairwise_cor(newsgroup_id,
               word,
               n,
               sort=TRUE)

news_cors_aggregated <- news_cors %>%
    rename(
    from = item1,
    to = item2)

news_cors_node <- wide_text_cleaned %>%
  rename(
    id = newsgroup_id
  )


news_cors_aggregated_joined <- news_cors_aggregated %>%
  inner_join(news_cors_node, by = c("from"= "id")) %>%
  inner_join(news_cors_node, by = c("to" = "id"))

news_cors_aggregated_joined <- news_cors_aggregated_joined[c("from","to","correlation","PUBLISHED.x","PUBLISHED.y")]

news_cors_aggregated_joined_sort <- news_cors_aggregated_joined %>%
  arrange(desc(correlation), PUBLISHED.x)

ind <- seq(2,nrow(news_cors_aggregated_joined_sort),by=2)
news_cors_aggregated_joined_sort_noduplicates <- news_cors_aggregated_joined_sort[ind,]

news_cors_aggregated_joined_sort_noduplicates_50 <- news_cors_aggregated_joined_sort_noduplicates %>%
  filter(correlation > 0.80)

news_cors_node_filtered <- news_cors_node[(news_cors_node$id %in% news_cors_aggregated_joined_sort_noduplicates_50$from)| (news_cors_node$id %in% news_cors_aggregated_joined_sort_noduplicates_50$to),]

visNetwork(news_cors_node_filtered,
           news_cors_aggregated_joined_sort_noduplicates_50) %>%
  visEdges(arrows="to") %>%
  visIgraphLayout(layout = "layout_with_fr") %>%
  visOptions(highlightNearest = list(enabled = TRUE),
             nodesIdSelection = TRUE,
             selectedBy = "SOURCE") %>%
  visLegend() %>%
  visLayout(randomSeed = 124)


#Visualising Biases - LDA

packages_biases = c('openxlsx','rJava', 'NLP','openNLP','RWeka')

for (p in packages_biases) {
  if(!require(p, character.only = T)){
    install.packages_biases(p)
  }
  library(p, character.only = T)
}

word_sci_newsgroups <- usenet_words %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup()%>%
  filter(word_total > 50)



word_sci_dtm <- word_sci_newsgroups %>%
  unite(document, SOURCE, newsgroup_id) %>%
  count(document,word) %>%
  cast_dtm(document, word, n)

word_lda <- LDA(word_sci_dtm, k=4, control = list(seed = 2016))  

word_lda %>%
  tidy() %>%
  group_by(topic) %>%
  slice_max(beta, n = 8) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()