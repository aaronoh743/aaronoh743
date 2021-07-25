
#Preparing Packages
packages = c('tidytext','widyr','wordcloud',
             'DT','ggwordcloud','textplot',
             'lubridate', 'hms', 'tidyverse',
             'tidygraph', 'ggraph', 'igraph','stringr','tidyr', 'ggplot2',
             'visNetwork', 'topicmodels', 'crosstalk', 'utf8','ldatuning', 'topicmodels')

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

#n-grams
trigrams_news <- wide_text_cleaned %>%
  unnest_tokens(trigram, TEXT , token = "ngrams", n = 3)

trigrams_news <- trigrams_news %>%
  separate(trigram, into = c("first","second","third"), sep = " ", remove = FALSE) %>%
  anti_join(stop_words, by = c("first" = "word")) %>%
  anti_join(stop_words, by = c("second" = "word")) %>%
  anti_join(stop_words, by = c("third" = "word")) %>%
  filter(str_detect(first, "[a-z]") &
           str_detect(second, "[a-z]") &
           str_detect(third, "[a-z]"))
  

words_by_newsgroup <- trigrams_news %>%
  count(newsgroup_id, trigram, sort=TRUE) %>%
  separate (newsgroup_id, c('newsgroup','id'), '_', remove=FALSE)

news_cors <- words_by_newsgroup %>%
  pairwise_cor(newsgroup_id,
               trigram,
               n,
               sort=TRUE)


news_cors_aggregated <- news_cors %>%
    rename(
    to = item1,
    from = item2)

news_cors_node <- wide_text_cleaned %>%
  rename(
    id = newsgroup_id,
    group = SOURCE
  )


news_cors_aggregated_joined <- news_cors_aggregated %>%
  inner_join(news_cors_node, by = c("to"= "id")) %>%
  inner_join(news_cors_node, by = c("from" = "id"))

news_cors_aggregated_joined <- news_cors_aggregated_joined[c("from","to","correlation","PUBLISHED.x","PUBLISHED.y")]

news_cors_aggregated_joined_sort <- news_cors_aggregated_joined %>%
  arrange(desc(correlation), PUBLISHED.x)

ind <- seq(1,nrow(news_cors_aggregated_joined_sort),by=2)
news_cors_aggregated_joined_sort_noduplicates <- news_cors_aggregated_joined_sort[ind,]

news_cors_aggregated_joined_sort_noduplicates_50 <- news_cors_aggregated_joined_sort_noduplicates %>%
  filter(correlation >= 0.40)


news_cors_aggregated_joined_sort_noduplicates_50_top_10 <- news_cors_aggregated_joined_sort_noduplicates_50 %>%
  count(from) %>%
  top_n(30)

news_cors_aggregated_joined_sort_noduplicates_final <- news_cors_aggregated_joined_sort_noduplicates_50 %>%
  inner_join(news_cors_aggregated_joined_sort_noduplicates_50_top_10, by = "from")


news_cors_node_filtered <- news_cors_node[(news_cors_node$id %in% news_cors_aggregated_joined_sort_noduplicates_final$from)| (news_cors_node$id %in% news_cors_aggregated_joined_sort_noduplicates_final$to),]

visNetwork(news_cors_node_filtered,
           news_cors_aggregated_joined_sort_noduplicates_final) %>%
  visEdges(arrows="to") %>%
  visIgraphLayout(layout = "layout_with_fr") %>%
  visOptions(highlightNearest = list(enabled = TRUE),
             nodesIdSelection = TRUE,
             selectedBy = "group") %>%
  visLayout(randomSeed = 123)



#Visualising Biases - LDA



tidy_source_text <- wide_text_cleaned %>%
  unnest_tokens(word,TEXT) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]")) %>%
  group_by(word) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count>30)

tidy_source_dtm <- tidy_source_text %>%
  unite(document,SOURCE,newsgroup_id) %>%
  count(document, word) %>%
  cast_dtm(document, word, n)


tidy_source_lda <- LDA(tidy_source_dtm, k=8, control=list(seed=2020))

set.seed(2020)
tidy_source_lda  %>%
  tidy() %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

result <- FindTopicsNumber(
  tidy_source_dtm,
  topics = seq(from = 2, to =15, by =1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 20202),
  mc.cores = 2L,
  verbose = TRUE
  
)

FindTopicsNumber_plot(result)


tidy_source_lda %>%
  tidy(matrix = "gamma") %>%
  separate(document, c("SOURCE","newsgroup_id"), sep = "_") %>%
  mutate(SOURCE = reorder(SOURCE, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ SOURCE) +
  labs(x = "Topic",
       y = "# of messages where this was the highest % topic")
  








#Question 3
#Reading in Datasource

employees_records <- readxl::read_xlsx("C:/Users/aaron/Documents/Aaron Documents/Semester 3/VA/Assignment/Raw Data/EmployeeRecords.xlsx")
employees_records_cleaned <- employees_records %>%
  unite(fullname, FirstName, LastName, sep = " ", remove=FALSE) %>%
  mutate_if(is.character, utf8_encode)

employees_records_cleaned$fullname <- trimws(employees_records_cleaned$fullname, which = c("both"))

email_records <- read_csv("C:/Users/aaron/Documents/Aaron Documents/Semester 3/VA/Assignment/Raw Data/email headers.csv")
email_records_cleaned <- email_records %>%
  mutate(To = str_remove_all(To,"@gastech.com.kronos|@gastech.com.tethys")) %>%
  mutate(To = str_replace_all(To, "[.]", " ")) %>%
  mutate(From = str_remove_all(From,"@gastech.com.kronos|@gastech.com.tethys")) %>%
  mutate(From = str_replace_all(From, "[.]", " ")) %>%
  mutate(Date = parse_date_time(x = Date, orders =c("%m%d%y %H%M","%m%d%y"))) %>%
  mutate_if(is.character, utf8_encode)

email_records_cleaned2 <- strsplit(email_records_cleaned$To, split=",")
email_records_cleaned2 <- data.frame(From = rep(email_records_cleaned$From, sapply(email_records_cleaned2, length)), To = unlist(email_records_cleaned2), Date = rep(email_records_cleaned$Date, sapply(email_records_cleaned2, length)),Subject =  rep(email_records_cleaned$Subject, sapply(email_records_cleaned2, length)))
email_records_cleaned2$To <- trimws(email_records_cleaned2$To, which = c("both"))
email_records_cleaned2$From <- trimws(email_records_cleaned2$From, which = c("both"))
email_records_cleaned2_rename$Subject <- gsub("[[:punct:]]", "", email_records_cleaned2_rename$Subject)

#Tidying node list
employees_records_cleaned_rename <- employees_records_cleaned %>%
  rename(id = fullname) %>%
  rename (group = CurrentEmploymentType) %>%
  arrange(id)

#Tidying edge list
email_records_cleaned2_rename <- email_records_cleaned2 %>%
  rename (from = From) %>%
  rename (to = To) %>%
  mutate(Subject = str_remove_all(Subject,"RE: "))




email_records_cleaned2_rename_aggregated <- email_records_cleaned2_rename %>%
  group_by (from, to, Subject) %>%
  summarise(Weight = n()) %>%
  filter(from!=to) %>%
  filter(to!=from) %>%
  filter(Weight > 1) %>%
  ungroup()



#High Level Network Analysis
visNetwork(employees_records_cleaned_rename,
           email_records_cleaned2_rename_aggregated) %>%
  visEdges(arrows="to") %>%
  visIgraphLayout(layout = "layout_with_fr") %>%
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = TRUE,selectedBy = "group", collapse=TRUE) %>%
  visLayout(randomSeed = 123)


#Deeper Analysis

email_records_analysis <- email_records_cleaned2_rename %>%
  group_by(from,to,Subject) %>%
  summarise(count=n()) %>%
  filter(from!=to) %>%
  arrange(desc(count))
  ungroup()

datatable(email_records_analysis, filter='top', options = list(
  autoWidth = TRUE
))
  
  
  
  ggplot(aes(reorder_within(Subject,count,from),count,
             fill = from)) +
    geom_col(alpha =0.8, show.legend = FALSE) + 
    scale_x_reordered() +
    coord_flip() +
    facet_wrap(~from, scales = "free") +
    scale_y_continuous(expand = c(0,0))
  
email_records_analysis

email_records_analysis_cleaned <- email_records_analysis %>%
  count(from,to, sort=TRUE) %>%
  group_by(from) %>%
  top_n(20) %>%
  filter(from == c("Linnea Bergen")) %>%
  ungroup () %>%
  ggplot(aes(reorder_within(word,count,from),count,
             fill = from
             )) +
  geom_col(alpha =0.8, show.legend = FALSE) + 
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~from, scales = "free") +
  scale_y_continuous(expand = c(0,0))

email_records_analysis_cleaned
