

library(ggplot2)
library(igraph)
library(ggraph)
library(dplyr)
library(tidytext)
library(tidyr)
library(widyr)
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_51.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(rJava)
library(markdown)
library(mailR)
library(gmailr)
library(googlesheets)
suppressMessages(library(dplyr))


impact.ref = as_tibble(read.csv("impactRef_data/impactData_1-150.csv", stringsAsFactors = F))
impact.ref_title_data = data.frame(id = impact.ref$CaseStudy_URLExt, title = impact.ref$Title, stringsAsFactors = F)
impact.ref_abstract_data = data.frame(id = impact.ref$CaseStudy_URLExt, abstract = impact.ref$Abstract, stringsAsFactors = F)
impact.ref_subject_data = data.frame(id = impact.ref$CaseStudy_URLExt, subject = impact.ref$Subject, stringsAsFactors = F)%>%
  unnest(subject)
impact.ref_casestudy_data = data.frame(id = impact.ref$CaseStudy_URLExt, casestudy = impact.ref$Casestudy, stringsAsFactors = F)
impact.ref_impacttype_data = data.frame(id = impact.ref$CaseStudy_URLExt, impacttype = impact.ref$ImpactType, stringsAsFactors = F)%>%
  unnest(impacttype)

##---- Processing
my_stopwords <- data_frame(word = c(as.character(2000:2020),as.character(1:10), #numbers
                                    "impact", "engagement", "research", "new", "innovative", "development", "public", "key", "evidence", #buzz terms/padding
                                    "project", "university", "also", "work", "study", "studies", 
                                    "within", "including", "programme", "first", "british", "centre", "available",
                                    "professor", "dr", "well", "since", "case", "based", "working", "support", "group",
                                    "provide", "significant", "uk", "improving","include",  "contribute", "understanding",
                                    "source", "process", "related", "level",  #inconsequential terms
                                    "http", "—",  #metadata and grammar leftovers
                                    "used",  "use","developed", "led",  "perform", "provide", "inform",   #verbs
                                    "practice", "design", 
                                    "national", "international", "world", "london", #geography
                                    "social", "cultural", "arts", "people", "education", #subject areas captured in metadata
                                    "conference", "report", "intern", "published", "policy", "publications","publication", 
                                    "established","senior", "lecturer", "issues", "peer", "reviewed", "provided", "academic", "time", "attachment_data"  #not of interest
                                    
                                    
                                    
                                    
))

eResearch_words = c("digital",
                    "data", 
                    "computer", 
                    "computing", 
                    "computational",
                    "cloud",
                    "web", 
                    "internet",
                    "online",
                    "technology",
                    "modelling",
                    "website", 
                    "online",
                    "hpc",
                    "cyber", 
                    "database", 
                    "repository", 
                    "system")



impact.ref_impacttype <- impact.ref_impacttype_data %>%
  mutate(subject = toupper(impacttype))


#impact.ref_title = impact.ref_title_data %>%
 # unnest_tokens(sentence, title, token = "sentences")



impact.ref_abstract = impact.ref_abstract_data %>%
  unnest_tokens(sentence, abstract, token = "sentences")

eResearchAbstract_projs = data.frame(id = as.character(), 
                                     eResearchSentence = as.character(), 
                                     eResearchWord_assoc = as.character())
#i = 2
for (i in 1:length(eResearch_words)){
  eResearchAbstract_sentences = impact.ref_abstract[grepl(eResearch_words[i], impact.ref_abstract$sentence),]
  eResearchAbstract_sentences = cbind(eResearchAbstract_sentences, eResearch_words[i])
  eResearchAbstract_projs = rbind(eResearchAbstract_projs, eResearchAbstract_sentences)
}

#eResearchAbstract_projs$word = gsub("systems", "system", eResearchAbstract_projs$word)

eResearchAbstract_projs = eResearchAbstract_projs %>%
  unnest_tokens(word, sentence)%>%
  anti_join(stop_words) %>%
  anti_join(my_stopwords)

eResearch_words_df = as.data.frame(as.character(eResearch_words))
names(eResearch_words_df) = "word"

abstract_count <- eResearchAbstract_projs %>%
  anti_join(eResearch_words_df) %>%
  count(word, sort = T)

impactData_gs %>% gs_edit_cells(ws = "abstract_general_count", input = head(abstract_count, 20), anchor = "B23")

abstract_word_pairs <- eResearchAbstract_projs %>%
  pairwise_count(word, id, sort = T, upper = F)


abstract_word_pairs$item1 = gsub("systems", "system", abstract_word_pairs$item1, fixed = T)
abstract_word_pairs$item2 = gsub("systems", "system", abstract_word_pairs$item2, fixed = T)


impact.ref_casestudy = impact.ref_casestudy_data %>%
  unnest_tokens(sentence, casestudy, token = "sentences")

eResearchCaseStudy_projs = data.frame(id = as.character(), 
                                     eResearchSentence = as.character(), 
                                     eResearchWord_assoc = as.character())
#i = 2
for (i in 1:length(eResearch_words)){
  eResearchCaseStudy_sentences = impact.ref_casestudy[grepl(eResearch_words[i], impact.ref_casestudy$sentence),]
  eResearchCaseStudy_sentences = cbind(eResearchCaseStudy_sentences, eResearch_words[i])
  eResearchCaseStudy_projs = rbind(eResearchCaseStudy_projs, eResearchCaseStudy_sentences)
}

eResearchCaseStudy_projs = eResearchCaseStudy_projs %>%
  unnest_tokens(word, sentence)%>%
  anti_join(stop_words) %>%
  anti_join(my_stopwords)


casestudy_count <- eResearchCaseStudy_projs %>%
  anti_join(eResearch_words_df) %>%
  count(word, sort = T)

impactData_gs %>% gs_edit_cells(ws = "casestudy_general_count", input = head(casestudy_count, 20), anchor = "B23")

casestudy_word_pairs <- eResearchCaseStudy_projs %>%
  pairwise_count(word, id, sort = T, upper = F)


casestudy_word_pairs$item1 = gsub("systems", "system", casestudy_word_pairs$item1, fixed = T)
casestudy_word_pairs$item2 = gsub("systems", "system", casestudy_word_pairs$item2, fixed = T)



abstract_tfidf <- eResearchAbstract_projs %>%
  count(id, word, sort = T) %>%
  ungroup()%>%
  bind_tf_idf(word, id, n)

casestudy_tfidf <- eResearchCaseStudy_projs %>%
  count(id, word, sort = T) %>%
  ungroup()%>%
  bind_tf_idf(word, id, n)

## -----


abstract_word_pairs_cleaned = data.frame(as.character(), as.character(), as.character())
for (i in 1:nrow(abstract_word_pairs)){
  if(abstract_word_pairs$item1[i] %in% eResearch_words & abstract_word_pairs$item2[i] %in% eResearch_words){
  #abstract_word_pairs_cleaned = abstract_word_pairs_cleaned  
  } else {
    cleaned = abstract_word_pairs[i,]
    abstract_word_pairs_cleaned = rbind(abstract_word_pairs_cleaned, cleaned)
  }
}

names(abstract_word_pairs_cleaned) <- names(abstract_word_pairs)

set.seed(1234)
jpeg(filename = "impactRef_graphs/eResearch_WordClusters_Abstract.jpeg", quality = 100,width = 700, height = 700)
abstract_word_pairs_cleaned %>%
  filter(n >= 4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle("eResearch word clusters in Abstracts (Impact Ref UK 2014)")+
  theme_void()

#dev.print(jpeg, "impactRef_graphs/eResearch_WordClusters_Abstract.jpeg", width = 8, height = 6)
dev.off()

casestudy_word_pairs_n100 = casestudy_word_pairs %>%
  filter(n >= 30)
  
#i = 7
casestudy_word_pairs_n100cleaned = data.frame(as.character(), as.character(), as.character(), as.character())
for (i in 1:nrow(casestudy_word_pairs_n100)){
  if(casestudy_word_pairs_n100$item1[i] %in% eResearch_words & casestudy_word_pairs_n100$item2[i] %in% eResearch_words){
    #abstract_word_pairs_cleaned = abstract_word_pairs_cleaned  
    #print = "do nothing"
  } else {
    cleaned = cbind(casestudy_word_pairs_n100[i,])
    casestudy_word_pairs_n100cleaned = rbind(casestudy_word_pairs_n100cleaned, cleaned)
  }
}

names(casestudy_word_pairs_cleaned) <- names(casestudy_word_pairs) 

set.seed(1234)
jpeg(filename = "impactRef_graphs/eResearch_WordClusters_CaseStudy.jpeg", quality = 100,width = 700, height = 700)
casestudy_word_pairs_n100cleaned %>%
  filter(n >= 40) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "orange") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle("eResearch word clusters in Case Studies (Impact Ref UK 2014)")+
  theme_void()

#dev.print(jpeg, "impactRef_graphs/eResearch_WordClusters_Abstract.jpeg", width = 8, height = 6)
dev.off()

abstract_tfidf_impacttype <- full_join(abstract_tfidf, eResearchAbstract_projs[1:2], by = "id")
names(abstract_tfidf_impacttype)[7] <- "eResearchWord"

set.seed(1234)
jpeg(filename ="impactRef_graphs/eResearch_ImportantWords_Abstracts.jpeg", quality = 100,width = 700, height = 700)
abstract_tfidf_impacttype %>%
  filter(!near(tf, 1)) %>%
  filter(eResearchWord %in% eResearch_words) %>%
  arrange(desc(tf_idf)) %>%
  group_by(eResearchWord) %>%
  distinct(word, eResearchWord, .keep_all = TRUE) %>%
  top_n(8, tf_idf) %>% 
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(word, tf_idf, fill = eResearchWord)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~eResearchWord, ncol = 2, scales = "free") +
  coord_flip() +
  labs(title = "eResearch words in Abstracts and important word association",
       caption = "Impact Ref (UK)",
       x = NULL, y = "tf-idf")


dev.off()


casestudy_tfidf_impacttype <- full_join(casestudy_tfidf, eResearchCaseStudy_projs[1:2], by = "id")
names(casestudy_tfidf_impacttype)[7] <- "eResearchWord"

set.seed(1234)
jpeg(filename ="impactRef_graphs/eResearch_ImportantWords_CaseStudy.jpeg", quality = 100,width = 700, height = 700)
casestudy_tfidf_impacttype %>%
  filter(!near(tf, 1)) %>%
  filter(eResearchWord %in% eResearch_words) %>%
  arrange(desc(tf_idf)) %>%
  group_by(eResearchWord) %>%
  distinct(word, eResearchWord, .keep_all = TRUE) %>%
  top_n(8, tf_idf) %>% 
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(word, tf_idf, fill = eResearchWord)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~eResearchWord, ncol = 2, scales = "free") +
  coord_flip() +
  labs(title = "eResearch words in Case Studies and important word association",
       caption = "Impact Ref (UK)",
       x = NULL, y = "tf-idf")


dev.off()


## bigrams

impact_abstract_bigrams <- eResearchAbstract_projs %>%
  unnest_tokens(bigrams, word, token = "ngrams", n = 2)

abstract_bigrams <- impact_abstract_bigrams %>%
  count(bigrams, sort = T)

impactData_gs %>% gs_edit_cells(ws = "abstract_general_count", input = head(abstract_bigrams, 20), anchor = "I23")


bigrams_separated <- impact_abstract_bigrams %>%
  separate(bigrams, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>%
  count(id, bigram) %>%
  bind_tf_idf(bigram, id, n) %>%
  arrange(desc(tf_idf))

bigram_tfidf_impacttype <- full_join(bigram_tf_idf, eResearchAbstract_projs[1:2], by = "id")
names(bigram_tfidf_impacttype)[7] <- "eResearchWord"

set.seed(1234)
jpeg(filename = "impactRef_graphs/eResearch_Bigrams_Abstracts.jpeg", quality = 100, width = 700, height = 700)
bigram_tfidf_impacttype %>%
  filter(!near(tf, 1)) %>%
  filter(eResearchWord %in% eResearch_words) %>%
  arrange(desc(tf_idf)) %>%
  group_by(eResearchWord) %>%
  distinct(bigram, eResearchWord, .keep_all = TRUE) %>%
  top_n(8, tf_idf) %>% 
  ungroup() %>%
  mutate(word = factor(bigram, levels = rev(unique(bigram)))) %>%
  ggplot(aes(bigram, tf_idf, fill = eResearchWord)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~eResearchWord, ncol = 3, scales = "free") +
  coord_flip() +
  labs(title = "eResearch bigrams in Abstracts",
       caption = "Impact Ref (UK)",
       x = NULL, y = "tf-idf")


dev.off()

impact_casestudy_bigrams <- eResearchCaseStudy_projs %>%
  unnest_tokens(bigrams, word, token = "ngrams", n = 2)

casestudy_bigrams <- impact_casestudy_bigrams %>%
  count(bigrams, sort = T)

impactData_gs %>% gs_edit_cells(ws = "casestudy_general_count", input = head(casestudy_bigrams, 20), anchor = "I23")

