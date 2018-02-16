## ---- webscraper for in

library(ggplot2)
library(igraph)
library(ggraph)
library(dplyr)
library(tidytext)
library(tidyr)
library(widyr)
# send email
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_51.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(rJava)
library(markdown)
library(mailR)
library(gmailr)
library(googlesheets)
suppressMessages(library(dplyr))


## preparing Google sheets

impactData_gs = gs_title("ImpactData_R")

impact.ref = as_tibble(read.csv("impactRef_data/impactData_1-150.csv", stringsAsFactors = F))


impact.ref_title = data.frame(id = impact.ref$CaseStudy_URLExt, title = impact.ref$Title, stringsAsFactors = F)
impact.ref_abstract = data.frame(id = impact.ref$CaseStudy_URLExt, abstract = impact.ref$Abstract, stringsAsFactors = F)
impact.ref_subject = data.frame(id = impact.ref$CaseStudy_URLExt, subject = impact.ref$Subject, stringsAsFactors = F)%>%
  unnest(subject)
impact.ref_casestudy = data.frame(id = impact.ref$CaseStudy_URLExt, casestudy = impact.ref$Casestudy, stringsAsFactors = F)
impact.ref_impacttype = data.frame(id = impact.ref$CaseStudy_URLExt, impacttype = impact.ref$ImpactType, stringsAsFactors = F)%>%
  unnest(impacttype)

##---- Processing
my_stopwords <- data_frame(word = c(as.character(2000:2020),as.character(1:10), #numbers
                                    "impact", "impacts", "engagement", "research", "new", "innovative", "development", "public", "key", "evidence", #buzz terms/padding
                                    "project", "university", "also", "work", "study", "studies", 
                                    "within", "including", "programme", "first", "british", "centre", "available",
                                    "professor", "dr", "well", "since", "case", "based", "working", "support", "group",
                                    "provide", "significant", "uk", "improving","include",  "contribute", "understanding", "involved", "included",
                                    "source", "process", "related", "level", "finding", "findings",  #inconsequential terms
                                     "http", "—",  #metadata and grammar leftovers
                                    "used",  "use","developed", "led",  "perform", "provide", "inform",   #verbs
                                    "practice", "design", 
                                    "national", "international", "world", "london", #geography
                                    #"social", "cultural", "arts", "people", "education", #subject areas captured in metadata
                                    "conference", "report", "intern", "published", "policy", "publications","publication", 
                                    "established","senior", "lecturer", "phd", "researchers", "student", "issues", "peer", 
                                    "reviewed", "provided", "academic", "time", "funded", "fund", "attachment_data"  #not of interest
                                     
                                      
                                    
                                    
))


impact.ref_title = impact.ref_title %>%
  unnest_tokens(word, title)%>%
  anti_join(stop_words) %>%
  anti_join(my_stopwords) 

impact.ref_abstract = impact.ref_abstract %>%
  unnest_tokens(word, abstract)%>%
  anti_join(stop_words) %>%
  anti_join(my_stopwords)

impact.ref_casestudy = impact.ref_casestudy %>%
  unnest_tokens(word, casestudy)%>%
  anti_join(stop_words) %>%
  anti_join(my_stopwords)


impact.ref_impacttype <- impact.ref_impacttype %>%
  mutate(subject = toupper(impacttype))

##----Exploring
title_count <- impact.ref_title %>%
  count(word, sort = T)

impactData_gs %>% gs_edit_cells(ws = "title_general_count", input = head(title_count, 20), anchor = "B1")

eR_title_match = pmatch(eResearch_words, title_count$word)
eR_title_match = eR_title_match[!is.na(eR_title_match)]
eR_title_match = eR_title_match[order(eR_title_match)]

impactData_gs %>% gs_edit_cells(ws = "title_general_count", input = eR_title_match, anchor = "L2")

eR_title_match = title_count[eR_title_match,]
impactData_gs %>% gs_edit_cells(ws = "title_general_count", input = eR_title_match, anchor = "M1")


abstract_count <- impact.ref_abstract %>%
  count(word, sort = T)

impactData_gs %>% gs_edit_cells(ws = "abstract_general_count", input = head(abstract_count, 20), anchor = "B1")


eR_abstract_match = pmatch(eResearch_words, abstract_count$word)
eR_abstract_match = eR_abstract_match[!is.na(eR_abstract_match)]
eR_abstract_match = eR_abstract_match[order(eR_abstract_match)]

impactData_gs %>% gs_edit_cells(ws = "abstract_general_count", input = eR_abstract_match, anchor = "L2")

eR_abstract_match = abstract_count[eR_abstract_match,]
impactData_gs %>% gs_edit_cells(ws = "abstract_general_count", input = eR_abstract_match, anchor = "M1")

casestudy_count = impact.ref_casestudy %>%
  count(word, sort = T)

impactData_gs %>% gs_edit_cells(ws = "casestudy_general_count", input = head(casestudy_count, 20), anchor = "B1")

eR_casestudy_match = pmatch(eResearch_words, casestudy_count$word)
eR_casestudy_match = eR_casestudy_match[!is.na(eR_casestudy_match)]
eR_casestudy_match = eR_casestudy_match[order(eR_casestudy_match)]


impactData_gs %>% gs_edit_cells(ws = "casestudy_general_count", input = eR_casestudy_match, anchor = "L2")

eR_casestudy_match = casestudy_count[eR_casestudy_match,]
impactData_gs %>% gs_edit_cells(ws = "casestudy_general_count", input = eR_casestudy_match, anchor = "M1")


count_subject <- impact.ref_subject %>%
  group_by(subject) %>%
  count(sort = T)

## ---- Exploring paired words

title_word_pairs <- impact.ref_title %>%
  pairwise_count(word, id, sort = T, upper = F)


impactData_gs %>% gs_edit_cells(ws = "title_general_count", input = head(title_word_pairs, 20), anchor = "D1")

abstract_word_pairs <- impact.ref_abstract %>%
  pairwise_count(word, id, sort = T, upper = F)

impactData_gs %>% gs_edit_cells(ws = "abstract_general_count", input = head(abstract_word_pairs, 20), anchor = "D1")

casestudy_word_pairs <- impact.ref_casestudy %>%
  pairwise_count(word, id, sort = T, upper = F)

impactData_gs %>% gs_edit_cells(ws = "casestudy_general_count", input = head(casestudy_word_pairs, 20), anchor = "D1")

## --- tf-idf

title_tfidf <- impact.ref_title %>%
  count(id, word, sort = T) %>%
  ungroup()%>%
  bind_tf_idf(word, id, n)

title_tfidf %>%
  arrange(-tf_idf)

abstract_tfidf <- impact.ref_abstract %>%
  count(id, word, sort = T) %>%
  ungroup()%>%
  bind_tf_idf(word, id, n)

abstract_tfidf %>%
  arrange(-tf_idf)

casestudy_tfidf <- impact.ref_casestudy %>%
  count(id, word, sort = T) %>%
  ungroup()%>%
  bind_tf_idf(word, id, n)

casestudy_tfidf %>%
  arrange(-tf_idf)



## ---- Graphs

dir.create("impactRef_graphs")

library(ggplot2)
library(igraph)
library(ggraph)

set.seed(1234)
jpeg("impactRef_graphs/WordClusters_Titles.jpeg", quality = 100,width = 700, height = 700)
title_word_pairs %>%
  filter(n >= 3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle("Wordcloud for Titles (Impact Ref UK)")+
  theme_void()

dev.off()

set.seed(1234)
jpeg(filename = "impactRef_graphs/WordClusters_Abstract.jpeg", quality = 100,width = 700, height = 700)
abstract_word_pairs %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle("Word clusters in Abstracts (Impact Ref UK)")+
  theme_void()

dev.off()

set.seed(1234)
jpeg(filename = "impactRef_graphs/WordClusters_CaseStudy.jpeg", quality = 100,width = 1000, height = 1000)
casestudy_word_pairs %>%
  filter(n >= 200) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "orange") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines"), check_overlap = T) +
  ggtitle("Word clusters in Case Studies (Impact Ref UK)")+
  theme_void()

dev.off()



abstract_tfidf_impacttype <- full_join(abstract_tfidf, impact.ref_impacttype, by = "id")

set.seed(1234)
abstract_tfidf_impacttype %>%
  filter(!near(tf, 1)) %>%
  filter(subject %in% c("TECHNOLOGICAL", "CULTURAL", "SOCIETAL", 
                        "ENVIRONMENTAL", "POLITICAL", "HEALTH", 
                        "LEGAL", "ECONOMIC")) %>%
  arrange(desc(tf_idf)) %>%
  group_by(impacttype) %>%
  distinct(word, impacttype, .keep_all = TRUE) %>%
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(word, tf_idf, fill = impacttype)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~impacttype, ncol = 3, scales = "free") +
  coord_flip() +
  labs(title = "Important words in Abstracts",
       caption = "Impact Ref (UK)",
       x = NULL, y = "tf-idf")

dev.print(pdf, "impactRef_graphs/ImportantWords_Abstracts.pdf", width = 8, height = 6)
dev.off()

casestudy_tfidf_impacttype <- full_join(casestudy_tfidf, impact.ref_impacttype, by = "id")
set.seed(1234)
casestudy_tfidf_impacttype %>%
  filter(!near(tf, 1)) %>%
  filter(subject %in% c("TECHNOLOGICAL", "CULTURAL", "SOCIETAL", 
                        "ENVIRONMENTAL", "POLITICAL", "HEALTH", 
                        "LEGAL", "ECONOMIC")) %>%
  arrange(desc(tf_idf)) %>%
  group_by(impacttype) %>%
  distinct(word, impacttype, .keep_all = TRUE) %>%
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(word, tf_idf, fill = impacttype)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~impacttype, ncol = 3, scales = "free") +
  coord_flip() +
  labs(title = "Highest tf-idf words in Impact Ref (UK) case studies",
       caption = "",
       x = NULL, y = "tf-idf")
dev.print(pdf, "impactRef_graphs/ImportantWords_CaseStudies.pdf", width = 8, height = 6)
dev.off()
