# NLB articles 
# TITLE: TOPIC ####

# LOAD DATA AND PACKAGES #####
rm(list = ls())

library(tidytext)
library(ggplot2)
library(tidyverse)
library(janitor)
library(quanteda)

load(file = "data_gov_sg/nlb_articles/data/data_personality.RData")

unique(data1$year)
min(data1$year)
max(data1$year)

tabyl(data1$year)
tabyl(data1$sex)
data1$sex <- factor(data1$sex)
# FIRST CONVERSION: CONVERT CORPUS -> TOKENS -> DFM ->  TIDYTEXT #####

## Corpus -> token -> dfm -> tidytext ====
library(quanteda)
corp1 <- corpus(data1,
                docid_field = "id",
                text_field = "summary")

tok1 <- 
    corp1 %>% 
    tokens(remove_numbers = T,
           remove_separators = T,
           remove_url = T,
           remove_symbols = T,
           remove_punct = T,
           include_docvars = T) %>% 
    tokens_tolower() %>% 
    tokens_select(pattern = c(stopwords("en")),
                  selection = "remove") %>% 
    tokens_wordstem()

#tokens_select(pattern = c(stopwords("en"),
#                          low_tfidf,
#                          mystopwords), 
#              selection = "remove")

# tokens_wordstem()

## DFM  ====
dfm1 <- 
    tok1 %>% 
    dfm()


## tidytext format  ====
tidy1 <- 
    dfm1 %>% 
    tidy() %>% 
    rename(id = document,
           word = term) %>% # Tidy will auto-name the identifier as "document"
    mutate(id = as.numeric(id))

topwords <- 
    tidy1 %>% 
    count(word, count) # the b and d are for birth and death

data1 %>% 
    filter(str_detect(summary, pattern = "d.")) %>% 
    select(summary)

# SECOND CONVERSION ####

##  stopword list ====
mystopwords = c("singapore", "b", "d", 
                "known", "also")


tok2 <- 
    corp1 %>% 
    tokens(remove_numbers = T,
           remove_separators = T,
           remove_url = T,
           remove_symbols = T,
           remove_punct = T,
           include_docvars = T) %>% 
    tokens_tolower() %>% 
    tokens_select(pattern = c(stopwords("en"),
                              mystopwords),
                  selection = "remove") %>% 
    tokens_wordstem()

dfm2 <- 
    tok2 %>% 
    dfm()

tidy2 <- 
    dfm2 %>% 
    tidy() %>% 
    rename(id = document,
           word = term) %>% # Tidy will auto-name the identifier as "document"
    mutate(id = as.numeric(id))

topwords2 <- 
    tidy2 %>% 
    count(word, count) # the b and d are for birth and death

## remove Singapor ====
tidy2 <- 
    tidy2 %>% 
    filter(word != "singapor")

# TFIDF 
tfidf <- 
    tidy2 %>% 
    bind_tf_idf(word, id, count) %>% 
    arrange(desc(tf_idf))



tfidf_smallest <-
    tfidf %>% 
    slice_min(tf_idf, n = 100) %>% 
    select(id, word)



# Tidymodel approach to topic modeling ####
# https://www.tidytextmining.com/topicmodeling.html

# https://rstudio-pubs-static.s3.amazonaws.com/643113_f2daf42a1919425194df1b50fe8bdbaa.html
#https://quantdev.ssri.psu.edu/sites/qdev/files/topic_modeling_tutorial-Gutenberg-chapter_as_document.html

tidy2_reduced <- 
    anti_join(tidy2, tfidf_smallest)


tidy2_dfm  <- tidy2_reduced %>%  cast_dfm(id, word, count)


## STM 
#sherlock_dfm <- dfm_topic

#install.packages("stm")

library(stm)

topic_model <- stm(tidy2_dfm, K = 5, 
                   verbose = FALSE, init.type = "Spectral")
plot(topic_model)

#blogs_dfm_stm <- convert(blogs_dfm, to = "stm", docvars = docvars(blogs_corpus))


td_beta <- tidy(topic_model)

td_beta %>%
    group_by(topic) %>%
    top_n(8, beta) %>%
    ungroup() %>%
    mutate(topic = paste0("Topic ", topic),
           term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = as.factor(topic))) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    labs(x = NULL, y = expression(beta),
         title = "Highest word probabilities for each topic",
         subtitle = "Different words are associated with different topics")

td_gamma <- tidy(topic_model, matrix = "gamma",                    
                 document_names = rownames(tidy2_dfm))

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
    geom_histogram(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ topic, ncol = 3) +
    labs(title = "Distribution of document probabilities for each topic",
         subtitle = "Each topic is associated with 1-3 stories",
         y = "Number of stories", x = expression(gamma))



## https://bookdown.org/Maxine/tidy-text-mining/tuning-number-of-topics.html
library(furrr)
plan(multiprocess)

#inaugural_dfm <- dfm_topic

models <- tibble(K = 2:15) %>%
    mutate(topic_model = future_map(K, ~ stm(tidy2_dfm,
                                             init.type = "Spectral",
                                             K = .,
                                             verbose = FALSE)))


heldout <- make.heldout(tidy2_dfm)

k_result <- models %>%
    mutate(exclusivity        = map(topic_model, exclusivity),
           semantic_coherence = map(topic_model, semanticCoherence, tidy2_dfm),
           eval_heldout       = map(topic_model, eval.heldout, heldout$missing),
           residual           = map(topic_model, checkResiduals, tidy2_dfm),
           bound              = map_dbl(topic_model, function(x) max(x$convergence$bound)),
           lfact              = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
           lbound             = bound + lfact,
           iterations         = map_dbl(topic_model, function(x) length(x$convergence$bound)))


k_result %>%
    transmute(K,
              `Lower bound`         = lbound,
              Residuals             = map_dbl(residual, "dispersion"),
              `Semantic coherence`  = map_dbl(semantic_coherence, mean),
              `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
    pivot_longer(-K, names_to = "metrics", values_to = "value") %>%
    ggplot(aes(K, value, color = metrics)) +
    geom_line(size = 1.5) +
    facet_wrap(~ metrics, scales = "free_y") ->  plot_final1

getwd()
ggsave("data_gov_sg/nlb_articles/plots/chooseemodels.png", plot = plot_final1, type = 'cairo', width = 12, height = 8.5, dpi = 400, units = "in", bg = "white")




topic_model <- stm(tidy2_dfm, K = 9, 
                   verbose = FALSE, init.type = "Spectral")
                  # prevalence = ~as.factor(sex),
                  # data = docvars(corp1))

summary(topic_model)


####
#labelTopics(topic_model, c(1, 2, 3,4,5))
#
#thoughts6 <- findThoughts(topic_model,  n = 2, topics = 5)$docs[[1]]
#plotQuote(thoughts6, width = 30, main = "Topic 6")
#
#topic_model$meta$sex <- as.factor(topic_model$meta$sex)
#prep <- estimateEffect(1:5~ as.factor(sex), topic_model, meta = topic_model$meta, uncertainty = "Global")
#summary(prep, topics = 1)
#

####
td_beta <- tidy(topic_model)

td_beta %>%
    group_by(topic) %>%
    top_n(6, beta) %>%
    ungroup() %>%
    mutate(topic = paste0("Topic ", topic),
           term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = as.factor(topic))) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    labs(x = NULL, y = expression(beta),
         title = "Highest word probabilities for each topic",
         subtitle = "Different words are associated with different topics") ->  plot_final2

getwd()
ggsave("data_gov_sg/nlb_articles/plots/keywords_topic.png", plot = plot_final2, type = 'cairo', width = 12, height = 8.5, dpi = 400, units = "in", bg = "white")

lib_documents <- tidy(topic_model, matrix = "gamma",
                      document_names = rownames(tidy2_dfm))

lib_documents

#assignments <- augment(topic_model, data = lib_documents)

lib_documents <- 
    lib_documents %>%
    arrange(desc(topic, gamma)) %>% 
    mutate(document = as.numeric(document)) 
    
lib_documents1 <- 
    lib_documents %>% 
    rename(id = document) %>% 
    group_by(id) %>% 
    slice_max(gamma, n=1)

lib_documents2 <-
    left_join(data1,lib_documents1) %>% 
    select(id, book_title, sex, topic, gamma, year, summary)

# Diffferences by sex
lib_documents2 %>% 
    tabyl(sex, topic) %>% 
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 0) %>%
    adorn_ns()

library(tidymodels)
lib_documents2$topic_cat <- as.factor(lib_documents2$topic)
chisq_test(lib_documents2, sex ~ topic_cat)

sex_topic <-
    lib_documents2 %>% 
    count(topic, sex)

sex_topic2 <-
    sex_topic %>% 
    group_by(sex) %>% 
    mutate(total = sum(n)) %>% 
    mutate(pct =  n/total)


sex_topic2$sex <- factor(sex_topic2$sex, labels = c("male", "female"), levels = c("m", "f"))
sex_topic2$topic <- factor(sex_topic2$topic)


ggplot(sex_topic2, mapping = aes(x = topic, y = pct, fill = sex)) + 
    geom_col(position = "dodge") +
    facet_wrap(vars(sex)) +
    scale_y_continuous(labels = scales::percent) + 
    theme_minimal() + 
    theme(legend.position = "none")   +
    labs(y = "Percentage",
         x = "Topic") ->  plot_final3

getwd()
ggsave("data_gov_sg/nlb_articles/plots/sex_topic.png", plot = plot_final3, type = 'cairo', width = 12, height = 8.5, dpi = 400, units = "in", bg = "white")



## lda in tidy package

#tidy2_dtm <- tidy2 %>%  cast_dtm(id, word, count)

#install.packages("ldatuning")
library(ldatuning)
result <- FindTopicsNumber(
    tidy2_dtm,
   # topics = seq(from = 2, to = 60, by = 1),
    topics = seq(from = 2, to = 14, by = 1),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = 2L,
    verbose = TRUE)

FindTopicsNumber_plot(result)



#install.packages("topicmodels")
library(topicmodels)
#lib_lda <- LDA(tidy2_dtm, k = 13, control = list(seed = 1234))
lib_lda <- LDA(tidy2_dtm, k = 7, control = list(seed = 1234))

lib_lda

library(tidytext)

lib_topics <- tidy(lib_lda, matrix = "beta")
lib_topics


lib_top_terms <- lib_topics %>%
    group_by(topic) %>%
    slice_max(beta, n = 10) %>% 
    ungroup() %>%
    arrange(topic, -beta)

lib_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()


beta_wide <- lib_top_terms %>%
    mutate(topic = paste0("topic", topic)) %>%
    pivot_wider(names_from = topic, values_from = beta) %>% 
    filter(topic1 > .001 | topic2 > .001) %>%
    mutate(log_ratio = log2(topic2 / topic1))

beta_wide

lib_documents <- tidy(lib_lda, matrix = "gamma")





# KEYATM approach
library(keyATM)
keyATM_docs <- keyATM_read(texts = dfm_topic)
summary(keyATM_docs)

set.seed(225)  # set the seed before split the dfm
docs_withSplit <- keyATM_read(texts = dfm_topic,
                              split = 0.3)  # split each document

out <- weightedLDA(docs              = docs_withSplit$W_split,  # 30% of the corpus
                   number_of_topics  = 5,  # the number of potential themes in the corpus 
                   model             = "base",
                   options           = list(seed = 250))
top_words(out)  # top words can aid selecting keywords

out <- keyATM(docs              = docs_withSplit,  # 70% of the corpus
              no_keyword_topics = 5,               # number of topics without keywords
              keywords          = keywords,        # selected keywords
              model             = "base",          # select the model
              options           = list(seed = 250))
