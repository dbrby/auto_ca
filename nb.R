require(tidyverse)
require(quanteda)
require(quanteda.textmodels)
require(caret)

cap_pmqs <- read.csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/uk_pmqs.csv")

cap_pmqs <- select(cap_pmqs, id, year, date, minister, question, majortopic)


cap_pmqs_corp <- corpus(cap_pmqs, text_field = "question")

cap_pmqs_toks <- tokens(cap_pmqs_corp, remove_punct = TRUE, 
                    remove_numbers = TRUE, remove_symbol = TRUE) %>%
  tokens_remove(stopwords("en")) 

cap_pmqs_dfm <- dfm(cap_pmqs_toks)


tmod_nb <- textmodel_nb(cap_pmqs_dfm, cap_pmqs_dfm$majortopic)
summary(tmod_nb)


pmqs <- read.csv("pmqs_questions.csv")


pmqs <- filter(pmqs, nchar(speech) > 50, date > "2010-05-05", date < "2019-12-12")

pmqs_corp <- corpus(pmqs, text_field = "speech")

pmqs_toks <- tokens(pmqs_corp, remove_punct = TRUE, 
                    remove_numbers = TRUE, remove_symbol = TRUE) %>%
  tokens_remove(stopwords("en")) 

pmqs_dfm <- dfm(pmqs_toks)


predicted_class <- predict(tmod_nb, newdata = 
                                 pmqs_dfm)
