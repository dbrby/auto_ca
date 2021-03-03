## Basic R Syntax
 
# Calcultor

1 + 2

2 / 2

4 * 3

6 * (4 + 7)

# Objects
 
x = 4

x * 2

y <- "The cat sat on the mat."

y

# Vectors

x <- c(1, 2, 3, 4, 5, 6)

x <- 
1:6

y <- c("will  the om..;", "cat", "sat", "on", "the", "mat")

# Dataframes

df <- data.frame(x, y)

# Operators

df$x

df$y

# Packages

install.packages("tidyverse")

library(tidyverse)
library(quanteda)
library(stm)


# Functions

df$x <- as.character(df$x)


df$x

# dplyr %>%

df$x <- as.numeric(df$x) %>% sqrt()

df$x

# Loading in data


pmqs_questions <- read.csv("pmqs_questions.csv")

parlScot <- readRDS("parlScot_parl_v1.1.rds")



require(quanteda.corpora)

corp_news <- download("data_corpus_guardian")




## String Detection

require(tm)

pmqs_questions$speech <- pmqs_questions$speech %>%
  tolower() %>% removePunctuation() 

pmqs_questions$constituency <- pmqs_questions$constituency %>%
  tolower() %>% removePunctuation() %>% tolower()

constituency_terms <- "my constituen|own constituen"

pmqs_questions$possessive <- as.integer(str_detect(pmqs_questions$speech,
                                                  constituency_terms))

table(pmqs_questions$decompose)

pmqs_questions$explicit <- as.integer(str_detect(pmqs_questions$speech,
                                       pmqs_questions$constituency))

pmqs_questions$decompose <- as.integer(str_detect(pmqs_questions$speech,
                                                  gsub("and", "|", pmqs_questions$constituency)))

pmqs_questions$reference <- c()
pmqs_questions$reference[pmqs_questions$possessive + pmqs_questions$explicit + pmqs_questions$decompose >= 1] <- 1
pmqs_questions$reference[pmqs_questions$possessive + pmqs_questions$explicit + pmqs_questions$decompose == 0] <- 0

pmqs_agg <- pmqs_questions %>% group_by(date) %>%
  summarise(n = length(id),
            ref = mean(reference)) %>% unique()

ggplot(pmqs_agg, aes(date, ref)) + geom_point() 

## Dictionary Based Content Analysis

# Load in Dictionairy
dict_lg <- dictionary(file = "LaverGarry/LaverGarry.cat",
                      encoding = "UTF-8")

#Tokenize documents
toks_irish <- tokens(data_corpus_irishbudgets, remove_punct = TRUE)

#Convert to DFM, documents x vocabulary
dfmat_irish <- dfm(toks_irish)
print(dfmat_irish)
summary(toks_irish)


#Lookup dictionary 
dfmat_irish_lg <- dfm_lookup(dfmat_irish, dictionary = dict_lg, levels = 1)
print(dfmat_irish_lg)

dfmat_irish_lg

# Write your own dictionary

dict_refugee <- dictionary(list(refugee = c("refugee*", "asylum*"),
                        worker = c("worker*", "employee*"),
                        family = c("famil*", "househ*", "parent*")))
print(dict_refugee)

# Efficient and minimal code

# We convert to dfm and do dicttionary lookup simultaneously
dfmat_irish_refugee <- dfm(data_corpus_irishbudgets, dictionary = dict_refugee, remove_punct = TRUE)
print(dfmat_irish_refugee)



## Dictionary Based Sentiment Analysis

require(quanteda)

lengths(data_dictionary_LSD2015)

# select only the "negative" and "positive" categories
data_dictionary_LSD2015_pos_neg <- data_dictionary_LSD2015[1:2]

toks_news <- tokens(corp_news, remove_punct = TRUE)

# get relevant keywords and phrases
gov <- c("government", "cabinet", "prime minister")

# only keep tokens specified above and their context of ±10 tokens
# note: use phrase() to correctly score multi-word expressions
toks_gov <- tokens_keep(toks_news, pattern = phrase(gov), window = 10)

toks_gov_lsd <- tokens_lookup(toks_gov, dictionary = data_dictionary_LSD2015_pos_neg)

# create a document document-feature matrix and group it by day
dfmat_gov_lsd <- dfm(toks_gov_lsd) %>% 
  dfm_group("date")

#Plotting
matplot(dfmat_gov_lsd$date, dfmat_gov_lsd, type = "l", lty = 1, col = 1:2,
        ylab = "Frequency", xlab = "")
grid()
legend("topleft", col = 1:2, legend = colnames(dfmat_gov_lsd), lty = 1, bg = "white")


## Sentiment Analysis as a semi-supervised model

install.packages("LSX")

require(quanteda)
require(quanteda.corpora)
require(LSX)

# Convert corpus to sentences
corp_sent <- corpus_reshape(corp_news, to =  "sentences")
toks_sent <- corp_sent %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_remove(stopwords("en")) %>%
  tokens_remove(c("*-time", "*-timeUpdated", "GMT", "BST", "*.com"))  

# create a document feature matrix from the tokens object
dfmat_sent <- toks_sent %>% 
  dfm(remove = "") %>% 
  dfm_trim(min_termfreq = 5)

#Take top and bottom sentiment terms
seed <- as.seedwords(data_dictionary_sentiment)
print(seed)


#Isolate words related to the economy: probability equal >99.5%
eco <- char_context(toks_sent, pattern = "econom*", p = 0.05)


# run LSS model
tmod_lss <- textmodel_lss(dfmat_sent, seeds = seed,
                          terms = eco, k = 300, cache = TRUE)

# What have we got

head(coef(tmod_lss), 20) # most positive words

tail(coef(tmod_lss), 20) # most negative words

textplot_terms(tmod_lss, data_dictionary_LSD2015["negative"])

dfmat_doc <- dfm_group(dfmat_sent)
dat <- docvars(dfmat_doc)
dat$fit <- predict(tmod_lss, newdata = dfmat_doc)

plot(dat$fit, dat$date)


# Time-Series Analysis

dat_smooth <- smooth_lss(dat, engine = "locfit")

plot(dat$date, dat$fit, col = rgb(0, 0, 0, 0.05), pch = 16, ylim = c(-0.5, 0.5),
     xlab = "Time", ylab = "Economic sentiment")
lines(dat_smooth$date, dat_smooth$fit, type = "l")
lines(dat_smooth$date, dat_smooth$fit + dat_smooth$se.fit * 1.96, type = "l", lty = 3)
lines(dat_smooth$date, dat_smooth$fit - dat_smooth$se.fit * 1.96, type = "l", lty = 3)
abline(h = 0, lty = c(1, 2))


## Topic Models

parlScot <- filter(parlScot, is_speech == 1, date > "2016-05-05",
                   nchar(speech) >100)


stopwords_scotParl <- c("mr", "lady", "gentleman",
                        "hon", "today", "time",
                        "question", "problem",
                        "issue", "week", "year",
                        "month", "ask", "speaker",
                        "minister", "ministers",
                        "minister's", "member",
                        "member's", "go", "can", "agree",
                        "snp", "lab", "con", "please", "north",
                        "south", "ms", "intervention",
                        "interruption", "bill", "policy",
                        "s-00", "two", "one", "even", "state",
                        "told", "let", "review", "process",
                        "parliament", "secretary", "cabinet",
                        "will", "debatr")

parl_corp <- corpus(parlScot, text_field = "speech")

parl_toks <- tokens(parl_corp, remove_punct = TRUE, 
                    remove_numbers = TRUE, remove_symbol = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(stopwords_scotParl) %>%
  tokens_wordstem()

#Make dfm %>% pipeline
parl_dfm <- dfm(parl_corp) %>%
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")
# Trim words by document and term props

topic.count = 20 # Number of topics

parl2stm <- convert(parl_dfm, to = "stm")

model.stm <- stm(parl2stm$documents, parl2stm$vocab, K = topic.count, data = parl2stm$meta, init.type = "Spectral")


# Visualizing our topics

plot(model.stm, type = "summary", text.cex = 0.5)

plot(model.stm, type = "perspectives", topics = c(18,19)) # Topics #18 and #19



# Create year variables and regress topic props around it
require(lubridate)
parl2stm$meta$year <- as.numeric(year(parl2stm$meta$date))
model.stm.labels <- labelTopics(model.stm, 1:topic.count)
parl2stm$meta$datum <- as.numeric(parl2stm$meta$year)
model.stm.ee <- estimateEffect(1:topic.count ~ year, model.stm, meta = parl2stm$meta)

par(mfrow=c(4,4))
for (i in seq_along(sample(1:topic.count, size = 16)))
{
  plot(model.stm.ee, "year", method = "continuous", topics = i, main = paste0(model.stm.labels$prob[i,1:3], collapse = ", "), printlegend = F)
}

