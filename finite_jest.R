require(textrank)
require(readtext)
require(udpipe)

ij <- readtext("https://raw.githubusercontent.com/rcompton/ryancompton.net/master/assets/dfw/David-Foster-Wallace-Infinite-Jest-v2.0.txt")

ij <- paste(ij, collapse  = "\n")


tagger <- udpipe_download_model("english")
tagger <- udpipe_load_model(tagger$file_model)
finite <- udpipe_annotate(tagger, ij)
finite <- as.data.frame(finite)

finite$textrank_id <- unique_identifier(finite, c("doc_id",
                                                  "paragraph_id",
                                                  "sentence_id"))

sentences <- unique(finite[, c("textrank_id", "sentence")])
terminology <- terminology[, c("textrank_id", "lemma")]
head(terminology)

tr <- textrank_sentences(data = sentences, terminology = terminology)
names(tr)

s <- summary(tr, n = 2400, keep.sentence.order = TRUE)
