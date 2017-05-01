# install.packages("tm")
# install.packages('SnowballC')
# install.packages('RWeka')
# install.packages('wordcloud')

# system("java -version")
# Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_131/")

library(tm)
library(RWeka)
library(SnowballC)
library(ggplot2)
library(wordcloud)

setwd("C:\\Users\\Jonathan\\Desktop\\Coursera Scripts\\10. Data Science Capstone\\")

# getSources()

# Blogs file
con <- file(".\\Data\\final\\en_US\\en_US.blogs.txt", "r")

resBlogs <- readLines(con, skipNul = TRUE, encoding="UTF-8")
TotalNoOfLinesBlogs <- length(resBlogs) # 899288

close(con)

# News file
conNews <- file(".\\Data\\final\\en_US\\en_US.news.txt", "r")

resNews <- readLines(conNews, skipNul = TRUE, encoding="UTF-8")
TotalNoOfLinesNews <- length(resNews) # 77259

close(conNews)

# Twitter file
conTwitter <- file(".\\Data\\final\\en_US\\en_US.twitter.txt", "r")

resTwitter <- readLines(conTwitter, skipNul = TRUE, encoding="UTF-8")
TotalNoOfLinesTwitter <- length(resTwitter) # 2360148

close(conTwitter)

#######################################################################

set.seed(12345)

# Sample data %
PercentSample <- 0.05

SampleRows <- TotalNoOfLinesBlogs * PercentSample
SampleBlogs <- sample (resBlogs, size = SampleRows, replace = FALSE)

#length(SampleBlogs)

SampleRows <- TotalNoOfLinesNews * PercentSample
SampleNews <- sample (resNews, size = SampleRows, replace = FALSE)

#length(SampleNews)

SampleRows <- TotalNoOfLinesBlogs * PercentSample
SampleTwitter <- sample (resTwitter, size = SampleRows, replace = FALSE)

#length(SampleTwitter)

SampleAll <- c(SampleBlogs, SampleNews, SampleTwitter)
# TotalNoOfLinesSample <- length(SampleTwitter) + length(SampleBlogs) + length(SampleNews)

# Remove some variables to free up memory
rm(list = c('resNews','resBlogs','resTwitter','SampleBlogs','SampleNews','SampleTwitter'))

#######################################################################

# Convert to Corpus
allCorpus <- VCorpus(VectorSource(SampleAll))

rm(list = c('SampleAll'))

# writeCorpus(allCorpus, path = ".")

# class(allCorpus)
# str(allCorpus) # list
# head(allCorpus, 10)
# summary(allCorpus)  #check what went in

# Some work we need to do to clean up the corpus
 
# Removing special characters 
# Removing infrequent words

# List all possible transformations
# getTransformations()

allCorpus <- tm_map(allCorpus, removeNumbers) # Removing Numbers
allCorpus <- tm_map(allCorpus, removePunctuation) # Removing punctuation
allCorpus <- tm_map(allCorpus, stripWhitespace) # Removing extra whitespace

# Twitter File contains extended characters that throws error in further processing
# Handling special characters
# Reference: http://stackoverflow.com/questions/9637278/r-tm-package-invalid-input-in-utf8towcs
allCorpus <- tm_map(allCorpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
allCorpus <- tm_map(allCorpus, content_transformer(tolower)) # Making all text lowercase

allCorpus <- tm_map(allCorpus, removeWords, stopwords("english")) # Removing common words (e.g the, to, a, and, of, in, etc.) # this stopword file is at C:\Users\[username]\Documents\R\win-library\2.13\tm\stopwords 
allCorpus <- tm_map(allCorpus, stemDocument, language = "english")
# It is debatable if we should really remove stop words and stemming the document for text prediction
# https://www.coursera.org/learn/data-science-project/discussions/weeks/2/threads/Wn9Nm_nDEeacPhIeW4iKZg

# Remove profane words
# profanity_file <- "./Data/profanity_list.txt"
# 
# # if the file does not exists, then download and unzip the file
# if (!file.exists(profanity_file)) {
#     # download the file from CMU we
#     download.file("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt", profanity_file)
# }
# 
# # load the profane words from the file
# profane_words <- readLines(profanity_file)
# 
# # remove the profane words from the corpus
# allCorpus <- tm_map(allCorpus, removeWords, profane_words) # Removing Profanity 

# Dataset ins now cleaned and we can begin data exploration. 
# We use the RWeka::NGramTokenizer to tokenize the words into 1-gram, 2-gram and 3-gram tokens. 
# n-gram is a continuous sequence of n words that appear in the corpus. 
# This helps identify the frequencies of 1 word, 2 words together, and 3 words together, and so on.

# Next we will build a TermDocumentMatrix which will show the frequency of each term/word. 
# For this analysis we will just look at unigram (single word) terms. For the final prediction 
# algorithm we will look at 2,3,4 and possibly more n-gram word combinations.

unigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))
bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
trigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))

# We create a tm::TermDocumenMatrix using the tokenizer functions. 
# A TermDocumentMatrix is a matrix where the rows are the tokens and columns are the datasets. 
# Each cell in this matrix represents the frequencies of the tokens in the datasets.

unigramTDM <- TermDocumentMatrix(allCorpus, control=list(tokenize=unigramTokenizer))
bigramTDM <- TermDocumentMatrix(allCorpus, control=list(tokenize=bigramTokenizer))
trigramTDM <- TermDocumentMatrix(allCorpus, control=list(tokenize=trigramTokenizer))

rm(list = 'allCorpus')

# Save TDM to file 
saveRDS(object =  unigramTDM, file = 'unigramTDM.RData')
saveRDS(object =  bigramTDM, file = 'bigramTDM.RData')
saveRDS(object =  trigramTDM, file = 'trigramTDM.RData')

# Remove sparse terms to significantly reduce the size of the TDM
unigramTDM <- removeSparseTerms(unigramTDM, 0.9999)
bigramTDM <- removeSparseTerms(bigramTDM, 0.9999)
trigramTDM <- removeSparseTerms(trigramTDM, 0.9999)

saveRDS(object =  unigramTDM, file = 'unigramTDM_NonSparse.RData')
saveRDS(object =  bigramTDM, file = 'bigramTDM_NonSparse.RData')
saveRDS(object =  trigramTDM, file = 'trigramTDM_NonSparse.RData')

# Read the TDM file
# unigramTDM <- as.TermDocumentMatrix(readRDS('unigramTDM.RData'))
# bigramTDM <- as.TermDocumentMatrix(readRDS('bigramTDM.RData'))
# trigramTDM <- as.TermDocumentMatrix(readRDS('trigramTDM.RData'))

PlotNgram <- function (ngram, topN = 25, nGramLabel = "1 Gram") {
    
    df_words <- as.data.frame(slam::row_sums(ngram, na.rm=T))
    
    colnames(df_words)<- "freq"
    df_words <- cbind(word = rownames(df_words), df_words)
    rownames(df_words) <- NULL
    
    df_words <- df_words[order(df_words$freq, decreasing = TRUE),]
    df_words_top <- head(df_words, topN)
    #head(df_words_top)

    ggplot(df_words_top, aes(reorder(word, freq), freq, fill=word)) +
        geom_text(aes(label = df_words_top$freq), vjust=-0.5, size=2) + 
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 0.5, size = 10)) +
        labs(x="Words") + 
        labs(y="Frequency") +
        labs(title= paste("Top",topN,"Words for", nGramLabel))
}

WordcloudNgram <- function (ngram, topN = 25) {
    
    df_words <- as.data.frame(slam::row_sums(ngram, na.rm=T))
    
    colnames(df_words)<- "freq"
    df_words <- cbind(word = rownames(df_words), df_words)
    rownames(df_words) <- NULL
    
    df_words <- df_words[order(df_words$freq, decreasing = TRUE),]
    df_words_top <- head(df_words, topN)

    wordcloud(
        df_words_top$word,
        df_words_top$freq,
        max.words = 50,
        rot.per = 0.3,
        colors=brewer.pal(8, "Dark2")
        #vfont=c("serif","plain")
        )
}

#######################################################################
## Plots ##

# Bar Charts
PlotNgram(unigramTDM, 20, "1 Gram", topN = 15)
PlotNgram(bigramTDM, 20, "2 Gram", topN = 15)
PlotNgram(trigramTDM, 20, "3 Gram", topN = 15)

# Word Clouds
WordcloudNgram(ngram = unigramTDM)
WordcloudNgram(ngram = bigramTDM)
WordcloudNgram(ngram = trigramTDM)

#######################################################################

# Remove sparse terms, i.e. terms occurring only in very few documents. Normally, this reduces 
# the matrix dramatically without losing significant relations inherent to the matrix:
# This function removes those terms which have at least a 40 percentage of sparse elements

unigramTDM <- as.TermDocumentMatrix(readRDS('unigramTDM.RData'))
bigramTDM <- as.TermDocumentMatrix(readRDS('bigramTDM.RData'))
trigramTDM <- as.TermDocumentMatrix(readRDS('trigramTDM.RData'))

inspect(unigramTDM)

# Get terms that occur at least 5000 times
findFreqTerms(unigramTDM, 5000)

# Find associations with at least 0.8 correlation for the term 'hate'
findAssocs(unigramTDM, "hate", 0.8)

object.size()
gc()

# writeCorpus
save(allCorpus, file='allCorpus.RData')
load('allCorpus.RData')
object.size(en_US_corpus); gc()
rm(list=ls(all=TRUE));gc(reset=TRUE);

## cont... https://rpubs.com/edlewis4/95047
## https://rpubs.com/aumahesh/explore-predictive-text-model
