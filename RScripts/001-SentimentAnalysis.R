##' ---------------------------------------------------------------------------------
##' Script name: 001-SentimentAnalysis.R
##'
##' Purpose of script: Mini Projeto DataScienceAcademy FCD
##'
##' Author: Carlos Bertaglia
##'
##' Date Created: 2019-04-08
##'
##' Copyright (c) Carlos Bertaglia, 2019
##' Email: bertaglia.carlos@gmail.com
##'
##' ---------------------------------------------------------------------------------
##'
##'  Notes: Script parte do MiniProjeto DSA
##'         Curso R com Azure.
##'         Analise de sentimentos Twitter
##' ---------------------------------------------------------------------------------

# dir
getwd()

# load all required packages
requiredpackages <- c(
    "lintr",
    "ggplot2",
    "styler",
    "config",
    "twitteR",
    "tm",
    "tidyverse",
    "tidytext",
    "glue",
    "data.table",
    "httr",
    "SnowballC",
    "wordcloud",
    "RColorBrewer",
    "grDevices"
)

# for each package needed ahead, install and load
for (package in requiredpackages) {
    if (!library(package,
                 character.only = TRUE,
                 logical.return = TRUE)) {
        #print(package)
        install.packages(package)
        library(package, character.only = TRUE)
    }
}

#include external resources
source('RScripts/utils.R')
##' ---------------------------------------------------------------------------------


##' ---------------------------------------------------------------------------------
##' Configuring Twitter access
##' remember to keep your config.yml out of your vcs (eg: include it in your .gitignore)
##' recovering twitter's api keys from config.yml (this file should be created with your own
##' api's keys)
##' ---------------------------------------------------------------------------------
twitter_con <- config::get("ConsumerKey")
twitter_acc <- config::get("AccessKey")

twitteR::setup_twitter_oauth(
    twitter_con$api_key,
    twitter_con$api_secret_key,
    twitter_acc$access_token,
    twitter_acc$access_token_secret
)
##' ---------------------------------------------------------------------------------


##' ---------------------------------------------------------------------------------
##' retrieving data from twitter
##' ---------------------------------------------------------------------------------
#twitter's search parameters
subject <- "enem"
qt_twt  <- 1000
lang    <- "pt"
inidt   <- toString(Sys.Date() - 1000)  #yyyy-mm-dd

#retrieving the data
tweetdata <- searchTwitter(
    searchString = subject,
    n = qt_twt,
    lang = lang,
    since = inidt,
    resultType = "mixed"
)
##' ---------------------------------------------------------------------------------


##' ---------------------------------------------------------------------------------
##' processing, cleaning and organizing
##' ---------------------------------------------------------------------------------
#conv from list to vector/matrix
tweetlist <- sapply(tweetdata, tweet_get_text)

#convert to utf-8
tweetlist <- iconv(tweetlist, to = "utf-8", sub = "")

#clear, RT, @, numbers and punctuation
tweetlist <- limpaTweets(tweetlist)   # from Utils.R

#remove NAs
tweetlist <- na.omit(tweetlist)

View(tweetlist)

#writing recovered tweets into a txt file for later analysis 
file_name <- paste("tweets", subject, toString(Sys.Date()), ".txt", sep = "")
file_path <- paste("data/processed/", file_name, sep = "")
write_lines(x = tweetlist,
            path = file_path,
            append = FALSE)
##' ---------------------------------------------------------------------------------


##' ---------------------------------------------------------------------------------
##' populating a corpus to create a wordCloud
##' ---------------------------------------------------------------------------------
##' 
##' source: https://www.youtube.com/watch?v=pvjhm5TTd2A
##' source: https://davetang.org/muse/2013/04/06/using-the-r_twitter-package/
##' source: DSA Project solution
##' ---------------------------------------------------------------------------------

#load my own stopwords_ptBR list gathered from many internet sources
stpwrd <- readr::read_lines(file = "data/processed/stopwords_ptBR.txt")

#corpus
tweet_corp <- tm::Corpus(VectorSource(tweetlist))
tweet_corp <- tm::tm_map(tweet_corp, content_transformer(tolower))

#remove all english and portuguese stopwords tm package
tweet_corp <- tm::tm_map(tweet_corp, function(x)removeWords(x, stopwords()))
tweet_corp <- tm::tm_map(tweet_corp, function(x)removeWords(x, stopwords(kind = "portuguese")))
tweet_corp <- tm::tm_map(tweet_corp, function(x)removeWords(x, stpwrd))

#remove single letters
tweet_corp <- tm::tm_map(tweet_corp, function(x){gsub(pattern = "\\b[A-z]\\b{1}", replacement = " ",x)})

#remove all extra blank spaces
tweet_corp <- tm::tm_map(tweet_corp, function(x){tm::stripWhitespace(x)})
tweet_corp <- tm::tm_map(tweet_corp, function(x){tm::removePunctuation(x)})
tweet_corp <- tm::tm_map(tweet_corp, function(x){trimws(x)})

#reviewing/checking
tm::inspect(tweet_corp)
tweet_corp

#Gerando uma nuvem palavras
#RColorBrewer::display.brewer.all()
#colors01 <- brewer.pal(08, "Dark2")
#colors01 <- grDevices::rainbow()
colors01 <- brewer.pal(08, "Dark2")

wordcloud::wordcloud(
    tweet_corp,
    min.freq = 2,
    scale = c(5, 1),
    random.color = FALSE,
    max.word = 100,
    random.order = FALSE,
    colors = colors01
)
##' ---------------------------------------------------------------------------------


##' ---------------------------------------------------------------------------------
##' words' frequencies and associations
##' ---------------------------------------------------------------------------------

#matrix
tweettdm <- tm::TermDocumentMatrix(tweet_corp)
tweettdm

# Finding frequent words
tm::findFreqTerms(tweettdm, lowfreq = 11)

# Seeking for associations
tm::findAssocs(tweettdm, c("futuro","universidade","exame","estudar","nota"), 0.60)

# Removing sparse terms 
tweet2tdm <- tm::removeSparseTerms(tweettdm, sparse = 0.9)

# scaling 
tweet2tdmscale <- scale(tweet2tdm)

# Distance Matrix
tweetdist <- dist(tweet2tdmscale, method = "euclidean")

# Preparing the dendrogram
tweetfit <- hclust(tweetdist)

# Ploting the dendrogram
plot(tweetfit)

# verifying the word groups
cutree(tweetfit, k = 4)

# Visualizing the groups 
rect.hclust(tweetfit, k = 3, border = "red")
##' ---------------------------------------------------------------------------------

