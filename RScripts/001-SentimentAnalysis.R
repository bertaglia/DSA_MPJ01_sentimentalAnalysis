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
##' github: https://github.com/bertaglia
##' ---------------------------------------------------------------------------------
##'  Notes: Script parte do MiniProjeto DSA
##'         Curso R com Azure.
##'         Analise de sentimentos Twitter
##' ---------------------------------------------------------------------------------
##' Credits:
##' source: https://www.youtube.com/watch?v=pvjhm5TTd2A
##' source: https://davetang.org/muse/2013/04/06/using-the-r_twitter-package/
##' source: https://github.com/rdenadai/sentiment-analysis-2018-president-election/
##' source: http://ontolp.inf.pucrs.br/Recursos/downloads-OpLexicon.php
##' source: DSA Project solution
##'

# dir
getwd()

# load all required packages
requiredpackages <- c(
    "lintr",
    "plyr",
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
    "grDevices",
    "lattice",
    "Rstem",
    "sentiment",
    "stringr"
)

# for each package needed ahead, install and load
for (package in requiredpackages) {
    if (!library(package,
                 character.only = TRUE,
                 logical.return = TRUE)) {
        # print(package)
        #for packages in resources
        if (package == "Rstem") {
            install.packages(
                "resources/Rstem_0.4-1.tar.gz",
                sep = "",
                repos = NULL,
                type = "source"
            )
        }
        if (package == "sentiment") {
            install.packages(
                "resources/sentiment_0.2.tar.gz",
                sep = "",
                repos = NULL,
                type = "source"
            )
        }
        install.packages(package)
        library(package, character.only = TRUE)
    }
}

# include external resources
source("RScripts/utils.R")
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
# twitter's search parameters
subject <- "bovespa"
qt_twt <- 5000
lang <- "pt"
inidt <- toString(Sys.Date() - 1000) # yyyy-mm-dd

# retrieving the data
tweetdata <- searchTwitter(
    searchString = subject,
    n = qt_twt,
    lang = lang,
    since = inidt,
    resultType = "mixed"
)

View(tweetdata)
##' ---------------------------------------------------------------------------------


##' ---------------------------------------------------------------------------------
##' processing, cleaning and organizing
##' ---------------------------------------------------------------------------------
# conv from list to vector/matrix
tweetlist <- sapply(tweetdata, tweet_get_text)

# convert to utf-8
tweetlist <- iconv(tweetlist, to = "utf-8", sub = "")

# clear, RT, @, numbers and punctuation
tweetlist <- limpaTweets(tweetlist) # from Utils.R

# remove NAs
tweetlist <- na.omit(tweetlist)

# remove "" character
tweetlist <- tweetlist[tweetlist != ""]

# writing recovered tweets into a txt file for later analysis
file_name <-
    paste("tweets", subject, toString(Sys.Date()), ".txt", sep = "")
file_path <- paste("data/processed/", file_name, sep = "")
write_lines(x = tweetlist, path = file_path, append = FALSE)
##' ---------------------------------------------------------------------------------


##' ---------------------------------------------------------------------------------
##' populating a corpus to create a wordCloud
##' ---------------------------------------------------------------------------------
##'
##' ---------------------------------------------------------------------------------

# load my own stopwords_ptBR list gathered from many internet sources
stpwrd <-
    readr::read_lines(file = "data/processed/stopwords_ptBR.txt")

# corpus
tweet_corp <- tm::Corpus(VectorSource(tweetlist))
tweet_corp <- tm::tm_map(tweet_corp, content_transformer(tolower))

# remove all english and portuguese stopwords tm package
tweet_corp <-
    tm::tm_map(tweet_corp, function(x)
        removeWords(x, stopwords()))
tweet_corp <-
    tm::tm_map(tweet_corp, function(x)
        removeWords(x, stopwords(kind = "portuguese")))
tweet_corp <-
    tm::tm_map(tweet_corp, function(x)
        removeWords(x, stpwrd))

# remove single letters
tweet_corp <-
    tm::tm_map(tweet_corp, function(x) {
        gsub(pattern = "\\b[A-z]\\b{1}", replacement = " ", x)
    })

# remove all extra blank spaces
tweet_corp <-
    tm::tm_map(tweet_corp, function(x) {
        tm::stripWhitespace(x)
    })
tweet_corp <-
    tm::tm_map(tweet_corp, function(x) {
        tm::removePunctuation(x)
    })
tweet_corp <- tm::tm_map(tweet_corp, function(x) {
    trimws(x)
})

# reviewing/checking
tm::inspect(tweet_corp)
tweet_corp



# Gerando uma nuvem palavras
# RColorBrewer::display.brewer.all()
# colors01 <- brewer.pal(08, "Dark2")
# colors01 <- grDevices::rainbow()
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
##' words frequencies and associations
##' ---------------------------------------------------------------------------------
# matrix
tweettdm <- tm::TermDocumentMatrix(tweet_corp)
tweettdm

# Finding frequent words
tm::findFreqTerms(tweettdm, lowfreq = 11)

# Seeking for associations
tm::findAssocs(tweettdm,
               c("indice", "subiu", "acoes"),
               0.60)

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


##' ---------------------------------------------------------------------------------
##' Sentiment Analysis
##' ---------------------------------------------------------------------------------
##'
##' source: https://www.youtube.com/watch?v=-JW6_kcHDj4
##' source: DSA Project solution
##' ---------------------------------------------------------------------------------

#leitura do dicionario com o peso das palavras para classificacao
wrd <- readr::read_csv(file = "data/raw/lexico_v3.0.txt",
                       col_names = c("word", "type", "sent", "type_classific"))


sentimento.score <-
    function(sentences,
             wrd,
             .progress = "none") {
        # Criando um array de scores com lapply
        scores <-
            laply(sentences, function(sentence, wrd) {
                sentence <- gsub("[[:punct:]]", "", sentence)
                sentence <- gsub("[[:cntrl:]]", "", sentence)
                sentence <- gsub("\\d+", "", sentence)
                sentence <- sapply(sentence, tryTolower)
                #print(sentence)
                word.list <- str_split(sentence, "\\s+")
                words <- unlist(word.list)
                sc <- plyr::laply(words, function(item, wrd) {
                    a <-  wrd[wrd$word == item, ]$sent
                    if (length(a) <= 0) {
                        a = 0
                    }
                    return(a[1])
                }, wrd)
                score <- sum(sc)
                return(score)
            }, wrd, .progress = .progress)
        scores.df <- data.frame(text = sentences, score = scores)
        return(scores.df)
    }

##' ---------------------------------------------------------------------------------
##' small data to evaluete the function behavior
##' ---------------------------------------------------------------------------------
##'
##'
# Criando massa de dados para teste
teste <- c(
    "saiba voce precisa assessor investimentos assessor parar educar",
    "bonito bonito bonito bonito bonito",
    "mercado horrivel"
)
# Testando a função em nossa massa de dados dummy
testesentimento <- sentimento.score(teste, wrd)
#class(testesentimento)

# Verificando o score
# 0 - expressão não possui palavra em nossas listas de palavras positivas e negativas ou
# encontrou uma palavra negativa e uma positiva na mesma sentença
# 1 - expressão possui palavra com conotação positiva
# -1 - expressão possui palavra com conotação negativa
testesentimento$score

#convert processed corpus back to a list
df <-
    data.frame(text = sapply(tweet_corp, identity),
               stringsAsFactors = F)
twlist <- as.list.data.frame(df$text)

# calculating and plotting sentiment score
scores <- sentimento.score(tweetlist, wrd, .progress = "text")
boxplot(x = scores$score)
histogram(x = scores$score, main = "Análise de Sentimentos")

# Using Naive Bayes to determine emotion
# https://cran.r-project.org/src/contrib/Archive/Rstem/
# https://cran.r-project.org/src/contrib/Archive/sentiment/

# Classificando emocao
class_emo <-
    classify_emotion(twlist, algorithm = "bayes", prior = 1.0)
emotion <- class_emo[, 7]

# Substituindo NA's por "Neutro"
emotion[is.na(emotion)] <- "Neutro"

# Classificando polaridade
class_pol <- classify_polarity(twlist, algorithm = "bayes")
polarity <- class_pol[, 4]

# Gerando um dataframe com o resultado
sent_df <- data.frame(
    text = twlist,
    emotion = emotion,
    polarity = polarity,
    stringsAsFactors = FALSE
)

# Ordenando o dataframe
sent_df <- within(sent_df,
                  emotion <- factor(emotion, levels = names(sort(
                      table(emotion),
                      decreasing = TRUE
                  ))))

# Emoções encontradas
ggplot(sent_df, aes(x = emotion)) +
    geom_bar(aes(y = ..count.., fill = emotion)) +
    scale_fill_brewer(palette = "Dark2") +
    labs(x = "Categorias", y = "Numero de Tweets")

# Polaridade
ggplot(sent_df, aes(x = polarity)) +
    geom_bar(aes(y = ..count.., fill = polarity)) +
    scale_fill_brewer(palette = "RdGy") +
    labs(x = "Categorias de Sentimento", y = "Numero de Tweets")
