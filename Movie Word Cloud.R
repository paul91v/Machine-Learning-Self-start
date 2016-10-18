
#Viralheat function to Clean text
clean.text <- function(some_txt)
{  
  some_txt = gsub("&amp", "", some_txt)
  some_txt = gsub("(RT|via)((?:\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  # define "tolower error handling" function
  try.tolower = function(x)
    {     y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
        y = tolower(x)
    return(y)
    }
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}

#Testing the function
movieWordCloud = function(movie)
{
  library('twitteR')
  api_key <- "f4dwZEmNZ4xYBeJbz3ojGtXlu"
  api_secret <- "CZomRzS2GrBZe1GtfujeRfqvWBpCOUxN1dbnBsuDJk5N5qJaTs"
  access_token <- "1222478131-ylr2OkGoU1dnFls3AMuyV2StUltiTtMn7SzUw9t"
  access_token_secret <-"qyQLDWv5hF8p2KXx1pvfFdL74mv04tTjKE7lwfzmOSCNP"
  setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
  
  #Extracting the Tweets
  
  library(twitteR)
  library(ROAuth)
  library(ggplot2)
  library(plyr)
  
  movie.list = searchTwitter(movie, n = 1000)
  movie.df = twListToDF(movie.list)
  
  library(plyr)
  movie.txt = laply(movie.list,function(t)t$getText())
  
  
  library(tm)
  library(wordcloud)
  movie.clean = clean.text(movie.txt)
  movie.corpus = Corpus(VectorSource(movie.clean))
  movie.tdm = TermDocumentMatrix(movie.corpus, control = list(removePunctuation = TRUE, stopwords = c("machine","learning",stopwords("English")), removeNumbers = TRUE, tolower = TRUE))
  movie.m = as.matrix(movie.tdm)
  movie.word_freqs = sort(rowSums(movie.m), decreasing = TRUE)
  movie.dm = data.frame(words = names(movie.word_freqs), freq = movie.word_freqs)
  movie.trim.dm <- movie.dm[!(movie.dm$freq<10),]
  wordcloud(movie.trim.dm$words, movie.trim.dm$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
}

#movie1 = 
movieWordCloud(readline( prompt = "Enter the Name of the Movie: "))