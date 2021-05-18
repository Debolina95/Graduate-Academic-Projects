library(ggplot2)
library(dplyr)
install.packages('tm',dependencies = TRUE)
install.packages("wordcloud") 
library('tm')
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(lubridate)


## Which genre movie is earning the highest Box office revenue
Disney$income <- Disney$`Box Office_Inflation Adjusted`
subsetData <- subset(Disney, Disney$genre != "")

group_genre <- aggregate(income ~ genre, data = subsetData, FUN = sum)

currencyinbillion <- (group_genre$income/1000000000)
group_genre$currency <- currencyinbillion
head(group_genre)

group_genre$genre <- factor(group_genre$genre, levels = group_genre$genre[order(group_genre$currency)])

ggplot(group_genre, aes(genre,currencyinbillion)) + geom_bar(stat = "identity", aes(fill = genre), width = 0.5) + theme_minimal() +
   xlab("Genre")+ylab("Total Gross(billion)") + theme(panel.grid.major = element_blank(),
                                                     axis.text.x  = element_text(angle=75,size=9, vjust=0.7))



##  Which movie earned the highest income? 
datacloud <- head(arrange(Disney,desc(`Box Office_Inflation Adjusted`)), n = 20)

datacloud$Rank <- rank(datacloud$`Box Office_Inflation Adjusted`) 


docs <- Corpus(VectorSource(datacloud$title))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case

docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers

docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords

docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove punctuations

docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces

docs <- tm_map(docs, stripWhitespace)

dataframe <- data.frame(text=sapply(docs, identity), stringsAsFactors=F)

datacloud$movie_title <- dataframe$text


wordcloud(words = datacloud$movie_title ,freq = datacloud$Rank, min.freq=1,scale = c(1,0.5),
          max.words=200,random.order=FALSE,rot.per=0.35,colors=brewer.pal(8,"Dark2") ) 


## Top performing movings in terms of IMDB
a <- subset(Disney, Disney$imdb != "N/A")
d <- unique(head(arrange(a[,c("title", "imdb")],desc(imdb)),11))
d$numimdb <- as.numeric(d$imdb)
d
ggplot(data=d, aes(x=title, y=numimdb)) +
  geom_bar(stat="identity")

## Top performing movings in terms of Rotten tomatoes
a2 <- subset(Disney, Disney$rotten_tomatoes != "N/A")
e <- unique(head(arrange(a2[,c("title", "rotten_tomatoes")], desc(rotten_tomatoes)),10))
e$numimdb <- as.numeric(e$rotten_tomatoes)
e

## Top performing movings in terms of Metascore
a3 <- subset(Disney, Disney$metascore != "N/A")
e1 <- unique(head(arrange(a3[,c("title", "metascore")], desc(metascore)),12))
e1$numimdb <- as.numeric(e$metascore)
e1

## Top performing movies in terms of Genre
topbygenre <- unique(head(arrange(a[,c("title", "genre", "numimdb")],desc(imdb)),11, group_by(genre)))
topbygenre$numimdb <- as.numeric(topbygenre$imdb)

##regression
model <- lm(log(as.numeric(imdb)) ~ income, data=Disney)
summary(model)

model1 <- lm(as.numeric(Box.office..float.) ~ as.numeric(Budget..float.) + 
              Running.time + Country + genre + Language, data=Disney)
summary(model1)
plot(model1)

model2 <- lm((as.numeric(imdb)) ~ `Budget_Inflation Adjusted` + 
               Running.time..int. + Country + genre + Language, data=Disney)
summary(model2)
plot(model2)

## What is the trend of movies over the years
trendData <- Disney[,c(8,12,14)]

trendData$total_gross <- Disney$income


trendData$release_date <- parse_date_time(Disney$Release.date..datetime., orders = c("ymd", "dmy", "mdy"))
trendData$year <- year(trendData$release_date)

trendData$release_date <- NULL

trendData1 <- subset(trendData,trendData$genre != "")

group_genre_date <- aggregate(total_gross ~ year + genre,data = trendData1,FUN = sum)

subseTrendData <- subset(group_genre_date,group_genre_date$total_gross != 0)

options(scipen=1000000)
currency <- (subseTrendData$total_gross / 100000000) 
subseTrendData$sample <- currency

ggplot(subseTrendData, aes(x=year, y=currency, color=genre)) + geom_point(size = 1.9) + theme_minimal() + ylab("Total earnings in billion")

## Runtime distribution
runtime <- Disney[,c(2,9)]
runtime$num <- as.numeric(runtime$Running.time..int.)

ggplot(data=runtime, aes(x=num) +
  geom_histogram(binwidth = 5, fill = "white", color = "black") +
  scale_x_continuous(breaks = seq(0, 240, 60)) +
  scale_y_continuous(labels = scales::unit_format(scale = 1e-3, suffix = "k"), limits = c(0, 45e3)) +
  labs(x = "Runtime (minutes)", y = "Count (of movies)")
  
hist(runtime$num, main="Runtime distribution", xlab="Runtime (minutes)", ylab="Count (of movies)") 



##Prediction
as.numeric(Budget..float.) + Running.time + Country + genre + Language, data=Disney)


newdata <- data.frame(`Budget_Inflation Adjusted` = 164000000, Running.time..int. = "88", Country = "United States", genre = "Adventure", Language = "English")

predict(model2, newdata, interval = "prediction", level = 0.95)

##movies released per month
trendData2 <- Disney[,c(8,12,14)]


trendData2$release_date <- parse_date_time(Disney$Release.date..datetime., orders = c("ymd", "dmy", "mdy"))
trendData2$month <- month(trendData2$release_date)

hist(trendData2$month, main="Movies released per month", xlab="Month", ylab="Count (of movies)") 
