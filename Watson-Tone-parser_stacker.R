## Parser Stacker - Rack and Stack
## https://stackoverflow.com/questions/15347282/split-delimited-strings-in-a-column-and-insert-as-new-rows
## I tried this as test file: https://www.kaggle.com/rounakbanik/ted-talks/version/3  
##  "These datasets contain information about all audio-video recordings of TED Talks uploaded to the official TED.com website until September 21st, 2017. The TED main dataset contains information about all talks including number of views, number of comments, descriptions, speakers and titles. The TED transcripts dataset contains the transcripts for all talks available on TED.com.
## ted_main.csv - Contains data on actual TED Talk metadata and TED Talk speakers. and transcripts.csv - Contains transcript and URL information for TED Talks
## The data has been scraped from the official TED Website and is available under the Creative Commons License.


library(stringr)

setwd("/Users/ryan/Documents/Service - Natural Language Classifier (NLC)")
getwd()

data <- read.table("ted_transcripts.csv", header=FALSE, sep=",")
df <- as.data.frame(data)
df$V1 <- gsub("\\?", "?|", df$V1)  # here is where we want new lines / new rows flagged with "|"
df$V1 <- gsub("\\. ", ". |", df$V1) # here is where we want new lines / new rows flagged with "|"
paste(df$V1[[44]])  # we ok?

tempdf <- data.frame(NULL) # for loop hard work
newdf <- data.frame(NULL) # for our final destnation for all

 
dim(df)[1] # lenth of df
i=1
for(i in 1:dim(df)[1])
{
    print(paste("Step:",i))
    tempdf <- data.frame(strsplit(as.character(df$V1[[i]]),"\\|"))
    colnames(tempdf) <- "X1" 
    newdf <- rbind.data.frame(newdf,tempdf)
}

# write.csv(newdf, file = "ted_tall_data.csv",row.names=FALSE)

### Let's clean
newdf <- read.table("ted_tall_data2.csv", header=FALSE, sep=",")
iconv(newdf, to = "ASCII//TRANSLIT")

for(i in 1:10)
{
  print(i)
  print(paste(newdf$V1[i]))
}


### Lets' fresh start

tempdf <-  data.frame(matrix("", ncol = 14, nrow = dim(newdf)))  
colnames(tempdf) <- c("X1","anger","disgust","fear","joy","sadness","analytical","confident","tentative","openness","conscientiousness","extraversion","agreeableness","emotional_range")

## OK from here we should be able to call APIs
#for(i in 1:dim(newdf))
tempdf$X1 <- as.character(tempdf$X1)

# yes this is terrible and brutal , but time to move on ;)
tempdf$anger <- as.numeric(tempdf$anger)
tempdf$disgust <- as.numeric(tempdf$disgust)
tempdf$fear <- as.numeric(tempdf$fear)
tempdf$joy <- as.numeric(tempdf$joy)
tempdf$sadness <- as.numeric(tempdf$sadness)
tempdf$analytical <- as.numeric(tempdf$analytical)
tempdf$confident <- as.numeric(tempdf$confident)
tempdf$tentative <- as.numeric(tempdf$tentative)
tempdf$openness <- as.numeric(tempdf$openness)
tempdf$conscientiousness <- as.numeric(tempdf$conscientiousness)
tempdf$extraversion <- as.numeric(tempdf$extraversion)
tempdf$agreeableness <- as.numeric(tempdf$agreeableness)
tempdf$emotional_range <- as.numeric(tempdf$emotional_range)

tempdf[,2:14] <- 0  # set them all to 0

dim(tempdf)[1]

# OK - here's big loop - e.g. 268 k queries
for(i in 1:dim(tempdf)[1])
  {
  print(i)
  text <- paste(newdf$V1[i])
  print(text)
  tempdf$X1[i] <- text
  abc <- process_data_to_tone(URLencode(text))
  print(abc)
  t(abc$signal) # transpose results

  # here's a hacky way to transpose results to table
  for(j in 1:13)
    {
      temp <- t(abc$signal)[1,j]
      print(temp)
      tempdf[,(j+1)][i] <- temp
  }
}


  dem
  tempdf$joy[4] = 66
  
  
  
  tempdf$anger
  
  
  typeof(tempdf)[4] = 44
  
  t(abc$signal)[1,1]
  tempdf$anger[i] <- t(abc$signal)[1,1]
    i
  }

i=1000



