######################################################
### Experimental Code.  Experimental R Interface for IBM Watson Services -
### Focus: IBM Watson Tone Analyzer is a service that helps people comprehend, consume, and revise the language tone of their writing for more efficient communications 
### https://tone-analyzer-demo.mybluemix.net/
### http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/tone-analyzer/
### http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/apis/tone-analyzer-apis.html
### https://github.com/rustyoldrake/R_Scripts_for_Watson - Ryan Anderson  - this is my test code.  Use at your own peril!  Representations do not necessarily represent the views of my employer
### Inside Out is a 2015 American 3D computer-animated comedy-drama adventure film produced by Pixar Animation Studios and released by Walt Disney Pictures. All media and images belong to Pixar/Disney and used under fair use (teaching and research)
######################################################

library(RCurl) # to talk to Watson - REST APIS # install.packages("RCurl") # if the package is not already installed
library(httr) # comms
library(XML)  # comms and data
library(data.table) # data shaping
library(reshape2) # data shaping
library(tidyr) # data cleaning
library(dplyr) # data cleaning
library(png) # for the presenting of images

######### Housekeeping And Authentication 
setwd("/Users/ryan/Documents/Service - Tone Analyzer")
getwd()
source("keys.R") # this files is where you put your Access Credentials from Bluemix (username and password)

base_url_TON = "https://gateway.watsonplatform.net/tone-analyzer-beta/api"

### GET - Basic Test 
getURL("https://gateway.watsonplatform.net/tone-analyzer-beta/api/v3/tone?version=2016-02-11&text=hello",userpwd = username_password_TON ) 

### Function to process output from API and table
tidyResponse <- function(data)
{
  data <- as.data.frame(strsplit(as.character(data),"\"score\""))
  data <- data[-c(1), ] # remove dud first row
  data  <- gsub("\"tone_id\":","",data)
  data  <- gsub(":","",data)
  data  <- gsub("\"","",data)
  data  <- gsub("_big5","",data)
  data <- data.frame(data)
  data <- data.frame(do.call('rbind', strsplit(as.character(data$data),',',fixed=TRUE)))
  data <- data[,-c(3:6), ] # remove dud first row
  data <- data[c("X2", "X1")]
  data$X1 <- as.character.numeric_version(data$X1) # not sure why, but coercing to numbers requires this
  data$X1 <- as.numeric(data$X1)
  data$X1 <- round((data$X1),2)
  setnames(data,c("trait","signal"))
  return(data)
}


### FUNCTION to post data to Tone Analyzer and return results
process_data_to_tone <- function(text)
{
  response <- POST(url="https://gateway.watsonplatform.net/tone-analyzer-beta/api/v3/tone?version=2016-02-11",
                   authenticate(username_TON,password_TON),
                   add_headers("Content-Type"="text/plain","charset"="UTF-8"), 
                   body=text )

  response_text <- content(response, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
  abc <- tidyResponse(response_text)
  return(abc)
}


process_data_to_tone("I am certainly going to make this project a success") # not perfect, but good enough for now!
query <- "I am super scared and nervous about bees"
process_data_to_tone(query)

# trait signal
# 1              anger   0.01
# 2            disgust   0.02
# 3               fear   0.99
# 4                joy   0.03
# 5            sadness   0.02
# 6         analytical   0.00
# 7          confident   0.00
# 8          tentative   0.00
# 9           openness   0.21
# 10 conscientiousness   0.22
# 11      extraversion   0.76
# 12     agreeableness   0.37
# 13       neuroticism   0.99


  
### FUNCTION to recieve results of analysis, pull out 5 traits, and show image and plot
display_results <- function(results){
  results <- data.frame(results[1:5,]) # just the gang :)
  results <- results[with(results, order(-signal)),] # reorder
  results <- data.frame(results,stringsAsFactors=FALSE) # stringsAsFactors=FALSE keeps junky factors out
  results
  
  main_trait <- paste(results$trait[1]) # paste kills levels 
  main_signal <- results$signal[1]
  signal_threshold <- 0.40 # this is level that we present 
  
  ## first, let's kick the SMS notice out from Twilio (see other file)
  send_SMS_results(query,main_trait) # send the 
  
  ### Inside Out is a 2015 American 3D computer-animated comedy-drama adventure film produced by Pixar Animation Studios and released by Walt Disney Pictures. All media and images belong to Pixar/Disney and used under fair use (teaching and research)
  ### These PNG mages belong to studios - for education purposes only (fair use) - be good!
  
  ## Switch Statement
  if(main_signal>signal_threshold)
  { 
    
    switch(main_trait,
           "joy" = image <- readPNG("joy.png"),
           "sadness" = image <- readPNG("sad.png"),
           "anger" = image <- readPNG("anger.png"),
           "disgust" = image <- readPNG("disgust.png"),
           "fear" = image <- readPNG("fear.png")
    )
  } else{image <- readPNG("all.png")}
  
  #image <- readPNG("anger.png")
  par(mfrow=c(1, 2))
  plot(1:1, type="n", main="Tone Results", xlab="", ylab="", asp=1)
  lim <- par()
  rasterImage(image, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  
  barplot(results$signal, main="Tone Analysis", 
          xlab="", ylab=query, 
          names.arg=results$trait,
          border="red",las=3, srt=45) 
}


###  OK!  LET'S DO STUFF
###  OK!  LET'S DO STUFF
###  OK!  LET'S DO STUFF




### OK - let's throw a few softballs - one for each inside out character (my kids love the movie!)

query <- "STEM is wonderful! I love to code and build stuff"
analysis <- process_data_to_tone(query)
display_results(analysis)

query <- "I'm terrified that the monkeys memorized my PIN"
analysis <- process_data_to_tone(query)
display_results(analysis)

query <- "Dont pick your nose - it's really gross"
analysis <- process_data_to_tone(query)
display_results(analysis)

query <- "I got a speeding ticket and I'm furious!"
analysis <- process_data_to_tone(query)
display_results(analysis)

query <- "I cant stop crying.  The world is such a dull place"
analysis <- process_data_to_tone(query)
display_results(analysis)





graphics.off()

