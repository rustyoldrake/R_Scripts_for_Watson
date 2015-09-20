###################################################### 
### Experimental Code. R Interface for IBM Watson Services 
### Focus: PERSONALITY INSIGHTS - USER PERSONAE SEGMENTATION - R Programming Language Interface
### Adapting the PERSONALITY INSIGHTS APIs https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/apis/ to RCURL and HTTR
### DOCS: https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/personality-insights/
### REFERENCE https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/personality-insights/api/v2/ 
### Before you begin you will need (1) An IBM Bluemix demo account (2) A dialog App and (3) Credentials to that Service and confirm you're able to CURL service with
######################################################

### Warning - this is experimental code - it runs, but has not been reviewed and is rough around the edges
### Demo - try this to familiarize yourself with service:  https://watson-pi-demo.mybluemix.net

library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(XML)
library(data.table)
library(reshape2)
library(dplyr)
library(stringr)
library(reshape2)

setwd("/Users/ryan/Documents/Project Daisy")  # contains a CSV for me to look up external content to scrape

# Personality-Insights-Service-Blue - credentials"
pi_url="https://gateway.watsonplatform.net/personality-insights/api/v2/profile"
username = "555555-5555-5555-5555-55555555" # your own here 
password = "444444444"  # your own here 
username_password = paste(username,":",password,sep="")
alldata <- data.frame(NULL) # where we will store our analysis

###### FUNCTION - ANalyze TEXT/DOC RECEIVED with Personality Insights servicen and return Analysis
watson.personality_insights.analyze <- function(TEXT)
{
  return(POST(url=pi_url,
          authenticate(username,password),
          add_headers("Content-Type"="text/plain","charset"="utf-8" ),
          body = TEXT
          ))
}
## Works: ## curl -X POST -u $USERNAME:$PASSWORD -H "Content-Type: text/plain" -d "$PROFILE" "https://gateway.watsonplatform.net/personality-insights/api/v2/profile"


#### FUNCTION TO TIDY UP THE PI RESPONSE TO TABLE FORMAT - some rough methods that can be improved below
tidyResponse <- function(data) 
{
  data <- as.data.frame(strsplit(as.character(data),"\"id\":\""))
  data <- data[-c(1:5), ] # remove dud first row
  data <- data.frame(matrix(data)) 
  data[,1]  <- gsub("\"","",data[,1] ) 
  data <- data.frame(do.call('rbind', strsplit(as.character(data$matrix.data),',',fixed=TRUE)))
  data <- data[!grepl('name:',data$X5),]
  data <- data[!grepl('children:',data$X5),]
  data <- data[,-c(2,6), ] # remove columns we dont need - duplicates or dont care for SAMPLING ERROR (now) but mght later
  setnames(data,c("trait","category","percentage","error"))
  data$percentage <- gsub("percentage:","",data$percentage) 
  data$category <- gsub("category:","",data$category) 
  data$error <- gsub("sampling_error:","",data$error) 
  data$error <- gsub("}","",data$error) # crude but effective
  data$error <- gsub("]","",data$error) # crude but effective
  data$percentage <- round((as.numeric(data$percentage)),4) # if you prefer % format like this
  data$error <- round((as.numeric(data$error)),4) # if you prefer % format like this
  rownames(data) <- NULL # resets row names to remove 'gaps'
  data$row <- as.numeric(rownames(data))
  return(data)
}

########## FUNCTION - ScreenScrape support Clean up Response from HTML to DOC.TXT (import speeches)
cleanDoc <- function(doc) {
  doc <- gsub("<.*?>", "", doc)
  doc <- gsub("\\t","",doc)
  doc <- gsub("\\n","",doc)
  doc <- gsub("\\r","",doc)
  doc <- gsub("\\t","",doc)
  doc <- gsub("\\n","",doc)
  doc <- gsub("\\r","",doc)
  doc <- gsub("&nbsp;","",doc)
  doc <- gsub("&quot;","",doc)
  return(doc)
}
## this is optimized for *test code* from http://www.americanrhetoric.com/top100speechesall.html - but you should substitute your own, and  tailor scrubbers
## for CSV - i did (a) a MANUAL transfer to XLS of the 100 urls -(b) removed atypicals, and (c) set up in a way 
## source site is copyright ar and many speeches, as they noted, are copyright or public domain, as noted in text.  Can also scrape library of congress - for example only
## format used here for this example ( you should map to your own)
## index	speaker	                    speech_name	      URL
##  1	    Martin Luther King, Jr. 	  I Have A Dream	  http://www.americanrhetoric.com/speeches/mlkihaveadream.htm
##  2	    John Fitzgerald Kennedy	    Inaugural Address	http://www.americanrhetoric.com/speeches/jfkinaugural.htm


###### OK - LET'S GO!  ACTION!  

####################### THIS PART OF THE CODE IS FIGURE OUT HOW TO SCRAPE/IMPORT TEST TEXT/CORPUS FOR ANALYSIS
getwd()
speeches <- read.csv("speeches.csv",header=TRUE)
speeches <- data.frame(speeches)
speeches
length <- dim(speeches)[1]+0  # how long is our list?
for (i in 1:length) 
{
    doc <- getURL(speeches$URL[i])
    doc <- cleanDoc(doc)
    response <- watson.personality_insights.analyze(doc)
    data <- content(response, "text") # here are the personality insights!
    data <- tidyResponse(data)
    data$speaker <- paste(speeches$speaker[i],"#",i) # adds the 'who' attribute # number for repeat people
    data$speech_name <- speeches$speech_name[i] # adds speec
    alldata <- rbind(alldata,data)
    print(paste("processed: ",speeches$speaker[i],"#",i,speeches$speech_name[i]))
} # takes about 60 seconds to process 95 speeches  
head(alldata)

#alldata <- setNames(alldata,c("trait","category","percentage","error","row","speaker","speech_name"))
#alldata_backup = alldata
#write.csv(alldata,"post-process-PI.csv")

head(alldata)  ## END OF CHAPTER - FULL DATA SET - YOU SHOULD HAVE nearly 100 people/speeches and 52 traits for each one (nearly 5k rows of 7 variables)

####### OK - Next CHapter - let's play around a little Look at a few things

alldata <- alldata[,-c(2,4,5) ] # remove category, error, row
alldata$percentage <- as.numeric(alldata$percentage) # keep things numeric
alldata$speaker <- gsub("\\\\","",alldata$speaker) # keep things clean
alldata$speech_name <- gsub("\\\\","",alldata$speech_name) # keep things clean

## Ok let's melt (elongate) along speaker speech and trait
alldata.melted <- melt(alldata,id.vars = c("speaker", "speech_name", "trait"))
head(alldata.melted)
alldata.cast <- dcast(alldata.melted, speaker  ~ trait)  ### PIVOT TABLE STYLE - Traits on top on X axis, speaker on Y
head(alldata.cast) ## SUCCESS!
write.csv(alldata.cast,"post-process-PI-pivot.csv")


### IF YOU LOOK AT this data in alldata.cast, you can see it's pretty much ready for you to 'take it' and translate it to test/validate/explore your hypothesis


#   speaker	                        Achievement	Activity level	Adventurousness	Agreeableness	Altruism
#76	Ronald Wilson Reagan # 29	      0.5469	    0.2969	0.7036	0.9306	0.9518
#16	Dorothy Ann Willis Richards	    0.1508	    0.2259	0.3805	0.9849	0.9499
#78	Ronald Wilson Reagan # 7	      0.3052	    0.2881	0.6313	0.9548	0.9483
#37	Geraldine Anne Ferraro # 54	    0.6561	    0.642	  0.6502	0.9206	0.9355
#45	Jesse Louis Jackson # 47	      0.2901	    0.3477	0.4982	0.9362	0.9331
#58	Lyndon Baines Johnson # 89	    0.4653	    0.1804	0.8074	0.8046	0.9261



# end of chapter - end of code 
# other code can take it from here here, clustering, Kohonen self organizing maps, devleopment of user personae, and machine learning (random forest) on this source info to for validation and testing of hypotheses


# for more information on the service
# http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/personality-insights.html
# science behind (and limitations of) the service: http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/personality-insights/science.shtml


# LIST OF TRAITS: 
# "Achievement striving" "Activity level"       "Adventurousness"     
# "Agreeableness"        "Altruism"             "Anger"                "Anxiety"             
# "Artistic interests"   "Assertiveness"        "Cautiousness"         "Challenge"           
# "Cheerfulness"         "Closeness"            "Conscientiousness"    "Conservation"        
# "Cooperation"          "Curiosity"            "Depression"           "Dutifulness"         
# "Emotionality"         "Excitement"           "Excitement-seeking"   "Extraversion"        
# "Friendliness"         "Gregariousness"       "Harmony"              "Hedonism"            
# "Ideal"                "Imagination"          "Immoderation"         "Intellect"           
# "Liberalism"           "Liberty"              "Love"                 "Modesty"             
# "Morality"             "Neuroticism"          "Openness"             "Openness to change"  
# "Orderliness"          "Practicality"         "Self-consciousness"   "Self-discipline"     
# "Self-efficacy"        "Self-enhancement"     "Self-expression"      "Self-transcendence"  
# "Stability"            "Structure"            "Sympathy"             "Trust"               
# "Vulnerability"
