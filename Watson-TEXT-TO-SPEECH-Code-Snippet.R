###################################################### 
### IBM Watson - TEXT TO SPEECH - Code Snippet 
### Experimental Code. R Interface for IBM Watson Services 
### http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/text-to-speech.html
### Before you begin you will need (1) An IBM Bluemix demo account (2) TTS Services stood up (3) Credentials to each Service 
######################################################
  
library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(audio) 
library(seewave) # need to play wav back?
library(Rtts)
library(splitstackshape)
 
# TEXT TO SPEECH - AUTHENTICATION AND CREDENTIALS 
url_TTS <- "https://stream.watsonplatform.net/text-to-speech/api"
username_TTS <-"54816193-48c4-486e-98de-YOUR_KEY_HERE" # you need your own - STT service credentials from bluemix
password_TTS <- "YOUR_PW_HERE"  # you need your own - STT service credentials from bluemix
username_password_TTS = paste(username_TTS,":",password_TTS,sep="")
setwd("/Users/ryan/Documents/Service - Text to Speech")
getwd()

####### Function to list voices
watson.TTS.listvoices <- function()
{
  voices <- GET(url=paste("https://stream.watsonplatform.net/text-to-speech/api/v1/voices"),authenticate(username_TTS,password_TTS))
  data <- content(voices,"text")
  data <- as.data.frame(strsplit(as.character(data),"name"))
  data <- data[-c(1:2), ] # remove dud first row
  data <- strsplit(as.character(data),",")
  data <- data.frame(matrix(data))
  colnames(data) <- "V1"
  data <- cSplit(data, 'V1', sep="\"", type.convert=FALSE)
  data <- data.frame(data$V1_04)
  data[,1]  <- gsub("\\\\","",data[,1] )
  return(data)
}
watson.TTS.listvoices()

########  FUNCTION --- TEXT TO SPEECH
watson.TTS.execute <- function(url1,text1,voice1,filename1)
{
  the_audio = CFILE(filename1, mode="wb") 
  curlPerform(url = paste(url1,"?text=",text1,"&voice=",voice1,sep=""),
              userpwd = username_password_TTS,
              httpheader=c(accept="audio/wav"),
              writedata = the_audio@ref)
  close(the_audio)
  system(paste("open",filename1,"-a vlc"))
}

###########
voices <- watson.TTS.listvoices()
voices
    #1        ja-JP_EmiVoice
    #2    en-US_AllisonVoice  # voices[2,]  #  "en-US_AllisonVoice"
    #3      fr-FR_ReneeVoice
    #4  it-IT_FrancescaVoice
    #5      es-ES_LauraVoice
    #6     de-DE_BirgitVoice
    #7    es-ES_EnriqueVoice
    #8     de-DE_DieterVoice
    #9       en-US_LisaVoice
    #10      en-GB_KateVoice
    #11   en-US_MichaelVoice
    #12     es-US_SofiaVoice


#### OK -  LET"S LAUNCH
url <- "https://stream.watsonplatform.net/text-to-speech/api/v1/synthesize"
text <- URLencode("The quick brown fox jumped over the lazy crazy dog")
voice <- voices[9,] # us lisa  #voice <- voices[10,] # uk kate
filename <- "audio_file.wav"
watson.TTS.execute(url,text,voice,filename)


#### OK -  LET"S LAUNCH
url <- "https://stream.watsonplatform.net/text-to-speech/api/v1/synthesize"
text <- URLencode("Today was a great day.  The sun was shining and the surf was good")
voice <- voices[11,] # us michael
filename <- "audio_file.wav"
watson.TTS.execute(url,text,voice,filename)


########## LICENSE
#    Licensed under the Apache License, Version 2.0 (the "License");  you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
#    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.

    




