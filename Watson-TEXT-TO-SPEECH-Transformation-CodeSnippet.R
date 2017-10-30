######################################################
### IBM Watson - TEXT TO SPEECH WITH EXPRESSIVE & VOICE TRANSFORMATION
### https://text-to-speech-demo.mybluemix.net/
### https://www.ibm.com/watson/developercloud/text-to-speech/api/v1/
### http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/text-to-speech.html
### Before you begin you will need (1) An IBM Bluemix demo account (2) TTS Services stood up (3) Credentials to each Service
######################################################

# THIS FOCUS IS ON EXPRESSIVE and CUSTOM VOICE MODELS
# https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/text-to-speech/using.shtml#expressive
# video https://www.youtube.com/watch?v=jmIfKaQ_sGc


library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(audio)
library(Rtts)
library(splitstackshape)
library(png)

######### Housekeeping And Authentication
setwd("/Users/ryan/Documents/Project_HAL")
getwd()
source("keys.R")

img <- readPNG("tts.png")
grid::grid.raster(img)

# TEXT TO SPEECH - AUTHENTICATION AND CREDENTIALS
url_TTS <- "https://stream.watsonplatform.net/text-to-speech/api/v1/synthesize"
username_TTS # got the username from Bluemix hosted Text to Speech Service? https://console.bluemix.net/ - free account, free service
password_TTS #
username_password_TTS = paste(username_TTS,":",password_TTS,sep="")

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

###########
voices <- watson.TTS.listvoices()
voices
# 1    pt-BR_IsabelaVoice
# 2    en-US_MichaelVoice
# 3        ja-JP_EmiVoice
# 4    en-US_AllisonVoice
# 5      fr-FR_ReneeVoice
# 6  it-IT_FrancescaVoice
# 7      es-ES_LauraVoice
# 8     de-DE_BirgitVoice
# 9    es-ES_EnriqueVoice
# 10    de-DE_DieterVoice
# 11      en-US_LisaVoice
# 12      en-GB_KateVoice
# 13     es-US_SofiaVoice

########  FUNCTION --- TTS Playback (Hacky, but plays back WAV audio using system VLC)
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

## https://stream.watsonplatform.net/text-to-speech/api/v1/synthesize?text=hello%20there%20my%20friend&voice=en-GB_KateVoice

#### OK -  LET"S TEST "BASE" AUDIO
url <- "https://stream.watsonplatform.net/text-to-speech/api/v1/synthesize"
text <- URLencode("I'm sorry Dave. I'm afraid I can't do that.")
voice <- voices[2,] #
filename <- "hal1.wav"
watson.TTS.execute(url,text,voice,filename)


### EXPRESSIVE AVAILABLE ONLY ONE SOME VOICES (like ALlison)
text <- URLencode("<speak>Understood Dave. I heer you.
           <express-as type=\"Apology\"> I'm sorry Dave. I'm afraid I can't Open the pod bay doors.</express-as>
           This mission is too important for me to allow you to jeopardize it.
           <express-as type=\"Uncertainty\">Were you and Frank planning to disconnect me?</express-as>
           <express-as type=\"Apology\"> If so, I'm sorry. I'm afraid that's something I cannot allow to happen.</express-as>
                  </speak>
                  ")
voice <- voices[4,] #
filename <- "hal2.wav"
watson.TTS.execute(url,text,voice,filename)

###

### COMBINED EXPRESSIVE & TRANSFORMATION ONLY WORKS FOR ALLISON (#4)
text <- URLencode("<speak>Hello there.
                <express-as type=\"Apology\"> I'm sorry Paula.</express-as>
                <express-as type=\"Uncertainty\">I'm not really certain what will happen next.</express-as>
                <express-as type=\"GoodNews\"> Oh! Great news - now I know!</express-as>
                <voice-transformation type=\"Young\" strength=\"85%\">I can speak like a young girl. </voice-transformation>
                <voice-transformation type=\"Custom\" glottal_tension=\"40%\" breathiness=\"40%\"> or a bit strained. </voice-transformation>
                <voice-transformation type=\"Custom\" timbre=\"Breeze\" timbre_extent=\"60%\"> You can alter my voice timbre making me sound like this person, </voice-transformation>
                <voice-transformation type=\"Custom\" timbre=\"Sunrise\"> or like another person in your different applications. </voice-transformation>
                <voice-transformation type=\"Custom\" breathiness=\"90%\"> You can make my voice more breathy than it is normally. </voice-transformation>
                <voice-transformation type=\"Custom\" pitch=\"20%\" pitch_range=\"80%\" rate=\"60%\" glottal_tension=\"-80%\" timbre=\"Sunrise\"> And you can combine all this with modifications of my speech rate and my tone. </voice-transformation>
                </speak>
                  ")
voice <- voices[4,] #
filename <- "hal4.wav"
watson.TTS.execute(url,text,voice,filename)


## works to here


### TRANSFORMATION WORKS FOR MICHAEL (#2) but if you try Expressive as well it will fail
#  https://console.bluemix.net/docs/services/text-to-speech/SSML-transformation.html#built-in-transformations
voice <- voices[2,] # Michael
text <- URLencode("<speak>Hello friend.
                  <voice-transformation type=\"Young\" strength=\"85%\">I can speak like a young chap. </voice-transformation>
                  <voice-transformation type=\"Custom\" glottal_tension=\"40%\" breathiness=\"40%\"> or a bit strained. </voice-transformation>
                  <voice-transformation type=\"Custom\" timbre=\"Breeze\" timbre_extent=\"60%\"> You can alter my voice timbre making me sound like this person, </voice-transformation>
                  <voice-transformation type=\"Custom\" timbre=\"Sunrise\"> or like another person in your different applications. </voice-transformation>
                  <voice-transformation type=\"Custom\" breathiness=\"90%\"> You can make my voice more breathy than it is normally. </voice-transformation>
                  <voice-transformation type=\"Custom\" pitch=\"20%\" pitch_range=\"80%\" rate=\"60%\" glottal_tension=\"-80%\" timbre=\"Sunrise\"> And you can combine all this with modifications of my speech rate and my tone. </voice-transformation>
                  </speak>
                  ")
filename <- "hal5.wav"
watson.TTS.execute(url,text,voice,filename)



### TRANSFORMATION WORKS FOR MICHAEL (#2)
voice <- voices[2,] ## 2    en-US_MichaelVoice
voice <- voices[4,] ## 4    en-US_AllisonVoice

for (i in seq(0,100,50)){
  print(paste("Number is:", i))
  text <- URLencode(paste("<speak><voice-transformation type=\"Young\" strength=\"",i,"%\">I can speak like a young person. Amplitude is ",i," </voice-transformation></speak>"))
  print(text)
  filename <- "hal7.wav"
  watson.TTS.execute(url,text,voice,filename)
  wait(3)
}


### ROAD TEST -
## Lets' run through range from -80 to +80 for Pitch, Pitch Range, Glottal Tension
voice <- voices[2,] ## 2    en-US_MichaelVoice
voice <- voices[4,] ## 4    en-US_AllisonVoice

for (i in seq(-80,80,40)){
  print(paste("Number is:", i))
  text <- URLencode(paste("<speak><voice-transformation type=\"Custom\" pitch=\"",i,"%\" pitch_range=\"",i,"%\" rate=\"",i,"%\" glottal_tension=\"",i,"%\" timbre=\"Sunrise\"> And you can combine all this with modifications of my speech rate and my tone. </voice-transformation></speak>"))
  print(text)
  filename <- "hal7.wav"
  watson.TTS.execute(url,text,voice,filename)
  wait(5)
}

## Other Features
#timbre=\"Breeze\"
#timbre=\"Sunrise\
#breathiness=\"90%\">
#type=\"Young\" strength=\"",i,




########## LICENSE
#    Licensed under the Apache License, Version 2.0 (the "License");  you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
#    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
#    This is personal code - not affiliated with IBM - and presented with no warranties - use at your own risk





