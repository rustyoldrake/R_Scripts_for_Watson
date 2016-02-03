### WARNING - this is very rough, but given VERY limited info I found, I figured I'd pop my code up here



http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/text-to-speech.html

####################################  TEXT TO SPEECH EXPERIMENT

(messy, but got it working - WAV format is a bit tempermental, VLC will play - other players no -
RIFF����WAVEfmt

library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(audio)
library(seewave) # need to play wav back?
library(Rtts)

play(sin(1:10000/32)) # test audio


# http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/text-to-speech.html
# Designed for streaming low-latency synthesis of audio from written text. The service synthesizes natural-sounding speech from input text in a variety of languages and voices that speak with appropriate cadence and intonation.

## To begin: this line sets CERT Global to make a CA Cert go away - http://stackoverflow.com/questions/15347233/ssl-certificate-failed-for-twitter-in-r
#options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),httpauth=AUTH_BASIC))

# TEXT TO SPEECH - RED
url_TTS <- "https://stream.watsonplatform.net/text-to-speech/api"
username_TTS <-"54816193-48c4-486e-98de-YOURSHERE" # you need your own - STT service credentials from bluemix
password_TTS <- "YOURS"  # you need your own - STT service credentials from bluemix
username_password_TTS = paste(username_TTS,":",password_TTS,sep="")
setwd("/Users/ryan/Documents/Service - Speech to Text (STT)")
getwd()

# this works from terminal (saves file)
# curl -u "54816193-48c4-486e-98de-YOU":"YOU" -X POST -H "Content-Type: application/json" -H "Accept: audio/wav" -D "{\"text\":\"hello world\"}" "https://stream.watsonplatform.net/text-to-speech/api/v1/synthesize" > hello_world2.wav
#this works from FIREFOX browser (prompts for user/pwd)
# https://stream.watsonplatform.net/text-to-speech/api/v1/synthesize?text=hi%20there%20ryan%20-%20you%20are%20great

######## WORKS!  this hits the IBM Watson Text to Speech API and then brings back (and saves) WAV

## WOOT WOOT - WORKS! WAV Playable with VLC - but still need some TLC
the_audio = CFILE("audio501.wav", mode="wb")
curlPerform(url = "https://stream.watsonplatform.net/text-to-speech/api/v1/synthesize?text=hi%20there%20ryan%20-%20you%20are%20great%20yes%20for%20sure",
            userpwd = username_password_TTS,
            httpheader=c(accept="audio/wav"),
            writedata = the_audio@ref)
close(the_audio)

system("open audio501.wav -a vlc")  # Great!  Took the text and gave it a voice!


## WOOT WOOT - WORKS! WAV with English Female accent
the_audio = CFILE("audio409.wav", mode="wb")
curlPerform(url = "https://stream.watsonplatform.net/text-to-speech/api/v1/synthesize?text=hi%20there%20ryan%20-%20you%20are%20great%20yes%20for%20sure&voice=en-GB_KateVoice",
            userpwd = username_password_TTS,
            httpheader=c(accept="audio/wav"),
            writedata = the_audio@ref)
close(the_audio)

system("open audio409.wav -a vlc")  # Great!  Took the text and gave it a voice!  (afplay not working - I think these are pretty 'rough' wav files)


## much more work - but very late - will post this as interim 
## still trying to get more standard POST and GET working.  no luck. 
