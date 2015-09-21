###################################################### 
### Experimental Code. R Interface for IBM Watson Services 
### Focus: SPEECH TO TEXT - R Programming Language Interface
### DOCS: https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/speech-to-text/
### Before you begin you will need (1) An IBM Bluemix demo account (2) A dialog App and (3) Credentials to that Service and confirm you're able to CURL service with
### (4) some sample WAV files or method to utter audio to watson STT service
######################################################

library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)

# Speech-To-Text-Orange credentials": {
url <- "https://stream.watsonplatform.net/speech-to-text/api"
username <-"55555555-5555-5555-5555-55555555" # you need your own - STT service credentials from bluemix
password <- "44444444444"  # you need your own - STT service credentials from bluemix
username_password = paste(username,":",password,sep="")

# TEST CURL AND CREDS# API Endpoint  https://stream.watsonplatform.net/speech-to-text/api
# curl -u $USERNAME:$PASSWORD "https://stream.watsonplatform.net/speech-to-text/api/v1/models" # WORKS

### FUNCTION to test connectivity and return models availalle
watson.speech_to_text.getmodels <- function()
{return(GET(url=paste(url,"/v1/models",sep=""),
           authenticate(username,password)))}
## function done.  
watson.speech_to_text.getmodels() # returns list of 10+ models ##### works


# command line call works: curl -u $USERNAME:$PASSWORD -H "content-type: audio/wav" --data-binary @"ryan_rasp_pi1.wav" "https://stream.watsonplatform.net/speech-to-text/api/v1/recognize"

###### FUNCTION - ANalyze AUDIO WAV file with IBM Watson Speech to Text service
watson.speech_to_text.recognize <- function(audio_file)
{ return(POST(url=paste(url,"/v1/recognize",sep=""),
              authenticate(username,password),
              add_headers("Content-Type"="audio/wav"),
              body = (file = upload_file(audio_file))  
              ))} #works # hope this helps you with syntax!

#### FUNCTION TO TIDY UP the STT response - just export the TRANSCRIPT ONLY
stt_transcript_only <- function(raw) 
{
  data <- as.data.frame(strsplit(as.character(raw),"\\n"))
  data <- data[c(7), ] # for now, grab just what we want
  data <- paste(data) # kill levels, - fyi this nukes confidence % info (may want later)
  data <- gsub("  ","",data) # remove excessive whitespace  0 cannot use ALL [[punct]] here
  data <- gsub("\\\\","",data) # remove punct we dont like
  data <- gsub("\"","",data) # remove punct we dont like
  data <- gsub("transcript","",data) # remove excessive whitespace
  data <- gsub(":","",data) # remove excessive whitespace - later: Improve this tidy step. 
  return(data) 
}


######### OK - let's do stuff!

## TESTS OK
response <- watson.speech_to_text.recognize("ryan_rasp_pi1.wav") #make sure your WAV is in the working directory getwd() to check
response # takes about 5 seconds - this is not best method # You're looking for a "200" 
#content(response,"text") # raw results
transcript <- stt_transcript_only(content(response,"text"))
transcript # extracted the core translation (deleted a bunch more)


########## LICENSE

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
