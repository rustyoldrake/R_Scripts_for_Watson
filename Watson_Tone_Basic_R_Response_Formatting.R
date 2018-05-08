### TONE Analyzer for IBM Watson TONE - R Programming language
library(httr)
library(jsonlite)

### Keys to Kingdom and URL
setwd("/Users/ryan/Documents/Service - Tone Analyzer")
getwd()
source("keys.R")
base_url_TON = "https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19"
# VERSION MATTERS - thresholds
# 2017-09-21: The service can return results for the following tone IDs: anger, fear, joy, and sadness (emotional tones); analytical, confident, and tentative (language tones). The service returns results only for tones whose scores meet a minimum threshold of 0.5.
# 2016-05-19: The service can return results for the following tone IDs of the different categories: for the emotion category: anger, disgust, fear, joy, and sadness; for the language category: analytical, confident, and tentative; for the social category: openness_big5, conscientiousness_big5, extraversion_big5, agreeableness_big5, and emotional_range_big5. The service returns scores for all tones of a category, regardless of their values.

### Check we are locked and loaded with credentials
username_TON
password_TON
username_password_TON
base_url_TON


process_data_to_tone <- function(text)
{
  response <- POST(url="https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19",
                   authenticate(username_TON,password_TON),
                   add_headers("Content-Type"="text/plain","charset"="UTF-8"), 
                   body=text )
  
  response_text <- content(response, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
  return(response_text)
}

## WORDS TO TEST  - good response? 
words <- ("I am so happy that this API call worked I could burst with joy")
process_data_to_tone(words)

## OK - let's capture response, flatten and pick out what we need into data frames
watsonResponse <- process_data_to_tone(words)
watsonResponse

result <- fromJSON(watsonResponse, simplifyVector = TRUE, flatten = FALSE) # flatten
result

result$document_tone
# 1  emotion_tone  Emotion Tone
# 2 language_tone Language Tone
# 3   social_tone   Social Tone


## 1 EMOTIONAL Category - Anger, Disgust, Fear, Joy, Sadness
result_emotional_tones <- as.data.frame(result$document_tone$tone_categories$tones[[1]])
result_emotional_tones

## 2 LANGUAGE Category - Anaytical, Confident, Tentative
result_languages_tones <- as.data.frame(result$document_tone$tone_categories$tones[[2]])
result_languages_tones

## 3 SOCIAL Category - Big 5 - Openness, Conscientiousness, Extraversion, Agreeableness, Emotional Range
result_big5_tones <- as.data.frame(result$document_tone$tone_categories$tones[[3]])
result_big5_tones



# > result_emotional_tones <- as.data.frame(result$document_tone$tone_categories$tones[[1]])
# > result_emotional_tones
# score tone_id tone_name
# 1 0.013708   anger     Anger
# 2 0.007774 disgust   Disgust
# 3 0.020936    fear      Fear
# 4 0.845372     joy       Joy
# 5 0.032751 sadness   Sadness


# > result_languages_tones <- as.data.frame(result$document_tone$tone_categories$tones[[2]])
# > result_languages_tones
# score    tone_id  tone_name
# 1 0.000000 analytical Analytical
# 2 0.000000  confident  Confident
# 3 0.681699  tentative  Tentative

# > result_big5_tones <- as.data.frame(result$document_tone$tone_categories$tones[[3]])
# > result_big5_tones
# score                tone_id         tone_name
# 1 0.079059          openness_big5          Openness
# 2 0.180101 conscientiousness_big5 Conscientiousness
# 3 0.375533      extraversion_big5      Extraversion
# 4 0.984994     agreeableness_big5     Agreeableness
# 5 0.109405   emotional_range_big5   Emotional Range
