###################################################### 
### Adapting the RETRIEVE & RANK SERVICES APIs to R - RCURL and HTTR
### This service helps users find the most relevant information for their query by using a combination of search and machine learning algorithms 
### to detect "signals" in the data. Built on top of Apache Solr, developers load their data into the
### service, train a machine learning model based on known relevant results, 
### 
### http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/retrieve-rank.html
### http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/apis/#!/retrieve-and-rank
### https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/retrieve-rank/get_start.shtml
### Before you begin you will need (1) An IBM Bluemix demo account (2) R&R Service (3) and Credentials to that Service
###
### October 9, 2015
######################################################

#######  WARNING  - this is VERY EARLY STAGE CODE -  Has worked  but has areas that need improvement
#######  WARNING  - ONLY RETRIEVE - RANK (Ground Truth) NOT IMPLEMENTED YET
 
library(RCurl) 
library(httr)
library(XML)
library(data.table)
library(reshape2)
library(dplyr)
library(plyr)
library(stringr)
library(reshape2)
library(splitstackshape)

setwd("C:/Users/Ryan Anderson/Documents/_IBM Experimental/Project Haystack") #pc
#setwd("/Users/ryan/Documents/Project Haystack")  # mac jack

### CREDENTIALS FOR WATSON ECOSYSTEM SERVICE - RETRIEVE & RANK
url="https://gateway.watsonplatform.net/retrieve-and-rank/api"
username="12345678-5555-5555-5555-12345678999" # you will need your own
password="5555555555" # you will need your own
username_password = paste(username,":",password,sep="")


# PROBLEM > If you get this > Error in function (type, msg, asError = TRUE)  :  SSL certificate problem: self signed certificate in certificate chain 
# SOLUTION then you need to do this > YOU MUST KEEP THIS IN FOR below to work To begin: this line sets CERT Global to make a CA Cert go away - http://stackoverflow.com/questions/15347233/ssl-certificate-failed-for-twitter-in-r
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),httpauth=AUTH_BASIC)) # NOTE - the "httpauth=AUTH_BASIC" piece gets rid of the "Error: UNAUTHORIZED" message 


### >> Hierarchy  SOLR CLUSTER (CLUSTERID) 
  ### >> SET (CONFIGURATION)  
    ### >>  CREATE (COLLECTION) 
      ### >> UPLOAD CONTENT
        ### >> Then (later) Ranking - Ground Truth 

solr_config_file <- "cranfield_solr_config.zip"     # CONTAINS XMLs > SCHEMA, SOLRCONFIG, STOPWORDS, SYNONYMS, PROTWORDS and CURRENCY.XML
solr_config_name <- "haystack_config2"  #   You gotta call config something. (replaced 'example')
solr_collection_name <- "example-collection" # OTHER > "haystack_collection2"  # you gotta call collection something (replaced 'example')



### FUNCTION DECLARE: LIST Solr clusters  RAW GET - /v1/solr_clusters/
watson.retrieve_and_rank.SOLR_clusters_raw <- function() 
  {getURL(paste(url,"/v1/solr_clusters/",sep=""),userpwd = username_password)}

watson.retrieve_and_rank.SOLR_clusters_raw()  ### TEST - HAL, are you there? >> solr_cluster_status\":\"READY\"}]}"

### FUNCTION DECLARE:List Solr clusters  GET - /v1/solr_clusters/  # WHO IS READY?
watson.retrieve_and_rank.SOLR_clusters_list <- function() 
{
  response <- watson.retrieve_and_rank.SOLR_clusters_raw()
  response <- as.data.frame(strsplit(as.character(response),",",""))
  response[,1] <- gsub("\"clusters\":","",response[,1])  # trim the fat before text to columns
  setnames(response,"V1")
  response <- cSplit(response, 'V1', sep=":", type.convert=FALSE)
  setnames(response,c("solr","trait")) 
  response$solr <- gsub("[[:punct:]]","",response$solr) 
  solrclusterid <- response$trait[1]
  solrclusterid <- gsub("\"","",solrclusterid) 
  return(solrclusterid)  # <--- HERE IS THE SOLR CLUSTER ID
}

watson.retrieve_and_rank.SOLR_clusters_list()  ### solrclusterid comes back


### FUNCTION - CREATE SOLR Cluster - post /v1/solr_clusters/  ## first time through took about 5 minutes to be 'ready
watson.retrieve_and_rank.create_cluster <- function() 
{ response <- POST(url=paste(url,"/v1/solr_clusters/",sep=""),authenticate(username,password))
  return(content(response, "text"))}
## TO DO - Figure out how to # trouble with specifying name (syntax) - #{\"cluster_size\":\"1\",\"cluster_name\":\"My cluster\"}" 


# CREATE SOLR
# watson.retrieve_and_rank.create_cluster()  # only lets you do one :-) > may not create any more Solr clusters (current limit: 1)."

# CLUSTER LIST - LIST *** ALL *** SOLR Clusters
getURL(paste(url,"/v1/solr_clusters/",sep=""),userpwd = username_password)

# RETRIEVE & STORE SOLR CLUSTER ID - what is ID of the cluster we created?
solrclusterid <- watson.retrieve_and_rank.SOLR_clusters_list()

# CLUSTER STATUS (ONE CLUSTER) - Returns status and other information about ONE cluster. One ID - ready freddy?
getURL(paste(url,"/v1/solr_clusters/",solrclusterid,sep=""),userpwd = username_password)

# CLUSTER CONFIG (ONE CLUSTER) - Retrieves all configurations for a cluster. {\"solr_configs\":[]}"
getURL(paste(url,"/v1/solr_clusters/",solrclusterid,"/config/",sep=""),userpwd = username_password)

###### FUNCTION - DELETE CLUSTER - Receives name of CLUSTER to Kill; May not be able to do this until training complete - DELETE /v1/solr_clusters/{solr_cluster_id}
watson.retrieve_and_rank.deletecluster <- function(kill_cluster) 
  { return(DELETE(paste(url,"/v1/solr_clusters/",kill_cluster,sep=""), userpwd = username_password)) }
### WARNING / DANGER - (works)
solrclusterid # Cluster about to be KILLED
# watson.retrieve_and_rank.deletecluster(solrclusterid)  #### CAREFUL // WARNING DELETE

## CONFIGURE -- CONFIGURATION SOLR - worked
# Upload Solr configuration > Uploads the configuration files to use with your Solr collection. 
# Include schema.xml, solrconfig.xml, and other files you need for your configuration. 
# https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/retrieve-rank/get_start.shtml
# Upload the sample configuration that you downloaded in Step 2. We name the configuration


## CONFIGURE THE CLUSTER - UPLOAD  - (to do: make this function)
watson.retrieve_and_rank.configurecluster <- function()
{
    response <- POST(url=paste(url,"/v1/solr_clusters/",
                               solrclusterid,"/config/",solr_config_name,
                               sep=""),
                     authenticate(username,password),
                     body = (file = upload_file(solr_config_file)))
    # response  # post configure, what did we get back?
    return(content(response, "text"))  ## worked
}
watson.retrieve_and_rank.configurecluster() # test


# CONFIGS - Retrieves all configurations for a cluster. {\"solr_configs\":[]}"
getURL(paste(url,"/v1/solr_clusters/",solrclusterid,"/config/",sep=""),userpwd = username_password)


  ### CREATE --- CREATE COLLECTION Create a collection that is named example-collection and associate it with the example-config configuration:
  

  watson.retrieve_and_rank.createcollection <- function(solr_collection_name)
  {
    response <- POST(url=paste(url,"/v1/solr_clusters/",
                             solrclusterid,
                             "/solr/admin/collections",
                             sep=""),
                   authenticate(username,password),
                   body = list(action="CREATE",
                               name=solr_collection_name,
                               collection.configName="haystack_config"),
                   encode = "form")
  # response
  response # response back?
  return(content(response, "text"))  # what's inside?
  }  
  watson.retrieve_and_rank.createcollection("haystack_collection2")
  # solr_collection_name <- "haystack_collection2"       #solrcollectionname <- "example-collection"
  

    ### LIST COLLECTION - #works
  watson.retrieve_and_rank.listcollections <- function(collection_name,config_name)
  {
    response <- POST(url=paste(url,"/v1/solr_clusters/",solrclusterid,
                "/solr/admin/collections",sep=""),authenticate(username,password),
                body = list(action="LIST",name=collection_name,
                collection.configName=config_name),encode = "form")
    response
    return(content(response, "text"))
  } # to do later - update to pass in config name and collection name
  watson.retrieve_and_rank.listcollections("example-collection","haystack_config") # test
    
    
    
  ### SOLR CLUSTER STATUS
  getURL(paste(url,"/v1/solr_clusters/",sep=""),userpwd = username_password) 
  
    
    ### CONFIGS?
    getURL(paste(url,"/v1/solr_clusters/",solrclusterid,"/config/",sep=""),userpwd = username_password) 
    
    
    ### UPLOAD CONTENT #Adds content to a Solr index so you can search it. # post /v1/solr_clusters/{solr_cluster_id}/solr/{collection_name}/update 
    POST /v1/solr_clusters/{solr_cluster_id}/config/{config_name}
    content_file <- "cranfield_data.json"
    response <- POST(url=paste(url,"/v1/solr_clusters/",
                               solrclusterid,"/solr/",
                               solr_collection_name,
                               "/update", sep=""),
                     authenticate(username,password),
                     add_headers("Content-Type"="application/json"),
                     body = (file = upload_file(content_file))
                    )
    response 
    content(response, "text")

    # status 200 - "{\"responseHeader\":{\"status\":0,\"QTime\":1974}}\n"


    getURL(paste(url,"/v1/solr_clusters/",solrclusterid,"/config/",sep=""),userpwd = username_password)    

    ######## LET'S ASK QUESTIONS
    ######## GET /v1/solr_clusters/{solr_cluster_id}/solr/{collection_name}/select
          

    ### SIMPLE TEST --> (Static)
    getURL(paste(url,"/v1/solr_clusters/",solrclusterid,"/solr/",solr_collection_name,"/select",
     "?q=","what%20is%20the%20basic%20mechanism%20of%20the%20transonic%20aileron%20buzz&wt=json&fl=id,title", 
     sep=""),userpwd = username_password)
    
    
    
    solrclusterid <- "scc555555_5555_5555_5555_55555555" 
    solr_collection_name <- "example-collection"  # this was trained, the other not yet
  
        ### FUNCTION DECLARE  ---- RETRIEVE AND RANK QUERY -  CORE FUNCTION TO RECEIVE PROCESS AND FORMAT THE RESPONSE
        watson.retrieve_and_rank.query <- function(question) 
        {   response <- getURL(paste(url,"/v1/solr_clusters/",solrclusterid,"/solr/",solr_collection_name,"/select",
                        "?q=",URLencode(question),
                        "&wt=json&fl=id,title",sep=""),
                        userpwd = username_password)  
            response <- as.data.frame(strsplit(as.character(response),"\"id\"",""))
            colnames(response) <- c("V1")
            stats <- paste(response$V1[1])
            response <- response[-c(1), ] # now that we've captured - remove dud first row
            response <- data.frame(response)
            response$response <- gsub("\"title\":","|",response$response) 
            response <- cSplit(response, 'response', sep="|", type.convert=FALSE)
            colnames(response) <- c("index","title")
            response$index <- gsub("[[:punct:]]","",response$index) 
            response$title <- gsub("[[:punct:]]","",response$title) 
            return(response)
            }
      
        
      ### EXERCISE THE RETRIEVE AND RANK QUERY
      responses <- watson.retrieve_and_rank.query("papers on shock-sound wave interaction?")
      responses   # Ground Truth --> "papers on shock-sound wave interaction.","64","4","65","1","496","0"
    
      ### EXERCISE THE RETRIEVE AND RANK QUERY
      responses <- watson.retrieve_and_rank.query("experimental studies of creep buckling?")  # lots in this 101X Range - if no ranking - fildcard
      responses   # Ground Truth --> "experimental studies of creep buckling.","1020","4","1018","3","1019","3","1024","2","1012","1","1016","1","1022","1","950","0"
    
      ### QUERY AND FORMAT
      response <- watson.retrieve_and_rank.query("how does heat impact design?")
      


## --- INCOMPLETE ----  RETRIEVE ABOVE SORT OF DONE, MORE WORK TO DO ON RANK AND UPLOADING GROUND TRUTH (later)
# https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/retrieve-and-rank/api/v1/
      
      getwd()
        

