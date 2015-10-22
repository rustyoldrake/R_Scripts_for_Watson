#####  IBM WATSON - Retrieve and Rank - Ground Truth Seed Crystal
#####  Model to Analyze the Ranker Ground Truth CSV  (GT Health Check)
#####  Oct 22 2015 Update

library(stringr)
library(plyr)
library(dplyr) # pivot table of the ID's (count and distribution)
library(knitr)
library(markdown)
library(ggplot2)

setwd("/Users/ryan/Documents/Project Haystack")  
getwd()


#################################################
print("IBM Watson - Ground Truth Analysis Tool") # later for PDF text output
print("Analysis of 'Ground Truth' CSV File used for Retrieve and Rank Service")

## Import of CSV File that is R&R Ground truth - let's take a look (FIND AND REPLACE COMMAS FIRST!)
gt_file <- "cranfield_gt_no_commas.csv" ## CAREFUL WITH RAW CSV - IF QUERIES HAVE COMMAS - NEED TO SANITIZE
gt <- read.csv(gt_file, header=FALSE)
head(gt,1)

## Function Declaration
clean_data <- function(data) { 
  ### REMOVE STUFF WE DONT CARE ABOUT (NA's, Blanks, Zeros, Brackets)
  data <- gsub("\\)",",",data)
  data <- gsub("c","",data)
  data <- gsub("\\(","",data)
  data <- gsub("\\n",",",data)
  data <- gsub(" ","",data)  # kill all the numerous "space zero comma" events (leading space essential)
  data <- gsub(",0","",data)  # kill ALL zeros - NEEDS TO BE LEADING COMMA! 
  data <- as.list(strsplit(data, ",")[[1]]) # listify!
  data <- as.numeric(data) # numberfy
  data <- data[!is.na(data)] # reomve NA's - not quite sure how they got back in 
  data <- data[!(data==0)]  # not sure why zeros got back in here (be careful) - but this kills them
  return(data)
} 



####  THe NAME GAME - let's assign better column names
names <- names(gt[2:(dim(gt)[2])])  # Ok - let's pull all the columns after 'query' and sort by ID vs RANK
names  # here's what we start with after import
names <- chartr("V"," ",names) # remove "V" 
names <- as.numeric(names) # Numberfy
## Below - alternate rows are "ID" and "RANK" (even/odd test) - could also do modulus
names <- ifelse(!(names%%2),str_c('ID-',names/2),  str_c("RANK-",(names-1)/2))
names
# OLD - USED FUNCTION: even_test <- function(testing) { return(testing %% 2 == 0)} # quick function to test EVEN (true) or ODD (false)  # OLD: names <- ifelse(even_test(names),str_c('ID-',names/2),  str_c("RANK-",(names-1)/2))

gt <- setNames(gt,c("query",names))  
head(gt,1) # Peek 

# lets substitute the NA with 0 so we can sum
gt[is.na(gt)] <- 0  # kill NA

# Short Test to ensure we can touch data  (Can Remove)
sum(gt$`ID-4`)
mean(gt$`RANK-4`)
plot(gt[2:3],main="GT test plot - first column only")



####### OK - Lets check a few - confirm we have our DATA TABLE ready to work with - 
dim(gt) # Dimensions of our Data Table
height <- dim(gt)[1]
width <- dim(gt)[2]

#################################################
## LATER _ THIS WILL BE A PDF CHUNK

print("SECTION #1 - R&R GROUND TRUTH RANKER - VITAL STATISTICS:")
print(paste("Report Run:        ",Sys.time()))
print(paste("Filename:       ",gt_file))
print(paste("Height: Total Number of rows (queries): ",height))
print(paste("Width: Max Number of columns: ",width))


### SPLIT MAIN TABLE - Create Dedicated ID and JUST rank tables # seq(2, width, by=2 )  # the ID's # seq(3, width+1, by=2 )  # the RANKS's
gt.ids <- gt[,seq(2, width, by=2 ) ]  # the ID's only
gt.ranks <- gt[,seq(3, width+1, by=2 ) ]  # the RANKS's ONLT (0-5)

#### ANALYZE ID's Let's create a histogram in R of how many times ID's are mentioned and First, we need to pull ALL mentions of IDs from all columns
#### PROCESS ALL IDS - ID's Very Bad/messy method for aggregating all ID's so we can make a histogram (fix later)
data <- NULL
width.ids <- dim(gt.ids)[2] # how wide is our ID table?
## RACK THEM AND STACK THEM - this next step takes rectangular data and makes a TALL TOWER (list)
for (i in 1:width.ids ) 
    { data <- paste(data,gt.ids[i],sep="") }
data  # OK - we have ALL the ID's (and lots of zeros!)

data <- clean_data(data)
summary(data)
length(data) # HOW MANY IDs? (some are duplicates for sure!)

roof <- max(data)
roof # what is largest ID ? (top of range - rooftop :)  # small chance we 'lose' any unassigned numbers beyond top listed one - but assuming edge case or non-critical event for most scenarios 

#################################################
print("SECTION #3 - ID TRAITS, PLOTS & STATS:") ## let's generate a frequency table - and include 'dark spots' of no ID"s present (assumes the top ID is the largest in set - so careful)

### PLOT ####
hist(data, 
     main="IBM Watson - Retrieve and Rank:
     Count of # of Times ID is used in Ground Truth", 
     xlab="Query ID #", 
     ylab="Count of Mentions",
     border="red", col="red",
     xlim=c(0,roof),
     las=1, 
     breaks=roof)
### THIS SHOWS US AMPLITUDE (COUNT) OF ID's IN FILE

#### DISTIL down and GROUP
freq_table <- factor(data, levels = c(1:roof))
freq_table <- table(freq_table)
freq_table <- data.frame(freq_table)
freq_table <- freq_table[order(freq_table$Freq, decreasing = TRUE),]
dim(freq_table) # fist column ID, second column tells how many appearances
head(freq_table,15)
 
## DISTIL FURTHER : Haha Here is a Table of Table - Tells us how many times Numbrt of appearances for ID's are shown in Table.   
table(freq_table$Freq)
dim(freq_table)[1] # this tells us how many IDs are in RANGE
superfreak <- (table(freq_table$Freq))# this tells us what DISTRIBUTION is for ID's by number of occurances - pay attention to ZERO
superfreak
sum(superfreak) # should be the same as our "ROOF" - need to represent each BIN - even if zero
# Zeros here are ID's that are NOT MENTIONED - the rest are counts of mentions
roof

### Ratio - how many "ORPHAN IDs" do we have - i.e. how many from 1-ROOF that are not even mentioned?  Poor orphans.
orphan_pct <- round(superfreak[1]/sum(superfreak),3)*100
orphan_pct # if this number is high, that's probably a bad signal for thin GT


### PLOT ####
barplot(table(freq_table$Freq),
        main="IBM Watson - Retrieve and Rank Ground Truth:
        Distribution of ID's by number frequency (count)", 
        sub=paste("ORPHAN RATIO: ",superfreak[1],"/",sum(superfreak)," = ",orphan_pct,"%",sep=""),
        xlab="Number of Mentions in File \n ", 
        ylab="Count of ID's",
        border="black", col="red",
        las=1
        )

### PLOT ####
barplot(freq_table$Freq,
        main="IBM Watson - Retrieve and Rank Ground Truth:
        Distribution of ID's by frequency (area)", 
        sub=paste("ORPHAN RATIO: ",superfreak[1],"/",sum(superfreak)," = ",orphan_pct,"%",sep=""),
        xlab="Number of Mentions in File \n ", 
        ylab="Count of ID's",
        border="red", col="red",
        las=1
)  # this is "Area" type plot


### PLOT ####
plot(freq_table,
     main="IBM Watson - Retrieve and Rank Ground Truth:
     Plot of ID's by frequency - Scatterplot", 
     sub=paste("ORPHAN RATIO: ",superfreak[1],"/",sum(superfreak)," = ",orphan_pct,"%",sep=""),
     xlab="Number of Mentions in File \n ", 
     ylab="Count of ID's"
     ) 

############## OK - now how about the RANKs?
############## OK - now how about the RANKs?
############## OK - now how about the RANKs?



gt.ranks
# Q: how do we differentiate the ID 0's from the no-data 0's?
# A: we count the 1,2,3,4,5 above - from total ID set (1400) and what's left over must be TRUE ZEROS
count <- roof  # e.g. 1400
head(gt.ranks)

data <- NULL
width.ranks <- dim(gt.ranks)[2] # how wide is our RANKS table?
## RACK THEM AND STACK THEM - this next step takes rectangular data and makes a TALL TOWER (list)
for (i in 1:width.ranks ) 
{ data <- paste(data,gt.ranks[i],sep="") }
data  # OK - we have ALL the ID's (and lots of zeros!)
data <- clean_data(data)
summary(data)


### Lets organize rank histogram (N+1 ia a bit of a pain for rownames)
rank_count <- data.frame(0:5,rownames=FALSE)
names(rank_count) <- c("rank","count")

zero_ranks <- roof - sum(table(data)) # if we add up 1,2,3,4,5 and subtract from superset, what's left is zero's (which we blew away above indiscriminantly :)
zero_ranks  ## 1047 (1-4) + 353 (true zeros?) = 1400 true ID's ?  NEED TO VERIFY IN EXCEL
rank_count[0+1,2]=zero_ranks
rank_count
## loopify later
  for (i in 1:5 ) 
    { rank_count[i+1,2] <- table(data)[i] }
rank_count
rank_count[is.na(rank_count)] <- 0  # kill NA and replace with zeros
rank_count

### PLOT ####  (UGLY :-) )
barplot(rank_count$count,
        main="IBM Watson - Retrieve and Rank Ground Truth:
        Distribution of RANKINGS",
        col = rank_count$rank,
        legend.text = rank_count$rank
        )

rank_count

pie(rank_count$count, 
    col = rank_count$rank,
    labels = rank_count$rank
    )


## end of file
