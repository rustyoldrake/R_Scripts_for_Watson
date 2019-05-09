#  Edge_Simulate_Visualize
#  A simple program to generate 50-1000 Edge Nodes, so we can  
#  (a) Create some 'medium' simulation data (b) play with Visualization Methods (c) Start to get a feel for policy Driven State


library(ggplot2) # for the viz
library(wesanderson)  # for GGPLOT colors  # # Install # install.packages("wesanderson") # for GGPLOT colors
library(treemapify) # for tree map example way down below

library(kohonen) # old but good. Self organizing maps
library(SOMbrero)  # first time I've tried this package - looks pretty cool



## 1 START WITH MATRIX BONES - node_count number of rows
node_count <- 1001# later, this will be a user input , for "size of test set to gen"

nodes_df <- as.data.frame(matrix(1:node_count, nrow = node_count, ncol = 13))# let's get started
colnames(nodes_df) <- c("index","type","name","size","servers","cpus","cores","range_x","range_y","magnitude","angle","capacity_utilization","policy_noncompliant") # name columns

head(nodes_df,10) # what do we have?  show first 10 rows 

## 2 LET'S GROUP the set into N Groups (1=SMALL (e.g. Raspi); 2=Medium (e.g. Edge Gateway), (3) = Large Edge Device (e.g. ICP on Prem))
# See: https://wiki.akraino.org/pages/viewpage.action?pageId=1147248 for model outlining Cruiser, Rover, Tricycle concept
# CAVEAT - THIS iS FOR DISCUSSION ONLY - handle with care!
node_type_lookup <- data.frame("type" = 1:5, 
                               "name" = c("rover","satellite", "unicycle", "tricycle", "cruiser"), 
                               "size" = c(1,5,50,100,1000), 
                               "servers" = c(0,2,42,126,252), ## Approx. SME needs to REVIEW THIS (!)
                               "cpus" = c(1,2,200,800,1600), ## Approx. SME needs to REVIEW THIS (!)
                               "cores" = c(0,16,1600,6400,10000), ## Approx. SME needs to REVIEW THIS (!)
                               "pct_allocation" = c(0.944,0.04,0.01,0.005,0.001), # later; make more configurable
                               "count" = c(0,0,0,0,0) # will calculate this later
                               )
# " Can we relate to processors and architecture? Like rover = Armv6, Armv7hf, Arm Cortex 4, up to four cores. Next class up = Atom, next class = Xeon-class x86_64, and so on?"

# check our Allocation % add up to 100%
sum(node_type_lookup$pct_allocation)
if (sum(node_type_lookup$pct_allocation)==1){print("pct_allocation = OK - sums to 1 ")} else {print("pct_allocation Check = BAD - node sums dont line up")}


# calculate number of whole number nodes in each TYPE category given user input
node_type_lookup$count <- round(node_type_lookup$pct_allocation * node_count)
node_type_lookup
sum(node_type_lookup$count)
node_count
node_count == sum(node_type_lookup$count) # True if we've done work well here (and rounded well)

# different ways to count types in table (5 here)
dim(node_type_lookup)[1] # how many types /rows
nrow(node_type_lookup) # also 5


## BIG MOMENT OF TYPE ASSIGMENT  - DID this with loop in case you have lots of types (you can hand code this sectio nif hless than 10)
index_prior = 1
index_new = 1
for (i in 1:nrow(node_type_lookup)){
  print("---")
  print(i)
  index_new <- (node_type_lookup$count[i]+index_prior) # add our new range to old range
  
  ## DO THE ALLOCATION - BIG MOMENT
  print(index_prior)
  print(index_new-1)
  nodes_df$type[index_prior:(index_new-1)] <- node_type_lookup$type[i] # SETS THE TYPE 
  nodes_df$name[index_prior:(index_new-1)] <- paste(node_type_lookup$name[i]) # SETS THE NAME  

  nodes_df$size[index_prior:(index_new-1)] <- paste(node_type_lookup$size[i]) # SETS THE SIZE  
  nodes_df$servers[index_prior:(index_new-1)] <- paste(node_type_lookup$servers[i]) # SETS THE SERVERS  
  nodes_df$cpus[index_prior:(index_new-1)] <- paste(node_type_lookup$cpus[i]) # SETS THE CPU (LATER MAY BE VARIETY HERE)
  nodes_df$cores[index_prior:(index_new-1)] <- paste(node_type_lookup$cores[i]) # SETS THE CORES (LATER MAY BE VARIETY HERE)
  
  ## Set up for next time
  index_prior <- index_new # set up for next loop
}

## CHECK
node_table <- table(nodes_df$type) # check this worked like you expect
node_table
sum(table(nodes_df$type))
sum(table(nodes_df$type)) == node_count # hopefully true - if you did this right
table(nodes_df$name) # check this worked like you expect
plot(sort(table(nodes_df$name),decreasing=TRUE))

## PRIME Number fucntion - purely to allocate "Red" state (Non Compiance)
is.prime <- function(num) {
  if (num == 2) {TRUE} else if (any(num %% 2:(num-1) == 0)) {FALSE} 
  else {TRUE}}
## test - is.prime(8)


###



## 4 LOCATION - SEMI RANDOM - ZONING FOR VIZ CORE = BIG, Cruiser - EDGES = Rovers
head(nodes_df,20) # check first 20
tail(nodes_df,20) # check last 20

## let's get stared with some plus and minus coordinate data (scale by type later)
nodes_df$range_x <- sample(-100:100,node_count,replace=TRUE)
nodes_df$range_y <- sample(-100:100,node_count,replace=TRUE)

head(nodes_df,20) # check first 20
tail(nodes_df,20) # check last 20


#nodes_df$range_x <- nodes_df$range_x*1000/nodes_df$size

# SCALE RANGE TO EDGE - there's a more elegant way to do this in R - so improve later ()
# for (i in 1:node_count)
#   {
#   nodes_df$range_x[i] <- nodes_df$range_x[i]*100/as.numeric(nodes_df$size[i])
#   nodes_df$range_y[i] <- nodes_df$range_y[i]*100/as.numeric(nodes_df$size[i])
# } # this centralizes the big stuff




# rnorm(1, mean=10, sd=1)/10    # provide our own mean and standard deviation

# ANGLES For Polar coordinates  - not sure we need this (may remove later if not used)
nodes_df$angle <- sample(0:359,node_count,replace=TRUE)

nodes_df$capacity_utilization <- sample(0:100,node_count,replace=TRUE)

# Magnutude seeds for polar coords (like the length of the Minutes hand in clock)
nodes_df$magnitude <- sample(50:100,node_count,replace=TRUE)


# TRY ANOTHER METHOD FOR DONUT POINT CLOUD GENERATE
for (i in 1:node_count)
{
  nodes_df$magnitude[i] <- nodes_df$magnitude[i]/as.numeric(nodes_df$type[i]) # divide them down by 'bands'
  nodes_df$range_x[i] <- round(nodes_df$magnitude[i] * sin(pi*nodes_df$angle[i]/180)/as.numeric(nodes_df$type[i]))
  nodes_df$range_y[i] <- round(nodes_df$magnitude[i] * cos(pi*nodes_df$angle[i]/180)/as.numeric(nodes_df$type[i]))

  #for some color - let's tag all PRIME Number magnitudes as non-compliant (Paint Red)
  nodes_df$policy_noncompliant[i] <- is.prime(nodes_df$index[i])
} # 



## PLOT   - Light BLUES are problems (works)
ggplot(nodes_df, aes(x=range_x, y=range_y, size=type, color=policy_noncompliant)) + geom_point()


# Shades of Life Aquatic (works but hard to read)
pal <- wes_palette("Zissou1", 100, type = "continuous")
# Add some color 
ggplot(nodes_df, aes(x = range_x, y = range_y , size=type, fill = type)) +
  geom_tile() + 
  scale_fill_gradientn(colours = pal) + 
  coord_equal() 

# Add some color 
ggplot(nodes_df, aes(x = range_x, y = range_y , size=type, fill = policy_noncompliant)) +
  geom_tile() + 
  scale_fill_gradientn(colours = pal) + 
  coord_equal() 


# works (by size)
ggplot2::ggplot(nodes_df, ggplot2::aes(area = type, fill = size)) +
  geom_treemap() 

# works (by policy_noncompliant)
sp <- ggplot2::ggplot(nodes_df, 
                ggplot2::aes(area = type, 
                fill = policy_noncompliant))+
                geom_treemap() 
sp


# works (by CPU)
ggplot2::ggplot(nodes_df, ggplot2::aes(area = type, size = cpus, fill = cpus)) +
  geom_treemap() 

# works - by LFEdge/Akraino Label 
ggplot2::ggplot(nodes_df, ggplot2::aes(area = type, size = type, fill = name )) +
  geom_treemap() 


# works - by LFEdge/Akraino Label  (sort of works - but labels EVERY box, so zone lable better if we can figure out)
# ggplot2::ggplot(nodes_df, ggplot2::aes(area = type, size = type, fill = name, label = name)) +
#   geom_treemap() + 
#   geom_treemap_text()

 
#########  KOHONEN 
namelist <- data.frame(names(nodes_df))
namelist  # in case you want to see which numbers map (for selection) 
### OK - for now, use this line below to CHERRY PICK your TRAITS/VALUES for explirng 
data <- nodes_df[c(2,4,5,6,7,12,13)] # the countable and sortable stuff 
names(data) # OK - here is what we selected (from the 52 traits/values)

head(data)
data <- data.matrix(data)

## what do we have?
head(data)
data.sc <- scale(data)
head(data.sc)

set.seed(7)
data.som <- som(data.sc, grid = somgrid(10, 8, "hexagonal"))

# add rows / node count to TITLE
title_cluster = paste("\n System Summary \n", nrow(data), "ICP/Edge Nodes Clustered")
title_count = paste("\n System Summary \n", nrow(data), "ICP/Edge Nodes COUNT")

## Fancy Colors
coolBlueHotRed <- function(n, alpha = 1) { rainbow(n, end=4/6, alpha=alpha)[n:1] } ## palette for nicer color scheme 

## Let's Plot
plot(data.som, main = title_cluster, palette.name =  coolBlueHotRed)
plot(data.som, main = title_count, type = "counts", palette.name = coolBlueHotRed, heatkey = TRUE)
plot(data.som, type = "mapping", pchs = 20, main = "Mapping Type SOM")


# Good documentation here https://clarkdatalabs.github.io/soms/SOM_NBA 


## GOOD TO HERE
sommap <- som(scale(data), grid = somgrid(6, 4, "hexagonal"))
plot(data.som, type = "mapping", property = getCodes(sommap, 1)[,1],
     main = colnames(getCodes(sommap, 1))[1])

plot(kohmap, type="codes", main = c("Codes X", "Codes Y"))
add.cluster.boundaries(sommap, som.hc)


##
# temp <- as.factor(nodes_df$name) # where vintages used to go
# kohmap <- xyf(scale(data), temp,
#               grid = somgrid(10, 10, "hexagonal"), rlen=100)
# plot(kohmap, type="changes")
# counts <- plot(kohmap, type="counts", shape = "straight")
# add.cluster.boundaries(sommap, som.hc)

## show both sets of codebook vectors in the map
#par(mfrow = c(1,2))


## add background colors to units according to their predicted class labels
xyfpredictions <- classmat2classvec(getCodes(kohmap, 2))
bgcols <- c("gray", "pink", "lightgreen")
plot(kohmap, type="mapping", 
     col = as.integer(temp),
     pchs = as.integer(temp), 
     bgcol = bgcols[as.integer(xyfpredictions)],
     main = "another mapping plot", shape = "straight", border = NA)

 
bgcols <- c("gray", "pink", "lightgreen")
plot(data.som, type="mapping", 
     col = as.integer(temp),
     pchs = as.integer(temp), 
     bgcol = bgcols[as.integer(xyfpredictions)],
     main = "another mapping plot", shape = "straight", border = NA)

##
name_lfedge <- as.factor(nodes_df$name) # where vintages used to go
plot(data.som, 
     type = "mapping", 
     pchs = as.integer(name_lfedge), 
     col = as.integer(name_lfedge),
     main = "Mapping Type SOM",
     bgcols <- c("gray", "pink", "lightgreen"), 
     palette.name = coolBlueHotRed, 
     heatkey = TRUE)


## FRESH TRY
data.som <- som(data.sc, grid = somgrid(10, 8, "hexagonal"))
plot(data.som, 
     main = title_cluster, 
     # type = "mapping", 
     pchs = as.integer(name_lfedge), 
     col = as.integer(name_lfedge),
     palette.name = coolBlueHotRed, 
     heatkey = TRUE)

