# Chapter 4
# Randy Armknecht
# 27 July 2014

# set the working directory
location <- "/Github/data-driven-security/"
setwd(paste("~",location,"book/ch04",sep=""))

# ensure necessary packages are installed
pkg <- c("bitops","ggplot2","maps","maptools",
         "sp","grid","car","reshape","gridExtra",
         "igraph","plyr","colorspace")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if(length(new.pkg)) {install.packages(new.pkg)}

# Utility Functions / Listing 4-1
# Requires bitops
library(bitops)

# ip2long - convert an ip "xx.xx.xx.xx" to a long int
ip2long <- function(ip) {
  # convert string to character vector
  ips <- unlist(strsplit(ip,'.',fixed=TRUE))
  # setup a function to bitshift and OR the octets
  octet <- function(x,y) { bitOr(bitShiftL(x,8),y) }
  # reduce applies the function cumulatively from left to right
  Reduce(octet, as.integer(ips))
}

# long2ip - convert a long int to an ip address
long2ip <- function(longip) {
  # setup reverse bit manipulation
  octet <- function(nbits) { bitAnd(bitShiftR(longip, nbits),0xFF) }
  # Map applies a function to each element of the argument
  # paste converts arguments to character and concatenates them
  paste(Map(octet,c(24,16,8,0)), sep="", collapse=".")
}

# testing the ip2long & long2ip functions
long2ip(ip2long("10.10.57.14"))

# Additional Utility Function / Listing 4-2
ip.is.in.cidr <- function(ip,cidr) {
  long.ip <- ip2long(ip)
  cidr.parts <- unlist(strsplit(cidr,"/"))
  cidr.range <- ip2long(cidr.parts[1])
  cidr.mask <- bitShiftL(bitFlip(0),(32-as.integer(cidr.parts[2])))
  return(bitAnd(long.ip, cidr.mask) == bitAnd(cidr.range,cidr.mask))
}

# testing the CIDR function
ip.is.in.cidr("10.0.1.15","10.0.1.3/24")
ip.is.in.cidr("10.0.1.15","10.0.2.0/24")

# Listing 4-3 / Prepare AlienVault GeoIP Data for mapping
# --------------------------------------------------------

# read in the alienvault data
avRep <- "data/reputation.data"
av.df <- read.csv(avRep, sep="#", header=FALSE)
colnames(av.df) <- c("IP","Reliability","Risk","Type",
                     "Country", "Locale", "Coords", "x")

# create a vector of lat/long data by splitting on ","
av.coords.vec <- unlist(strsplit(as.character(av.df$Coords),","))
# convert the vector into a 2-column matrix
av.coords.mat <- matrix(av.coords.vec, ncol=2, byrow=TRUE)
# project into a data frame
av.coords.df <- as.data.frame(av.coords.mat)
# name the columns
colnames(av.coords.df) <- c("lat","long")
# convert the characters to numerica values
av.coords.df$long <- as.double(as.character(av.coords.df$long))
av.coords.df$lat <- as.double(as.character(av.coords.df$lat))

# Listing 4-4 / Time to Map the Data!
# -------------------------------------

library(ggplot2)
library(maps)
library(RColorBrewer)
library(scales)

# extract a color palette from the RColorBrewer pkg
set2 <- brewer.pal(8,"Set2")

# extract the polygon info for the world map, minus Antarctica
world <- map_data("world")
world <- subset(world, region != "Antarctica")

# plot the map with the points marking lat/long of the geocoded entries
# see chapter 5 for more mapping details
gg <- ggplot()
gg <- gg + geom_polygon(data=world, aes(long, lat, group=group), fill="white")
gg <- gg + geom_point(data=av.coords.df, aes(x=long, y=lat), 
                      color=set2[2], size=1, alpha=0.1)
gg <- gg + labs(x="", y="")
gg <- gg + theme(panel.background=element_rect(fill=alpha(set2[3],0.2),
                                               colour="white"))
gg

# Listing 4-5 / Incorporate IANA Data
# -------------------------------------

# read in the iana data
iana.data <- "data/ipv4-address-space.csv"
iana <- read.csv(iana.data)

# clean up the iana prefix
iana$Prefix <- sub("^(00|0)","",iana$Prefix, perl=TRUE)
iana$Prefix <- sub("/8$","",iana$Prefix, perl=TRUE)

# define vectorized function to strip 'n' characters from a string
rstrip <- function(x,n) {
  substr(x, 1, nchar(x)-n)
}

# extract just the prefix from the Alien Vault listing
library(stringr)
av.IP.prefix <- rstrip(str_extract(as.character(av.df$IP),
                                   "^([0-9]+)\\."),1)

# there are faster ways than "sapply()" but the point is to 
# understand the general "apply" pattern as we will use it quite a bit
av.df$Designation <- sapply(av.IP.prefix, function(ip) {
  iana[iana$Prefix == ip, ]$Designation
})

# Listing 4-6 / Merge IANA Designations w/ AV
# ----------------------------------------------------

# create a new data frame from the iana designation factors
iana.df <- data.frame(table(factor(iana$Designation)))
colnames(iana.df) <- c("Registry","IANA.Block.Count")

# make a data frame of the counts of the av iana designation factors
tmp.df <- data.frame(table(factor(av.df$Designation)))
colnames(tmp.df) <- c("Registry","AlienVault.IANA.Count")

# merge (join) the data frames on the reg column
combined.df <- merge(iana.df, tmp.df)
print(combined.df[with(combined.df, order(-IANA.Block.Count)),],
      row.names=FALSE)

# Listing 4-7 / Plot the table of AlienVault vs. IANA
# -----------------------------------------------------

library(reshape)
library(grid)
library(gridExtra)

# normalize the IANA and AV values to % so bar chart scales
# match and make it easier to compare
combined.df$IANA.pct <- 100 * (combined.df$IANA.Block.Count / 
                                 sum(combined.df$IANA.Block.Count))
combined.df$AV.pct <- 100 * (combined.df$AlienVault.IANA.Count / 
                               sum(combined.df$AlienVault.IANA.Count))

combined.df$IANA.vs.AV.pct <- combined.df$IANA.pct - combined.df$AV.pct

melted.df <- melt(combined.df)
# plot the new melted data frame values
gg1 <- ggplot(data=melted.df[melted.df$variable=="IANA.pct",],
              aes(x=reorder(Registry, -value), y=value))
gg1 <- gg1 + ylim(0,40)
gg1 <- gg1 + geom_bar(stat="identity", fill=set2[3])
gg1 <- gg1 + labs(x="Registry", y="%", title="IANA %")
gg1 <- gg1 + coord_flip()
gg1 <- gg1 + theme(axis.text.x = element_text(angle=90, hjust=1),
                   panel.background=element_blank(),
                   legend.position="none")

gg2 <- ggplot(data=melted.df[melted.df$variable=="AV.pct",],
              aes(x=reorder(Registry, -value), y=value))
gg2 <- gg2 + ylim(0,40)
gg2 <- gg2 + geom_bar(stat="identity", fill=set2[3])
gg2 <- gg2 + labs(x="Registry", y="%", title="AlienVault %")
gg2 <- gg2 + coord_flip()
gg2 <- gg2 + theme(axis.text.x = element_text(angle=90, hjust=1),
                   panel.background=element_blank(),
                   legend.position="none")

# use grid to precisely arrange multiple ggplot objects
grid.arrange(gg1,gg2,ncol=1,nrow=2)

# Listing 4-8 / Comparison based on Block Size
# ----------------------------------------------

gg3 <- ggplot(data=combined.df,
              aes(x=reorder(Registry, -IANA.Block.Count), y=AV.pct))
gg3 <- gg3 + geom_bar(stat="identity", fill=set2[4])
gg3 <- gg3 + labs(x="Registry", y="Count", 
                  title="AV/IANA by IANA (low-to-high)")
gg3 <- gg3 + coord_flip()
gg3 <- gg3 + theme(axis.text.x = element_text(angle=90, hjust=1),
                   panel.background=element_blank(),
                   legend.position="none")
gg3

##
##  Recommended Reading:
##  http://statisticssolutions.com/academic-solutions/resources
##        /directory-of-statistical-analyses
##        /correlation-pearson-kendall-spearman/
##

# Listing 4-9 / Plot Block Count vs. AV Count
# --------------------------------------------

gg <- ggplot(data=combined.df)
gg <- gg + geom_point(aes(x=IANA.Block.Count,
                          y=AlienVault.IANA.Count),
                      color=set2[1], size=4)
gg <- gg + labs(x="IANA Block Count", y="Alien Vault Count",
                title="IANA ~ Alien Vault")
gg <- gg + theme(axis.text.x = element_text(angle=90,hjust=1),
                 panel.background = element_blank(),
                 legend.position = "none")
gg

# check the correlation
cor(combined.df$IANA.Block.Count,
    combined.df$AlienVault.IANA.Count, method="spearman")

# Listing 4-11 / Prepare for Zeus Analysis
# -----------------------------------------

zeusURL <- "https://zeustracker.abuse.ch/blocklist.php?download=ipblocklist"
zeusData <- "data/zeus.csv"
if(file.access(zeusData)) {download.file(zeusURL,zeusData)}
zeus <- read.table(zeusData,skip=5,header=FALSE,col.names=c("IP"))

# HELPER FUNCTION MENTIONED IN THE BOOK
# BUT NOT IN THE PRINTED LISTINGS

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# HELPER FUNCTION MENTIONED IN THE BOOK
# BUT NOT IN THE PRINTED LISTINGS

BulkOrigin <- function(ip.list,host="v4.whois.cymru.com",port=43) {
  
  # setup query
  cmd <- "begin\nverbose\n" 
  ips <- paste(unlist(ip.list), collapse="\n")
  cmd <- sprintf("%s%s\nend\n",cmd,ips)
  
  # setup connection and post query 
  con <- socketConnection(host=host,port=port,blocking=TRUE,open="r+")  
  cat(cmd,file=con)
  response <- readLines(con)
  close(con)
  
  # trim header, split fields and convert results
  response <- response[2:length(response)]
  response <- laply(response,.fun=function(n) {
    sapply(strsplit(n,"|",fixed=TRUE),trim)
  })
  response <- adply(response,c(1))
  response <- subset(response, select = -c(X1) )
  names(response) = c("AS","IP","BGP.Prefix","CC",
                      "Registry","Allocated","AS.Name")
  
  return(response)
  
}

# HELPER FUNCTION MENTIONED IN THE BOOK
# BUT NOT IN THE PRINTED LISTINGS

BulkPeer <- function(ip.list,host="v4-peer.whois.cymru.com",port=43) {
  
  # setup query
  cmd <- "begin\nverbose\n" 
  ips <- paste(unlist(ip.list), collapse="\n")
  cmd <- sprintf("%s%s\nend\n",cmd,ips)
  
  # setup connection and post query
  con <- socketConnection(host=host,port=port,blocking=TRUE,open="r+")  
  cat(cmd,file=con)
  response <- readLines(con)
  close(con)
  
  # trim header, split fields and convert results
  response <- response[2:length(response)]
  response <- laply(response,function(n) {
    sapply(strsplit(n,"|",fixed=TRUE),trim)
  })  
  response <- adply(response,c(1))
  response <- subset(response, select = -c(X1) )
  names(response) <- c("Peer.AS","IP","BGP.Prefix","CC",
                       "Registry","Allocated","Peer.AS.Name")
  return(response)
  
}

# HELPER FUNCTION MENTIONED IN THE BOOK
# BUT NOT IN THE PRINTED LISTINGS

BulkOriginASN <- function(asn.list,host="v4.whois.cymru.com",port=43) {
  
  # setup query
  cmd <- "begin\nverbose\n" 
  ips <- paste(unlist(asn.list), collapse="\n")
  cmd <- sprintf("%s%s\nend\n",cmd,ips)
  
  # setup connection and post query
  con <- socketConnection(host=host,port=port,blocking=TRUE,open="r+")  
  cat(cmd,file=con)
  response <- readLines(con)
  close(con)
  
  # trim header, split fields and convert results
  
  response <- response[2:length(response)]
  response <- laply(response,.fun=function(n) {
    sapply(strsplit(n,"|",fixed=TRUE),trim)
  })
  
  response <- adply(response,c(1))
  response <- subset(response, select = -c(X1) )
  names(response) <- c("AS","CC","Registry","Allocated","AS.Name")
  
  return(response)
  
}

# Listing 4-12 / Zeus Graph Structure by Country
# ----------------------------------------------)----

library(igraph)
library(plyr)
library(colorspace)

ips <- as.character(zeus$IP)

# get bgp origin data & peer data
origin <- BulkOrigin(ips)
# start graphing
g <- graph.empty()
# IP endpoints are red
g <- g + vertices(ips, size=4, color=set2[4], group=1)
# Make BGP vertices
g <- g + vertices(origin$CC, size=4, color=set2[2], group=2)
# for each IP, get the origin AS CC and return as a pair to 
# create the IP -> CC edge list
ip.cc.edges <- lapply(ips, function(x){
  #iCC <- origin[origin$IP==x]$CC   # this is what the book had
  iCC <- subset(origin,IP==x)$CC    # I changed it to this
  lapply(iCC, function(y){c(x,y)})
})

g <- g + edges(unlist(ip.cc.edges))
# simplify graph by combining common edges
g <- g + simplify(g, edge.attr.comb=list(weight="sum"))
# remove "one off ASN's", delete vertex with degree of 0
g <- delete.vertices(g, which(degree(g) < 1))
E(g)$arrow.size <- 0
V(g)[grep("\\.", V(g)$name)]$name <- ""

# Listing 4-13 / Graph the generated graph structure
# The greater the itterartions, the cleaner the graph will look
# --------------------------------------------------------------
L <- layout.fruchterman.reingold(g,niter=100000, area=30*vcount(g)^2)
par(bg='white', mfrow=c(1,1))
plot(g, margin=0, layout=L, vertex.label.dist=0.5,
     vertex.label.cex=0.75,
     vertex.label.color='black',
     vertex.label.family='sans',
     vertex.label.font=2,
     main="Zeus botnet nodes clustered by country")

# Listing 4-14 / Translation table for country codes
# ----------------------------------------------------
zeus.cc <- grep("[A-Z]",V(g)$name, value=TRUE)
zeus.cc <- zeus.cc[order(zeus.cc)]
# read in the country code data
cc.df <- read.csv("data/countrycode_data.csv")
# display cc & name for just our data set
print(head(cc.df[cc.df$iso2c %in% zeus.cc, c(7,1)], n=10), row.names=FALSE)

# Listing 4-16 / Firewall Data + Alienvault
# --------------------------------------------------
avRep <- "data/reputation.data"
av.df <- read.csv(avRep,sep="#",header=FALSE)
colnames(av.df) <- c("IP","Reliability","Risk","Type","Country","Locale","Coords","x")

# read in firewall log ips
dest.ips <- read.csv("data/dest.ips", col.names=c("IP"))

# table of reliability of seen ips
table(av.df[av.df$IP %in% dest.ips$IP, ]$Reliability)

# extract only bad IPs (reliability > 6)
ips <- as.character(av.df[(av.df$IP %in% dest.ips$IP) &
                            (av.df$Reliability > 6), ]$IP)

graph.cc <- function(ips, alien.vault.df, show.plot=TRUE) {
  
  options("warn" = -1)
  
  # Lookup ASN info for the incoming IP list which will
  # have country of origin info that's fairly accurate
  origin <- BulkOrigin(ips)
  
  # filter out IP and Type from the alienvault DB only for our ip list
  ips.types <- alien.vault.df[alien.vault.df$IP %in% ips,c(1,2,4)]
  
  # get a tabular summary of the types and counts
  ftab <- table(factor(ips.types[ips.types$IP %in% ips,]$Type))
  
  # build a color table from the tabular summary
  # myColors <- rainbow_hcl(length(names(ftab)),c=60,l=70,start=20)
  myColors <- set2
  col.df <- data.frame(Type=names(ftab),Color=myColors)
  
  # begin graph creation
  g <- graph.empty()
  
  # add our ip list as the starting vertices
  g <- g + vertices(ips,size=3,group=1)
  
  # i don't the df is necessary anymore...will test later
  ips.df <- data.frame(ips)
  colnames(ips.df) <- c("IP")
  
  # get the current list of vertex names...i think i can remove this too
  v.names <- V(g)$name
  
  # assign colors to the vertices based on the type
  V(g)$color <- as.character(col.df[col.df$Type %in% ips.types[ips.types$IP %in% v.names,]$Type,]$Color)
  
  # add country vertices
  g <- g + vertices(
    unique(origin$CC),
    size=3,color="black",group=2)
  
  # build country->ip edges
  ip.cc.edges <- lapply(ips,function(x) {
    iCC <- origin[origin$IP==x,]$CC
    lapply(iCC,function(y){
      c(x,y)
    })
  })
  
  # add edges
  g <- g + edges(unlist(ip.cc.edges))
  
  # simplify (though it's almost not necessary given the low
  # complexity of the graph)
  g <- simplify(g, edge.attr.comb=list(weight="sum"))
  
  # remove lone wolf vertices
  g <- delete.vertices(g, which(degree(g) < 1))
  
  # arrows: ugh
  E(g)$arrow.size <- 0
  
  # we only want to see country labels, not IPs
  V(g)[grep("[0-9]",V(g)$name)]$name <- ""
  
  # 10000 makes pretty graphs and takes a pretty long time
  L <- layout.fruchterman.reingold(g, niter=10000, area=30*vcount(g)^2)
  
  # for when I add community options
  c <- walktrap.community(g, steps=10)
  v <- evcent(g)$vector
  
  if (show.plot) {
    def.par <- par(no.readonly = TRUE)
    par(bg = 'white')
    layout(matrix(c(1,2,3,1), 1, 2, byrow = TRUE), widths=c(5,1))
    plot(g,margin=0,layout=L,vertex.label.dist=0.6, 
         vertex.label.cex=0.75, 
         vertex.label.color="black",
         vertex.label.family="sans", 
         vertex.label.font=2)
    par(mar=c(5,0,2,2))
    barplot(ftab,horiz=TRUE,las=1,cex.names=0.75,cex.axis=0.75,
            col=as.character(col.df[col.df$Type %in% unlist(labels(ftab)),]$Color))
    par(def.par)
  }
  
  options("warn" = 0)
  
  return(g)
}

# ADDITIONAL HELPER FUNCTION MENTIONED IN THE BOOK BUT NOT PRINTED

graph.asn <- function(ips,alien.vault.df,add.peers=FALSE,show.plot=TRUE,show.labels=FALSE) {
  
  options("warn" = -1)
  
  # Lookup ASN info for the incoming IP list
  origin <- BulkOrigin(ips)
  
  if (add.peers) { # and peers if specified
    peers <- BulkPeer(ips)
  }
  
  # filter out IP and Type from the alienvault DB only for our ip list
  ips.types <- alien.vault.df[alien.vault.df$IP %in% ips,c(1,2,4)]
  
  # get a tabular summary of the types and counts
  ftab <- table(factor(ips.types[ips.types$IP %in% ips,]$Type))
  
  # build a color table from the tabular summary
  #myColors <- rainbow_hcl(length(names(ftab)),c=60,l=70,start=20)
  myColors <- set2
  col.df <- data.frame(Type=names(ftab),Color=myColors)
  
  # begin graph creation
  g <- graph.empty()
  
  # add our ip list as the starting vertices
  g <- g + vertices(ips,size=3,group=1)
  
  # i don't think the df is necessary anymore...will test later
  ips.df <- data.frame(ips)
  colnames(ips.df) <- c("IP")
  
  # get the current list of vertex names...i think i can remove this too
  v.names <- V(g)$name
  
  # assign colors to the vertices based on the type
  V(g)$color <- as.character(col.df[col.df$Type %in% ips.types[ips.types$IP %in% v.names,]$Type,]$Color)
  
  # add BGP->IP vertices and - if requested - add peer ASN vertices
  if (add.peers) { 
    g <- g + vertices(
      unique(c(peers$Peer.AS, origin$AS)),
      size=4,color="black",group=2)
  } else {
    g <- g + vertices(
      unique(origin$AS),
      size=4,color="black", group=2)
  }
  
  # Make IP/BGP edges
  ip.edges <- lapply(ips,function(x) {
    iAS <- origin[origin$IP==x,]$AS
    lapply(iAS,function(y){
      c(x,y)
    })
  })
  
  if (add.peers) { # same for peers if specified
    bgp.edges <- lapply(
      grep("NA",unique(origin$BGP.Prefix),value=TRUE,invert=TRUE),
      function(x) {
        startAS <- unique(origin[origin$BGP.Prefix==x,]$AS)
        lapply(startAS,function(z) {
          pAS <- peers[peers$BGP.Prefix==x,]$Peer.AS
          lapply(pAS,function(y) {
            c(z,y)
          })
        })
      })
  }
  
  # build ASN->IP edges
  g <- g + edges(unlist(ip.edges))
  
  if (add.peers) { # same for peers if specified
    g <- g + edges(unlist(bgp.edges))
  }
  
  # simplify the structure (prbly needed since it's already
  # well organized w/o dupes
  g <- simplify(g, edge.attr.comb=list(weight="sum"))
  
  # delete any standalone vertices (lone wolf ASNs)
  g <- delete.vertices(g, which(degree(g) < 1))
  
  # arrows: ugh
  E(g)$arrow.size <- 0
  
  # if we do show labels, we only want to see the ASNs
  V(g)[grep("\\.",V(g)$name)]$name <- ""
  
  # 10000 makes it pretty...and pretty slow
  L <- layout.fruchterman.reingold(g, niter=10000, 
                                   area=30*vcount(g)^2)
  
  # shld make an options parameter and if-block this
  c <- walktrap.community(g, steps=10)
  v <- evcent(g)$vector
  
  if (show.plot) {
    def.par <- par(no.readonly = TRUE)
    par(bg = 'white')
    layout(matrix(c(1,2,3,1), 1, 2, byrow = TRUE),
           widths=c(5,1))
    if (show.labels) {
      plot(g,
           margin=0,
           layout=L,
           vertex.label.dist=0.6, 
           vertex.label.cex=0.75, 
           vertex.label.color="black",
           vertex.label.family="sans", 
           vertex.label.font=2)
    } else {
      plot(g,margin=0,vertex.label=NA,layout=L)      
    }
    par(mar=c(5,0,2,2))
    barplot(ftab,horiz=TRUE,las=1,cex.names=0.75,cex.axis=0.75,
            col=as.character(
              col.df[col.df$Type %in% unlist(labels(ftab)),]$Color))
    par(def.par)
  }
  
  options("warn" = 0)
  return(g)
}


g.cc <- graph.cc(ips,av.df)

