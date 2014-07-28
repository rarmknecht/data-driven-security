# Chapter 4
# Randy Armknecht
# 27 July 2014

# set the working directory
location <- "/Github/data-driven-security/"
setwd(paste("~",location,"book/ch04",sep=""))

# ensure necessary packages are installed
pkg <- c("bitops","ggplot2","maps","maptools",
         "sp","grid","car","reshape","gridExtra")
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
