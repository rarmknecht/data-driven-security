# Chapter 4
# Randy Armknecht
# 27 July 2014

# set the working directory
location <- "/Github/data-driven-security/"
setwd(paste("~",location,"book/ch04",sep=""))

# ensure necessary packages are installed
pkg <- c("bitops","ggplot2","maps","maptools",
         "sp","grid","car")
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
