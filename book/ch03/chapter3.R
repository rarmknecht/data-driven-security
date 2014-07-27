# Chapter 3
# Randy Armknecht
# 26 July 2014

# Listing 3-1
# ----------------------------
# set the working directory
location <- "/Github/data-driven-security/"
setwd(paste("~",location,"book/ch03",sep=""))

# make sure that the packages needed are installed
# install if neccesary
pkg <- c("ggplot2","scales","maptools","sp","maps","grid","car")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if(length(new.pkg)) { install.packages(new.pkg)}

# Listing 3-2: 
# ----------------------------
# Pull down the book version of the AlienVault IP reputation
avURL <- "http://datadrivensecurity.info/book/ch03/data/reputation.data"

# setup a relative path for storing the data
avRep <- "data/reputation.data"

# using an if-wrapped test with download.file() vs read.xxx()
# directly avoids having to redownload the file everytime
# we run the script
if (file.access(avRep)) {download.file(avURL,avRep)}

# Listing 3-4
# ---------------------------
# read in the av IP reputation data into a dataframe
av <- read.csv(avRep,sep="#",header=FALSE)

# assign the column names so they are readable
colnames(av) <- c("IP","Reliability","Risk","Type",
                  "Country","Locale","Coords","x")

# get an overview of the dataframe
str(av)

# take a quick look at the first few rows
head(av)

# Listing 3-7
# ----------------------------
# Compute the five number summary of the INT fields in the data frame
summary(av$Reliability)
summary(av$Risk)

# External Source: http://www.slideshare.net/alienvault/building-an-ip-reputation-engine-tracking-the-miscreants

# Listing 3-9
# ----------------------------
table(av$Reliability)
plot(table(av$Reliability)) # Visualize it
table(av$Risk)
plot(table(av$Risk)) # Visualize it

# summary sorts by count as a default
# maxsum sets how many factors to display
summary(av$Type, maxsum=10)
summary(av$Country, maxsum=40)

# Listing 3-11
# Produce bar chart of the top countries
# ------------------------------------------

library(ggplot2)

# get the top 20 countries' names
country.top20 <- names(summary(av$Country))[1:20]

# provide ggplot with a subset of data (top 20)
# map the x value to the sorted count of country
gg <- ggplot(data=subset(av,Country %in% country.top20),
             aes(x=reorder(Country,Country,length)))
# we want a bar chart
gg <- gg + geom_bar(fill="#000099")
# setup decent labels
gg <- gg + labs(title="Country Counts", x="Country", y="Count")
# rotate the chart to improve readability
gg <- gg + coord_flip()
# remove 'chart junk'
gg <- gg + theme(panel.grid=element_blank(),
                 panel.background=element_blank())
# render the chart
print(gg)

# Listing 3-12
# Produce bar chart of the risks
# -------------------------------
gg <- ggplot(data=av, aes(x=Risk))
gg <- gg + geom_bar(fill="#990000")
# force X to scale to be just the limits of the data and discrete vs. continuous
gg <- gg + scale_x_discrete(limits=seq(max(av$Risk)))
gg <- gg + labs(title="Risk Counts",x="Risk Score", y="Count")
gg <- gg + theme(panel.grid=element_blank(),
                 panel.background=element_blank())
print(gg)

# Listing 3-13
# Produce bar chart of the Reliability
# -------------------------------------
gg <- ggplot(data=av, aes(x=Reliability))
gg <- gg + geom_bar(fill="#009900")
# force X to scale to be just the limits of the data and discrete vs. continuous
gg <- gg + scale_x_discrete(limits=seq(max(av$Reliability)))
gg <- gg + labs(title="Reliability Counts",x="Reliability Score", y="Count")
gg <- gg + theme(panel.grid=element_blank(),
                 panel.background=element_blank())
print(gg)

# Listing 3-19
# Contingency Table for Risk & Reliability
# -----------------------------------------
rr.tab <- xtabs(~Risk+Reliability, data=av)
ftable(rr.tab)
# need to use the levelplot function from lattice package
library(lattice)
# cast the table into a dataframe
rr.df = data.frame(table(av$Risk, av$Reliability))
# set the column names since table uses Var1 and Var2
colnames(rr.df) <- c("Risk", "Reliability", "Freq")
levelplot(Freq~Risk*Reliability, data=rr.df, main="Risk ~ Reliability", ylab="Reliability",
          xlab="Risk", shrink=c(0.5,1),col.regions=colorRampPalette(c("#dcefdc","#990000"))(20))

# Listing 3-22
# Generate same plot from random sample
# ---------------------------------------
set.seed(1492)
rel=sample(1:7,260000,replace=T)
rsk=sample(1:10,260000,replace=T)
tmp.df = data.frame(table(factor(rsk),factor(rel)))
colnames(tmp.df) <- c("Risk","Reliability","Freq")
levelplot(Freq~Risk*Reliability, data=tmp.df, main="Risk ~ Reliability", ylab="Reliability",
          xlab="Risk", shrink=c(0.5,1),col.regions=colorRampPalette(c("#dcefdc","#990000"))(20))
