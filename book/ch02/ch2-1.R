# Listing 2-1
# Page 33
# R Data Frame Example

# create a new data frame of hosts & high vuln counts
assets.df <- data.frame(
  name=c("danube","gander","ganges","mekong","orinoco"),
  os=c("W2K8","RHEL5","W2K8","RHEL5","RHEL5"),
  highvulns=c(1,0,2,0,0))

# take a look at the data frame structure & contents
str(assets.df)
head(assets.df)

# show a slice of just the OS
# by defaulr R creates "factors" for categorical data so
# we user as.character() to expand the factors out
head(assets.df$os)

# add a new column
assets.df$ip <- c("192.168.1.5","10.2.7.5","192.168.1.7","10.2.7.6","10.2.7.7")

# extract only nodes with more than one high vulnerability
head(assets.df[assets.df$highvulns > 1,])

# create a zones column based on IP prefix
assets.df$zones <-
  ifelse(grepl("^192",assets.df$ip),"Zone1","Zone2")

# take a final look at the dataframe
head(assets.df)
