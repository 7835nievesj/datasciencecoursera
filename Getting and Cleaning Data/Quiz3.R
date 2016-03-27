## Code for Coursera class "Getting and Cleaning Data"
## Quiz 3

## Question 1Ω
## Create a logical vector that identifies the households on greater than 10 acres 
## who sold more than $10,000 worth of agriculture products. Assign that logical 
## vector to the variable agricultureLogical. Apply the which() function like this 
## to identify the rows of the data frame where the logical vector is TRUE. 
## which(agricultureLogical) What are the first 3 values that result?

## Load the data from the ACS source into variable ds
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv ",
              destfile = "./ACS.csv", method = "curl", quiet = TRUE)
ds <- read.csv("./ACS.csv")
##
## create agricultureLogical acres > 10 (ACR = 3) and products > $10k (AGS = 6)
agricultureLogical <- (!is.na(ds$ACR)) & (!is.na(ds$AGS)) & (ds$ACR == 3) & (ds$AGS == 6)
##
## Apply the which command as specified
which(agricultureLogical)

## ------------------------------------------------------------------------------
## Question 2
## Using the jpeg package read in the following picture of your instructor into R 
## https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg 
## Use the parameter native=TRUE. What are the 30th and 80th quantiles of the 
## resulting data?
##
## Read the file into pic
library("jpeg")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg", 
              destfile = "./jeffpic.jpg", method = "curl", quiet = TRUE)
pic <- readJPEG("./jeffpic.jpg", native = TRUE)
quantile(pic, probs = c(.30, .80))

## ------------------------------------------------------------------------------
## Question 3
## Load the Gross Domestic Product data for the 190 ranked countries in this data set: 
## https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv 
## Load the educational data from this data set: 
## https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv 
## Match the data based on the country shortcode. How many of the IDs match? 
## Sort the data frame in descending order by GDP rank (so United States is last). 
## What is the 13th country in the resulting data frame?
##
## Load the files:
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv",
              destfile = "./GDPdata.csv", method = "curl")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", 
              destfile = "./EDUdata.csv", method = "curl")
gdp <- read.csv("./GDPdata.csv", skip = 5, header = FALSE)
edu <- read.csv("./EDUdata.csv")
## The GDP file is messy. The second column contains the GDP rankings, and where blank,
## other irrelevant data has been added as observations. Remove all observations that 
## have a non-numeric second variable
gdp$V2 <- as.numeric(as.character(gdp$V2))
gdp <- gdp[!is.na(gdp$V2),]
##
## Merge the datasets based on the first column of GDP ("V1") and the "CountryCode"
## column of edu
mergebycountry <- merge(gdp, edu, by.x = "V1", by.y = "CountryCode", all = FALSE)
nrow(mergebycountry)
##
## Sort based on GDP
mergebycountry <- mergebycountry[order(desc(mergebycountry$V2)),]
## print the 13th country
mergebycountry[[13,"Long.Name"]]
##
## ------------------------------------------------------------------------------
## Question 4
##
## What is the average GDP ranking for the "High income: OECD" and "High income: nonOECD" group?
##
## Use a split/apply/combine approach
countrysplit <- split(mergebycountry$V2, mergebycountry$Income.Group)
sapply(countrysplit, mean)
##
## ------------------------------------------------------------------------------
## Question 5
##
## Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group. 
## How many countries are Lower middle income but among the 38 nations with highest GDP?
##
mergebycountry$qtile <- cut(mergebycountry$V2, breaks = quantile(mergebycountry$V2, 
                                                                 probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), include.lowest = TRUE, labels = 1:5)
table(mergebycountry$qtile, mergebycountry$Income.Group)
