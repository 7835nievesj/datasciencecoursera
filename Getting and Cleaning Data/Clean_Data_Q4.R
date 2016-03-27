# Question 1

## Download data and load to R. 
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url, "Idaho_housing.csv", method = "curl")
Idaho_housing.csv <- read.csv("Idaho_housing.csv", header = TRUE)

## Apply strsplit() to split all the names of the data frame on the 
## characters "wgtp".
wgtp.split <- strsplit(names(Idaho_housing.csv), "wgtp")
wgtp.split[123]

# Question 2 

## Load the Gross Domestic Product
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(url2, "GDP.csv", method = "curl")
GDP.csv <- read.csv( "GDP.csv", header = FALSE, skip = 5, nrows = 190)
attach(GDP.csv)
## Remove comas from GDP numbers and average them. 
mean(as.numeric(gsub(pattern = ",", replacement = "", x = GDP.csv$V5)))

# Question 3
countryNames <- GDP.csv$V4
length(grep(pattern = "^United", x = countryNames))

# Question 4

## Load the Gross Domestic Product data for the 190 ranked countries 
## in this data set:

url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(url2, "GDP.csv", method = "curl")
GDP.csv <- read.csv( "GDP.csv", header = FALSE, skip = 5, nrows = 190)

## Load the educational data from this data set:
url3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url3, destfile = "Edu.csv", method = "curl")
Edu.csv <- read.csv("Edu.csv", header = TRUE)

## Match the data based on the country shortcode. Of the countries for which 
## the end of the fiscal year is available, how many end in June.
Gdp.Edu <- merge(GDP.csv[,c(1,2)], Edu.csv[,c(1,10)], by.x = "V1", by.y = "CountryCode" )
length(grep("Fiscal year end: June", Gdp.Edu$Special.Notes))

# Question 5

## How many values were collected in 2012? How many values were collected on
## Mondays in 2012?






