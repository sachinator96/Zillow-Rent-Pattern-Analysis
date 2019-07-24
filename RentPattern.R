# Author - Sachin Santhosh
# Date - July 15th
# Description - Analyzing Rent patterns on Zillow.

rm(list=ls())

install.packages("data.table")
install.packages("stringr")

install.packages("leaflet")
# Loading library packages:
library(leaflet)
library(data.table)
library(ggplot2)
library(stringr)




# Read in input files:
pricesqft <- data.frame(fread("C:\\Users\\18136\\Desktop\\Projects\\Sachin\\Rent Analysis Zillow\\input files\\pricepersqft.csv",
                              stringsAsFactors = FALSE))
pricedf <- data.frame(fread("C:\\Users\\18136\\Desktop\\Projects\\Sachin\\Rent Analysis Zillow\\input files\\price.csv",
                            stringsAsFactor = FALSE))



# To obtain city and location data (longitude/ latitude coordinates),
# We use the publicly available dataset on Kaggle.

globedata <- data.frame(fread("C:\\Users\\18136\\Desktop\\Projects\\Sachin\\Rent Analysis Zillow\\input files\\worldcitiespop.csv"))
# We filter to include only the US cities.
usdata <- subset(globedata, Country == 'us')

# Merging city and location coordinates:
zillow <- merge(pricedf, 
                   usdata[, c('AccentCity', 'Region', 'Latitude', 'Longitude')], 
                   by.x = c('City', 'State'),
                   by.y = c('AccentCity', 'Region'),
                   all.x = TRUE)


# Some city names do not have location coordinates, because
# names are listed as having "township" as suffix.
#SO: We I)remove townships II)fill in the location co-ordinates in two iterartions.

x = subset(zillow, is.na(Latitude))
x$city2 <- tolower( x$City)
x$city2 <- gsub(" township", "", x$city2)

#II)(1)First Iteration: We merge two Dfs with City column from x and solve
xy <- merge(x, usdata[, c('City', 'Region', 'Latitude', 'Longitude')], 
            by.x = c('city2', 'State'),
            by.y = c('City', 'Region'),
            all.x = TRUE )
xy$Latitude <- xy$Latitude.y
xy$Latitude.x <- NULL
xy$Latitude.y <- NULL

xy$Longitude <- xy$Longitude.y
xy$Longitude.x <- NULL
xy$Longitude.y <- NULL
rm(x)

#II)(2)Second Iteration. We merge on basis of metro column of first df and fill in location co-ordinates.
#There is a chance of common data between metro2 and city2 with city of second df. 
xy$metro2 <- tolower(xy$Metro)
xy <- merge(xy, usdata[, c('City', 'Region', 'Latitude', 'Longitude')],
            by.x = c('metro2', 'State'),
            by.y = c('City', "Region"), 
            all.x = TRUE)

xy$Latitude <- ifelse(!(is.na(xy$Latitude.x)) , xy$Latitude.x,
                      ifelse(!(is.na(xy$Latitude.y)) , xy$Latitude.y,
                             NA))
xy$Longitude <- ifelse(!(is.na(xy$Longitude.x)) , xy$Longitude.x,
                       ifelse(!(is.na(xy$Longitude.y)) , xy$Longitude.y,
                              NA))

xy$Latitude.x = NULL
xy$Latitude.y = NULL

length(xy[is.na(xy$Latitude),])

# We still have about  87 names that do not have location coordinates. 

zillow2 <- merge(zillow, xy[,c('City.Code',  'Latitude', 'Longitude') ],
                    by = c('City.Code' ),
                    all.x = T)

zillow2$Latitude <- ifelse(!(is.na(zillow2$Latitude.x)) , zillow2$Latitude.x,
                              ifelse(!(is.na(zillow2$Latitude.y)) , zillow2$Latitude.y,
                                     NA))
zillow2$Longitude <- ifelse(!(is.na(zillow2$Longitude.x)) , zillow2$Longitude.x,
                               ifelse(!(is.na(zillow2$Longitude.y)) , zillow2$Longitude.y,
                                      NA))

zillow2$Latitude.x = NULL
zillow2$Latitude.y = NULL
zillow2$Longitude.x = NULL
zillow2$Longitude.y = NULL



#Columns 7:81 are the month/year:
#Making a proper column of Date type data.
timeframe <- colnames(zillow2[7:81])
timedf <- data.frame(yearmonth = timeframe)
timedf$year <- str_sub(timedf$yearmonth, -4, -1)
timedf$month <- substr(timedf$yearmonth, 1, 3)
timedf$t2 <- as.Date( paste0("01-", timedf$month, "-", timedf$year), "%d-%b-%Y")

monthlist <- as.array(timedf$t2)





#Removing some unwanted dataframes, to free up memory.
rm(globedata, zillow, usdata, xy, timedf)


# The dataframe zillow2 is the base dataframe that we will use for the analysis:


rentdf <- data.frame()
for(i in 1:nrow(monthlist))
{
  temp1 <- zillow2[, c(1:4, 6, (i+6), 82:83)]
  colnames(temp1) <- c('City_code', 'City', 'State', 'Metro', 
                       'Population_Rank', 'Rent',
                       'Latitude', 'Longitude')
  temp1$Month <- monthlist[i]
  
  rentdf <- rbind(rentdf, temp1)
  
  
}

#Data Cleaning Done!


#Analysis begins!
# (I) Let us take the data for Jan 2017, to do our ranking:
jandf <- subset(rentdf, Month == max(monthlist))


jandf <- jandf[order(jandf$Rent, decreasing = T),]
row.names(jandf) <- NULL
jandf$rank <- 1:nrow(jandf)
jandf$rankedtitle <- ifelse( jandf$rank < 10 , paste0(jandf$rank, ". ", jandf$City),
                             paste0("z", jandf$rank, ". ", jandf$City))

topfirmlist <- as.array(jandf$rankedtitle[1:10])

#(II)To plot graph of top 10 Cities with highest Rent


rankdf <- merge(rentdf , jandf[, c('City_code', 'rankedtitle', 'rank')],
                by = 'City_code', all.x = T)
rankdf$Rent[is.na(rankdf$Rent)] <- 0


toprankdf <- subset(rankdf, rankedtitle %in% topfirmlist)
ggplot(toprankdf, aes(Month, Rent)) + 
  geom_point() +
  facet_wrap(~rankedtitle, scales = "free") + 
  stat_smooth(color="red") +
  ggtitle('Top 10 US Cities with Highest Rent in January 2017')


x <- subset(jandf, !(is.na(jandf$Latitude)))
x$info <- paste0(x$City, ", rent = $", x$Rent)



# ----------------------------------- #
# ----------------------------------- #
# (III) Most expensive cities in Florida:
# ----------------------------------- #
# ----------------------------------- #
x1 <- subset( rankdf, State == 'FL' & Month == '2017-01-01')
x2 <- x1[order(x1$Rent, decreasing = T), ]
row.names(x2) <- NULL
x2$rentdisp <- paste0('$', x2$Rent)

# THe most expensive cities in FL in terms of average rent:
x2[1:20, c('City', 'rentdisp')]










# ----------------------------------- #
# ----------------------------------- #
# (IV)Top 20 cities by population rank:
# ----------------------------------- #
# ----------------------------------- #
poprank1 <- subset(rankdf, Month == '2017-01-01')
poprank2 <- poprank1[order(poprank1$Population_Rank),]
poprank2$disp <- paste0("$", poprank2$Rent)

# List of most populaous US Cities:
poprank2[1:20, c('City', 'State', 'Population_Rank' ,'disp')]

# The average rent in these top 20 cities:
summary(poprank2$Rent[1:20])
# Median rent = $1334



# ------------------------------------------------ #
# ------------------------------------------------ #
# (V)Top 20 cities with highest price per sq ft 
# ------------------------------------------------ #
# ------------------------------------------------ #
# For this we merge the price square foot file to our rankdf dataframe. 
psdf <- merge( subset(rankdf, Month == '2017-01-01'), 
               pricesqft[, c("City.Code", "January.2017")],
               by.y = 'City.Code' , by.x = "City_code",
               all.x = TRUE)

psdf2 <- psdf[order(psdf$January.2017, decreasing = T),]


psdf2$disp_sqf <- paste0("$", psdf2$January.2017)
psdf2$disp_rent <- paste0("$", psdf2$Rent)
rownames(psdf2)<-NULL

# US cities with the highest rent per square foot:
psdf2[1:20, c('City', 'State', "disp_rent", 'disp_sqf')]



#Plotting interactive map of the United States with rent at markers of locations

mapcity <- leaflet(x) %>%
   setView(-94.5786,39.0997 , zoom = 2) %>%
  addTiles() %>%
   addMarkers(~Longitude, ~Latitude, popup = ~info,
              options = popupOptions(closeButton = T),
              clusterOptions = markerClusterOptions())
 mapcity

 #Analysis complete!








