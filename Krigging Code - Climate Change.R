library(readr)
library(plotly)
library(dplyr)
library(sp)
library(gstat)
library(RColorBrewer)
library(ggmap)
suppressPackageStartupMessages({
  library(dplyr) # for "glimpse"
  library(ggplot2)
  library(scales) # for "comma"
  library(magrittr)
})
library(ggplot2)
library(gridExtra)
library(automap)
library(gstat)
library(elevatr)
library(lubridate)

treeOriginal <- read_csv("C:/Users/spenc/Downloads/tree_final_data (1).csv")
acornsOriginal <- read_csv("C:/Users/spenc/Downloads/acorn.rain (1).csv")

for(i in 1:12){
  tree <- treeOriginal
  acorns <- acornsOriginal
  
  #For tree data set, convert date_time to date format
  
  tree$date <- as.Date(tree$date, "%m/%d/%Y")
  tree$day <- day(tree$date)
  
  # Subset data, since it is too large right now to run effectively
  set.seed(123)
  myseq <- seq(1,550000, by=500)
  tree_subset <- tree[myseq,]
  
  
  # We will randomly split the data in two parts. 70% train, 30% test: 
  set.seed(22313)
  splitchoose <- sample(1:nrow(tree_subset), .7*nrow(tree_subset)) 
  train01 <- tree_subset[splitchoose, ] 
  test <- tree_subset[-splitchoose, ]
  
  
  # group by month, find average
  
  train01 <- train01 %>% select(Tree.Num, Year, latitude, longtitude, Month, day, temperature) %>% group_by(Month, Tree.Num, latitude, longtitude, day) %>%
    summarise(Avg_temp = mean(temperature))
  
  acorns <- acorns %>% select(Tree.Num, year, lat, long, Month) %>% group_by(Month, Tree.Num, lat, long)
  

# define x and y as longitude and latitude
train01 <- train01[train01$Month==i,] #subsetting by month
train01$x <- train01$latitude  # define x & y as longitude and latitude
train01$y <- train01$longtitude
test$x <- test$latitude
test$y <- test$longtitude

# make the dataframes into coordinates for idw model
train01 <- as.data.frame(train01)
coordinates(train01) = ~x+y
coordinates(test) = ~x+y

#interpolate - apply to test dataset
idw <- idw(formula = Avg_temp ~ 1, locations = train01, 
           newdata = test[,-c(1)])  # apply idw model for the data

# predicted outcome
idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("longitude", "latitude", "predicted_temperature")  # give names to the modelled variable

#compare
actual <- test$temperature
#MSE
mse = sqrt(sum((idw.output$predicted_temperature - actual)^2) / length(actual))

summary(idw.output$predicted_temperature) #check



train01 <- as.data.frame(train01)
acorns <- as.data.frame(acorns)
#acornsxy <- acorns[,c("lat", "long", "X1")]
#acornsxy <- na.omit(acornsxy) #if necessary, but cleaned data
coordinates(acorns) <- ~lat + long
coordinates(train01) <- ~latitude + longtitude

#interpolate and fix output
idw2 <- idw(formula = Avg_temp ~ 1, locations = train01,
            newdata = acorns)  # apply idw model for the data

idw.output2 = as.data.frame(idw2)  # output is defined as a data table
names(idw.output2)[3] <- c("Temperature")  

#add output to acorns dataset


merged <- merge(idw.output2, acorns, by = c("lat", "long"))

# acorns$Temperature <- idw.output2$Temperature

# tail(acorns[,c("Tree.Num", "lat", "long", "Temperature")])
summary(idw.output2$Temperature) #check
#write.csv(merged, file="acorns_average_temp.csv")


mergy <- merged  %>% select(lat, long, Temperature, Tree.Num, year) %>% group_by(Tree.Num) %>% summarise(Avg_temp = mean(Temperature))

#remove duplicate rows
#merged <- merged[!duplicated(merged), ]

write.csv(paste("mergy", i, "csv", sep = "."))
}

## Merging the files and getting the results, cleaning the column names

TempMonth01 <- read_csv("C:/Users/spenc/Dropbox/UCLA/Spring 2017 Classes/Stats 141/TempMonth01.csv")
TempMonth01 <- TempMonth01[,-c(1)]
colnames(TempMonth01)[2] <- "TempMonth01"

TempMonth02 <- read_csv("C:/Users/spenc/Dropbox/UCLA/Spring 2017 Classes/Stats 141/TempMonth02.csv")
TempMonth02 <- TempMonth02[,-c(1)]
colnames(TempMonth02)[2] <- "TempMonth02"

TempMonth03 <- read_csv("C:/Users/spenc/Dropbox/UCLA/Spring 2017 Classes/Stats 141/TempMonth03.csv")
TempMonth03 <- TempMonth03[,-c(1)]
colnames(TempMonth03)[2] <- "TempMonth03"

TempMonth04 <- read_csv("C:/Users/spenc/Dropbox/UCLA/Spring 2017 Classes/Stats 141/TempMonth04.csv")
TempMonth04 <- TempMonth04[,-c(1)]
colnames(TempMonth04)[2] <- "TempMonth04"

TempMonth05 <- read_csv("C:/Users/spenc/Dropbox/UCLA/Spring 2017 Classes/Stats 141/TempMonth05.csv")
TempMonth05 <- TempMonth05[,-c(1)]
colnames(TempMonth05)[2] <- "TempMonth05"

TempMonth06 <- read_csv("C:/Users/spenc/Dropbox/UCLA/Spring 2017 Classes/Stats 141/TempMonth06.csv")
TempMonth06 <- TempMonth06[,-c(1)]
colnames(TempMonth06)[2] <- "TempMonth06"

TempMonth07 <- read_csv("C:/Users/spenc/Dropbox/UCLA/Spring 2017 Classes/Stats 141/TempMonth07.csv")
TempMonth07 <- TempMonth07[,-c(1)]
colnames(TempMonth07)[2] <- "TempMonth07"

TempMonth08 <- read_csv("C:/Users/spenc/Dropbox/UCLA/Spring 2017 Classes/Stats 141/TempMonth08.csv")
TempMonth08 <- TempMonth08[,-c(1)]
colnames(TempMonth08)[2] <- "TempMonth08"

TempMonth09 <- read_csv("C:/Users/spenc/Dropbox/UCLA/Spring 2017 Classes/Stats 141/TempMonth09.csv")
TempMonth09 <- TempMonth09[,-c(1)]
colnames(TempMonth09)[2] <- "TempMonth09"

TempMonth10 <- read_csv("C:/Users/spenc/Dropbox/UCLA/Spring 2017 Classes/Stats 141/TempMonth10.csv")
TempMonth10 <- TempMonth10[,-c(1)]
colnames(TempMonth10)[2] <- "TempMonth10"

TempMonth11 <- read_csv("C:/Users/spenc/Dropbox/UCLA/Spring 2017 Classes/Stats 141/TempMonth11.csv")
TempMonth11 <- TempMonth11[,-c(1)]
colnames(TempMonth11)[2] <- "TempMonth11"

TempMonth12 <- read_csv("C:/Users/spenc/Dropbox/UCLA/Spring 2017 Classes/Stats 141/TempMonth12.csv")
TempMonth12 <- TempMonth12[,-c(1)]
colnames(TempMonth12)[2] <- "TempMonth12"


# Merge Files Now on key indicator = tree_num

Monthly_Temp_Predictions <- list(TempMonth01, TempMonth02, TempMonth03, TempMonth04, TempMonth05, TempMonth06, TempMonth07, TempMonth08, TempMonth09, TempMonth10, TempMonth11, TempMonth12) %>%
  Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="Tree.Num"), .)

# Merge with Acorns Dataset

acorns <- read_csv("C:/Users/spenc/Downloads/acorn.rain (1).csv")
Acorns_with_Predictions <- list(Monthly_Temp_Predictions, acorns) %>% Reduce(function(df1, df2) left_join(df1, df2, by="Tree.Num"), .)

# Write the csv files for both datasets

write.csv(Monthly_Temp_Predictions, file="Monthly_Temperature_Predictions.csv")
write.csv(Acorns_with_Predictions, file="Acorns_with_Predictions.csv")

## Producing a dataset with montly means of the trees with buttons

tree_monthly_avg <- tree %>% select(Tree.Num, Year, latitude, longtitude, Month, temperature) %>% group_by(Month, Tree.Num, latitude, longtitude) %>%
  summarise(Avg_temp = mean(temperature))

#write the csv file
write.csv(tree_monthly_avg, file="tree_monthly_avg")

## Producing a dataset with daily means of the trees with buttons

tree$date <- as.Date(tree$date, "%m/%d/%Y")
tree$day <- day(tree$date)

tree_daily_avg <- tree %>% select(Tree.Num, Year, latitude, longtitude, day, temperature) %>% group_by(day, Tree.Num, latitude, longtitude) %>%
  summarise(Avg_temp = mean(temperature))


#write the csv file

write.csv(tree_daily_avg, file="tree_daily_avg.csv")