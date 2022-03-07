# Author: Kun Xu
#setwd("/Users/kunxu219/Desktop/FE Project/raw_data")
#setwd("/Users/thomasserafin/Serafin_Documents/Cornell/Fall 2020/GSAM_Project/GSAM_Project-master/Raw_Data") # Tom's Directory, please leave commented. 
#setwd("/Users/shihuiwang/Desktop/GSAM/RawData")
library(readxl) # for reading xlsx file 
library(dplyr) # industry standard 

# ----------- load data ------------------- 
# load asset alloc data 
asset_alloc_data <- read_excel("2020.09 Models_tosend.xlsx")

# load the mutual fund and ETF data
etf = read.csv("2020.09 US ETFs_tosend.csv")
open_end_1 = read.csv("2020.09 US Open-End Funds_part1_tosend.csv")
open_end_2 = read.csv("2020.09 US Open-End Funds_part2_tosend.csv")
open_end_3 = read.csv("2020.09 US Open-End Funds_part3_tosend.csv")
open_end = rbind(rbind(open_end_1,open_end_2),open_end_3) # combine open_end funds data into one dataframe

# ------------ 
# Data Cleaning For Asset Allocation Data. 
# Will continue on later just started the initial steps and will leave the user the avaliable to add model portfolios moving from left to right. 
# Model portfolios can be added to the right. 
asset_alloc_data <- asset_alloc_data %>% select(-1, -2) # Removing the 1st and 2nd column, 
model_portfolios <- asset_alloc_data[1,2:ncol(asset_alloc_data)] # Getting each model portfolio

# Now renaming column names in order to use for dplr. e.g. $ method. 
colnames(asset_alloc_data)[2:ncol(asset_alloc_data)] <- model_portfolios
colnames(asset_alloc_data)[1] <- "Sector"

# Now removing the first row because it was moved up as the data headers 
asset_alloc_data <- asset_alloc_data[-1,]

# reformatting each column to be as.numeric(), helps us down the road. Putting the allocations in percentage terms
# i could use lapply in order to turn the below lines in 1 line of code. Will do this later. Just lazy at the moment. Feel free to automate it. 
asset_alloc_data$`GSAM 70:30% w/ Liq. Alts` <- as.numeric(asset_alloc_data$`GSAM 70:30% w/ Liq. Alts`) * 100
asset_alloc_data$`GSAM 60:40% w/ Liq. Alts` <- as.numeric(asset_alloc_data$`GSAM 60:40% w/ Liq. Alts`) * 100
asset_alloc_data$`Traditional 70:30%` <- as.numeric(asset_alloc_data$`Traditional 70:30%`) * 100
asset_alloc_data$`Traditional 60:40%` <- as.numeric(asset_alloc_data$`Traditional 60:40%`) * 100
asset_alloc_data$`Simple 70:30%` <- as.numeric(asset_alloc_data$`Simple 70:30%`) * 100
asset_alloc_data$`Simple 60:40%` <- as.numeric(asset_alloc_data$`Simple 60:40%`) * 100


# ...... to do more ... 

# ------------- 

#select only ETF 
etf<-etf[grep(pattern="ETF",etf$Name),]
#View(etf)

getMonth <- function(date_1, date_2){
  library("lubridate")
  num <- interval(date_1, date_2) %/% months(1) 
  return (num)
}

n <- getMonth("1990-01-01","2020-08-31")

# return the Fundid and the associated return
etf_fundId <- etf[,1]
return_etf <- etf[, 13:(13+n)]
return_etf <- cbind(etf_fundId,return_etf)

open_end_fundId <- open_end[,1]
return_open_end <- open_end[, 15:(15+n)]
return_open_end <- cbind(open_end_fundId,return_open_end)

# kinda hard-code above since I look at the data and count the first return starts at 13th for etf and 15th for open_end
# to avoid hard-code, I come up with this method

# define a function that return all start date and end date of the month between time period
names(etf)[names(etf) == "Return...to.1990.01.31..USD"] <- "Return..1990.01.01..to.1990.01.31..USD" # match the first return with others
date <- function(startDate, endDate){
  #startDate = paste(toString(startYear),"-01-01", sep="")
  startDate = as.Date(startDate, "%Y-%m-%d")
  endDate = as.Date(endDate,"%Y-%m-%d")
  len = length(seq(from=startDate, to=endDate, by='month')) - 1
  date.start.month <- seq(startDate,length=len+1,by="months")
  startDate <- seq(startDate, length = 2, by = "+1 months")[2]
  date.end.month <- seq(startDate,length=len+1,by="months")-1
  dates <- do.call(rbind, Map(data.frame, start=date.start.month, end=date.end.month))
  return (dates)
}
iter <- date("1990-01-01","2020-08-31")

# change to the format that the eft and open_end date has
iter <- format(iter, ".%Y.%m.%d.") 
iter2 <- c()
for (i in 1:nrow(iter)){
  iter2[i] <- paste("Return.", iter[i,1], ".to", iter[i,2], ".USD", sep="")
}

# now we can get the return straight like this
etf[iter2[2]]

# combine all return together by fundID
r_etf <- etf[iter2]
etf_id <- etf[,1]
r_etf <- cbind(etf_id,r_etf)


# ------------- Mapping: Asset class Names to MorningStar Category Notes------------- 
# by SW & Ci
#load the fifth tab (Mappings) in 2020.09 Models_tosend excel 
mapping <- read_excel("2020.09 Models_tosend.xlsx", 5) 
#copy asset_alloc_data
portfolios <- asset_alloc_data 
#remove NA rows
portfolios <- na.omit(portfolios) 
#create a new column "Morningstar" and initialize it
portfolios$Morningstar <- portfolios$Sector 

#mapping asset class name with morningstar
assetclass_names <- mapping[,"Asset Class Name"][[1]]

for (i in 1:nrow(portfolios)){
  if (portfolios[i,"Sector"] %in% assetclass_names){
    ind <- match(portfolios[i,"Sector"], assetclass_names)
    portfolios[i,"Morningstar"] <- mapping[ind,"Morningstar Category Notes"]}
}

# ------------- done mapping notes------------- 


# ------------- Mapping: Asset class Names to Corresponding FunIds -------------
# by Ci
## we want to create a list in the format above:
## aggreg_Id:
## MorningStar.Category                  x
## "US Fund Large Blend"             (FundIds)
# get the whole MorningStar Category list by combing ETF & Open end funds
category_etf <- etf %>% select(1, 4) # only select the columns of FundId and MorningStar.Category
category_open_end <- open_end %>% select(1, 4)
category <- rbind(category_etf,category_open_end)
# aggregate FunId by MorningStar Category
aggreg_Id <- aggregate(category$FundId,category["Morningstar.Category"],paste,collapse=' ')
View(aggreg_Id)
## we have 135 non_distinct morningstar categories among all the ETF&Open end funds


## we want to create a list with names to be the asset classes, and valus to be all the corresponding FundIds
## mapped_Id:
## $"Asset_class"
##   "" "" ... "" All the FundIDs that belong to the class
# initiate the list
mapped_Id <- vector(mode <- "list", length <- nrow(portfolios)) # create a empty list
names(mapped_Id) <- as.list(portfolios$Sector) # assign the names of the list as the asset class

# define a function to test if the morningstar category note have corresponding categories
notes_mapping <- function(note, aggreg_Id){
  for (i in 1:nrow(aggreg_Id)){
    id_category <- as.character(aggreg_Id[i,1])
    if (note == id_category){
      return(TRUE)
      break
    }
  }
  return(FALSE)
}

# map the asset classes with corresponding FunIds
for (j in 1:nrow(portfolios)){
  asset_class <- as.character(portfolios[j,"Sector"])
  category_note <- as.character(portfolios[j,"Morningstar"])
  split_note <- strsplit(category_note,", ")[[1]]
  if (j==9|j==12){
    region_code <- "EAA Fund USD"
  } else {
    region_code <- "US Fund"
  }
  for (p in 1:length(split_note)){
    note <- paste(region_code, split_note[p], sep=" ") ## format note by adding region code
    if (notes_mapping(note, aggreg_Id)){
      ind <- match(note,as.character(aggreg_Id[,1]))
      mapped_Id[asset_class] <- aggreg_Id[ind,"x"]
    }
  }
}
View(mapped_Id)

# ------------- done mapping FundIds------------- 
