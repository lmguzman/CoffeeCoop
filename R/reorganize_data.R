## This is code to reorganize the data from many small files into one long dataframe

## Andrew MacDonald, June 2013

## read in all the data
## code taken directly from do.accounts.R
## LOAD info AND people DATA
info <- read.csv("data/info.csv", as.is=TRUE,comment.char="#")
people <- read.csv("data/people.csv", as.is=TRUE, na.strings="")
date <- info$Date
nd <- length(date)

## QUALITY CHECKS
## makes sure all other dates are formatted correctly.
if ( any(as.character(as.Date(date[-nd])) != date[-nd]) )
  stop("Non-future dates must be in format YYYY-MM-DD")

files <- sprintf("data/%s.csv", info$Date)
if ( !all(file.exists(files)) ) # all files must exist!
  stop("Data sheets missing: ",
       paste(basename(files)[!file.exists(files)], collapse=", "))

## Extra files in the data directory are almost certainly bad news.
extra <- setdiff(c("info.csv", "people.csv", basename(files)),
                 dir("data"))
if ( length(extra) > 0 )
  stop("Extra files found in data directory: ",
       paste(extra, collapse=", "))

## LOAD FORMER DATA FILES

read.data <- function(filename){
  d <- read.csv(filename, stringsAsFactors=FALSE)
  cols <- c("ID", "Name", "Payment", "Payment.Date", "Milk", "Coffee", "Tea")
  if ( !all(cols %in% names(d)) )
    stop(sprintf("Missing columns in %s: %s", filename,
                 paste(setdiff(cols, names(d)), collapse=", ")))
  
  d[is.na(d)] <- 0
  
  # Exit if duplicates are found
  if( any(duplicated(d$ID)) ){ 
    cat("PROBLEM IN ",filename,": DUPLICATES! \n")
    error('\nDuplicates found. EXIT. \n')
  }
  ## MODIFICATION include the date
  data_date <- gsub(".csv","",basename(filename))
  d <- cbind(data_date,d)
  return(d)
}


dat <- lapply(files, read.data)


## rbind all of it?  Are the columns the same?

which(sapply(dat,ncol)==10)
dat[[20]]  ## weird year with extra data
dat[[20]] <- dat[[20]][!names(dat[[20]])%in%c("NA.","NA..1")]
sapply(dat,ncol)

all_data <- do.call(what=rbind,dat)
## remove lines without consumption
head(all_data)

## all data may be split into payments and consumption.  start with consumption:
consumption <- all_data[-which(rowSums(all_data[c("Coffee","Tea")])==0),]
consumption <- consumption[!names(consumption)%in%c("Payment","Payment.Date")]
## names to numbers -- are numbers unique?  
## may have to delete rows in person database

## destroy 'milk/nomilk' column.

library(data.table)

consumption_dt <- data.table(consumption,key=c("data_date","ID"))
info_dt <- data.table(info,key="Date")
people_dt <- data.table(people,key="ID")

info_dt["2010-08-04"]
consumption_dt[info_dt]

consumption_dt[info_dt,CostBlack*Coffee+Tea*CostMilk]


pplmoney <- consumption_dt[info_dt,list(ID,CoffeeCost=CostBlack*Coffee,CostMilk=Tea*CostMilk)]
pplmoney[,list(sum(CoffeeCost),sum(CostMilk)),by=ID]

merge(consumption_dt,people_dt,by="ID")

consumption_dt[J(people$ID)]

consumption_dt[J("2013-04-22",221)]


consumption_dt[J("",221)]
consumption_dt[people]

## this ends with the data all nicely organized into a sheet showing Milk and Coffee consumption.