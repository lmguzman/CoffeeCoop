setwd('../')

## LOAD info AND people DATA
info <- read.csv("data/info.csv", as.is=TRUE,comment.char="#")
people <- read.csv("data/people.csv", as.is=TRUE, na.strings="")
date <- info$Date
nd <- length(date)

## UPDATE THE INFO FILE
## By asking for manual entries
cat('\nEnter the date (\"YYYY-MM-DD\") you want for today\n')
today <- scan(nmax=1, what=character())

cat('\nEnter the price of black coffee (was $',info$CostBlack[nd],')\n')
pricecoffee <- scan(nmax=1, what=double())

cat('\nEnter the price of milk (was $',info$CostMilk[nd],')\n')
pricemilk <- scan(nmax=1, what=double())

cat('\nEnter the official number of coffees made by the machine in total (was ',info$Count[nd],')\n')
totcount <- scan(nmax=1, what=double());

## LOAD THE NEW DATA THAT IS STORED IN THE "Future.csv" FILE
load.new.date <- function(today=0){
  if(today==0){
    cat('Enter the date (\"YYYY-MM-DD\")\n')
    today <- scan(nmax=1, what='character')
  }

  d <- read.csv('data/Future.csv', stringsAsFactors=FALSE)

  get.id <- function(name){
    people[match(name,people$Name),]$ID
  }

  get.realname <- function(id){
    people[match(id, people$ID),]$Printed.Name
  }

  cols <- c("Name", "Payment", "Payment.Date", "Milk", "Coffee", "Tea")
  if ( !all(cols %in% names(d)) )
    stop(sprintf("Missing columns in %s: %s", filename,
                 paste(setdiff(cols, names(d)), collapse=", ")))

  theid <- unlist(lapply(d$Name, get.id))
  
  if(any(is.na(theid))){sapply(which(is.na(theid)), function(x){cat(toString(d[x,]$Name), 'is an unregistered new coop member\n')}); stop('-> Add them first to the people.csv file and give them an ID; then rerun.\n')}

  theid <- unlist(lapply(d$Name, get.id))
  
  dd <- cbind(ID=theid, d)
  dd[is.na(dd)] <- 0

  write.csv(dd, paste('data/',today,'.csv',sep=''), row.names=FALSE)
}

load.new.date(today)
info <- rbind(info,c(0, pricecoffee, pricemilk, totcount, NA, NA, NA))
info[nrow(info),]$Date <- today
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

# LOAD FORMER DATA FILES

read.data <- function(filename){
  print(filename)
  d <- read.csv(filename, stringsAsFactors=FALSE)
  cols <- c("ID", "Name", "Payment", "Payment.Date", "Milk", "Coffee", "Tea")
  if ( !all(cols %in% names(d)) )
    stop(sprintf("Missing columns in %s: %s", filename,
                 paste(setdiff(cols, names(d)), collapse=", ")))

  d[is.na(d)] <- 0

  # Now we have to remove duplicates
  if( any(duplicated(d$ID)) ){ cat("============= ',filename,' DUPLICATES! ============\n")
  }
  return(d)
}

dat <- lapply(files, read.data)

tmpID <- sort(unique(unlist(lapply(dat, "[[", "ID"))))
all.ID <- tmpID[order(people[match(tmpID, people$ID),]$Printed.Name)]

## Now, build matrices of
##   ID x {Coffee, Tea, Milk, Payment}
## expanded out to the full people list:
keep <- c("ID", "Name", "Payment", "Payment.Date", "Milk", "Coffee", "Tea")
tmp <- sapply(dat, function(x) x[match(all.ID, x$ID),keep])
f <- function(col) {
    ret <- do.call(cbind, tmp[col,])
    ret[is.na(ret)] <- switch(col, Milk=TRUE, Date=NA, 0)
    rownames(ret) <- people[match(all.ID,people$ID),]$Printed.Name
    colnames(ret) <- info$Date
    ret
  }
  cols <- c("Payment", "Payment.Date", "Milk", "Coffee", "Tea")
  data <- lapply(cols, f)
  names(data) <- cols


## ACCOUNTING FUNCTIONS

load.data.info <- function(info, data) {
  dd <- c(NA, diff(as.Date(info$Date)))
  info$Used <- c(NA, diff(info$Count))
  info$CostMilk[seq_len(which(!is.na(info$CostMilk))[1] - 1)] <- 0

  info$Paid        <- colSums(data$Coffee[,info$Date])
  info$MilkyCoffee <- colSums((data$Coffee * data$Milk)[,info$Date])

  info$UsedPerDay   <- info$Used / dd
  info$PaidPerDay   <- info$Paid / dd
  info$Honesty      <- info$Paid / info$Used
  info$TrueMilkCost <- info$MilkOutgoing / info$MilkyCoffee

  info$Tea <- colSums(data$Tea[,info$Date])
  info$TeaPerDay <- info$Tea / dd
  
  info
}

load.data.data <- function(info, data) {
  np <- nrow(data$Coffee)
  nd <- ncol(data$Coffee)
  cost.coffee <- matrix(info$CostBlack, np, nd, byrow=TRUE)
  cost.milk   <- matrix(info$CostMilk,  np, nd, byrow=TRUE)

  used <- cost.coffee * data$Coffee +
    cost.milk * data$Milk * data$Coffee +
      cost.milk * data$Tea
  if ( all(is.na(used[,ncol(used)])) )
    used[,ncol(used)] <- 0
  if ( any(is.na(used)) )
    stop("NA values computed for usage -- should never happen")
  
  used["Sally Otto",] <- 0

  data$Used <- used
  data
}

load.data.status <- function(info, data, people) {
  last <- suppressWarnings(apply(data$Payment > 0, 1, function(x)
                                 max(which(x))))
  last[is.infinite(last)] <- NA
  idx <- cbind(seq_along(last), last)

  status <- people[match(rownames(data$Coffee), people$Name),
                   c("Name", "Email", "Gone")]
  status$Gone[is.na(status$Gone)] <- FALSE

  status$TotalCoffee <- rowSums(data$Coffee)
  status$TotalTea <- rowSums(data$Tea)
  status$TotalIn <- rowSums(data$Payment)
  status$TotalOut <- rowSums(data$Used)
  status$LastCoffee <- data$Coffee[,ncol(data$Coffee)-1]
  status$LastTea <- data$Coffee[,ncol(data$Tea)-1]
  status$LastPaid <- data$Payment[idx]
  status$LastDate <- data$Payment.Date[idx]
  status$Balance <- status$TotalIn - status$TotalOut
  status$Milk <- data$Milk[,dim(data$Milk)[2]]
  status
}



  info <- load.data.info(info, data)
  data <- load.data.data(info, data)
  status <- load.data.status(info, data, people)
  out <- list(info=info, people=people, data=data, status=status)
  status2 <- load.data.status(info, data, out$people)
alldates <- colnames(data$Payment)



databypeople <- function(coopid){
    i <- which(all.ID==coopid)
    clean.payment.date <- unname((data$Payment.Date)[i,])
    clean.payment.date[clean.payment.date=="0"] <- ""
  
    data.frame(Date=alldates, HasMilk=data$Milk[i,], Coffee=unname((data$Coffee)[i,]), Tea=unname((data$Tea)[i,]), Milk=unname((data$Tea)[i,]+(data$Milk[i,])*(data$Coffee)[i,]), Payment=unname((data$Payment)[i,]), Payment.Date=clean.payment.date, InOut=rep(0, length(alldates)), Balance=rep(0, length(alldates)))
}

doaccountbypeople <- function(coopid){
  dxppl <- databypeople(coopid)
  io <- dxppl$Payment - dxppl$Coffee * info$CostBlack - dxppl$Milk * info$CostMilk
  balance <- cumsum(io)
  dxppl$InOut <- io
  dxppl$Balance <- round(balance,10)
  return(dxppl)
}

