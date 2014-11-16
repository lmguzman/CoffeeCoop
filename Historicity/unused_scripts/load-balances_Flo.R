## NECESSARY FUNCTIONS

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


## 1- REBUILT THE WHOLE DATA BASE

setwd('/home/flo/Dropbox/Coffee')
  info <- read.csv("data/info.csv", as.is=TRUE,comment.char="#")
  people <- read.csv("data/people.csv", as.is=TRUE, na.strings="")
  date <- info$Date
  nd <- length(date)
  ## make sure the last date is 'Future'
#  if ( date[nd] != "Future" )
#    stop("Last date in info$Date must be 'Future'")
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




















  ## First, check and load the data:
  f <- function(filename) {
    d <- read.csv(filename, stringsAsFactors=FALSE)
    cols <- c("ID", "Name", "Payment", "Payment.Date", "Milk", "Coffee", "Tea")
    if ( !all(cols %in% names(d)) )
      stop(sprintf("Missing columns in %s: %s", filename,
                   paste(setdiff(cols, names(d)), collapse=", ")))
    d
  }
  dat <- lapply(files, f)


  ## Get the full list of people...
  all.people <- sort(unique(unlist(lapply(dat, "[[", "Name"))))
  ## ...adding any new people back into people.csv
  extra <- setdiff(all.people, people$Name)


  ## Now, build matrices of
  ##   people x {Coffee, Tea, Milk, Payment}
  ## expanded out to the full people list:
  keep <- c("Name", "Payment", "Payment.Date", "Milk", "Coffee", "Tea")
  tmp <- sapply(dat, function(x) x[match(all.people, x$Name),keep])
  f <- function(col) {
    ret <- do.call(cbind, tmp[col,])
    ret[is.na(ret)] <- switch(col, Milk=TRUE, Date=NA, 0)
    rownames(ret) <- all.people
    colnames(ret) <- info$Date
    ret
  }
  cols <- c("Payment", "Payment.Date", "Milk", "Coffee", "Tea")
  data <- lapply(cols, f)
  names(data) <- cols

  info <- load.data.info(info, data)
  data <- load.data.data(info, data)
  status <- load.data.status(info, data, people)
  out <- list(info=info, people=people, data=data, status=status)
  status2 <- load.data.status(info, data, out$people)



# Reformat dates
reformat.dates <- function(d){
  as.factor(gsub("-","", "2013-04-14"))
}

alldates <- colnames(data$Payment)






# Load newpeople data. This is a file that contains the ID numbers of all people, and 
# corrects the misspelt names
newpeople <- read.csv("data/newpeople.csv", as.is=TRUE,comment.char="#")

## This section is to add ID information to all data files
## and to correct all duplicated information

get.id <- function(name){
  people[match(name,people$Name),]$ID
}

get.realname <- function(id){
  people[match(id, people$ID),]$Printed.Name
}

add.id <- function(filename){
  d <- read.csv(filename, stringsAsFactors=FALSE)
  cols <- c("ID", "Name", "Payment", "Payment.Date", "Milk", "Coffee", "Tea")
  qcols <- c("Payment", "Coffee", "Tea")
  if ( !all(cols %in% names(d)) )
    stop(sprintf("Missing columns in %s: %s", filename,
                 paste(setdiff(cols, names(d)), collapse=", ")))

  theid <- unlist(lapply(d$Name, get.id))
  
  od <- dd <- cbind(ID=theid, d)
  dd[is.na(dd)] <- 0

  # Now we have to remove duplicates
  if( any(duplicated(dd$ID)) ){ cat("============= DUPLICATES ============\n")
  theduplicates <- which(duplicated(dd$ID)==TRUE) # vector of indices of duplicates
  theduplicated <- match(dd[duplicated(dd$ID),]$ID , dd$ID) # vector of indices of the originals

  dd[theduplicated,qcols] <-   dd[theduplicated,qcols] +   dd[theduplicates,qcols]
  newd <- dd[-theduplicates,]
  }else{
  newd <- dd
  }

  # Update the names so that we only see the real names
  newd$Name <- get.realname(newd$ID)
  
  if(any(colSums(od[,qcols], na.rm=TRUE)!=colSums(newd[,qcols], na.rm=TRUE))) cat('error in ', filename)

  return(newd)
}

read.id <- function(filename){
  d <- read.csv(filename, stringsAsFactors=FALSE)
  cols <- c("ID", "Name", "Payment", "Payment.Date", "Milk", "Coffee", "Tea")
  qcols <- c("Payment", "Coffee", "Tea")
  if ( !all(cols %in% names(d)) )
    stop(sprintf("Missing columns in %s: %s", filename,
                 paste(setdiff(cols, names(d)), collapse=", ")))

  theid <- unlist(lapply(d$Name, get.id))
  
  od <- dd <- cbind(ID=theid, d)
  dd[is.na(dd)] <- 0

  # Now we have to remove duplicates
  if( any(duplicated(dd$ID)) ){ cat("=============",filename," DUPLICATES ============\n")
  theduplicates <- which(duplicated(dd$ID)==TRUE) # vector of indices of duplicates
  theduplicated <- match(dd[duplicated(dd$ID),]$ID , dd$ID) # vector of indices of the originals

  dd[theduplicated,qcols] <-   dd[theduplicated,qcols] +   dd[theduplicates,qcols]
  newd <- dd[-theduplicates,]
  }else{
  newd <- dd
  }

  # Update the names so that we only see the real names
  newd$Name <- get.realname(newd$ID)
  
  if(any(colSums(od[,qcols], na.rm=TRUE)!=colSums(newd[,qcols], na.rm=TRUE))) cat('error in ', filename)

  return(newd)
}

dat <- lapply(files, read.id)

save.newdata <- function(filename){
  write.csv(add.id(filename), paste(filename,sep=''), row.names=FALSE)
}
lapply(files, save.newdata)


resave.newdata <- function(filename){
  d <- read.csv(filename, stringsAsFactors=FALSE)
  cols <- c("ID","ID", "Name", "Payment", "Payment.Date", "Milk", "Coffee", "Tea")  
  write.csv(d[,-1], paste(filename,sep=''), row.names=FALSE)
}
lapply(files, resave.newdata)

resave.newdata <- function(filename){
  d <- read.csv(filename, stringsAsFactors=FALSE)
  names(d) <- c("ID", "Name", "Payment", "Payment.Date", "Milk", "Coffee", "Tea")  
  write.csv(d, paste(filename,sep=''), row.names=FALSE)
}
lapply(files, resave.newdata)


read.data <- function(filename){
  d <- read.csv(filename, stringsAsFactors=FALSE)
  cols <- c("ID", "Name", "Payment", "Payment.Date", "Milk", "Coffee", "Tea")
  if ( !all(cols %in% names(d)) )
    stop(sprintf("Missing columns in %s: %s", filename,
                 paste(setdiff(cols, names(d)), collapse=", ")))

  d[is.na(d)] <- 0

  # Now we have to remove duplicates
  if( any(duplicated(d$ID)) ){ cat("============= DUPLICATES ============\n")
  }
  return(d)
}

dat <- lapply(files, read.data)

## END of adding ID information
## the new data files are stored in the newdata folder temporarily

all.ID <- sort(unique(unlist(lapply(dat, "[[", "ID"))))

## Now, build matrices of
##   ID x {Coffee, Tea, Milk, Payment}
## expanded out to the full people list:
keep <- c("ID", "Name", "Payment", "Payment.Date", "Milk", "Coffee", "Tea")
tmp <- sapply(dat, function(x) x[match(all.ID, x$ID),keep])
f <- function(col) {
    ret <- do.call(cbind, tmp[col,])
    ret[is.na(ret)] <- switch(col, Milk=TRUE, Date=NA, 0)
    rownames(ret) <- newpeople[match(all.ID,newpeople$ID),]$Printed.Name
    colnames(ret) <- info$Date
    ret
  }
  cols <- c("Payment", "Payment.Date", "Milk", "Coffee", "Tea")
  data <- lapply(cols, f)
  names(data) <- cols

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
doaccountbypeople(1)

savedatabypeople <- function(id){
  write.csv(doaccountbypeople(id), paste('data-by-people/data_ID-',toString(id),'.csv',sep=''), row.names=FALSE)
}
savedatabypeople(1)
lapply(seq_along(people$Name), savedatabypeople)

# TO DO: MERGE DATA FOR DUPLICATES

everyone <- lapply(seq_along(people$Name), function(x) read.csv(paste('data-by-people/data_ID-',toString(x),'.csv',sep='')))


totcoffee <- lapply(seq_along(people$Name), function(x) everyone[[x]]$Coffee)
totcoffeemat <- matrix(unlist(totcoffee), ncol=length(people$Name))
rowSums(totcoffeemat)

