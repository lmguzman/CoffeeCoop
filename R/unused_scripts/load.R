## There is currently a problem here in loading "Shiao Hayakawa" who
## exists on my sheet but has no activity.  Should be included with a
## warning, I think.  This complicates the status generation.

## The start date that import came up with is not correct.
load.data <- function() {
  ## loads in the data from info and people
  ## info.csv contains simple global variables: how much coffee costs, how many have been made, etc
  ## people is a complete database of all people ever in the coop.
  info <- read.csv("data/info.csv", as.is=TRUE,comment.char="#")
  people <- read.csv("data/people.csv", as.is=TRUE, na.strings="")
  ## extract the date column
  date <- info$Date
  ## which is the last date?  (this is the only which is not an actual date, but
  ## rather the word "Future")
  nd <- length(date)
  ## make sure the last date is 'Future'
  if ( date[nd] != "Future" )
    stop("Last date in info$Date must be 'Future'")
  ## makes sure all other dates are formatted correctly.
  if ( any(as.character(as.Date(date[-nd])) != date[-nd]) )
    stop("Non-future dates must be in format YYYY-MM-DD")

  ## this is the tricky part.  this code expects all that every date listed has
  ## a corresponding datasheet.  It creates a confusing error if you are doing a
  ## rerun: it expects a datasheet for the most recent accounting date -- which
  ## in the case of a rerun, could be todays date! -- but, if you have removed
  ## or hidden this file, it will still look.
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
    cols <- c("Name", "Payment", "Payment.Date", "Milk", "Coffee", "Tea")
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
  if ( length(extra) > 0 ) {
    people.new <- data.frame(Name=extra, Email="",
                             Start=as.character(Sys.Date()), Gone=FALSE,
                             Gone.Date="", stringsAsFactors=FALSE)
    tmp <- rbind(people, people.new)
    tmp <- tmp[order(tmp$Name),]
    if ( any(duplicated(tmp$Name)) )
      stop("Duplicated names found")
    write.csv(tmp, "data/people.csv", row.names=FALSE, na="")
    message("Found new people:\n",
            paste("\t", sort(extra), collapse="\n", sep=""),
            "\nThese have been added to data/people.csv")
  }
  ## Make sure that blank people are added (happens rarely)
  extra <- setdiff(people$Name, all.people)
  if ( length(extra) > 0 ) {
    warning("Extra people in data/people.csv\n",
            paste("\t", sort(extra), collapse="\n", sep=""),
            "\nwith no activity reported")
    all.people <- sort(c(all.people, extra))
  }
  people$Gone[is.na(people$Gone)] <- FALSE

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
  list(info=info, people=people, data=data, status=status)
}

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
  status$Milk <- data$Milk[,"Future"]
  status
}

possibly.gone <- function(dat, cutoff=4) {
  info <- dat$info
  data <- dat$data
  ## Time since last seen:
  seen.c <- apply(dat$data$Coffee, 1, function(x) which(rev(x) > 0)[1])
  seen.t <- apply(dat$data$Tea,    1, function(x) which(rev(x) > 0)[1])
  seen.c[is.na(seen.c)] <- Inf
  seen.t[is.na(seen.t)] <- Inf
  seen <- pmin(seen.c, seen.t) - 2
  gone <- dat$people$Gone[match(names(seen), dat$people$Name)]
  gone[is.na(gone)] <- FALSE
  seen[seen > cutoff & !gone]
}
