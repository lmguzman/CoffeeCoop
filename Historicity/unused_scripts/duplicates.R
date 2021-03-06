info <- read.csv("data/info.csv", as.is=TRUE,comment.char="#")
people <- read.csv("data/people.csv", as.is=TRUE, na.strings="")
date <- info$Date
nd <- length(date)
files <- sprintf("data/%s.csv", info$Date)


get.id <- function(name){
  people[match(name,people$Name),]$ID
}

get.realname <- function(id){
  people[match(id, people$ID),]$Printed.Name
}

remove.duplicates <- function(filename){
  print(filename)
  d <- read.csv(filename, stringsAsFactors=FALSE)
  cols <- c("ID", "Name", "Payment", "Payment.Date", "Milk", "Coffee", "Tea")
  qcols <- c("Payment", "Coffee", "Tea")
  if ( !all(cols %in% names(d)) )
    stop(sprintf("Missing columns in %s: %s", filename,
                 paste(setdiff(cols, names(d)), collapse=", ")))

  theid <- unlist(lapply(d$Name, get.id))
  if(any(theid!=d$ID)){cat(filename, 'new ID,',which(theid!=d$ID),'NA',which(is.na(theid)), '\n')}

  d$ID <- unname(theid)
  od <- dd <- d
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
  
  if(any(colSums(od[,qcols], na.rm=TRUE)!=colSums(newd[,qcols], na.rm=TRUE))) {
    cat('error in ', filename,'\n')
  print(dd[theduplicated,qcols])
  print(dd[theduplicates,qcols])

  }

  return(newd)
}

dat <- lapply(files, remove.duplicates)


cat('Save new data?\n')
x <- scan(what=logical(), nmax=1)
if(x){
save.newdata <- function(filename){
  write.csv(remove.duplicates(filename), paste(filename,sep=''), row.names=FALSE)
}
lapply(files, save.newdata)
}
