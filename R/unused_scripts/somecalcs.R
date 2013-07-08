allppl <- lapply(all.ID, doaccountbypeople)

lastbalance <- function(index){
  ((allppl[[index]])$Balance)[nd]
}

alllastbalance <- lapply(seq_along(all.ID), lastbalance)
alllastbalance[which(all.ID==(people[people$Name=="Sally Otto",])$ID)] <- 0

alb <- unlist(alllastbalance)
cat('Total balance= ',sum(alb),'\n')

hist(alb, breaks=20)


allnames <- lapply(seq_along(all.ID), function(i) people[match(all.ID[i],people$ID),]$Printed.Name)


plotname <- function(i){
  text(alb[i], 0, labels=allnames[i], srt=90, adj=0)
}

lapply(seq_along(all.ID), function(i) plotname(i))

sortedbi <- sort(alb, decreasing=FALSE, index.return=TRUE)$ix
cbind(alb[sortedbi], allnames[sortedbi])





