allppl <- lapply(all.ID, doaccountbypeople)

lastbalance <- function(index){
  ((allppl[[index]])$Balance)[nd]
}

alllastbalance <- lapply(seq_along(all.ID), lastbalance)
alllastbalance[which(all.ID==(people[people$Name=="Sally Otto",])$ID)] <- 0

alb <- unlist(alllastbalance)
cat('Total balance= ',sum(alb),'\n')

hist(alb, breaks=20)


