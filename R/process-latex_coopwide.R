tabletemplate <-
    c('\\section{%s}\n', #
"\\begin{center}\n\\begin{tabular}{cccccccc}",
      "\\textbf{Date} & \\textbf{Milk} & \\textbf{Coffee} & \\textbf{Tea} & \\textbf{Payment} & \\textbf{Payment Date} & \\textbf{InOuts} & \\textbf{Balance} \\\\",
      "%s",
      "\\end{tabular}\n\\end{center}")



printoutput <- function(coopid){
  print(coopid)
  tmpsec <- paste('\\section*{', toString(people[match(coopid, people$ID),]$Printed.Name), '}\n', sep='')
  
  dtmp <- doaccountbypeople(coopid)
  tmptable <- subset(dtmp, select=(names(dtmp)!="Milk"))
  tmptable$HasMilk <- ifelse(tmptable$HasMilk, "yes", "no")
  tmptable$HasMilk[info$CostMilk<0.01] <- ""

  emptylines <- (tmptable$Coffee==0 & tmptable$Tea==0 & tmptable$Payment==0)
  tmptable <- tmptable[!emptylines,]

  tmptable$Payment <- format(tmptable$Payment, digits=2, nsmall=2)
  tmptable$InOut <- format(tmptable$InOut, digits=2, nsmall=2)
  tmptable$Balance <- format(tmptable$Balance, digits=2, nsmall=2)

  if(people[match(coopid, people$ID),]$Printed.Name == "Sally Otto") tmptable$Balance <- 0
  sprintf(#
    paste(tabletemplate, collapse='\n'), #
    toString(people[match(coopid, people$ID),]$Printed.Name), # Displays Name
#    toString(people[match(coopid, people$ID),]$ID), # Displays Coop ID
    paste(apply(tmptable, 1, paste, collapse=" & "), collapse="\\\\ \n")
  )
}

writeLines(paste(sapply(all.ID, printoutput), collapse='\n\n'), 'tex/essai.tex')

system('cd tex/; pdflatex alldata.tex')
