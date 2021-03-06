template <-
    c("\\begin{longtable}{|l|r|c|}\\hline",
      "\\textbf{Name} & \\textbf{Balance} & \\textbf{Gone? / Contact info?}\\endhead\\hline",
      "%s",
      "\\end{longtable}")

#   writeLines(sprintf(paste(template, collapse="\n"),
#                      paste(str, collapse="\n"),
#                      paste(rep("&&&\\\\\\hline", 20), collapse="\n")),
#              "../signup/table-coffee.tex")
#   system("cd ../signup; pdflatex signup-coffee-new.tex")

printoutputtoday <- function(coopid){
  tmptable <- doaccountbypeople(coopid)[nd,]
  if(people[match(coopid, people$ID),]$Printed.Name == "Sally Otto") tmptable$Balance <- 0
 # tmptable$Balance <- format(tmptable$Balance, digits=2, nsmall=2)

  sprintf(#
    paste(template, collapse='\n'), #
    toString(people[match(coopid, people$ID),]$Printed.Name), # Displays Name
#    toString(people[match(coopid, people$ID),]$ID), # Displays Coop ID
    paste(apply(tmptable, 1, paste, collapse="  "), collapse="\\\\ \n")
  )
  tmp <- cbind(people[match(coopid, people$ID),]$Printed.Name,
               sprintf(ifelse(tmptable$Balance < 0, "\\textbf{-\\$%2.2f}",
                              "\\$%2.2f"),
                       abs(tmptable$Balance)),
               "\\\\\\hline")
  str <- paste(apply(tmp, 1, paste, collapse=" & "), collapse="\n")
}

all.ID.notgone <- all.ID[!(people[match(all.ID, people$ID),]$Gone)]

strtable <- paste(sapply(all.ID[sortedbi], printoutputtoday), collapse='\n')

  writeLines(sprintf(paste(template, collapse="\n"),
                     strtable),
             "signup/shamedata.tex")
  system("cd signup; pdflatex shamelist.tex")


# writeLines(paste(sapply(all.ID, printoutput), collapse='\n\n'), 'tex/essai.tex')
# 
# system('cd tex/; pdflatex alldata.tex')
