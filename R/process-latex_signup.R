template <-
    c("\\begin{longtable}{|l|c|r|p{9cm}|p{5cm}|}\\hline",
      "\\textbf{Name} & \\textbf{No milk} & \\textbf{Balance}",
      "& \\textbf{Coffee}\\hspace{6cm} & \\textbf{Tea}\\endhead\\hline",
      "%s",
      "\\multicolumn{4}{l}{}\\\\[15ex]",
      "\\multicolumn{4}{l}{\\textbf{New members:}}\\\\[1ex]\\hline",
      "\\textbf{Name}&\\textbf{No milk}&&\\textbf{Coffee} & \\textbf{Tea} \\\\\\hline",
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
    paste(apply(tmptable, 1, paste, collapse=" & "), collapse="\\\\ \n")
  )
  tmp <- cbind(people[match(coopid, people$ID),]$Printed.Name,
               ifelse(!tmptable$Milk, "\\checkmark", ""),
               sprintf(ifelse(tmptable$Balance < 0, "\\textbf{-\\$%2.2f}",
                              "\\$%2.2f"),
                       abs(tmptable$Balance)),
               "& \\\\\\hline")
  str <- paste(apply(tmp, 1, paste, collapse=" & "), collapse="\n")
}

all.ID.notgone <- all.ID[!(people[match(all.ID, people$ID),]$Gone)]

strtable <- paste(sapply(all.ID.notgone, printoutputtoday), collapse='\n')

  writeLines(sprintf(paste(template, collapse="\n"),
                     strtable,
                     paste(rep("&&&\\\\\\hline", 10), collapse="\n")),
             "signup/table-coffee.tex")
  system("cd signup; pdflatex signup-coffee-new.tex")


# writeLines(paste(sapply(all.ID, printoutput), collapse='\n\n'), 'tex/essai.tex')
# 
# system('cd tex/; pdflatex alldata.tex')
