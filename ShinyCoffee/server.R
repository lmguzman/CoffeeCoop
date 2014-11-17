
## load the data
a_active <- read.csv("../SignupSheet/accounts_active.csv", stringsAsFactors = FALSE)

shinyServer(function(input, output){
  output$a_active <- renderTable({a_active})
})