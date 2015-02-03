library(dplyr)
## load the data
a_active <- read.csv("../SignupSheet/accounts_active.csv", stringsAsFactors = FALSE)

shinyServer(function(input, output){
  output$a_active <- renderTable({a_active %>% 
                                  #  filter(Printed.Name == input$select_name) %>% 
                                    filter(balance > input$balance_range[[1]])
                                  })
  output$output_person <- renderText({cat(input$balance_range)
                                      #cat(str(input$balance_range))
                                      paste("searching for", input$select_name)})
})