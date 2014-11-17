accts <- read.csv("../SignupSheet/accounts_active.csv", stringsAsFactors = FALSE)

shinyUI(fluidPage(
  titlePanel("BRC coffeh"),
  
  sidebarLayout(
    sidebarPanel("User inputs will be here",
                 selectInput("select_name",
                             label = "Name",
                             choices = unique(accts$Printed.Name)),
                 sliderInput("balance_range",
                             label = "the balances",
                             min = -20,
                             max = 40,
                             value = c(-20, 40),
                             format = "##.##")
                 ),
    mainPanel("My cool graphs will go here",
              textOutput("output_person"),
              tableOutput("a_active")
              )
    )
  )
  )