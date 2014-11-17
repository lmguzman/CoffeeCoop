shinyUI(fluidPage(
  titlePanel("BRC coffeh"),
  
  sidebarLayout(
    sidebarPanel("User inputs will be here",
                 selectInput("select_Printed.Name",
                             label = "Name",
                             choices = list("Ada Roman", "Alathea LeTaw"))
                 ),
    mainPanel("My cool graphs will go here",
              tableOutput("a_active")
              )
    )
  )
  )