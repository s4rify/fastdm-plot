library(shiny)
# ui = user interface script

# fluidpage is a function that leads to a window which is adapting to
# different display sizes
shinyUI(fluidPage(

  # add content by adding it to a panel function
  # more advanced features by adding html tags
  titlePanel(
    # add widgets to a panel with which the user can intract
    fluidRow(
      column(3,
        h3("button"),
        actionButton("a", label = "button1"),
        br(),
        submitButton("submit"),
        br(),
        checkboxInput("input", label = "option A", value = FALSE)
      )
    )
  ),
  
  sidebarLayout( #position = "right",
    sidebarPanel(
      # images must be in the www folder inside the app folder
      img(src = "heart.jpg", height = 400, width = 400)
    ),
    mainPanel(
      h6("Space. The final frontier.", align = "center"),
      h6("These are the stories", align = "center"),
      h5("of the spaceship ENTERPRISE!", align = "center"),
      h4("Its five year mission", align = "center"),
      h3("to explore strange new worlds.", align = "center"),
      h2("To boldly go", align = "center"),
      h1("where no man has gone before!", align = "center")
      )
  )
))  