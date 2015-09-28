shinyUI(pageWithSidebar(
  headerPanel("Power Law Analysis"),
  sidebarPanel(
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    tags$hr(),
    checkboxInput('header', 'Header', FALSE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab=' '),
                 'Comma'),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 'None')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Table",textOutput("Vertices"),textOutput("Edges"),tableOutput("Contents")),
      tabPanel("Network Plot", plotOutput("dcompplot")),
      tabPanel("PowerLaw Plot", plotOutput("ltplot")),
      tabPanel("log-log Plot", plotOutput("plplot"))
      )
  )
   
))
