shinyServer(function(input, output) {
  
  output$Vertices <- renderText({
    library(igraph)
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    rd <-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    rg <- graph.data.frame(rd)
    paste("The Graph contains ", vcount(rg), "Vertices")
  })
  
  output$Edges <- renderText({
    library(igraph)
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    rd <-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    rg <- graph.data.frame(rd)
    paste("The Graph contains ", ecount(rg), "Edges")
  })
  
  output$Contents <- renderTable({
    
  
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    rd <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    
  })
  output$dcompplot <- renderPlot({
    library(igraph)
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    rd <-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    rg <- graph.data.frame(rd)
    plot(rg, vertex.label=NA, layout=layout.auto, vertex.size=5, edge.width=1, edge.arrow.size=0.1)
  })  
  output$plplot <- renderPlot({
    library(igraph)
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    rd <-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    rg <- graph.data.frame(rd)
    d = degree(rg, mode = "all")
    dd = degree.distribution(rg, mode = "all", cumulative = FALSE)
    degree = 1:max(d)
    probability = dd[-1]
   
    nonzero.position = which(probability != 0)
    probability = probability[nonzero.position]
    degree = degree[nonzero.position]
    reg = lm(log(probability) ~ log(degree))
    cozf = coef(reg)
    power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
    alpha = -cozf[[2]]
    R.square = summary(reg)$r.squared
    print(paste("Alpha =", round(alpha, 3)))
    print(paste("R square =", round(R.square, 3)))
 
    plot(probability ~ degree, log = "xy", xlab = "Degree (log)", ylab = "Probability (log)", 
         col = 1, main = "Degree Distribution")
   curve(power.law.fit, col = "red", add = T, n = length(d))
  })  
  output$ltplot <- renderPlot({
    library(igraph)
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    rd1 <-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    rg1 <- graph.data.frame(rd1)
    d1 = degree(rg1, mode = "all")
    dd1 = degree.distribution(rg1, mode = "all", cumulative = FALSE)
    power.law.fit(dd1)
    plot(dd1,xlab="Projects", ylab="Developers")
    curve(power.law.fit, col = "red", add = T, n = length(d1) )
    
  })
})
