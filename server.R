library(shiny)
library(shinydashboard)

library(fda)

load("Temp.rda")

res = function(name){
   
   if (name == "Sydney(Observatory Hill)"){
      out = list(FF = FF.S, p=p.S, fpca = fpca.S, barp = barpS, delta = deltaS)
   }
   
   if (name == "Melbourne(Regional Office)"){
      out = list(FF = FF.M, p=p.M, fpca = fpca.M, barp = barpM, delta = deltaM)
   }
   
   if (name == "Boulia Airport"){
      out = list(FF = FF.B, p=p.B, fpca = fpca.B, barp = barpB, delta = deltaB)
   }
   
   if (name == "Cape Otway Lighthouse"){
      out = list(FF = FF.C, p=p.C, fpca = fpca.C, barp = barpC, delta = deltaC)
   }
   
   if (name == "Gayndah Post Office"){
      out = list(FF = FF.Ga, p=p.Ga, fpca = fpca.Ga, barp = barpGa, delta = deltaGa)
   }
   
   if (name == "Gunnedah Pool"){
      out = list(FF = FF.Gu, p=p.Gu, fpca = fpca.Gu, barp = barpGu, delta = deltaGu)
   }
   
   if (name == "Hobart(Ellerslie Road)"){
      out = list(FF = FF.H, p=p.H, fpca = fpca.H, barp = barpH, delta = deltaH)
   }
   
   if (name == "Robe Comparison"){
      out = list(FF = FF.R, p=p.R, fpca = fpca.R, barp = barpR, delta = deltaR)
   }
   
   return(out)
   
}

server <- function(input, output) {
   set.seed(122)
   histdata <- rnorm(500)
   
   output$plotN <- renderPlot({
      data <- histdata[seq_len(input$slider)]
      hist(data)
   })
   
   # Return the requested dataset
   datasetInput <- reactive({
      switch(input$dataset,
             "Sydney(Observatory Hill)" = fdata_S,
             "Melbourne(Regional Office)" = fdata_M,
             "Boulia Airport" = fdata_B, 
             "Cape Otway Lighthouse" = fdata_C, 
             "Gayndah Post Office" = fdata_Ga, 
             "Gunnedah Pool" = fdata_Gu, 
             "Hobart(Ellerslie Road)" = fdata_H,
             "Robe Comparison" = fdata_R)
   })
   
   # Show the first "n" observations
   output$plot <- renderPlot({
      plot(datasetInput(), main=input$dataset)
   })
   
   output$ff <- renderTable({
      res(name = input$dataset)$FF
   })
   
   output$fpca <- renderTable({
      res(name = input$dataset)$fpca[1:12,]
   })
   
   output$plot2 <- renderPlot({
      b = res(name = input$dataset)$barp
      barplot(b, main = "Change Explained by Eigenfunctions", 
              names=1:10, xlab="eigenfunction", ylab="explained change (%)")
   })
   
   output$plot3 <- renderPlot({
      b = res(name = input$dataset)$delta
      plot(b, main="Estimated Change Function")
   })
   
}