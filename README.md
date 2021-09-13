# Shiny-App-for-Climate-Change
This is a simple Shiny app that displays some information about climate change in Canada with CO2

library(shiny)
library(ggplot2)
library(dplyr)

(load("C02NorthernHemisphere.Rdata"))
(load("C02Worldwide.Rdata"))
(load("CanadianAvgSnow.Rdata"))
(load("CanadianMeanTemp.Rdata"))
(load("CanadianMaxTemp.Rdata"))
(load("CanadianMinTemp.Rdata"))
(load("CanadianPrecip.Rdata"))

"Co2North"
"Co2World"
"AllSnow"
"AllPrecip"
"MaxTemp"
"MeanTemp"
"MinTemp"


MinTemp1 <- MinTemp[,-(2:13)]
MeanTemp1 <- MeanTemp[,-(2:13)]
MaxTemp1 <- MaxTemp[,-(2:13)]

MeanTemp2 <- na.omit(replace(MeanTemp1, MeanTemp1=="-9999.9", NA))
MaxTemp2 <- na.omit(replace(MaxTemp1, MaxTemp1=="-9999.9", NA))
MinTemp2 <- na.omit(replace(MinTemp1, MinTemp1=="-9999.9", NA))

Allsnow <- na.omit(replace(AllSnow, AllSnow=="-9999.9", NA))
Co2North2 <- na.omit(replace(Co2North, Co2North=="-9999.9", NA))
AllPrecip2 <- na.omit(replace(AllPrecip, AllPrecip=="-9999.9", NA))



MeanCo2_44to90_Y_M <- Co2North %>%
  mutate(Year=as.integer(substr(YearDecimal,1,4)), Month=floor((YearDecimal-Year)*12)+1) %>%
  group_by(Year,Month) %>%
  summarise(mean_Co2_44to90 = mean(c(Latitude44value,Latitude49value,Latitude44value,Latitude49value,Latitude53value,Latitude58value,Latitude64value,Latitude72value,Latitude90value)))


CanadaMeanTemp_Y_M <- MeanTemp %>%
  group_by(Year) %>%
  summarize(Annual=mean(Annual),Jan=mean(Jan),Feb=mean(Feb),Mar=mean(Mar),Apr=mean(Apr),May=mean(May),Jun=mean(Jun),Jul=mean(Jul),Aug=mean(Jul),Sep=mean(Sep),Oct=mean(Oct),Nov=mean(Nov), Dec=mean(Dec))
CanadaMeanTemp_Y_M

CanadaMinTemp_Y_M <- MinTemp %>%
  group_by(Year) %>%
  summarize(Annual=mean(Annual),Jan=mean(Jan),Feb=mean(Feb),Mar=mean(Mar),Apr=mean(Apr),May=mean(May),Jun=mean(Jun),Jul=mean(Jul),Aug=mean(Jul),Sep=mean(Sep),Oct=mean(Oct),Nov=mean(Nov), Dec=mean(Dec))
CanadaMinTemp_Y_M

CanadaMaxTemp_Y_M <- MaxTemp %>%
  group_by(Year) %>%
  summarize(Annual=mean(Annual),Jan=mean(Jan),Feb=mean(Feb),Mar=mean(Mar),Apr=mean(Apr),May=mean(May),Jun=mean(Jun),Jul=mean(Jul),Aug=mean(Jul),Sep=mean(Sep),Oct=mean(Oct),Nov=mean(Nov), Dec=mean(Dec))
CanadaMeanTemp_Y_M


Min<- MinTemp2 %>%
  group_by(Year) %>%
  summarize(AnnMin=mean(Annual),WinMin=mean(Winter),SprMin=mean(Spring),SumMin=mean(Summer))
head(Min) 

Max<- MaxTemp2 %>%
  group_by(Year) %>%
  summarize(AnnMax=mean(Annual),WinMax=mean(Winter),SprMax=mean(Spring),SumMax=mean(Summer))

Mean<- MeanTemp2 %>%
  group_by(Year) %>%
  summarize(AnnMean=mean(Annual),WinMean=mean(Winter),SprMean=mean(Spring),SumMean=mean(Summer))



c=inner_join(Max,Min,by="Year")
Temp=inner_join(Mean,c,by="Year")
head(Temp)






function(input, output) {
  
  dataset <- reactive({
    Co2World[sample(nrow(Co2World), input$sampleSize),]
  })
  
  
  
  output$plot1 <- renderPlot({
    
    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
    
    if (input$color != 'None')
      {p <- p + aes_string(color=input$color)}
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
 
    print(p)
    
    x = MeanTemp[,"Annual"]
    if(input$DensityLogical){
      if(input$BoundaryCorrect ){  
        xuse = c(-x,x)
        Dens = density(xuse)
        Dens$y = Dens$y*2
      }else{
        Dens = density(x)
      }
      lines(Dens)
    }
  })
    
  
  
  
  
  
  output$plot2 <- renderPlot({
    
    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
    
    if (input$color != 'None')
    {p <- p + aes_string(color=input$color)}
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    

    print(p)
    
    x = MinTemp[,"Annual"]
    if(input$DensityLogical){
      if(input$BoundaryCorrect ){  
        xuse = c(-x,x)
        Dens = density(xuse)
        Dens$y = Dens$y*2
      }else{
        Dens = density(x)
      }
      lines(Dens)
    }
  })
    
  
  
  
  
  
  output$plot3 <- renderPlot({
    
    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
    
    if (input$color != 'None')
    {p <- p + aes_string(color=input$color)}
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
 
    print(p)

    x = MaxTemp[,"Annual"]
    if(input$DensityLogical){
      if(input$BoundaryCorrect ){  
        xuse = c(-x,x)
        Dens = density(xuse)
        Dens$y = Dens$y*2
      }else{
        Dens = density(x)
      }
      lines(Dens)
    }
  })
 
}

