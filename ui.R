(load("C02NorthernHemisphere.Rdata"))
(load("C02Worldwide.Rdata"))
(load("CanadianAvgSnow.Rdata"))
(load("CanadianMeanTemp.Rdata"))
(load("CanadianMaxTemp.Rdata"))
(load("CanadianMinTemp.Rdata"))
(load("CanadianPrecip.Rdata"))

library(shiny)
library(ggplot2)


dataset <- Co2World



shinyUI(fluidPage(
 
  titlePanel("Canada weather changes"),
  
  sidebarPanel(
    sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
                value=min(500, nrow(dataset)), step=50, round=0),
    
    
    selectInput('x', 'X', names(dataset),names(dataset)[[1]]),
    selectInput('y', 'Y', names(dataset), names(dataset)[[5]]),
    selectInput('color', 'Color', c('None', names(dataset)[-c(1,5)])),
    
  
    selectInput('facet_row', 'Row', c(None='.', names(dataset))),
    selectInput('facet_col', 'Column', c(None='.', names(dataset)))
    
    
  ),
  
    checkboxInput("DensityLogical", strong("Add density plot?"), FALSE),
  
  
    conditionalPanel(
      condition = "input.DensityLogical == true",   
      helpText(HTML("<h3>You might want to adjust the boundary estimate</h3>")),  
      checkboxInput("BoundaryCorrect", strong("Correct the density plot at zero?"), FALSE)  
    ), 
  
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Dynamic Plot", fluidRow(column(12,plotOutput('plot1')),
                                                    column(12,plotOutput('plot2')))),
                  tabPanel("relationship between temp and co2", plotOutput('plot3')),
                  tabPanel("About Me",source("about.R")$value())
      ),
      h1("Reference:"),
      p("  YDlugokencky, E.J., K.W. Thoning, P.M. Lang, and P.P. Tans (2017), NOAA Greenhouse Gas Reference from Atmospheric Carbon Dioxide Dry Air Mole Fractions from the NOAA ESRL Carbon Cycle Cooperative Global Air Sampling Network.
        Mekis, É. and L.A. Vincent, 2011: An overview of the second generation adjusted daily precipitation dataset for trend analysis in Canada. Atmosphere-Ocean, 49(2), 163-177.
        Vincent, L. A., X. L. Wang, E. J. Milewska, H. Wan, F. Yang, and V. Swail, 2012. A second generation of homogenized Canadian monthly surface air temperature for climate trend analysis, J. Geophys. Res., 117, D18110, doi:10.1029/2012JD017859.
        Wan, H., X. L. Wang, V. R. Swail, 2010: Homogenization and trend analysis of Canadian near-surface wind speeds. Journal of Climate, 23, 1209-1225.  
        Wan, H., X. L. Wang, V. R. Swail, 2007: A quality assurance system for Canadian hourly pressure data. J. Appl. Meteor. Climatol., 46, 1804-1817.
        Wang, X.L, Y. Feng, L. A. Vincent, 2013. Observed changes in one-in-20 year extremes of Canadian surface air temperatures. Atmosphere-Ocean. Doi:10.1080/07055900.2013.818526.
        ")
      )
   
))

    
