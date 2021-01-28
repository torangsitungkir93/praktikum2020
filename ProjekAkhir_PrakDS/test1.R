library(shiny)
library(shinydashboard)
library(vroom)
library(here)

  
  cleanedKata= vroom(here("raw-data","cleanedData.csv"))
  cleanedKata= data.frame(cleanedKata)
  
  cuitan = vroom(here("raw-data","cuitan.csv"))
  cuitan = data.frame(cuitan)
  
  
  