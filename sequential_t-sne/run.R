library("rsconnect")


#setwd("~/Documents/Programming/Prochownik/SeqRShiny/app_attempt2")
source("main.R")
source("helper_functions.R")



rsconnect::setAccountInfo(name='chpupsom19',token='04D9E130FCBEFD71BBF550C31D3CDF67',secret='6Y2jq88rh3tiUNXQ7yUMP6/H34N5Y56M4VYfUBYx')

shiny::runApp()
#deployApp(appTitle = "Sequential_t-SNE")