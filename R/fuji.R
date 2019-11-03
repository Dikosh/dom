#' @title Predicts Stock Price Movement for Given Stock Symbol
#'
#' @description This package predicts whether the stock price at tommorow's market close would be higher or lower compared to today's closing place.
#'
#' @param symbol
#'
#' @return NULL
#'
#' @examples  stock_predict('AAPL')
#'
#' @export fuji

#' @import data.table,dplyr,data.table,randomForest,rpart,plotly,readr,readxl,xgboost,Matrix,caret,splitstackshape,stringr,lubridate

fuji <- function(df){
  #library(devtools)
  #setwd("~/Desktop/Job/domilion/")

  df <- df[is.na(df$avtosalon),]
  df <- df[is.na(df$new_avaria_nahodu),]
  df <- df[df$rastamozhen!="Нет",]
  df <- df[is.na(df$V_nalichii),]
  df <- select(df,-c(V_nalichii,vin,avtosalon,new_avaria_nahodu,ne_ispolzuetsa))
  df$probeg <-  substr(df$probeg,1,nchar(df$probeg)-3)
  df$probeg <- as.integer(gsub(" ","",df$probeg))
  min <- 1900
  max <- 390000
  df$to_delete <- ifelse(is.na(df$probeg),55555,df$probeg)
  df <- df[df$to_delete>=min,]
  df <- df[df$to_delete<=max,]
  df$to_delete <- NULL
  df <- df[!is.na(df$Marka),]
  df <- df[!is.na(df$Model),]
  df$PID <- 1:nrow(df)

  return(df)
}

