library(tidyverse)
library(DT)
library(shinydashboard)
library(shinyFiles)
library(shinyjs)
library(shinymanager)
library(reactR)
library(listviewer)
library(rjson)



load_dataset <- function(input_path){
  my_df <- read_csv(input_path) %>%
    rename(Symbol=X1) %>% as.data.frame()
  rownames(my_df) <- my_df$Symbol
  my_df[,-1]
}

load_coldata <- function(input_path){
  read_csv(input_path)
}
####################################
# Server: Tab 0 Data Upload
####################################
INFO_STR <- "Please Select the dataset folder and then click Upload data."
# my_data <- load_dataset("dataset/counts.csv")
# my_data_norm <- load_dataset("dataset/norm-counts.csv")
# my_coldata <- load_coldata("dataset/design_tbl.csv")
# my_corner_stone <- read_csv("dataset/corner_stone.csv", col_names=FALSE)[["X1"]]
