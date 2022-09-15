# Bastiaen Boekelo, september 2022
# Doel: Classificaties van de STOWA bedekkingspercentages licht voorbewerken zodat het aansluit op het standaard format (zie 2_Intermediate)
# Generiek project

# Setup omgeving
#########################################################################################################

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Works if you have a recent version of RStudio.

# Libraries
library(reshape)
library(data.table)
library(stringr)
library(readr) # for parse_number

filename <- "KRW_Deelmaatlatten_Sloten_en_Kanalen_bedekkingspercentages_STOWA_2021-2027"
df_raw   <- read.csv(paste0("../Data/1_Input/STOWA Bedekkingspercentages/", filename, ".csv"), stringsAsFactors = F, sep=";", fileEncoding="UTF-8-BOM")
ID_REF   <- read.csv(paste0("../Data/4_Hulpbestanden/Unieke_IDs_per_input.csv"), stringsAsFactors = F, sep=";", fileEncoding="UTF-8-BOM")
ID_START <- ID_REF$seq_start[ID_REF$input_file == filename]

df       <- df_raw
# AFMAKEN AFMAKEN


df$unique <- paste0(df$Watertype, "_", df$Groeivorm)
df$test <- as.numeric(as.factor(as.character(df$unique)))
unique(df$unique)




