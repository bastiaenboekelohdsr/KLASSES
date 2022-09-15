# Bastiaen Boekelo, september 2022
# Doel: Exports van normentabel herstructureren voor automatische classificaties van monitoringsgegevens.
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

# sort_df: Putting a series of columns in front of the table
sort_df <- function(data, colnames){
  for(i in length(colnames):1){
    if(i == length(colnames)){
      new_df <- data[,c(which(colnames(data)==colnames[i]), which(colnames(data)!=colnames[i]))]
    } else {      new_df <- new_df[,c(which(colnames(new_df)==colnames[i]), which(colnames(new_df)!=colnames[i]))]    }}
  return(new_df)
}

# Creëer een functie die:
  # - Data inleest
  # - Zo voorberwerkt dat elke individuele range of waarde, met de bijbehorende beoordeling (e.g. 'Voldoet') een eigen rij toegekend krijgt
  # - Elke individuele classificatie is koppelbaar middels een ID_classificatie
  # - Afhankelijk van het soort classificatie (enkele range, dubbele range, extremen, exacte waardes) de classificatie eigenschappen definieert:
      # - Naam van de klasse / beoordeling (e.g. 'Voldoet')
      # - Bovengrens & ondergrens van de klasse  OF  enkel ondergrens  OF  enkel bovengrens
      # - Statement of de waarde zelf behoort tot de klasse 

# Input:
  # 1) Naam van de csv file 
  # 2) De kolommen waarbinnen de classificatiewaardes staan 
  # 3) Een hoge waarde die garant staat voor 

# Set van startnummers, te gebruiken voor unieke ID_nummers
IDs <- seq(100000000, 200000000, 100000)

# FILENAME <- "KRW maatlatten 2018 - Overige Waterflora"
# FILENAME <- "KRW maatlatten 2018 - Macrofauna"
# FILENAME <- "KRW maatlatten 2018 - Fytoplankton"
# FILENAME <- "KRW maatlatten 2018 - Vis"
# FILENAME <- "KRW fysisch-chemisch uit maatlatten 2018"
# 
# FILENAME <- "KRW overig - zoet"
# FILENAME <- "KRW prioritair - zoet"
# FILENAME <- "KRW prioritaire stoffen SGBP 2022-2027 - zoet"
# FILENAME <- "KRW spec. verontr. stoffen SGBP 2022-2027 - zoet"
# 
# UNIQIFIER       <- 3746584934746
# OORDEELKOLOMMEN <- oordeelkolommen_2

preprocess_classes_df <- function(FILENAME, OORDEELKOLOMMEN, UNIQIFIER){
  
  # Inlezen data
  df_raw                  <- read.csv(paste0("../Data/1_Input/KRW Waterkwaliteitsnormen Export 20220912/", FILENAME, ".csv"), stringsAsFactors = F, sep=";")
  df_raw$bron          <- FILENAME
  df_raw$ID_classificatie <- 1:nrow(df_raw) + UNIQIFIER
  df_raw                  <- sort_df(df_raw, c("ID_classificatie", "bron"))
  
  # Schrijf de opgeschoonde reference van de data weg
  df_clean                <- df_raw[,setdiff(names(df_raw),OORDEELKOLOMMEN)]
  write.csv(df_clean, paste0("../Data/2_Intermediate/01_", FILENAME, "__BESCHRIJVING.csv"), row.names = F)
  
  # Preprocessing
  melt_columns   <- setdiff(names(df_raw), OORDEELKOLOMMEN) # HOOFDSTAP: Hier worden de relevante kolommen 
  df             <- suppressWarnings(melt(df_raw, id = melt_columns))
  df             <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
  df$klasse      <- df$variable # variable generiek aangemaakt, we noemen het "klasse"
  df$variable    <- NULL 
  df$klasse      <- gsub("\\.", " ", df$klasse) # Weghalen puntjes uit kolomnamen in namen van de klasses
  
  df             <- df[!is.na(df$value),]
  df             <- df[!df$value == "",]
  
  
  #################### STEP 1) PREPROCESS BY GROUP ####################

  df_OF          <- df[df$value  %like% "OF",]          # GRP 1: Alle classificaties die bestaan uit 2 RANGES
  df_tmp         <- df[!df$value  %like% "OF",]         # TEMP: Alles wat NIET bestaat uit 2 RANGES
  df_EN_v1       <- df_tmp[df_tmp$value  %like% "EN",]  # GRP 2: Alle classificaties die bestaan uit 1 RANGE
  df_SINGLES     <- df_tmp[!df_tmp$value  %like% "EN",] # TEMP: Alle classificaties die bestaan uit 'range extremen en preciezen' 
  
  df_MAXS        <- df_SINGLES[str_detect(df_SINGLES$value, "^>"),] # GRP 3: Extreem hoog
  df_MINS        <- df_SINGLES[str_detect(df_SINGLES$value, "^<"),] # GRP 4: Extreem laag
  df_EXACTS      <- df_SINGLES[str_detect(df_SINGLES$value, "^="),] # GRP 5: Exacte waarde
  
  rm(df_tmp, df_SINGLES) # Weg met de TEMPs
  
  # GRP 1: Alle classificaties die bestaan uit 2 RANGES
  tmp1           <- as.data.frame(str_split_fixed(df_OF$value, " OF ", 2))
  tmp2  <- tmp3  <- df_OF
  tmp2$value     <- tmp1$V1
  tmp3$value     <- tmp1$V2
  df_EN_v2       <- rbind(tmp2, tmp3) # Nu df_OF geconverteerd naar enkele ranges
  rm(tmp1, tmp2, tmp3) # Weg met tijdelijke data
  
  # GRP 2: Alle classificaties die bestaan uit 1 RANGE
  df_EN          <- rbind(df_EN_v1, df_EN_v2) # meenemen van splits uit GRP 1
  rm(df_EN_v1, df_EN_v2) # Weg met de subdata
  
  if(nrow(df_EN) > 0){
    tmp            <- as.data.frame(str_split_fixed(df_EN$value, " EN ", 2))
    tmp$V1         <- gsub(" ", "", tmp$V1)
    tmp$V2         <- gsub(" ", "", tmp$V2)
    
    tmp$ondergrens <- parse_number(tmp$V1)
    tmp$bovengrens <- parse_number(tmp$V2)
    tmp$incl_onder <- "nee"
    tmp$incl_boven <- "nee"
    tmp$incl_onder[tmp$V1 %like% "="] <- "ja"
    tmp$incl_boven[tmp$V2 %like% "="] <- "ja"
    tmp[,c("V1", "V2")]               <- NULL
    
    df_EN_new           <- cbind(df_EN, tmp)
    rm(tmp)
  } else {
    df_EN_new           <- df_EN
  }
  
  # GRP 3: Extreem hoog
  if(nrow(df_MAXS) > 0){
    df_MAXS$ondergrens   <- parse_number(df_MAXS$value)
    df_MAXS$bovengrens   <- 1/0
    df_MAXS$incl_onder   <- "nee"
    df_MAXS$incl_onder[df_MAXS$value %like% "="] <- "ja"
    df_MAXS$incl_boven   <- "nee"
    unique(df_MAXS$bovengrens)
  }
  
  # GRP 4: Extreem laag
  if(nrow(df_MINS) > 0){
    df_MINS$ondergrens   <- -1/0
    df_MINS$bovengrens   <- parse_number(df_MINS$value)
    df_MINS$incl_onder   <- "nee"
    df_MINS$incl_boven   <- "nee"
    df_MINS$incl_boven[df_MINS$value %like% "="] <- "ja"
  }
  
  # GRP 5: Exacte waarde
  if(nrow(df_EXACTS) > 0){
    df_EXACTS$ondergrens <- parse_number(df_EXACTS$value)
    df_EXACTS$bovengrens <- parse_number(df_EXACTS$value)
    df_EXACTS$incl_onder <- "ja"
    df_EXACTS$incl_boven <- "ja"
  }
  
  
  #################### STEP 2) COMBINE ALL PREPROCESSED GROUPS ####################
  
  df_new    <- rbind(df_EN_new, df_MAXS, df_MINS, df_EXACTS)
  rm(df_EN, df_EN_new, df_EXACTS, df_MAXS, df_MINS, df_OF)
  
  col_sel   <- c("ID_classificatie", "bron", "klasse", "ondergrens", "bovengrens", "incl_onder", "incl_boven")
  df_new    <- df_new[,col_sel]
  
  #write.csv(df_new, paste0("../Data/2_Intermediate/01_", FILENAME, "__KLASSES.csv"), row.names = F)
  
  return(df_new)
}

# Run de functie
oordeelkolommen_1 <- c("Zeer.goed","Goed", "Matig", "Ontoereikend", "Slecht", "Referentie.slecht", "Referentie.goed")
oordeelkolommen_2 <- c("Voldoet", "Voldoet.niet")

# Preprocess elke tabel met de 
df1 <- preprocess_classes_df("KRW maatlatten 2018 - Overige Waterflora",         oordeelkolommen_1, IDs[1])
df2 <- preprocess_classes_df("KRW maatlatten 2018 - Macrofauna",                 oordeelkolommen_1, IDs[2])
df3 <- preprocess_classes_df("KRW maatlatten 2018 - Fytoplankton",               oordeelkolommen_1, IDs[3])
df4 <- preprocess_classes_df("KRW maatlatten 2018 - Vis",                        oordeelkolommen_1, IDs[4])
df5 <- preprocess_classes_df("KRW fysisch-chemisch uit maatlatten 2018",         oordeelkolommen_1, IDs[5])
df6 <- preprocess_classes_df("KRW overig - zoet",                                oordeelkolommen_2, IDs[6])
df7 <- preprocess_classes_df("KRW prioritair - zoet",                            oordeelkolommen_2, IDs[7])
df8 <- preprocess_classes_df("KRW prioritaire stoffen SGBP 2022-2027 - zoet",    oordeelkolommen_2, IDs[8])
df9 <- preprocess_classes_df("KRW spec. verontr. stoffen SGBP 2022-2027 - zoet", oordeelkolommen_2, IDs[9])

ID_klasses_alles <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9)
ID_klasses_alles <- ID_klasses_alles[order(ID_klasses_alles$ID_classificatie),]

write.csv(ID_klasses_alles, "../Data/2_Intermediate/01_KRW_ALLE_CLASSIFICATIES_MET_KOPPELING.csv", row.names = F)


