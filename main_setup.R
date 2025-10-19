# Project Setup
#aktivieren der Packages für Daten einlesen und bereinigen
#Bitte nicht vergessen die packages unter Tools vorab zu installieren !
library(readr)
library(dplyr)
library(tidyr)
library(skimr)

datensatz <- read.csv("C:/Github/payment-fraud-detection/Data/fraud_dataset_fhj25.csv")

head((datensatz))

skim(datensatz)


##check ob gleiche Datensätze vorhanden sind
any(duplicated(datensatz))


##Spalten unbenennen

colnames(datensatz) <- c("date", "age", "profession","region", "account_balance", "num_debit","num_credit","num_pos","num_mov_conto","incoming payments","outgoing payments","product number","num_fraud")



##2627 zeilen wurden gelöscht, schaupsi wann gehma bier trinken?
datensatz <- datensatz %>%
  filter(!(num_debit == 0 & 
             num_credit == 0 & 
             num_pos == 0 & 
             num_fraud == 0))


##Fokussierung u. sortieren der incoming payment spalte, Thomas is geil
##Große Transaktionen sind wichtig

df_sorted <- datensatz %>%
  arrange(desc(`incoming payments`))

