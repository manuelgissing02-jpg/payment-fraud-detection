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
