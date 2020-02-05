# import data
setwd("C:/git/epoc/data")

epoc2017 <- read.table("data_2017.txt", sep="\t",quote="")
epoc2018 <- read.table("data_2018.txt",header=T, sep="\t",quote="")

epoc_2019a <- read.csv("export_data_liste_mars_avril_mai_2019.csv",sep=";",dec=".")
epoc_2019b <- read.csv("export_data_liste_juin_juillet_2019.csv",sep=";",dec=".")































