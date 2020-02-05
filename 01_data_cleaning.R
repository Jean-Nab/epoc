# import data
  setwd("C:/git/epoc/data")
  
  epoc2017 <- read.table("data_2017.txt",header = T,skip=1, sep="\t",quote="")
  epoc2018 <- read.table("data_2018.txt",header=T,skip=1, sep="\t",quote="",encoding="UTF-8") # vs pb d'accent
  
  epoc2019a <- read.csv("export_data_liste_mars_avril_mai_2019.csv",sep=";",dec=".",encoding="UTF-8")
  epoc2019b <- read.csv("export_data_liste_juin_juillet_2019.csv",sep=";",dec=".",encoding="UTF-8")
  
# homogeneisation data ----
    # Travaux preleminaires (chgt de noms de colonnes, raccords de nom de variables entre les differents tableaux)
        # work sur data format (.csv) ----
        # Changement du nom de la 1er colonne des formats csv : "X.U.FEFF.Ref" --> "Ref"
          nam2019a <- colnames(epoc2019a)
          
          colnames(epoc2019a)[1]<- colnames(epoc2017)[1]
          colnames(epoc2019b)[1]<- colnames(epoc2017)[1]
      
      # Work sur data format (.txt) ----
        nam2017 <- colnames(epoc2017)
        nam2018 <- colnames(epoc2018)
        # Cas 2018 (- de changements)
    
  # selection des variables a garder (a partir des epoc 2019)
  '
    var <- c("Ref","ID.Espèce.Biolovision", "Nom.espèce","Date","Jour","Mois","Année","Jour.de.l.année",
             "Décade","numéro.de.la.semaine","Horaire","ID.liste","Heure.début","Heure.de.début","Minute.de.début",
             "Heure.fin","Heure.de.fin","Minute.de.fin","Liste.complète..","Commentaire.de.la.liste","ID.Lieu.dit",
             "Lieu.dit","Commune","Département","Code.INSEE","Type.de.localisation","Lat..WGS84.",
             "Lon..WGS84.","Maille","Altitude","Estimation","Nombre","Détails","Code.atlas","Protocole","Remarque",
             "Remarque.privée","Nom","Prénom")
    var1 <- c("Ref","ID.Espèce.Biolovision", "Nom.espèce","Date","Jour","Mois","Année","Jour.de.l.année",
                         "Décade","numéro.de.la.semaine","Horaire","ID.liste","Heure.début","Heure.de.début","Minute.de.début",
                         "Heure.fin","Heure.de.fin","Minute.de.fin","Liste.complète..","Commentaire.de.la.liste","ID.Lieu.dit",
                         "Lieu.dit","Commune","Département","Code.INSEE","Type.de.localisation","Lat..WGS84.",
                         "Lon..WGS84.","Maille","Altitude","Estimation","Nombre","Détails","Code.atlas","Protocole","Remarque",
                         "Remarque.privée") # temporaire : retrait des nom/prenoms
   '
  # On procede par un match pour associer les variables identiques des 2 tableaux
      var2018 <- match(colnames(epoc2018),colnames(epoc2019a))
      var2018 <- na.omit((var2018)) # position des variables de 2018 concordant avec variables de 2019
      var2018 <- colnames(epoc2019a)[var2018] # nom des variables identiques dans les 2 tableaux
      
      var2017 <- match(colnames(epoc2017),colnames(epoc2019a))
      var2017 <- na.omit((var2017)) # position des variables de 2018 concordant avec variables de 2019
      var2017 <- colnames(epoc2019a)[var2017]
  
  
    

        
    
  # merge des datasets
    epoc2018.bis <- epoc2018[,var2018]
    epoc2019a.bis <- epoc2019a[,var2018]
    epoc2018_2019 <- rbind(epoc2018.bis,epoc2019a.bis)
    #epoc2018_2019 <- epoc2018_2019[1:1000,] # a retirer par la suite
    
  # nettoyage du tableau unifie (i.e : retrait des accents / gestion des caracteres speciaux [?, ])
    # gsub sur nom des colonnes
      colnames(epoc2018_2019) <- gsub("è","e",colnames(epoc2018_2019))
      colnames(epoc2018_2019) <- gsub("é","e",colnames(epoc2018_2019))
      colnames(epoc2018_2019) <- gsub("\\.\\.","",colnames(epoc2018_2019))
      colnames(epoc2018_2019) <- gsub("\\.","_",colnames(epoc2018_2019))
    # gsub par variables posant problèmes
      # Nom_espece
        epoc2018_2019[,"Nom_espece"] <- as.character(gsub("é","e",epoc2018_2019[,"Nom_espece"]))
        epoc2018_2019[,"Nom_espece"] <- as.character(gsub("è","e",epoc2018_2019[,"Nom_espece"]))
        epoc2018_2019[,"Nom_espece"] <- as.character(gsub("ê","e",epoc2018_2019[,"Nom_espece"]))
        epoc2018_2019[,"Nom_espece"] <- as.character(gsub("ï","i",epoc2018_2019[,"Nom_espece"]))
        epoc2018_2019[,"Nom_espece"] <- as.character(gsub(" d'","_",epoc2018_2019[,"Nom_espece"]))
        epoc2018_2019[,"Nom_espece"] <- as.character(gsub(" ","_",epoc2018_2019[,"Nom_espece"]))
        
        
      # Nom / Prenom
      


# testing ground ----
    head(epoc2017,3)
    
    j <- which(colnames(epoc2018) == colnames(epoc2019a),arr.ind = TRUE)
    u <- match(colnames(epoc2018),colnames(epoc2019a))
    u <- na.omit((u)) # position des variables de 2018 concordant avec variables de 2019
    colnames(epoc2019a)[u]
    

    
    test <- epoc2018[,var1[1:5]]
    test <- epoc2018[,c("Ref","ID.Espèce.Biolovision", "Nom.espèce","Date","Jour")]


    # detection des caracteres speciaux 
      o <- grep("ê", epoc2019a.bis[,"Nom.espèce"]) # 66688 occurrences
      o <- grep("ë", epoc2019a.bis[,"Nom.espèce"]) # 0 occurrences
      o <- grep("ï", epoc2019a.bis[,"Nom.espèce"]) # 3843 occurrences
      o <- grep("o", epoc2019a.bis[,"Nom.espèce"]) # 0 occurrences
      o <- grep("ö", epoc2019a.bis[,"Nom.espèce"]) # 0
      o <- grep("ô", epoc2019a.bis[,"Nom.espèce"]) # 0
      o <- grep("û", epoc2019a.bis[,"Nom.espèce"]) # 0

      
    # fusion colonne nom + prenom
      epoc2019b[,"Nom"] <- as.character(epoc2019b[,"Nom"])
      epoc2019b[,"Prénom"] <- as.character(epoc2019b[,"Prénom"])

      epoc2019b[,"Observateur"] <- paste(epoc2019b[,"Prénom"],epoc2019b[,"Nom"])

      























