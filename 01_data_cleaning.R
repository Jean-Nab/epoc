# import data
  setwd("C:/git/epoc/data")
  
  epoc2017a <- read.csv("export_mars_aout2017.csv",sep=";",dec=",",encoding="UTF-8")
  epoc2017b <- read.csv("export_septembre_decembre2017.csv",sep=";",dec=",",encoding="UTF-8",skip=1)
  
  epoc2018a <- read.csv("export_janvier_mars2018.csv",sep=";",dec=",",encoding="UTF-8")
  epoc2018b <- read.csv("export_avril_mai_juin2018.csv",sep=";",dec=",",encoding="UTF-8")
  epoc2018c <- read.csv("export_juillet_decembre2018.csv",sep=";",dec=",",encoding="UTF-8")

  epoc2019a <- read.csv("export_janvier_mars2019.csv",sep=";",dec=",",encoding="UTF-8")
  epoc2019b <- read.csv("export_avril_mai2019.csv",sep=";",dec=",",encoding="UTF-8")
  epoc2019c <- read.csv("export_juin_septembre2019.csv",sep=";",dec=",",encoding="UTF-8")
  epoc2019d <- read.csv("export_octobre_decembre2019.csv",sep=";",dec=",",encoding="UTF-8")
  
  o <- match(colnames(epoc2017a),colnames(epoc2017b))
  
  
# homogeneisation data ----
    # changement nom de la 1er colone des tableaux (colonne d'identifiants)
      colnames(epoc2017b)[1] <- colnames(epoc2017a[1])
    
# merge des datasets ----
  epoc.all <- rbind(epoc2017a,epoc2017b,epoc2018a,epoc2018b,epoc2018c,epoc2019a,epoc2019b,epoc2019c,epoc2019d)

# nettoyage du tableau unifie ----
  colnames(epoc.all)[1] <- "UUID" # changement de nom de la 1ere colonne (pb de conversion tableur --> csv)
  
  # retrait des accents / caracteres speciaux mal converti
    # retrait sur les noms de colonne
      colnames(epoc.all) <- gsub("è|é|ë","e",colnames(epoc.all))
      colnames(epoc.all) <- gsub("(\\.\\.)([a-zA-Z]{1,})","_\\2",colnames(epoc.all)) # "X.Lambert.IIe..m." --> "X.Lambert.IIe_m."
      colnames(epoc.all) <- gsub("\\.\\.","",colnames(epoc.all)) # "Liste.complete.." --> "Liste.complete"
      colnames(epoc.all) <- gsub("(\\.)([a-zA-Z]{1,})","_\\2",colnames(epoc.all)) # "Heure.de.debut" --> "Heure_de_debut"
      colnames(epoc.all) <- gsub("\\.","",colnames(epoc.all)) # "latitude_DMS." --> "latitude_DMS"
      
    # retrait sur les noms d'oiseaux
      epoc.all[,"Nom_espece"] <- as.character(gsub("é|è|ê","e",epoc.all[,"Nom_espece"]))
      epoc.all[,"Nom_espece"] <- as.character(gsub("É","E",epoc.all[,"Nom_espece"]))
      epoc.all[,"Nom_espece"] <- as.character(gsub("ï|î","i",epoc.all[,"Nom_espece"]))
      epoc.all[,"Nom_espece"] <- as.character(gsub(" d'","_",epoc.all[,"Nom_espece"]))
      epoc.all[,"Nom_espece"] <- as.character(gsub(" ","_",epoc.all[,"Nom_espece"]))
      
    # retrait sur les lieux dit
      epoc.all[,"Lieu_dit"] <- as.character(gsub("é|è|ê","e",epoc.all[,"Lieu_dit"]))
      epoc.all[,"Lieu_dit"] <- as.character(gsub("É","E",epoc.all[,"Lieu_dit"]))
      epoc.all[,"Lieu_dit"] <- as.character(gsub("ï|î","i",epoc.all[,"Lieu_dit"]))
      epoc.all[,"Lieu_dit"] <- as.character(gsub("ô|ö","o",epoc.all[,"Lieu_dit"]))
      epoc.all[,"Lieu_dit"] <- as.character(gsub("û|ü","u",epoc.all[,"Lieu_dit"]))
      epoc.all[,"Lieu_dit"] <- as.character(gsub("o","oe",epoc.all[,"Lieu_dit"]))
      epoc.all[,"Lieu_dit"] <- as.character(gsub(" ","_",epoc.all[,"Lieu_dit"]))
      
    # retrait de saut de ligne & caracteres speciaux dans les colonnes "Remarque" / "Remarque_privee" / "Commentaire"
      epoc.all[,"Remarque"] <- as.character(gsub("[\t\n\r\f\v]{1,}"," ",epoc.all[,"Remarque"]))
      epoc.all[,"Remarque"] <- as.character(gsub("[]$*+.?[^{|(\\#%&~_/<=>'!,:;`\")}@-]{1,}","_",epoc.all[,"Remarque"]))
      
      epoc.all[,"Remarque_privee"] <- as.character(gsub("[\t\n\r\f\v]{1,}"," ",epoc.all[,"Remarque_privee"]))
      epoc.all[,"Remarque_privee"] <- as.character(gsub("[]$*+.?[^{|(\\#%&~_/<=>'!,:;`\")}@-]{1,}","_",epoc.all[,"Remarque_privee"]))
      
      epoc.all[,"Commentaire_de_la_liste"] <- as.character(gsub("[\t\n\r\f\v]{1,}"," ",epoc.all[,"Commentaire_de_la_liste"]))
      epoc.all[,"Commentaire_de_la_liste"] <- as.character(gsub("[]$*+.?[^{|(\\#%&~_/<=>'!,:;`\")}@-]{1,}","_",epoc.all[,"Commentaire_de_la_liste"]))
    
      
    
# Formation d'une colonne observateur = fusion des colonnes Nom + Prenom ----
      epoc.all[,"Nom"] <- as.character(epoc.all[,"Nom"])
      epoc.all[,"Prenom"] <- as.character(epoc.all[,"Prenom"])
  
      epoc.all[,"Observateur"] <- paste(epoc.all[,"Prenom"],epoc.all[,"Nom"])
  
  
# Sauvegarde du tableau unifie (contenant des observations EPOC et non-EPOC de 1/03/2017 --> 31/12/2019) sur disque
      # Fichier trop gros tel quel --> saucissonnage en 5 parties
      part <- nrow(epoc.all)/5 ; part # part = 1041353
      epoc.alla <- epoc.all[1:1041353,]
      epoc.allb <- epoc.all[1041354:2082707,]
      epoc.allc <- epoc.all[2082708:3124061,]
      epoc.alld <- epoc.all[3124062:4165415,]
      epoc.alle <- epoc.all[4165416:nrow(epoc.all),]
      
      write.table(x = epoc.alla, file = paste0(sub("/data","/output",getwd()),"/export_2017_2019a.txt"),sep="\t",dec=","
                  ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
      write.table(x = epoc.allb, file = paste0(sub("/data","/output",getwd()),"/export_2017_2019b.txt"),sep="\t",dec=","
                  ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
      write.table(x = epoc.allc, file = paste0(sub("/data","/output",getwd()),"/export_2017_2019c.txt"),sep="\t",dec=","
                  ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
      write.table(x = epoc.alld, file = paste0(sub("/data","/output",getwd()),"/export_2017_2019d.txt"),sep="\t",dec=","
                  ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
      write.table(x = epoc.alle, file = paste0(sub("/data","/output",getwd()),"/export_2017_2019e.txt"),sep="\t",dec=","
                  ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
  
      
# chargement de la donnee
      epoc.loada <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019a.txt"),header=T,sep="\t", dec=","
                               , encoding="UTF-8",quote="")
      epoc.loadb <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019b.txt"),header=T,sep="\t", dec=","
                               , encoding="UTF-8",quote="")
      epoc.loadc <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019c.txt"),header=T,sep="\t", dec=","
                               , encoding="UTF-8",quote="")
      epoc.loadd <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019d.txt"),header=T,sep="\t", dec=","
                               , encoding="UTF-8",quote="")
      epoc.loade <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019e.txt"),header=T,sep="\t", dec=","
                               , encoding="UTF-8",quote="")
      # chargement possible
  


# testing ground (not run) ----
    
    epoc.alla1 <- epoc.all[1:5000,]  
    write.table(x = epoc.alla1, file = paste0(sub("/data","/output",getwd()),"/export_2017_2019a1.txt"),sep="\t",dec=","
                ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
   
     write.csv(x = epoc.alla1, file = paste0(sub("/data","/output",getwd()),"/export_2017_2019a1.csv"),fileEncoding = "UTF-8")
    
    
      
    aaa <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019a1.txt"),header=T,sep="\t", dec=","
                      , encoding="UTF-8", quote="")
  
    
    o <- grep("[\t\n\r\f\v]",epoc.all[,"Remarque"]) # 2829
    o <- grep("[]$*+.?[^{|(\\#%&~_/<=>'!,:;`\")}@-]",epoc.all[,"Remarque"]) # 32
    
    o <- grep("\n",epoc.all[,"Remarque_privee"]) # 340
    o <- grep("#",epoc.all[,"Remarque_privee"]) # 1
    
    o <- grep("\n",epoc.all[,"Commentaire_de_la_liste"]) # 98609
    o <- grep("#",epoc.all[,"Commentaire_de_la_liste"]) # 158
    o <- grep("\n",epoc.all[,"Details"]) # 0
    o <- grep("\n",epoc.all[,"Nom"])
    o <- grep("\n",epoc.all[,"Prenom"])
    o <- grep("\n",epoc.all[,"Lieu_dit"])
    o <- grep("\n",epoc.all[,"Nombre"])
      
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
      o <- grep("ö", epoc.all[,"Nom_espece"]) # 0
      o <- grep("î", epoc.all[,"Nom_espece"]) # 
      o <- grep("o", epoc.all[,"Lieu_dit"]) # 427
      o <- grep("Ó", epoc.all[,"Lieu_dit"]) # 0
      o <- grep("ô", epoc2019a.bis[,"Nom_espece"]) # 0
      o <- grep("û", epoc2019a.bis[,"Nom.espèce"]) # 0
      
      o <- grep("ê", epoc2019b[,"Observateur"]) # 0
      o <- grep("ë", epoc2019b[,"Observateur"]) # 4968
      o <- grep("ï", epoc2019b[,"Observateur"]) # 843
      o <- grep("o", epoc2019b[,"Observateur"]) # 
      o <- grep("ö", epoc2019b[,"Observateur"]) # 222
      o <- grep("ô", epoc2019b[,"Observateur"]) # 390
      o <- grep("û", epoc2019b[,"Observateur"]) # 0

      
    # fusion colonne nom + prenom
      epoc2019b[,"Nom"] <- as.character(epoc2019b[,"Nom"])
      epoc2019b[,"Prénom"] <- as.character(epoc2019b[,"Prénom"])
      
      # suppression des characteres apres " et"/", "/" &" dans la colonne prenom
        o <- grep(", | et| &",epoc2019a[,"Prénom"])
        pasbon <- as.data.frame(epoc2019a[o,"Prénom"]) ; colnames(pasbon) <- "Prénom"
        
        o <- grep(", | et| &",epoc2019a[,"Nom"])
        pasbon <- as.data.frame(epoc2019a[o,"Nom"]) ; colnames(pasbon) <- "Nom"
        
        pasbon[,"Prénom"] <- as.character(gsub(" et.*|,.*| &.*","",pasbon[,"Prénom"]))
        pasbon[,"Prénom"] <- as.character(gsub(" ","\\-",pasbon[,"Prénom"]))
        #epoc2019b[,"Prénom"] <- as.character(gsub(" et.","",epoc2019b[,"Prénom"]))

      epoc2019b[,"Observateur"] <- paste(epoc2019b[,"Prénom"],epoc2019b[,"Nom"])
      epoc2019b[,"Observateur"] <- as.character(gsub("é","e",epoc2019b[,"Observateur"]))
      epoc2019b[,"Observateur"] <- as.character(gsub("è","e",epoc2019b[,"Observateur"]))
      epoc2019b[,"Observateur"] <- as.character(gsub("ê","e",epoc2019b[,"Observateur"]))
      epoc2019b[,"Observateur"] <- as.character(gsub("ë","e",epoc2019b[,"Observateur"]))
      epoc2019b[,"Observateur"] <- as.character(gsub("ï","i",epoc2019b[,"Observateur"]))
      epoc2019b[,"Observateur"] <- as.character(gsub("ö","o",epoc2019b[,"Observateur"]))
      epoc2019b[,"Observateur"] <- as.character(gsub("ô","o",epoc2019b[,"Observateur"]))
      
      
      epoc2019b[,"Observateur"] <- as.character(gsub(" ","_",epoc2019b[,"Observateur"]))
      

  # recherche de tabulation
      o <- grep(pattern = "\t", epoc.all[,])





















