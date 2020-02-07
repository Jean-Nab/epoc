# chemin
setwd("C:/git/epoc/data")


# import data (faire un import a partir du dataset merged 2017-2018-2019)
'
  epoc.a <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019a.txt"),header=T,sep="\t", dec=","
                           , encoding="UTF-8",quote="")
  epoc.b <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019b.txt"),header=T,sep="\t", dec=","
                           , encoding="UTF-8",quote="")
  epoc.c <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019c.txt"),header=T,sep="\t", dec=","
                           , encoding="UTF-8",quote="")
  epoc.d <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019d.txt"),header=T,sep="\t", dec=","
                           , encoding="UTF-8",quote="")
  epoc.e <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019e.txt"),header=T,sep="\t", dec=","
                           , encoding="UTF-8",quote="")
                           
  epoc <- rbind(epoc.a,epoc.b,epoc.c,epoc.d,epoc.e)
'

  epoc <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019.txt"),header=T,sep="\t", dec=","
                       , encoding="UTF-8",quote="")

  
  
  # filtrage selon la mention "epoc" dans les commentaires/remarques ----
    # idee : recherche des termes epoc/Epoc/EPOC dans les commentaires
      # Rajout de mention epoc pour les ID de liste
        # Detection des ID_liste comprenant la mention d'epoc dans les remarques
            o <- grep("epoc|Epoc|EPOC|EPoc|EPOc",epoc[,"Remarque"]) # recherche de la mention epoc (ecrit sous differentes manieres dans la colonne "Remarque")
            ID.rq <- epoc[o,"ID_liste"] # Recherche des ID_liste associe
            
            obs_liste <- which(epoc[,"ID_liste"] %in%  ID.rq) # recherche de toutes les lignes avec un ID_liste contenu dans ID.rq
            
            epoc[obs_liste,"Remarque"] <- "EPOC" # attribution de la mention EPOC aux observations
            
        # Filtrage du jeu de donnees afin d'avoir uniquement les observations ou la mention epoc est integrer
            comm <- grep("epoc|Epoc|EPOC|EPoc|EPOc",epoc[,"Commentaire_de_la_liste"]) # detection des lignes contenant la mention epoc dans la colonne commentaire
            rq <- grep("epoc|Epoc|EPOC|EPoc|EPOc",epoc[,"Remarque"]) # detection des lignes contenant la mention epoc dans la colonne remarque
            
            comm_rq <- append(comm,rq) # concatenation des 2 vecteurs renseignant les positions
            comm_rq.unique <- unique(comm_rq) # retrait de possible doublon (les observation ou epoc est mentionne dans les commentaires et dans les remarques)
            
            epoc.filt1 <- epoc[comm_rq.unique,] # fromation du dtf filtre par le mentionnement du protocole epoc dans commmentaire/remarque
          
        # Enregistrement sur le disque
            write.table(x = epoc.filt1, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_1.txt"),sep="\t",dec=","
                        ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
  
  
# filtrage selon la duree d'ecoute ----
  # chgt [: --> ,] et transformation des heures (factor) en (numeric)
  # idee : passer par la valeur absolue et selectionner toutes les observations avec une valeur absolue <= 0.08
    epoc.filt2 <- epoc.filt1
    epoc.filt2[,"Heure_debut"] <- as.numeric(gsub("\\:","\\.",epoc.filt1[,"Heure_debut"]))
    epoc.filt2[,"Heure_fin"] <- as.numeric(gsub("\\:","\\.",epoc.filt1[,"Heure_fin"]))
    
    epoc.filt2[,"Tps_ecoute"] <- abs(epoc.filt2[,"Heure_fin"] - epoc.filt2[,"Heure_debut"]) # valeur absolue 
    
    tps_epoc <- which(epoc.filt2[,"Tps_ecoute"] >= 0.01 & epoc.filt2[,"Tps_ecoute"] <= 0.10)
    epoc.filt2 <- epoc.filt2[tps_epoc,] # dataframe contenant uniquement les observations de 5 a 8 minutes
    
  # Enregistrement sur le disque
    write.table(x = epoc.filt2, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_2.txt"),sep="\t",dec=","
                ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)  
    

      



# testing ground (not run) ----
    
    
    o <- which(epoc.filt2[,"Tps_ecoute"] <= 0.10)
    o <- which(epoc.filt2[,"Tps_ecoute"] >= 0.01 & epoc.filt2[,"Tps_ecoute"] <= 0.10)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    o <- grep("epoc|Epoc|EPOC|EPoc|EPOc",epoc[,"Commentaire_de_la_liste"])
    t <- grep("epoc|Epoc|EPOC|EPoc|EPOc",epoc[,"Remarque"])
    op <- append(o,t)
    op1 <- unique(op)
    
    test <- epoc[op1,]
    test1 <- epoc[epoc$Commentaire_de_la_liste == "epoc|Epoc|EPOC|EPoc|EPOc",]
    
    test <- epoc[epoc$Commentaire_de_la_liste == "epoc|Epoc|EPOC|EPoc|EPOc" | epoc$Remarque == "EPOC",]
    test <- epoc[which(epoc$Remarque == "EPOC" | epoc$Commentaire_de_la_liste == "epoc|Epoc|EPOC|EPoc|EPOc"),]
    
    
    
    
    test <- epoc[1:1000000,]
    o <- grep("epoc|Epoc|EPOC|EPoc|EPOc",test[,"Remarque"])
    ID.rq <- test[o,"ID_liste"]
    
    obs_liste <- c()
    u <- 1
    while (u <= length(ID.rq)){
      
      obs_liste.temp <- grep(ID.rq[u],test[,"ID_liste"])
      obs_liste <- append(obs_liste,obs_liste.temp)
      
      cat(u," / ",length(ID.rq),"\n")
      u <- u+1
    }
    
    test[obs_liste,"Remarque"] <- "EPOC"
    
    p <- which(test[,"ID_liste"] %in%  ID.rq)
    test[p,"Remarque"] <- "EPOC"
    
    view.test <- epoc[c(20745:20755,59150:59160),]
    
    
    view.test[,c("Remarque","ID_liste")]

    
    
    # boucle lecture des ID avec la mention d'epoc dans les remarques
      ID.rq <- test[o,"ID_liste"]
      i <- 1
      while (i <= nrow(test)){
        for (j in ID.rq){
          if (test[i,"ID_liste"] == j){
            test[i,"Remarque"] <- "EPOC"
          }
        }
      
        i <- i+1
      }
    
    
      
    test[test[,"ID_liste"] == ID.rq,"Remarque"] <- "EPOC"
    
    
    
  as.numeric(gsub("\\:","\\.",epoc.filt1[1,"Heure_debut"]))

  # recherche des termes epoc
    com_rq1 <- grep(patter= "epoc|Epoc|EPOC", epoc.flt1[,"Remarque"]) # 3442 mention d'epoc dans les remarques
    
    com_rq2 <- grep(patter= "epoc|Epoc|EPOC", epoc.flt1[,"Commentaire_de_la_liste"]) # detection du numeros des lignes contenant la mention epoc dans les commentaires
                                                                                     # 255565 mentions d'epoc dans les commentaires
    

epoc.flt1[4073:4086,c("Commentaire_de_la_liste","Remarque","ID_liste")]















