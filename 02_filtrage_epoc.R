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

  
  
# 1er filtrage selon la mention "epoc" dans les commentaires/remarques ----
    # idee : recherche des termes epoc/Epoc/EPOC dans les commentaires
      # Rajout de mention epoc pour les ID de liste
        # Detection des ID_liste comprenant la mention d'epoc dans les remarques
            rq <- grep("EPOC",toupper(epoc[,"Remarque"])) # recherche de la mention epoc (ecrit sous differentes manieres dans la colonne "Remarque")
            ID.rq <- epoc[rq,"ID_liste"] # Recherche des ID_liste associe
            
            obs_liste <- which(epoc[,"ID_liste"] %in%  ID.rq) # recherche de toutes les lignes avec un ID_liste contenu dans ID.rq
            
            epoc[obs_liste,"Remarque"] <- "EPOC" # attribution de la mention EPOC aux observations
            
        # Filtrage du jeu de donnees afin d'avoir uniquement les observations ou la mention epoc est integrer
            comm <- grep("EPOC",toupper(epoc[,"Commentaire_de_la_liste"])) # detection des lignes contenant la mention epoc dans la colonne commentaire
            
            comm_rq <- union(comm,rq) # concatenation des 2 vecteurs renseignant les positions sans prendre en compte les doublons
          
            epoc.filt1 <- epoc[comm_rq.unique,] # fromation du dtf filtre par le mentionnement du protocole epoc dans commmentaire/remarque
          
        # Enregistrement sur le disque
            write.table(x = epoc.filt1, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_1.txt"),sep="\t",dec=","
                        ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
  
  
# 2nd filtrage selon la duree d'ecoute ----
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
    
# 3eme filtrage selon les protocoles : ----
      # Retrait des protocoles : "SHOC" ; "STOC_EPS" ; "STOC_MONTAGNE" ; "STOC_ONF" ; "STOC_SITES" ; "WATERBIRD"
      # Donc, formation d'un dtf avec les protocoles "GENERIC_BIRD" et ""
        levels(epoc.filt2$Protocole)
        search.prot <- grep("GENERIC_BIRD|",epoc.filt2$Protocole) # Ici, length(search.prot) == nrow(epoc.filt2) ==> toutes les observations suivent le protocole "GENERIC_BIRD" ou ""
        
        epoc.filt3 <- epoc.filt2[search.prot,]
      
      # Enregistrement sur le disque
        write.table(x = epoc.filt3, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_3.txt"),sep="\t",dec=","
                    ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)  

# 4eme filtrage : retrait des observations "type de localisation == jardin" ----
        levels(epoc.filt3$Type_de_localisation)
        loc.jardin <- grep("Jardin",epoc.filt3$Type_de_localisation)
        
        epoc.filt4 <- epoc.filt3[-loc.jardin,]
        
      # Enregistrement sur le disque
        write.table(x = epoc.filt4, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_4.txt"),sep="\t",dec=","
                    ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE) 
        
# 5eme filtrage selon l'etat de la liste : selection des listes completes -----
        epoc.filt5 <- epoc.filt4[epoc.filt4$Liste_complete == 1,]
        
      # Enregistrement sur le disque
        write.table(x = epoc.filt5, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_5.txt"),sep="\t",dec=","
                    ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
        

# 6eme filtrage selon les details de l'observation ----
      # conditions : "En vol" ; "posé" ; "en main" ; "analyse de pelotes" ; "Contact auditif"
        # retrait des conditions "en main" ; "analyse de pelotes"
          cond.retir <- grep("en main|pelotes",epoc.filt5$Details)
          epoc.filt6 <- epoc.filt5[-cond.retir,]
          
        # retrait des accents dans la colonne details (possible source de pb lors du grep)
          epoc.filt6$Details <- gsub("é","e",epoc.filt6$Details)
          epoc.filt6$Details <- gsub("â","a",epoc.filt6$Details)
          epoc.filt6$Details <- tolower(epoc.filt6$Details)
          
        # formation de 4 nouvelles colonnes regroupant les informations de la colonne "Details"
          epoc.filt6$Nb_pose <- c(rep(0,nrow(epoc.filt6)))
          epoc.filt6$Nb_vol <- c(rep(0,nrow(epoc.filt6)))
          epoc.filt6$Nb_audition <- c(rep(0,nrow(epoc.filt6)))
          epoc.filt6$Nb_NA <- c(rep(0,nrow(epoc.filt6)))
          
        # Remplissage des 4 nouvelles colonnes selon les informations de details
            # Idee : formation d'une liste de characteres (det.list) contenant les informations de details en incluant une separation a chaque "/"
            # Formation de la liste et splitting
              det.list <- strsplit(epoc.filt6$Details, split = "/")
              
            # Boucle de lecture de la liste (det.list) et insertion de la donnees dans la colonne correspondante
            # i.e : ligne Y : 9x male (en vol) / 7x male (pose) ==> ligne Y : 9 (dans $Nb_vol) ; 7 (dans $Nb_pose)
              
              u <- 1 # initialisation de l'indice de lecture de la liste
              while (u <= length(det.list)){ # lecture de la liste
                p <- 1 # initialisation de l'indice de lecture des elements au sein de la liste
                while (p <= length(det.list[[u]])){ # lecture des elements au sein de la liste
                  # Mise en place de conditions permettant d'inserer les donnees numeriques dans les bonnes colonnes
                  if (grepl("pose",det.list[[u]][p]) == TRUE){ # Si presence de la mention (pose)
                    epoc.filt6[u,"Nb_pose"] <- epoc.filt6[u,"Nb_pose"] + as.numeric(gsub("([0-9]+).*$", "\\1", det.list[[u]][p])) # ajout de la valeur numerique dans la colonne Nb_pose
                  }
                  if (grepl("vol",det.list[[u]][p]) == TRUE) { # Si presence de la mention (en vol)
                    epoc.filt6[u,"Nb_vol"] <- epoc.filt6[u,"Nb_vol"] + as.numeric(gsub("([0-9]+).*$", "\\1", det.list[[u]][p])) # ajout de la valeur numerique dans la colonne Nb_vol
                  }
                  if (grepl("auditif",det.list[[u]][p]) == TRUE) {
                    epoc.filt6[u,"Nb_audition"] <- epoc.filt6[u,"Nb_audition"] + as.numeric(gsub("([0-9]+).*$", "\\1", det.list[[u]][p]))
                  }
                  p <- p+1 
                }
                cat(u," / ",length(det.list),"\n") # etat d'avancement de la boucle
                u <- u+1
              }
              
            # Formation de la colonne Nb_NA : regroupant le reste des informations de comptage non renseigne dans la colonne details
              epoc.filt6$Nb_NA <- epoc.filt6$Nombre - (epoc.filt6$Nb_pose + epoc.filt6$Nb_vol + epoc.filt6$Nb_audition)
              
# 6 bis : modification de la forme du tableau
    
        
# 7eme filtrage selon la periode ----
    # formation de 2 tableaux : 1 tableau avec des observations comprises entre le 1/03-30/06 et 1 tableau avec les observations en-dehors de cette periode
        in.period <- which(epoc.filt4$Jour >= 1 & epoc.filt4$Jour <= 31 & epoc.filt4$Mois >= 3 & epoc.filt4$Mois <= 6)
        dtf.in.period <- epoc.filt4[in.period,]
        dtf.out.period <- epoc.filt4[-in.period,]
        
        
# testing ground (not run) ----
        
    test6 <- epoc.filt6[grep("vol",epoc.filt6$Details),]
    #h <- grep("[0-9]{1,}x [a-z]{1,} (pose)", test6$Details,value=TRUE)
    
    #k <- regmatches(test6$Details,gregexpr("[0-9]{1,}x [a-z]+|[0-9]{1,}x (pose)",test6$Details))
    #k1 <- as.numeric(unlist(k))
    
        
    test6 <- epoc.filt6[1:150,]    
    test6$Details <- as.character(test6$Details)
    k <- strsplit(test6$Details, split = "/")
    
    u <- 1
    while (u <= length(k)){
      p <- 1
      while (p <= length(k[[u]])){
        if (grepl("pose",k[[u]][p]) == TRUE){
          test6[u,"Nb_pose"] <- test6[u,"Nb_pose"] + as.numeric(gsub("([0-9]+).*$", "\\1", k[[u]][p]))
        }
        if (grepl("vol",k[[u]][p]) == TRUE) {
          test6[u,"Nb_vol"] <- test6[u,"Nb_vol"] + as.numeric(gsub("([0-9]+).*$", "\\1", k[[u]][p]))
        }
        if (grepl("auditif",k[[u]][p]) == TRUE) {
          test6[u,"Nb_audition"] <- test6[u,"Nb_audition"] + as.numeric(gsub("([0-9]+).*$", "\\1", k[[u]][p]))
        }
        p <- p+1
      }
      cat(u,"\n")
      u <- u+1
    }
    
    # checkup de l'action de la boucle precedente
      test6$Nb_NA <- test6$Nombre - (test6$Nb_pose + test6$Nb_vol + test6$Nb_audition)
      head(test6[,c("Details","Nombre","Nb_pose","Nb_vol","Nb_audition","Nb_NA")],150)
    
    test6[,"Nb_vol"] <- as.numeric(gsub("([0-9]+).*$", "\\1", test6[,"Nb_vol"]))
    
    l <- grepl("pose",k)
    
    library(unglue)
    library(taRifx)
        
        
        
        
        
        
    o <- grep("posé|En vol|analyse de pelotes|Contact auditif|en main",epoc.filt4$Details, invert=TRUE)
        
    o <- which(is.na(epoc.filt4$Details))
    test4 <- epoc.filt4[cond.pose,]
    cond <- grep("en main|analyse de pelotes|Contact auditif",epoc.filt4$Details)
    o <- grep("6x (En vol) / 6x (posé)", epoc.filt4$Details)
        
        
        
    
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















