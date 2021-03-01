# chemin
setwd("C:/git/epoc/data")

# packages
  library(tidyverse)
  library(stringr)
  library(plyr)
  library(lubridate)
  library(data.table)


# import data (faire un import a partir du dataset merged 2017-2018-2019)

  epoc2017_2019 <- fread(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019.txt"),header=T, stringsAsFactors = F,
                         encoding="UTF-8",quote="")
  epoc2020 <- fread(file = paste0(sub("/data","/output",getwd()),"/export_2020.txt"), stringsAsFactors = F,
                    encoding="UTF-8",quote="")
  
  epoc <- rbind(epoc2017_2019,epoc2020,fill=T) # rbind de data.table
  epoc <- as.data.frame(epoc)
  
# 1er filtrage selon la mention "epoc" dans les commentaires/remarques ----
    # idee : recherche des termes epoc/Epoc/EPOC dans les commentaires
      # Rajout de mention epoc pour les ID de liste
        # Detection des ID_liste comprenant la mention d'epoc dans les remarques et commentaires des listes
            rq <- grep("EPOC",toupper(epoc[,"Remarque"])) # recherche de la mention epoc (ecrit sous differentes manieres dans la colonne "Remarque")
            comm <- grep("EPOC",toupper(epoc[,"Commentaire_de_la_liste"])) # recherche de la mention epoc (ecrit sous differentes manieres dans la colonne "Commentaire_de_la_liste")
            comm_rq <- union(comm,rq)
              
            ID.comm_rq <- epoc[comm_rq,"ID_liste"] # Recherche des ID_liste associe
            
            obs_liste <- which(epoc[,"ID_liste"] %in%  ID.comm_rq) # recherche de toutes les lignes avec un ID_liste contenu dans ID.rq
            
            epoc[obs_liste,"Remarque"] <- "EPOC" # attribution de la mention EPOC aux observations
            rq1 <- grep("EPOC",toupper(epoc[,"Remarque"]))
            
        # Filtrage du jeu de donnees afin d'avoir uniquement les observations ou la mention epoc est integrer
            epoc.filt1 <- epoc[rq1,] # formation du dtf filtre par le mentionnement du protocole epoc dans commmentaire/remarque
          
            epoc.filt1bis <- epoc[-rq1,]
            epoc.filt1bis <- epoc.filt1bis[epoc.filt1bis$Annee == 2017,]
            
        # Enregistrement sur le disque
            # write.table(x = epoc.filt1, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_1.txt"),sep="\t",dec=","
            #             ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
  
  
# 2nd filtrage selon la duree d'ecoute ----
  # chgt [: --> ,] et transformation des heures (factor) en (numeric)
  # idee : passer par la valeur absolue et selectionner toutes les observations avec une valeur absolue <= 0.08
    epoc.filt2 <- epoc.filt1
    epoc.filt2bis <- epoc.filt1bis        

    
    # POUR LISTES EPOC ds commentaire/remarque        
      duree_ecout_deb <- lubridate::hm(epoc.filt2$Heure_debut)
      duree_ecout_fin <- lubridate::hm(epoc.filt2$Heure_fin)
      
      duree_ecout <- (lubridate::hour(duree_ecout_fin)*60 + lubridate::minute(duree_ecout_fin)) - 
        (lubridate::hour(duree_ecout_deb)*60 + lubridate::minute(duree_ecout_deb))
       
      epoc.filt2[,"Tps_ecoute"] <- duree_ecout
      
      tps_epoc <- which(epoc.filt2[,"Tps_ecoute"] >= 5 & epoc.filt2[,"Tps_ecoute"] <= 8)
      epoc.filt2 <- epoc.filt2[tps_epoc,] # dataframe contenant uniquement les observations de 5 a 8 minutes
    
    # POUR LISTE sans EPOC dans commentaire/remarque  
      duree_ecout_deb <- lubridate::hm(epoc.filt2bis$Heure_debut)
      duree_ecout_fin <- lubridate::hm(epoc.filt2bis$Heure_fin)
      
      duree_ecout <- (lubridate::hour(duree_ecout_fin)*60 + lubridate::minute(duree_ecout_fin)) - 
        (lubridate::hour(duree_ecout_deb)*60 + lubridate::minute(duree_ecout_deb))
      
      epoc.filt2bis[,"Tps_ecoute"] <- duree_ecout
      
      tps_epoc <- which(epoc.filt2bis[,"Tps_ecoute"] >= 5 & epoc.filt2bis[,"Tps_ecoute"] <= 7)
      epoc.filt2bis <- epoc.filt2bis[tps_epoc,] # dataframe contenant uniquement les observations de 5 a 6 minutes
    
  # Enregistrement sur le disque
   # write.table(x = epoc.filt2, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_2.txt"),sep="\t",dec=","
   #             ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE) 
    
    
# 3eme filtrage selon les protocoles : ----
      # Retrait des protocoles : "SHOC" ; "STOC_EPS" ; "STOC_MONTAGNE" ; "STOC_ONF" ; "STOC_SITES" ; "WATERBIRD"
      # Donc, formation d'un dtf avec les protocoles "GENERIC_BIRD" et ""
        search.prot <- grepl("(GENERIC_BIRD|)",epoc.filt2$Protocole)|is.na(epoc.filt2$Protocole) # Ici, length(search.prot) == nrow(epoc.filt2) ==> toutes les observations suivent le protocole "GENERIC_BIRD" ou ""

        search.protbis <- grep("SHOC|STOC_EPS|STOC_MONTAGNE|STOC_ONF|STOC_SITES|WATERBIRD",epoc.filt2bis$Protocole) # Ici, length(search.prot) == nrow(epoc.filt2) ==> toutes les observations suivent le protocole "GENERIC_BIRD" ou ""
        
        epoc.filt3 <- epoc.filt2[search.prot,]
        
        epoc.filt3bis <- epoc.filt2bis[-search.protbis,]
      
      
      # Enregistrement sur le disque
       # write.table(x = epoc.filt3, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_3.txt"),sep="\t",dec=","
       #             ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)  
        

# 4eme filtrage : retrait des observations "type de localisation == jardin" ----
        
        loc.jardin <- grep("Jardin",epoc.filt3$Type_de_localisation)
        loc.jardinbis <- grep("Jardin",epoc.filt3bis$Type_de_localisation)
        
        epoc.filt4 <- epoc.filt3[-loc.jardin,]
        epoc.filt4bis <- epoc.filt3bis[-loc.jardinbis,]
        
      # Enregistrement sur le disque
        # write.table(x = epoc.filt4, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_4.txt"),sep="\t",dec=","
        #             ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE) 
        
        
# 5eme filtrage selon l'etat de la liste : selection des listes completes -----
        epoc.filt5 <- epoc.filt4[epoc.filt4$Liste_complete == 1,]
        epoc.filt5bis <- epoc.filt4bis[epoc.filt4bis$Liste_complete == 1,]
        
        epoc.filt5$Mention_EPOC <- c(rep(1,nrow(epoc.filt5)))
        epoc.filt5bis$Mention_EPOC <- c(rep(0,nrow(epoc.filt5bis)))
        
        epoc.filt5 <- rbind(epoc.filt5,epoc.filt5bis)
        
      # Enregistrement sur le disque
        # write.table(x = epoc.filt5, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_5.txt"),sep="\t",dec=","
        #             ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
        

# Rajout des colonnes Diversite_liste et Abondance_liste servant d'indice pour qualifier la liste ----
    # Idee : travaille de decompte d'espece et d'abondance total pour une liste a la fois (detection des listes & isolement d'une liste ==> calcul des indices)
        #epoc.filt5$Diversite_liste <- c(rep(0,nrow(epoc.filt5))) # formation d'une nouvelle colonne Diversite_liste (= nb d'espece specifique observe dans chaque liste)
        #epoc.filt5$Abondance_liste <- c(rep(0,nrow(epoc.filt5))) # formation d'une nouvelle colonne Abondance_liste (= total du comptage de chaque liste)
        
        div_list <- data.table::setDT(epoc.filt5)[, .(Diversite_liste = data.table::uniqueN(Nom_espece)), by = ID_liste]
        epoc.filt5 <- dplyr::left_join(epoc.filt5,div_list,by="ID_liste")
        
        
        ab_list <- data.table::setDT(epoc.filt5)[, .(Abondance_liste = sum(Nombre)), by = ID_liste] 
        epoc.filt5 <- dplyr::left_join(epoc.filt5,ab_list)
        

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
              
              
            # Remplissage de la colonne Nb_NA : regroupant le reste des informations de comptage non renseigne dans la colonne details
              epoc.filt6$Nb_NA <- epoc.filt6$Nombre - (epoc.filt6$Nb_pose + epoc.filt6$Nb_vol + epoc.filt6$Nb_audition)
              
            # ajout d'une colonne abondance (somme des abondances selon les la colonne details propre au protocole EPOC)
              # modification du nom de la colonne "nombre"
                colnames(epoc.filt6)[grep("Nombre",colnames(epoc.filt6))] <- "Abondance_brut"
              
              epoc.filt6$Abondance <- epoc.filt6$Abondance_brut - epoc.filt6$Nb_vol
              
            # Enregistrement sur le disque
              # write.table(x = epoc.filt6, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_6_court.txt"),sep="\t",dec=","
              #             ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)

              
# Rajout de la colonne experience de l'observateur (Experience)
              # CONCEPT : Expérience = nombre d'EPOC realise par l'observateur avant l'EPOC etudie
              # ==> Regroupement des EPOC par observateur (subset par observateur)
              # ==> Besoin d'ordonne les EPOC selon le temps
              
              epoc.filt6.exp <- epoc.filt6[,c("Observateur","ID_liste","Jour","Mois","Annee","Date","Heure_de_debut","Minute_de_debut")] 
              epoc.filt6.exp$date <- paste0(epoc.filt6.exp$Jour,"/",epoc.filt6.exp$Mois,"/",epoc.filt6.exp$Annee," ",epoc.filt6.exp$Heure_de_debut,":",epoc.filt6.exp$Minute_de_debut)
            
              
              id.obs <- unique(epoc.filt6$Observateur) # debut de boucle sur les observateurs
              dtf.exp <- data.frame() # fromation d'un noueau dataframe
              i <- 1
              while(i <= length(id.obs)){
                epoc.filt6.exp.tmp <- epoc.filt6.exp[epoc.filt6.exp$Observateur == id.obs[i],] # subsetting du dtf par observateur
                liste.dup <- which(duplicated(epoc.filt6.exp.tmp[,"ID_liste"]) == FALSE) # detection des doublons de listes (informations repetees inutiles dans ce cas)
                epoc.filt6.exp.tmp2 <- epoc.filt6.exp.tmp[liste.dup,] # formation d'un dtf ne contenant toutes les epocs realisees par un l'observateur i
                
                epoc.filt6.exp.tmp2$date <- as.POSIXct(epoc.filt6.exp.tmp2$date , format = "%d/%m/%Y %H:%M") # conversion de la colonne date en format horaire (pouvant etre trie)
                class(epoc.filt6.exp.tmp2)
                epoc.filt6.exp.tmp2 <- epoc.filt6.exp.tmp2[do.call(order, epoc.filt6.exp.tmp2), ] # tri par ordre croissant selon la date
                
                # fromation de la colonne experience
                epoc.filt6.exp.tmp2$Experience <- c(rep(0:(nrow(epoc.filt6.exp.tmp2)-1))) # ajout de 1 par ligne --> experience augmente avec le nombre d'epoc realisees
                
                
                dtf.exp <- rbind(dtf.exp,epoc.filt6.exp.tmp2)
                
                cat(i,"/ ",length(id.obs),"\n")
                i <- i+1
              }
              
              # Pour le moment : Experience liee a une ID --> besoin de rajouter l'experience a toutes les observations de meme ID_liste
                epoc.filt6 <- dplyr::left_join(epoc.filt6,dtf.exp[,c("ID_liste","Experience")])
            
              
# 6 bis : modification de la forme du tableau passage d'un format large a un format long ----
    epoc.filt6.long <- reshape(epoc.filt6, varying = c("Nb_pose","Nb_vol","Nb_audition","Nb_NA"),
                           v.names = "Nb_contact",
                           timevar = "Info_contact",
                           times = c("pose","en_vol","audition","na"),
                           direction="long")
    
    # Enregistrement sur le disque
      # write.table(x = epoc.filt6.long, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_6_long.txt"),sep="\t",dec=","
      #           ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
        
# 7eme filtrage selon la periode ----
    # formation de 2 tableaux : 1 tableau avec des observations comprises entre le 1/03-31/07 et 1 tableau avec les observations en-dehors de cette periode
        long.in.period <- which(epoc.filt6.long$Jour >= 1 & epoc.filt6.long$Jour <= 31 & epoc.filt6.long$Mois >= 3 & epoc.filt6.long$Mois <= 7)
        court.in.period <- which(epoc.filt6$Jour >= 1 & epoc.filt6$Jour <= 31 & epoc.filt6$Mois >= 3 & epoc.filt6$Mois <= 7)
      
        epoc.filt7.long.in <- epoc.filt6.long[long.in.period,]
        epoc.filt7.long.out <- epoc.filt6.long[-long.in.period,]
        
        epoc.filt7.court.in <- epoc.filt6[court.in.period,]
        epoc.filt7.court.out <- epoc.filt6[-court.in.period,]
        
    # Enregistrement sur le disque
        # write.table(x = epoc.filt7.long.in, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_7_long_in_period.txt"),sep="\t",dec=","
        #             ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE) 
        # write.table(x = epoc.filt7.long.out, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_7_long_out_period.txt"),sep="\t",dec=","
        #             ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE) 
        # 
        # 
        # write.table(x = epoc.filt7.court.in, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_7_court_in_period.txt"),sep="\t",dec=","
        #             ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE) 
        # write.table(x = epoc.filt7.court.out, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_7_court_out_period.txt"),sep="\t",dec=","
        #             ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE) 
        
        
# separation du jeu de donnees en 2 tableaux (var env/localisation et var communautes d'oiseaux) ----
  epoc.oiso <- epoc.filt7.court.in[,c("Ref","ID_liste","Observateur","Nom_espece","Nom_latin","Abondance_brut",
                                      "Estimation","Nb_pose","Nb_vol","Nb_audition","Nb_NA")]
        
  epoc.envi <- epoc.filt7.court.in[,c("UUID","Ref","ID_liste","Code_atlas","Jour","Mois","Annee","Jour_de_l_annee",
                                      "Heure_debut","Tps_ecoute","Diversite_liste","Abondance_liste","Mention_EPOC","Liste_complete","Maille","Departement","X_Lambert93_m","Y_Lambert93_m","Lat_WGS84","Lon_WGS84",
                                      "Altitude","Observateur","Experience")]
  
  #calcul des informations des listes
    # altitude moyenne
      alt_mean <- aggregate(Altitude ~ ID_liste, data = epoc.envi, FUN = mean) ; epoc.envi$Altitude <- NULL
      colnames(alt_mean)[2] <- "Altitude_moyenne"
      epoc.envi <- left_join(epoc.envi,alt_mean)
    # barycentre
      # Calcul du barycentre des observations par listes (proxy, position de l'observateur) ----
        bary.x <- aggregate(X_Lambert93_m ~ ID_liste, data=epoc.envi,mean) ; colnames(bary.x) <- c("ID_liste","X_barycentre_L93")
        bary.y <- aggregate(Y_Lambert93_m ~ ID_liste, data=epoc.envi,mean) ; colnames(bary.y) <- c("ID_liste","Y_barycentre_L93")
        
        bary <- left_join(bary.x,bary.y,by="ID_liste")
        
        epoc.envi <- left_join(epoc.envi,bary)
  
  epoc.bary <- which(duplicated(epoc.envi$ID_liste) == FALSE)
  epoc.bary <- epoc.envi[epoc.bary,]
  
  
  # formation de la table de communaute -- retrait des doublons
    epoc.oiso2 <- as.data.table(epoc.oiso)
    
    # Somme des abondance selon les methodes de detection apres filtrage des ID_liste & Nom d'espece --> les especes doublons d'une liste sont sommmees
    epoc.oiso2 <- epoc.oiso2[, lapply(.SD, sum), by = c("ID_liste","Observateur","Nom_espece","Nom_latin"), .SDcols = grep("Nb_|Abondance_brut", colnames(epoc.oiso2))]
    
    epoc.oiso2$Abondance <- epoc.oiso2$Abondance_brut - epoc.oiso2$Nb_vol
    epoc.oiso2 <- as.data.frame(epoc.oiso2)
  
  
    # write.table(x = epoc.oiso2, file = paste0(sub("/data","/output",getwd()),"/epoc_communaute.txt"),sep="\t",dec=","
    #             ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE) 
    # write.table(x = epoc.envi, file = paste0(sub("/data","/output",getwd()),"/epoc_environnement_observation.txt"),sep="\t",dec=","
    #             ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
    # write.table(x = epoc.envi.liste, file = paste0(sub("/data","/output",getwd()),"/epoc_environnement_liste.txt"),sep="\t",dec=","
    #             ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
  
    # save.image("C:/git/ODF/data/02_before_disk_writing.RData")
      
      write.table(x = epoc.oiso2, file = "C:/git/ODF/data/ODF_epoc_communaute.txt",sep="\t",dec=","
                               ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
      write.table(x = epoc.bary, file = "C:/git/ODF/data/ODF_epoc_barycentre.txt",sep="\t",dec=","
                  ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE) 
      write.table(x = epoc.filt7.court.in, file = "C:/git/ODF/data/ODF_epoc_observation.txt",sep="\t",dec=","
                  ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE) 
    
      
      
  
      
      
      
      
# testing ground (not run) ----
  















