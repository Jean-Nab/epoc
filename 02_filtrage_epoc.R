# chemin
setwd("C:/git/epoc/data")

# packages
  library(tidyverse)
  library(stringr)
  library(plyr)


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
            rq1 <- grep("EPOC",toupper(epoc[,"Remarque"]))
            
        # Filtrage du jeu de donnees afin d'avoir uniquement les observations ou la mention epoc est integrer
            comm <- grep("EPOC",toupper(epoc[,"Commentaire_de_la_liste"])) # detection des lignes contenant la mention epoc dans la colonne commentaire
            
            comm_rq <- union(comm,rq1) # concatenation des 2 vecteurs renseignant les positions sans prendre en compte les doublons
          
            epoc.filt1 <- epoc[comm_rq,] # formation du dtf filtre par le mentionnement du protocole epoc dans commmentaire/remarque
          
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
    
    tps_epoc <- which(epoc.filt2[,"Tps_ecoute"] >= 0.01 & epoc.filt2[,"Tps_ecoute"] <= 0.08)
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
        
# Retrait des especes doublons par liste -------
        
        id.liste <- unique(epoc.filt5$ID_liste) # boucle sur les id_listes
        
        epoc.clean <- data.frame() # formation d'un dtf receuillant uniquement les epoc ayant des doublons d'espece (apres un nettoyage)
        nb.doublons <- c() # initialisation d'un vecteur comptant le nombre de doublons + info sur le nombre de liste contenant les doublons
        
i <- 1
while(i <= length(id.liste)){ 
  dtf.tmp <- epoc.filt5[epoc.filt5$ID_liste == id.liste[i],] # formation d'un dtf temporaire contenant toutes les observations de la liste i
  logi <- duplicated(dtf.tmp[,"Nom_espece"]) # recherche de doublon
          
  if(TRUE %in% logi == TRUE){ # si presence de doublon
            
    loc.logi <- which(logi == TRUE) # detection des lignes d'especes doublons dans l'epoc
    dtf.tmp.clean <- aggregate(Nombre~Nom_espece,data=dtf.tmp,FUN=sum) # somme par nom d'especes differents
    dtf.tmp.clean2 <- merge(dtf.tmp[-loc.logi,],dtf.tmp.clean,all=T) # 2eme dtf temporaire contenant des toutes les observations (doublons/non-doublons)
            
    # nettoyage des especes doublons dans le dtf temporaire
            
    p <- duplicated(dtf.tmp.clean2$Nom_espece) # recherche des doublons de nom_d'espece
    p.tru <- which(p == TRUE) # rq : ligne k = esp A ; ligne k+1 = espece Abis
    nb.doublons <- append(nb.doublons,length(p.tru)) # rajout de la somme des doublons dans le vecteur (=> sum: donne le nombre de doublons totaux) & (=> length : le nombre de listes contant des doublons)
              
    dtf.tmp.clean3 <- dtf.tmp.clean2
              
    dtf.tmp.clean3[p.tru,3:ncol(dtf.tmp.clean2)] <- dtf.tmp.clean3[p.tru-1,3:ncol(dtf.tmp.clean2)]
    dtf.tmp.clean3 <- dtf.tmp.clean3[-(p.tru-1),]
              
    epoc.clean <- rbind(epoc.clean,dtf.tmp.clean3)
              
  }
  else{
    epoc.clean <- rbind(epoc.clean,dtf.tmp)
  }
          
    # nettoyage de la memoire
    rm(dtf.tmp)
          
    cat(i,"/ ",length(id.liste),"\n")
    i <- i+1
}
        
        epoc.filt5 <- epoc.clean
        
              # Enregistrement sur le disque
        write.table(x = epoc.filt5, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_5.txt"),sep="\t",dec=","
                    ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)

# Rajout des colonnes Diversite_liste et Abondance_liste servant d'indice pour qualifier la liste ----
    # Idee : travaille de decompte d'espece et d'abondance total pour une liste a la fois (detection des listes & isolement d'une liste ==> calcul des indices)
        epoc.filt5$Diversite_liste <- c(rep(0,nrow(epoc.filt5))) # formation d'une nouvelle colonne Diversite_liste (= nb d'espece specifique observe dans chaque liste)
        epoc.filt5$Abondance_liste <- c(rep(0,nrow(epoc.filt5))) # formation d'une nouvelle colonne Abondance_liste (= total du comptage de chaque liste)
        
        vec.ID <- unique(epoc.filt5$ID_liste) # vecteur regroupant les ID de listes
        
        # boucle de lecture des ID de listes 
        u <- 1
        while (u <= length(vec.ID)){
          
          # formation d'un tableau temporaire contenant des informations sur les identifiants de liste et les differentes especes
          dtf.tmp <- epoc.filt5[epoc.filt5$ID_liste == vec.ID[u],c("Nom_espece","ID_liste","Nombre")]       
          # calcul de la diversite (membre de droite) et ajout dans la colonne Diversite_liste pour toutes les lignes ayant un ID_liste identique 
          epoc.filt5[epoc.filt5$ID_liste == vec.ID[u],"Diversite_liste"] <- length(unique(dtf.tmp$Nom_espece)) 
          # calcul de l'abondance total par liste (somme realise sur un dtf temporaire regroupant toutes les observations d'une liste a la fois)
          epoc.filt5[epoc.filt5$ID_liste == vec.ID[u],"Abondance_liste"] <- sum(dtf.tmp$Nombre)
          
          
          cat(u,"/",length(vec.ID),"\n") # etat d'avancement de la boucle
          u <- u + 1 # incrementation de l'indice de lecture de boucle
        }

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
              
            # Enregistrement sur le disque
              write.table(x = epoc.filt6, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_6_court.txt"),sep="\t",dec=","
                          ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)

              
# Rajout de la colonne experience de l'observateur (Experience)
              # CONCEPT : Expérience = nombre d'EPOC realise par l'observateur avant l'EPOC etudie
              # ==> Regroupement des EPOC par observateur (subset par observateur)
              # ==> Besoin d'ordonne les EPOC selon le temps
              
              epoc.filt6.exp <- epoc.filt6[,c("Observateur","ID_liste","Jour","Mois","Annee","Date","Heure_de_debut","Minute_de_debut")] 
              
              i <- 1
              while(i <= nrow(epoc.filt6.exp)){
                
                
                # formation d'une nouvelle colonne regroupant toutes les informations temporelles
                epoc.filt6.exp[i,"date"] <- paste0(epoc.filt6.exp[i,"Jour"],"/",epoc.filt6.exp[i,"Mois"],"/",epoc.filt6.exp[i,"Annee"]," ",epoc.filt6.exp[i,"Heure_de_debut"],":",epoc.filt6.exp[i,"Minute_de_debut"])
                
                cat(i,"/ ",nrow(epoc.filt6.exp),"\n")
                i <- i+1
              }
              
              
              
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
              id.epoc <- unique(epoc.filt6$ID_liste)
              i <- 1
              while(i <= length(id.epoc)){
                
                epoc.filt6[epoc.filt6$ID_liste == id.epoc[i],"Experience"] <- dtf.exp[dtf.exp$ID_liste == id.epoc[i],"Experience"]
                
                cat(i,"/ ",length(id.epoc),"\n")
                i <- i+1
              }
# 6 bis : modification de la forme du tableau passage d'un format large a un format long ----
    epoc.filt6.long <- reshape(epoc.filt6, varying = c("Nb_pose","Nb_vol","Nb_audition","Nb_NA"),
                           v.names = "Nb_contact",
                           timevar = "Info_contact",
                           times = c("pose","en_vol","audition","na"),
                           direction="long")
    
    # Enregistrement sur le disque
      write.table(x = epoc.filt6.long, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_6_long.txt"),sep="\t",dec=","
                ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
        
# 7eme filtrage selon la periode ----
    # formation de 2 tableaux : 1 tableau avec des observations comprises entre le 1/03-31/07 et 1 tableau avec les observations en-dehors de cette periode
        long.in.period <- which(epoc.filt6.long$Jour >= 1 & epoc.filt6.long$Jour <= 31 & epoc.filt6.long$Mois >= 3 & epoc.filt6.long$Mois <= 7)
        court.in.period <- which(epoc.filt6$Jour >= 1 & epoc.filt6$Jour <= 31 & epoc.filt6$Mois >= 3 & epoc.filt6$Mois <= 7)
      
        epoc.filt7.long.in <- epoc.filt6.long[long.in.period,]
        epoc.filt7.long.out <- epoc.filt6.long[-long.in.period,]
        
        epoc.filt7.court.in <- epoc.filt6[court.in.period,]
        epoc.filt7.court.out <- epoc.filt6[-court.in.period,]
        
    # Enregistrement sur le disque
        write.table(x = epoc.filt7.long.in, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_7_long_in_period.txt"),sep="\t",dec=","
                    ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE) 
        write.table(x = epoc.filt7.long.out, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_7_long_out_period.txt"),sep="\t",dec=","
                    ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE) 
        
        
        write.table(x = epoc.filt7.court.in, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_7_court_in_period.txt"),sep="\t",dec=","
                    ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE) 
        write.table(x = epoc.filt7.court.out, file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_7_court_out_period.txt"),sep="\t",dec=","
                    ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE) 
        
        
# separation du jeu de donnees en 2 tableaux (var env/localisation et var communautes d'oiseaux) ----
  epoc.oiso <- epoc.filt7.court.in[,c("ID_liste","Observateur","Nom_espece","Nombre","Estimation","Nb_pose","Nb_vol","Nb_audition","Nb_NA")]
  epoc.envi <- epoc.filt7.court.in[,c("UUID","Ref","ID_liste","ID_Espece_Biolovision","Date","Jour","Mois","Annee","Jour_de_l_annee",
                                      "Pentade","Decade","numero_de_la_semaine","Horaire","Heure_debut","Heure_de_debut","Minute_de_debut","Heure_fin","Heure_de_fin",
                                      "Minute_de_fin","Liste_complete","Commentaire_de_la_liste","ID_Lieu_dit","Lieu_dit","Commune","Departement","Code_INSEE","Pays","Type_de_localisation",
                                      "X_Lambert_IIe_m","Y_Lambert_IIe_m","X_Lambert93_m","Y_Lambert93_m","Lat_WGS84","Lon_WGS84","latitude_DMS","longitude_DMS",
                                      "Maille","Altitude","Details","Code_atlas","Remarque","Remarque_privee","Nom","Prenom","Observateur","Experience","Tps_ecoute",
                                      "Diversite_liste","Abondance_liste")]
  epoc.envi.liste <- which(duplicated(epoc.envi$ID_liste) == FALSE)
  epoc.envi.liste <- epoc.envi[epoc.envi.liste,]
  
  
  # homogeneisation
    # retrait des especes doublons dans les listes du dtf de communautes
      epoc.oiso2 <- aggregate(Nombre ~ ID_liste + Observateur + Nom_espece + Estimation + Nb_pose + Nb_vol + Nb_audition,data=epoc.oiso,FUN=sum)
      epoc.oiso2 <- epoc.oiso2[order(epoc.oiso2$ID_liste),]
      epoc.oiso2$Nb_NA <- epoc.oiso2$Nombre - (epoc.oiso2$Nb_pose + epoc.oiso2$Nb_vol + epoc.oiso2$Nb_audition)
  
  
    write.table(x = epoc.oiso2, file = paste0(sub("/data","/output",getwd()),"/epoc_communaute.txt"),sep="\t",dec=","
                ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE) 
    write.table(x = epoc.envi, file = paste0(sub("/data","/output",getwd()),"/epoc_environnement_observation.txt"),sep="\t",dec=","
                ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
    write.table(x = epoc.envi.liste, file = paste0(sub("/data","/output",getwd()),"/epoc_environnement_liste.txt"),sep="\t",dec=","
                ,fileEncoding = "UTF-8", row.names = FALSE, quote=FALSE)
  
      
# testing ground (not run) ----
        
    test6a <- epoc.filt6[1:5000,]
    test6b <- reshape(test6a, varying = c("Nb_pose","Nb_vol","Nb_audition","Nb_NA"),
                      v.names = "Nb_contact",
                      timevar = "Info_contact",
                      times = c("pose","en_vol","audition","na"),
                      direction="long")
    
    head(test6b[test6b$ID_liste == 580355,c("Nom_espece","Info_contact","Nb_contact","id","ID_liste","Details","Nombre")],200)
        
        
        
        
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















