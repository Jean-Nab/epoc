# chemin
  setwd("C:/git/epoc/data")
# packages
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(raster)
  library(scales)
  library(reshape2)
  

# load des data
  load("C:/git/epoc/04bis_save3.RData") 

  loc.obs <- epoc.envi.obs[,c("Ref","Nom_espece","Nom_latin","ID_liste","X_Lambert93_m","Y_Lambert93_m")]
  loc.obs_sf <- st_as_sf(loc.obs,
                         coords = c("X_Lambert93_m","Y_Lambert93_m"),
                         crs=2154) # sf des observations
  
  dist.bary_obs <- plyr::join(loc.obs[,c("Ref","Nom_espece","Nom_latin","ID_liste")],
                              bary.reg[,c("ID_liste","X_barycentre_L93","Y_barycentre_L93")])
  dist.bary_obs_sf <- st_as_sf(dist.bary_obs,coords = c("X_barycentre_L93","Y_barycentre_L93"),
                               crs=2154)
  
  
  dist.observation <- as.numeric(st_distance(x = loc.obs_sf,
                                             y = dist.bary_obs_sf,
                                             by_element = TRUE))
  
  loc.obs$Distance_observation_m <- dist.observation
  
  # add de l'information à epoc.envi.obs
    epoc.envi.obs <- left_join(epoc.envi.obs,
                               loc.obs[,c("Ref","Distance_observation_m")])
  # selection de qqs genres typiquement retrouve proche de l'observateur ----
        sp.latin <- grep(pattern = "Sylvia|Parus|Phylloscopus|Troglodytes|Prunella|Aegithalos|Passer|Regulus|Fringilla|Carduelis"
                         ,x=epoc.envi.obs$Nom_latin)
        test <- epoc.envi.obs[sp.latin,]
    

  # exploration des data ----
    dist.list <- aggregate(Distance_observation_m ~ ID_liste,
                           data=epoc.envi.obs,
                           FUN=mean)
    cat("Nb liste w/ distance nulle :",length(which(dist.list$Distance_observation_m == 0)),"\n",
        "Nb liste w/ distance > 1km :",length(which(dist.list$Distance_observation_m >1000 )))
    quantile(dist.list$Distance_observation_m,c(c(1:3)*0.1,
                                                #c(31:39)*0.01,
                                                c(4:9)*0.1,
                                                c(91:100)*0.01))

    
    dist.espece <- aggregate(Distance_observation_m ~ Nom_espece,
                             data=epoc.envi.obs,
                             FUN=mean)
    
    
  # selection d'un ensemble de donnees pour le distance sampling (rm des distances nulles et > 1km) -----
    id.list.dist <- which(dist.list$Distance_observation_m < 0.001 | dist.list$Distance_observation_m >1000)
    id.list.dist <- dist.list[-id.list.dist,"ID_liste"]

    epoc.envi.obs$Use_distance_sampling <- epoc.envi.obs$ID_liste %in% id.list.dist

  # subsetting du jeu de donnee pour l'exploration vav du DS
    epoc.envi.obs.DS <- epoc.envi.obs[which(epoc.envi.obs$Use_distance_sampling == T),]
    
    
  # mesure de la variance selon le type d'habitats lors de l'EPOC -----
    # load de la couche raster
      occ.sol <- raster("C:/git/epoc/data/OCS_2018_CESBIO.tif")
    
    # determination de l'habitat de la liste (extract du raster)
      tabl.dist.list <- bary.reg[bary.reg$ID_liste %in% id.list.dist,c("ID_liste","X_barycentre_L93","Y_barycentre_L93")] # selection des listes compatible au DS
      
      tabl.dist.list <- left_join(tabl.dist.list,unique(epoc.envi.obs.DS[,c("ID_liste","Departement")])) # add de l'information des departements -> boucle pour l'attribution des categories d'habitats
      
      tabl.dist.list_sf <- st_transform(st_as_sf(tabl.dist.list, 
                                                 coords = c("X_barycentre_L93","Y_barycentre_L93"),
                                                 crs=2154),
                                        crs=crs(occ.sol)) # conversion en sf selon le crs de la couche occupation des sols
      
    # extract : manipulation des cellules de l'extract ------
      
      tabl.dist.list_sf$ID <- c(rep(1:nrow(tabl.dist.list_sf)))
      
      beginCluster()
      tabl.dist.list.cate.habitat <- extract(x = occ.sol, 
                                             y = st_buffer(tabl.dist.list_sf,
                                                           dist = 50),
                                             df=T,
                                             along=T)
      endCluster()
      
      # regroupement des habitats en categories d'habitats (1:bati / 2:ouvert / 3:foret)
      tabl.dist.list.cate.habitat$habitats <- ifelse(tabl.dist.list.cate.habitat$OCS_2018_CESBIO %in% c(1,2,3,4),
                                                     "Batis", ifelse(tabl.dist.list.cate.habitat$OCS_2018_CESBIO %in% c(16,17),
                                                               "Foret","Ouvert")
                                                     )
 
    # regroupement selon la categories majoritaire observée par buffer
      freq.habitat.list <- group_by(tabl.dist.list.cate.habitat[,c("ID","habitats")],ID) %>% # decompte du nombre de cellule selon leur categorie d'habitats sur chaque buffer
        plyr::count() %>%
        group_by(ID)
      
      max.habitat.list <- group_by(tabl.dist.list.cate.habitat[,c("ID","habitats")],ID) %>% # detection de l'habitat majoritaire a chaque buffer (warning : pour les egalite)
        plyr::count() %>%
        group_by(ID) %>%
        summarise(freq = max(freq))
      
      tabl.dist.list.cate.habitat2 <- left_join(max.habitat.list,freq.habitat.list)
      tabl.dist.list.cate.habitat2 <- tabl.dist.list.cate.habitat2[!(duplicated(tabl.dist.list.cate.habitat2$ID)),] # vs probleme d'egalite (-> legere surrepresentation del'habitat 2, 53 egalites)
      
      # harmonisation
      colnames(tabl.dist.list.cate.habitat2)[3] <- "Categories_habitats"
      
      
    # join au dtf tabl.dist.list_sf -> informations sur les ID_listes
      tabl.dist.list_sf <- left_join(tabl.dist.list_sf,tabl.dist.list.cate.habitat2[,c("ID","Categories_habitats")])
      
      
    # join w/ dtf regroupant l'ensemble des informations selon les observations ----
      epoc.envi.obs.DS <- left_join(epoc.envi.obs.DS,
                                    st_drop_geometry(tabl.dist.list_sf[,c("ID_liste","Categories_habitats")]))
      
  # sauvegarde 2 ----
    load("C:/git/epoc/07_save1.RData")
      
      
    # variation distance selon la catégories d'habitats
      epoc.envi.obs.DS$Presence <- c(rep(1,nrow(epoc.envi.obs.DS))) # nvlle colonne pour calculer le nb d'occurence de 'lobservation d'une espece selon l'habitat
      
    # calcul de la mediane de la distance observation-observateurs ----
      tabl.dist.esp <- aggregate(Distance_observation_m ~ Nom_espece + Nom_latin + Categories_habitats,
                                 data=epoc.envi.obs.DS,
                                 quantile, 
                                 c(0.5))
      tabl.dist.esp <- tidyr::spread(tabl.dist.esp, Categories_habitats, Distance_observation_m)
      
      # harmonisation
        colnames(tabl.dist.esp)[grep("Batis",colnames(tabl.dist.esp))] <- "Distance_mediane_Batis"
        colnames(tabl.dist.esp)[grep("Ouvert",colnames(tabl.dist.esp))] <- "Distance_mediane_Ouvert"
        colnames(tabl.dist.esp)[grep("Foret",colnames(tabl.dist.esp))] <- "Distance_mediane_Foret"
        
    # calcul des 95% de la distance observation-observateurs ----
      tabl.dist.esp.95 <- aggregate(Distance_observation_m ~ Nom_espece + Nom_latin + Categories_habitats,
                                   data=epoc.envi.obs.DS,
                                   quantile, 
                                   c(0.95))
      tabl.dist.esp.95 <- tidyr::spread(tabl.dist.esp.95, Categories_habitats, Distance_observation_m)
      colnames(tabl.dist.esp.95)[3:ncol(tabl.dist.esp.95)] <- paste(colnames(tabl.dist.esp.95)[3:ncol(tabl.dist.esp.95)],"X",sep="")
      
      # ajout de la distance de 95% des observations au dtf contenant l'information sur la mediane
        tabl.dist.esp <- left_join(tabl.dist.esp,tabl.dist.esp.95)
        
      # harmonisation
        colnames(tabl.dist.esp)[grep("BatisX",colnames(tabl.dist.esp))] <- "Distance_95_Batis"
        colnames(tabl.dist.esp)[grep("OuvertX",colnames(tabl.dist.esp))] <- "Distance_95_Ouvert"
        colnames(tabl.dist.esp)[grep("ForetX",colnames(tabl.dist.esp))] <- "Distance_95_Foret"
      
      
      
    # calcul du nombre d'occurence d'observation par categories d'habitat----
      tabl.dist.esp.nb.occ <- aggregate(Presence ~ Nom_espece + Nom_latin + Categories_habitats,
                                        data=epoc.envi.obs.DS,
                                        sum)
      tabl.dist.esp.nb.occ <- tidyr::spread(tabl.dist.esp.nb.occ, Categories_habitats, Presence)
      colnames(tabl.dist.esp.nb.occ)[3:ncol(tabl.dist.esp.nb.occ)] <- paste(colnames(tabl.dist.esp.nb.occ)[3:ncol(tabl.dist.esp.nb.occ)],"X",sep="")
        
      # ajout du nombre d'occurence d'observation au dtf globalisant l'information par habitat
        tabl.dist.esp <- left_join(tabl.dist.esp,tabl.dist.esp.nb.occ)
      
      # harmonisation
        colnames(tabl.dist.esp)[grep("BatisX",colnames(tabl.dist.esp))] <- "Nb_occ_habitat_Batis"
        colnames(tabl.dist.esp)[grep("OuvertX",colnames(tabl.dist.esp))] <- "Nb_occ_habitat_Ouvert"
        colnames(tabl.dist.esp)[grep("ForetX",colnames(tabl.dist.esp))] <- "Nb_occ_habitat_Foret"
    
      tabl.dist.esp$Nb_occ_total <- rowSums(tabl.dist.esp[,(ncol(tabl.dist.esp)-2):ncol(tabl.dist.esp)],na.rm = T)
    
    # récuperation des abondances selon differentes classes de distances (25 ; 100 ; 200 ; 200-1000) -----
    # récupération par liste uniquement (vs double comptage)
    # IDEE : st_within de buffer autour des barycentres
        loc.obs.DS <- loc.obs[loc.obs$ID_liste %in% id.list.dist,]      # localisation des obs compatible avec le DS (use de id.list.dist pour selectionner les bonnes listes)
        loc.obs.DS_sf <- st_as_sf(loc.obs.DS,
                                  coords = c("X_Lambert93_m","Y_Lambert93_m"),
                                  crs=2154)
    
    # boucle sur les listes -----
      i <- 1
      while(i <= length(id.list.dist)){
        
        # evaluation de la presence de l'observation dans un rayon de 25 m autour du barycentre de la liste
          obs.in.bary.buff.25 <- st_contains(y = loc.obs.DS_sf[which(loc.obs.DS_sf$ID_liste == id.list.dist[i]),],
                                             x = st_buffer(tabl.dist.list_sf[which(tabl.dist.list_sf$ID_liste == id.list.dist[i]),],
                                                           dist = 25),
                                             sparse = FALSE)
          loc.obs.DS[which(loc.obs.DS$ID_liste == id.list.dist[i]),"Within_buff_25"] <- as.vector(obs.in.bary.buff.25)
          
        # evaluation de la presence de l'observation dans un rayon de 100 m autour du barycentre de la liste
          obs.in.bary.buff.100 <- st_contains(y = loc.obs.DS_sf[which(loc.obs.DS_sf$ID_liste == id.list.dist[i]),],
                                             x = st_buffer(tabl.dist.list_sf[which(tabl.dist.list_sf$ID_liste == id.list.dist[i]),],
                                                           dist = 100),
                                             sparse = FALSE)
          loc.obs.DS[which(loc.obs.DS$ID_liste == id.list.dist[i]),"Within_buff_100"] <- as.vector(obs.in.bary.buff.100)
          
        # evaluation de la presence de l'observation dans un rayon de 200 m autour du barycentre de la liste
          obs.in.bary.buff.200 <- st_contains(y = loc.obs.DS_sf[which(loc.obs.DS_sf$ID_liste == id.list.dist[i]),],
                                             x = st_buffer(tabl.dist.list_sf[which(tabl.dist.list_sf$ID_liste == id.list.dist[i]),],
                                                           dist = 200),
                                             sparse = FALSE)
          loc.obs.DS[which(loc.obs.DS$ID_liste == id.list.dist[i]),"Within_buff_200"] <- as.vector(obs.in.bary.buff.200)
          
        # evaluation de la presence de l'observation dans un rayon de 1000 m autour du barycentre de la liste
          obs.in.bary.buff.1000 <- st_contains(y = loc.obs.DS_sf[which(loc.obs.DS_sf$ID_liste == id.list.dist[i]),],
                                             x = st_buffer(tabl.dist.list_sf[which(tabl.dist.list_sf$ID_liste == id.list.dist[i]),],
                                                           dist = 1000),
                                             sparse = FALSE)
          loc.obs.DS[which(loc.obs.DS$ID_liste == id.list.dist[i]),"Within_buff_1000"] <- as.vector(obs.in.bary.buff.1000)
        
        
        
        cat(i,"/",length(id.list.dist),"\n")
        i <- i +1
      }
        
    # join des indicateurs logique de presence dans les buffer (25;100;200;200+) au dtf epoc.envi.obs.DS
      epoc.envi.obs.DS <- left_join(epoc.envi.obs.DS,loc.obs.DS[,c("Ref","Within_buff_25","Within_buff_100",
                                                                   "Within_buff_200","Within_buff_1000")],
                                    by="Ref")
    
  # sauvegarde 2 -----
    load("C:/git/epoc/07_save2.RData")

      
    # calcul de l'abondance observée d'une espece selon la classe de distance et la catégories d'habitats -----
      # determination de l'utilisation de l'abondance observée selon sa présence/absence dans le buffer ----
        # buffer 25 m 
          epoc.envi.obs.DS$Abondance_buffer_25_m <- 0
          obs.in.25.m <- which(epoc.envi.obs.DS$Within_buff_25 == TRUE)      
          epoc.envi.obs.DS[obs.in.25.m,"Abondance_buffer_25_m"] <- epoc.envi.obs.DS[obs.in.25.m,"Abondance"]      
      
        # buffer 100 m 
          epoc.envi.obs.DS$Abondance_buffer_100_m <- 0
          obs.in.100.m <- which(epoc.envi.obs.DS$Within_buff_100 == TRUE)      
          epoc.envi.obs.DS[obs.in.100.m,"Abondance_buffer_100_m"] <- epoc.envi.obs.DS[obs.in.100.m,"Abondance"] 
      
        # buffer 200 m 
          epoc.envi.obs.DS$Abondance_buffer_200_m <- 0
          obs.in.200.m <- which(epoc.envi.obs.DS$Within_buff_200 == TRUE)      
          epoc.envi.obs.DS[obs.in.200.m,"Abondance_buffer_200_m"] <- epoc.envi.obs.DS[obs.in.200.m,"Abondance"] 
      
        # buffer 1000 m 
          epoc.envi.obs.DS$Abondance_buffer_1000_m <- 0
          obs.in.1000.m <- which(epoc.envi.obs.DS$Within_buff_1000 == TRUE)      
          epoc.envi.obs.DS[obs.in.1000.m,"Abondance_buffer_1000_m"] <- epoc.envi.obs.DS[obs.in.1000.m,"Abondance"] 
      
          
      # aggregation de l'abondance au differentes classes de distance selon l'espece et la categorie de l'habitat par liste -----
        # 25 m 
          class.dist.25m <- aggregate(Abondance_buffer_25_m ~ Nom_espece + Nom_latin + Categories_habitats,
                                      data= epoc.envi.obs.DS,
                                      FUN = sum)
        # 100 m 
          class.dist.100m <- aggregate(Abondance_buffer_100_m ~ Nom_espece + Nom_latin  + Categories_habitats,
                                      data= epoc.envi.obs.DS,
                                      FUN = sum)
        # 200 m 
          class.dist.200m <- aggregate(Abondance_buffer_200_m ~ Nom_espece + Nom_latin  + Categories_habitats,
                                      data= epoc.envi.obs.DS,
                                      FUN = sum)
        # 1000 m 
          class.dist.1000m <- aggregate(Abondance_buffer_1000_m ~ Nom_espece + Nom_latin  + Categories_habitats,
                                      data= epoc.envi.obs.DS,
                                      FUN = sum)
          
      # aggregation de l'abondance sans distinction des categories d'habitats -----
          class.dist.25m.merged <- aggregate(Abondance_buffer_25_m ~ Nom_espece + Nom_latin,
                                             data= epoc.envi.obs.DS,
                                             FUN = sum)
          # 100 m 
          class.dist.100m.merged <- aggregate(Abondance_buffer_100_m ~ Nom_espece + Nom_latin,
                                              data= epoc.envi.obs.DS,
                                              FUN = sum)
          # 200 m 
          class.dist.200m.merged <- aggregate(Abondance_buffer_200_m ~ Nom_espece + Nom_latin,
                                              data= epoc.envi.obs.DS,
                                              FUN = sum)
          # 1000 m 
          class.dist.1000m.merged <- aggregate(Abondance_buffer_1000_m ~ Nom_espece + Nom_latin,
                                               data= epoc.envi.obs.DS,
                                               FUN = sum)
          
        # regroupement sur un dtf ----
          # avec distinction des categories d'habitats
            class.dist.all <- left_join(class.dist.25m,class.dist.100m)
            class.dist.all <- left_join(class.dist.all,class.dist.200m)
            class.dist.all <- left_join(class.dist.all,class.dist.1000m)
            
          # sans distinction des categories d'habitats
            class.dist.all.merged <- left_join(class.dist.25m.merged,class.dist.100m.merged)
            class.dist.all.merged <- left_join(class.dist.all.merged,class.dist.200m.merged)
            class.dist.all.merged <- left_join(class.dist.all.merged,class.dist.1000m.merged)
            class.dist.all.merged$Categories_habitats <- "All"

          class.dist.all <- rbind(class.dist.all,class.dist.all.merged)
          
      # calcul des ratios d'abondances observés entre 2 classes ----
        class.dist.all$Ratio_abondance_25_100 <- class.dist.all$Abondance_buffer_25_m / class.dist.all$Abondance_buffer_100_m
        class.dist.all$Ratio_abondance_25_200 <- class.dist.all$Abondance_buffer_25_m / class.dist.all$Abondance_buffer_200_m
        class.dist.all$Ratio_abondance_25_1000 <- class.dist.all$Abondance_buffer_25_m /class.dist.all$Abondance_buffer_1000_m
      
        # gestion des NAs
          for(n in grep("Ratio",colnames(class.dist.all))){
            class.dist.all[is.na(class.dist.all[n]),n] <- 0
          }
          
    # estimation de la probabilité de détection ----
      # load de la fonction R.Lorriliere & R. Julliard ----
        ##' .. Estimation à partir du ratio entre observation réalisé dans deux classes de distance ..
        ##' @title Estimation de la probabilité de détection
        ##' @param propN vecteur numérique du ratio d'abondances observées entre deux classes de distance propN = N[0..x1]/(N[0..x1] + N[x1..x2]) avec x1<x2
        ##' @param propR la proportion des rayons des deux classes de distances propR = R[0..x1]/R[0..x2] avec x1 < x2
        ##' @return une estimation de la probablité de detection dans la classe de distance x1..x2
        proba_detec <- function(propN,propR) {
          x <- 1/propN
          min_val <- 1
          max_val <- 1/(propR^2)
          return((x-min_val)/(max_val-min_val))
        }
        
      # calcul de la proba de detection selon les classes de distances ----
        class.dist.all$Prob_detection_25_100 <- proba_detec(propN = class.dist.all$Ratio_abondance_25_100,
                                                            propR = 25/100)
        
        class.dist.all$Prob_detection_25_200 <- proba_detec(propN = class.dist.all$Ratio_abondance_25_200,
                                                            propR = 25/200)
        
        class.dist.all$Prob_detection_25_1000 <- proba_detec(propN = class.dist.all$Ratio_abondance_25_1000,
                                                            propR = 25/1000)
        
    # Focus sur les oiseaux communs (bcp d'oiseaux w/ fortes variations d'abondance entre les classes de distances) ----
      class.dist.all <- left_join(class.dist.all,unique(oiso.reg.all[,c("Nom_espece","Nom_latin","communs")]))
      class.dist.all[is.na(class.dist.all$communs),"communs"] <- 0 
      
      class.dist.all.communs <- subset(class.dist.all,communs == 1) # warning : au especes communes ayant bcp d'observations non compatibles au DS
      
      
  # visualisation des données ------
    # table des annotations (cf. tabl.esp) ------
      tabl.annot.sp <- tabl.dist.esp[,c("Nom_latin","Nom_espece","Nb_occ_habitat_Batis","Nb_occ_habitat_Ouvert","Nb_occ_habitat_Foret")]
      
      # Gestion des NAs
        for(n in grep("occ",colnames(tabl.annot.sp))){
          tabl.annot.sp[is.na(tabl.annot.sp[n]),n] <- 0
        }
      
      i <- 1
      while(i <= nrow(tabl.annot.sp)){
        
        annot.bat <- paste("Nb occ Batis : ",tabl.annot.sp[i,"Nb_occ_habitat_Batis"],sep="")
        annot.ouv <- paste("Nb occ Ouvert : ",tabl.annot.sp[i,"Nb_occ_habitat_Ouvert"],sep="")
        annot.for <- paste("Nb occ Foret : ",tabl.annot.sp[i,"Nb_occ_habitat_Foret"],sep="")
        
        annot.row <- paste(c(annot.bat,"\n",annot.ouv,"\n",annot.for),collapse = "")
        
        tabl.annot.sp[i,"Annotation"] <- annot.row
        
        
        i <- i +1
        cat(i,"/",nrow(tabl.annot.sp),"\n")
      }

      
    # Variation de la distance d'observation selon la catégories d'habitats par espece (ttes especes comprises) ------
      epoc.envi.obs.DS$Nom_latin <- as.character(epoc.envi.obs.DS$Nom_latin)
      
      # nettoyage des especes/observations pour la representation graphique
        epoc.envi.obs.DS.graph <- epoc.envi.obs.DS[which(epoc.envi.obs.DS$Distance_observation_m <= 1000),] # retrait des individus observe à plus de 1km
        epoc.envi.obs.DS.graph <- left_join(epoc.envi.obs.DS.graph,class.dist.all[,c("Nom_espece","Nom_latin","communs")])
        
        
        sp.for.graph <- as.character(tabl.dist.esp[which(tabl.dist.esp$Nb_occ_total >= 25),"Nom_latin"]) # representation des especes avec au moins 25 occurrences d'observations
        epoc.envi.obs.DS.graph <- epoc.envi.obs.DS.graph[epoc.envi.obs.DS.graph$Nom_latin %in% sp.for.graph,]
      
      # Preparation a la boucle des graphs -----
        vec.name <- sort(unique(epoc.envi.obs.DS.graph$Nom_latin)) # ordonne les nom latins 
        vec.name.pos <- sort(unique(seq(1,length(vec.name),15))) # vecteur de position (formation de groupe de 20 en 20 pour le plot)
        count <- 0 # differenciation des plots 
      
      for(i in vec.name.pos){
        count <- count + 1
        
        vec.sp <- vec.name[i:min((i+14),length(vec.name))]
        
        # subset des donnees
          dtf.graph <- subset(epoc.envi.obs.DS.graph, Nom_latin %in% vec.sp)
          annot.graph <- subset(tabl.annot.sp, Nom_latin %in% vec.sp)
          
        # plot
          graph.dist <- ggplot(dtf.graph) +
            geom_density(aes(x = Distance_observation_m,                        # graph de densite
                             fill = Categories_habitats),alpha=0.45) +
            xlab("Distances d'observations des individus (en m)") +
            scale_fill_manual(values = c("red","green4","lightgoldenrod3")) +
            scale_x_continuous(breaks = pretty_breaks()) +
            xlim(0,1000) +
            geom_text(data=annot.graph,                                         # encadre d'annotations
                      aes(x = Inf, y = Inf, hjust = 1.05, vjust = 1.05,
                          label = Annotation),
                      size=2.5) +
            
            theme(legend.background = element_rect(fill = "ivory2",             # design encadre de legende
                                                   size = 0.5, 
                                                   linetype = "solid",
                                                   colour = "sienna4")) +
            
            facet_wrap(.~ Nom_latin,scales="free",ncol=3)
        
        # sauvegarde
          ggsave(paste0("C:/git/epoc/output/graphs/",
                        "Variation_distance_espece_",count,".png"),
                 width = 30, height = 30,units = "cm")
      }
      
      
    # Courbe d'accumulation des abondances en fonction de la distance au barycentre -----
      # Calcul de la somme cumulée des abondances selon la distance d'observation ----
        # en prenant en compte les catégories d'habitats
          tabl.somm.cum <- subset(epoc.envi.obs.DS.graph[,c("Nom_latin","Abondance","Categories_habitats","Distance_observation_m","communs")], communs == 1) %>%
            arrange(Distance_observation_m) %>%
            group_by(Categories_habitats,Nom_latin) %>%
            mutate(Somme_cumulee_habitats = cumsum(Abondance))
          
          tabl.somm.cum <- tabl.somm.cum %>%
            group_by(Categories_habitats,Nom_latin) %>%
            mutate(Max_somme_cumulee_habitat = max(Somme_cumulee_habitats))
        
          tabl.somm.cum$Max_somme_cumulee_habitat_part <- 100*(tabl.somm.cum$Somme_cumulee_habitats / 
                                                                 tabl.somm.cum$Max_somme_cumulee_habitat)
          
        # sans prendre en compte les catégories d'habitats
          tabl.somm.cum.merged <- subset(epoc.envi.obs.DS.graph[,c("Nom_latin","Abondance","Distance_observation_m","communs")], communs == 1) %>%
            arrange(Distance_observation_m) %>%
            group_by(Nom_latin) %>%
            mutate(Somme_cumulee_habitats = cumsum(Abondance))
          
          tabl.somm.cum.merged <- tabl.somm.cum.merged %>%
            group_by(Nom_latin) %>%
            mutate(Max_somme_cumulee_habitat = max(Somme_cumulee_habitats))
          
          tabl.somm.cum.merged$Max_somme_cumulee_habitat_part <- 100*(tabl.somm.cum.merged$Somme_cumulee_habitats / 
                                                                 tabl.somm.cum.merged$Max_somme_cumulee_habitat)
          tabl.somm.cum.merged$Categories_habitats <- "All"
          
        # rbind des 2 ddtfs
          tabl.somm.cum.all <- rbind(tabl.somm.cum,tabl.somm.cum.merged) # add .all -> change pour le dtf d'intersects
        
      
      # Dtf des intersects ----
        # 25m
          tabl.intersect.25.tmp <- tabl.somm.cum.all[which(tabl.somm.cum.all$Distance_observation_m >= 25),]
          tabl.intersect.25.tmp$duplicated <- duplicated(tabl.intersect.25.tmp[,c("Nom_latin","Categories_habitats")])
          
          tabl.intersect.25.tmp <- subset(tabl.intersect.25.tmp, duplicated == F)
          colnames(tabl.intersect.25.tmp)[grep("_part",colnames(tabl.intersect.25.tmp))] <- "Abondance_cumulee_25m_%"
          
          tabl.intersect.25.tmp <- tabl.intersect.25.tmp[,c(1,3,5,8)]
          
        # 100m
          tabl.intersect.100.tmp <- tabl.somm.cum.all[which(tabl.somm.cum.all$Distance_observation_m >= 100),]
          tabl.intersect.100.tmp$duplicated <- duplicated(tabl.intersect.100.tmp[,c("Nom_latin","Categories_habitats")])
          
          tabl.intersect.100.tmp <- subset(tabl.intersect.100.tmp, duplicated == F)
          colnames(tabl.intersect.100.tmp)[grep("_part",colnames(tabl.intersect.100.tmp))] <- "Abondance_cumulee_100m_%"
          
          tabl.intersect.100.tmp <- tabl.intersect.100.tmp[,c(1,3,5,8)]
        
        # 200m
          tabl.intersect.200.tmp <- tabl.somm.cum.all[which(tabl.somm.cum.all$Distance_observation_m >= 200),]
          tabl.intersect.200.tmp$duplicated <- duplicated(tabl.intersect.200.tmp[,c("Nom_latin","Categories_habitats")])
          
          tabl.intersect.200.tmp <- subset(tabl.intersect.200.tmp, duplicated == F)
          colnames(tabl.intersect.200.tmp)[grep("_part",colnames(tabl.intersect.200.tmp))] <- "Abondance_cumulee_200m_%"
        
          tabl.intersect.200.tmp <- tabl.intersect.200.tmp[,c(1,3,5,8)]
        
        # join des dtf
          tabl.intersect.all <- left_join(tabl.intersect.25.tmp,tabl.intersect.100.tmp)
          tabl.intersect.all <- left_join(tabl.intersect.all,tabl.intersect.200.tmp)
          
        # Determination distance de prospection de 95% de la population (Somme cumulee - tabl.somm.cum.all --> add result to tabl.intersect.all) -----  
          tabl.intersect.95.pop <- tabl.somm.cum.all[which(tabl.somm.cum.all$Max_somme_cumulee_habitat_part >= 95),]
          tabl.intersect.95.pop$duplicated <- duplicated(tabl.intersect.95.pop[,c("Nom_latin","Categories_habitats")])    
          
          tabl.intersect.95.pop <- subset(tabl.intersect.95.pop, duplicated == F)
          colnames(tabl.intersect.95.pop)[grep("observation",colnames(tabl.intersect.95.pop))] <- "Distance_95_prospectee"
          
          tabl.intersect.95.pop <- tabl.intersect.95.pop[,c(1,3,4,5,8)]
          
          # add des informations a la table d'intersection generale
            tabl.intersect.all <- left_join(tabl.intersect.all,tabl.intersect.95.pop[,1:(ncol(tabl.intersect.95.pop)-1)])
          
          
        write.csv(x = tabl.intersect.all, file="C:/git/epoc/output/Somme_cumulee_intersection_buffer.csv")
          
      # Preparation a la boucle des graphs -----
        vec.name <- sort(unique(tabl.somm.cum$Nom_latin)) # ordonne les nom latins 
        vec.name.pos <- sort(unique(seq(1,length(vec.name),12))) # vecteur de position (formation de groupe de 20 en 20 pour le plot)
        count <- 0
      
      # boucle des graphs ----
        for(i in vec.name.pos){
          count <- count + 1
          
          vec.sp <- vec.name[i:min((i+11),length(vec.name))]
          
          # subset des donnees
            dtf.graph <- subset(tabl.somm.cum, Nom_latin %in% vec.sp)
            dtf.graph2 <- subset(tabl.somm.cum.merged, Nom_latin %in% vec.sp)
            dtf.graph3 <- subset(tabl.intersect.all, Nom_latin %in% vec.sp)
            annot.graph <- subset(tabl.annot.sp, Nom_latin %in% vec.sp)
          
          # plot -----
            graph.cumu <- ggplot(dtf.graph,aes(x = Distance_observation_m,                         # courbes d'accumulation
                                               y = Max_somme_cumulee_habitat_part)) +
              geom_step(aes(colour = Categories_habitats),
                        direction = "hv",
                        linetype = 1,
                        size = 0.75,
                        alpha= 0.60) +
              
              geom_step(data=dtf.graph2,aes(x = Distance_observation_m,                       # courbes d'accumulation
                                            y = Max_somme_cumulee_habitat_part,
                                            colour = Categories_habitats),
                        direction = "hv",
                        linetype = 4,
                        size = 0.75,
                        alpha= 0.5) +
        
              scale_colour_manual(values = c("magenta2","red","green4","darkgoldenrod3")) +  # couleurs des courbes

              
              geom_vline(xintercept = c(25,100,200), linetype = "dashed",         # indication des buffer
                         color = "grey38" , size = 0.5, alpha = 0.25) +
              geom_hline(yintercept = 100, linetype = "dashed",                   # indication de la limite à 100 %
                         color = "lightcoral" , size = 1, alpha = 0.35) +
              
              geom_vline(data=dtf.graph3,aes(xintercept = Distance_95_prospectee,
                                             colour = Categories_habitats),
                         size = 0.25,
                         alpha = 0.75) + # a modif'
              
              xlab("Distances d'observations des individus (en m)") +
              ylab("Part de l'abondance observée (en %)") +
              
              geom_text(data=annot.graph,                                         # encadre d'annotations
                        aes(x = Inf, y = -Inf, hjust = 1.05, vjust = -0.1,
                            label = Annotation),
                        size=2.5) +
              
              theme(legend.background = element_rect(fill = "ivory2",             # design encadre de legende
                                                     size = 0.5, 
                                                     linetype = "solid",
                                                     colour = "sienna4")) +
              
              facet_wrap(.~ Nom_latin,scales="free",ncol=4)
          
          # sauvegarde
            ggsave(paste0("C:/git/epoc/output/graphs/",
                          "Courbe_accumulation_espece_communes_",count,".png"),
                   width = 30, height = 30,units = "cm")
        }
      
          
    # Evolution de la proba de detection en fonction du rayon du buffer selon les categories d'habitats ----
      # Récuperation de l'information a partir du dtf class.dist.all.communs ----
        tabl.prob.dist.communs <- class.dist.all.communs[,c(1:3,grep("Prob",colnames(class.dist.all.communs)))]
          
        tabl.prob.dist.communs <- melt(tabl.prob.dist.communs, id=c("Nom_latin","Nom_espece","Categories_habitats"))  # conversion du dtf wide --> long (recuperation de la proba de detection par buffer)
        colnames(tabl.prob.dist.communs)[(ncol(tabl.prob.dist.communs)-1):ncol(tabl.prob.dist.communs)] <- c("Distance_buffer","Estimation_proba_detection")       
        
        tabl.prob.dist.communs$Distance_buffer <- ifelse(grepl("1000",tabl.prob.dist.communs$Distance_buffer),
                                                         1000, ifelse(grepl("200",tabl.prob.dist.communs$Distance_buffer),
                                                                     200, 100))
          
      # Preparation a la boucle des graphs -----
        vec.name <- sort(unique(tabl.prob.dist.communs$Nom_latin)) # ordonne les nom latins 
        vec.name.pos <- sort(unique(seq(1,length(vec.name),12))) # vecteur de position (formation de groupe de 20 en 20 pour le plot)
        count <- 0
        
      # boucle des graphs
        for(i in vec.name.pos){
          count <- count + 1
          
          vec.sp <- vec.name[i:min((i+11),length(vec.name))]
          
          # subset des donnees
            dtf.graph <- subset(tabl.prob.dist.communs, Nom_latin %in% vec.sp)
            annot.graph <- subset(tabl.annot.sp, Nom_latin %in% vec.sp)
          
          
          # plot
            graph.prob <- ggplot(dtf.graph) +
              geom_line(aes(x = Distance_buffer,                                 # plot de la ligne
                            y = Estimation_proba_detection,
                            colour = Categories_habitats),
                        alpha = 0.75) +
              
              geom_point(aes(x = Distance_buffer,                                # plot des points
                              y = Estimation_proba_detection,
                              colour = Categories_habitats,
                              shape = Categories_habitats),
                         size = 2.5,
                         alpha = 0.75) +
              
              scale_colour_manual(values = c("magenta2","red","green4","darkgoldenrod3")) +
              scale_x_continuous(breaks = pretty_breaks()) +
              
              xlab("Rayon buffer (en m)") +
              ylab("Estimation de la probabilité de détection") +
              
              geom_text(data=annot.graph,                                         # encadre d'annotations
                        aes(x = Inf, y = Inf, hjust = 1.05, vjust = 1.05,
                            label = Annotation),
                        size=2.5) +
              
              theme(legend.background = element_rect(fill = "ivory2",             # design encadre de legende
                                                     size = 0.5, 
                                                     linetype = "solid",
                                                     colour = "sienna4")) +
              facet_wrap(.~ Nom_latin,scales="free",ncol=4)
              
          # sauvegarde
            ggsave(paste0("C:/git/epoc/output/graphs/",
                          "Evolution_proba_detection_estimee_espece_communes_",count,".png"),
                   width = 30, height = 30,units = "cm")
          
        }
          
  # sauvegarde 3 -----
    load("C:/git/epoc/07_save3.RData")       

  # add de la catégories d'habiats sur la table epoc.envi.obs      
        epoc.envi.obs <- left_join(epoc.envi.obs,
                                   st_drop_geometry(tabl.dist.list_sf[,c("ID_liste","Categories_habitats")]))
        
  # save des fichiers utile pour DS       
    write.csv(x = epoc.envi.obs,
              file = "C:/git/epoc/DS/epoc_environnement_observation_DS.csv")
    write.csv(x = epoc.envi.liste,
              file = "C:/git/epoc/DS/epoc_environnement_liste_DS.csv")
    write.csv(x = epoc.oiso,
              file = "C:/git/epoc/DS/epoc_communaute_DS.csv")
    write.csv(x = oiso.reg.all,
              file = "C:/git/epoc/DS/liste_oiseaux_communs_DS.csv")
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
          
          
          
          
          
          
