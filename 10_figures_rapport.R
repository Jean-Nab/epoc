# chemin
  setwd("C:/git/epoc/data")
# packages
  library(sf)
  library(ggplot2)
  library(tmap)
  library(lubridate)
  library(ggspatial)
  library(dplyr)

  library(ranger)

# upload data -----
  # informations EPOCs
    load("C:/git/epoc/04bis_save2_mars_juillet.RData")
  
    epoc.envi.obs <- read.csv(file = paste0(sub("/data","/DS.v2",getwd()),"/epoc_environnement_observation_DS.csv"))  
    epoc.envi.liste <- read.csv(file = paste0(sub("/data","/DS.v2",getwd()),"/epoc_environnement_liste_DS.csv"))  
    bary.list <- read.csv("C:/git/epoc/DS.v2/epoc_barycentre_liste_density_add.csv")
    list.flag <- read.csv("C:/git/epoc/data/flags_lists.csv")
  
    epoc.court.in <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_7_court_in_period.txt"),header=T,sep="\t", dec=","
                                , encoding="UTF-8",quote="")
    epoc.court.in$Departement <- gsub("\\","",epoc.court.in$Departement,fixed=TRUE) # 3 departements avec des \\ inclus
    
    
    list.all.var <- read.csv("C:/git/epoc/data/Donnees_Yves/GI_Coordonnee_listes_EPOC_cleaned.csv")
    list.all.var2 <- read.csv("C:/git/epoc/data/Donnees_Yves/GI_Coordonnee_listes_EPOC_cleaned_v2.1.csv")
    
  # objets spatiaux
    eco.reg <- st_transform(st_read("C:/git/epoc/data/france_ecoregions.shp"),crs=2154)
    eco.reg.v3 <- st_transform(st_read("C:/git/epoc/data/france_ecoregions_v3.shp"),crs=2154)
    fra.adm0.l93 <- st_transform(st_read(dsn = "C:/Users/Travail/Desktop/Ressource QGis/france/adm/FRA_adm0.shp"),crs=2154)
    fra.adm2.l93 <- st_transform(st_read(dsn = "C:/Users/Travail/Desktop/Ressource QGis/france/adm/FRA_adm2.shp"),crs=2154)
    
  # fonctions 
    time_to_decimal <- function(x) {
      x <- hm(x, quiet = TRUE)
      hour(x) + minute(x) / 60
    }
    length_unique <- function(x){
      u <- unique(x)
      o <- length(u)
      return(o)
    }
    

# Carte position des listes sur la france metropolitaine -----
    bary.list_sf <- st_as_sf(bary.list, coords = c("X_barycentre_L93","Y_barycentre_L93"),crs=2154)
    
    ggplot() + 
      geom_sf(data=fra.adm2.l93) +
      geom_sf(data = bary.list_sf,alpha = 0.15) +
      theme(legend.position = "none") +
      annotation_scale(location = "bl",width_hint = 0.25) +
      annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(1.1, "cm"), pad_y = unit(0.7, "cm"),
                           style = north_arrow_fancy_orienteering) #+
      #ggtitle("Localisation des EPOCs")
  
    
# Carte Nb epoc par département ------
    length_unique <- function(x){
      u <- unique(x)
      o <- length(u)
      return(o)
    }
    
    epoc.envi.obs1 <- epoc.envi.obs[which(epoc.envi.obs$Annee == 2019),]
    
    epoc_dep <- aggregate(ID_liste ~ Departement, data=epoc.envi.obs1, FUN=length_unique)
    colnames(epoc_dep)[2] <- "Nb_EPOC_dep"
    
    colnames(fra.adm2.l93)[7] <- "Departement"
    epoc_dep$Departement <- gsub("\\","",epoc_dep$Departement,fixed=TRUE)
    fra.adm2.l93 <- merge(fra.adm2.l93,epoc_dep,by="Departement",all.x=TRUE) # Ajout des donnees de comptage d'epoc par departement dans l'objet sf
    
    tm_shape(fra.adm2.l93) +
      tm_fill(col="Nb_EPOC_dep",
              style="fixed", breaks =c(1,5,15,50,100,250,500,1000,1500,2770),
              #style="fixed", breaks = quantile(epoc_dep$Nb_EPOC_dep,rep(1:10)/10),
              palette="BuGn",
              title = "Nombre \nd'EPOC par\ndépartement",
              as.count = F,
              interval.closure = "right",
              colorNA = "burlywood2",
              textNA = "Absence d'EPOC",
              legend.format = list(text.separator= "à")) +
      tm_compass(position = c("right","top"), type = "8star",size = 5, lwd=0.5) +
      tm_scale_bar(position = c("left","top"), width = 0.15) +
      tm_borders(col=NA,lwd=0,alpha=0.50)    
    
    
# Biais effort echantillonnage : gam ab/div ~ heure de debut -----
    epoc.court.in$Heure_debut <- gsub("\\.","\\:",epoc.court.in$Heure_debut)
    epoc.court.in$Heure_debut <- time_to_decimal(epoc.court.in$Heure_debut)
    
    hist(epoc.court.in$Heure_debut) # Grande majorite des observations faites entre 6h et 12-13h
    
    epoc.court.in_liste <- epoc.court.in[which(duplicated(epoc.court.in$ID_liste) == F),]
    
    
    # GAM jour
    mod.jour.ab <- mgcv::gam(Abondance_liste ~ s(Jour_de_l_annee), data=epoc.court.in, family="nb",method="REML")
    plot(mod.jour.ab)
    
    mod.jour.dv <- mgcv::gam(Diversite_liste ~ s(Jour_de_l_annee), data=epoc.court.in, family="nb",method="REML")
    plot(mod.jour.dv)
    
    # GAM heure de début
    mod.tp.ab <- mgcv::gam(Abondance_liste ~ s(Heure_debut), data=epoc.court.in_liste, family="nb",method="REML")
    plot(mod.tp.ab,rug=T)
    
    mod.tp.dv <- mgcv::gam(Diversite_liste ~ s(Heure_debut), data=epoc.court.in_liste, family="nb",method="REML")
    plot(mod.tp.dv,rug=T)
    
    # GAM temps d'écoute
    mod.tpec.ab <- mgcv::gam(Abondance_liste ~ s(Tps_ecoute), data=epoc.court.in, family="nb",method="REML")
    plot(mod.tpec.ab)
    
    mod.tpec.dv <- mgcv::gam(Diversite_liste ~ s(Tps_ecoute), data=epoc.court.in, family="nb",method="REML")
    plot(mod.tpec.dv)
    
    
# Histogramme : nb listes par observateur -------
    epoc_by_obs <- aggregate(ID_liste ~ Observateur, data = epoc.envi.obs,
                             FUN = length_unique)
    epoc_by_obs$Observateur <- factor(epoc_by_obs$Observateur, levels = epoc_by_obs$Observateur[order(epoc_by_obs$ID_liste,decreasing = T)])
    epoc_by_obs <- epoc_by_obs[order(epoc_by_obs$ID_liste,decreasing = T),]
    
    vec.col <- c("red","purple","green","brown","blue",rep("grey50",length((6:100))))
    
    
    ggplot(data = epoc_by_obs[1:100,], aes(x=Observateur,y=ID_liste)) +
      geom_bar(stat ="identity",bins = 2,fill = vec.col) +
      geom_hline(yintercept = 500,color="red",alpha = 0.5) +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.25))) +
      xlab("Observateur") + ylab("Nombre d'EPOC")
    
# histogramme : nb liste par heure de debut -----
    epoc_by_hour <- epoc.court.in[which(duplicated(epoc.court.in$ID_liste) == F),]
    ggplot(epoc_by_hour) + geom_histogram(aes(x=Heure_debut)) + xlab("Heure de début d'écoute") + ylab("Nombre d'EPOCs")
    
    hist(epoc_by_hour$Heure_de_debut)
# Carte localisation des grands contributerus -----
    epoc_by_obs <- aggregate(ID_liste ~ Observateur, data = epoc.envi.obs,
                             FUN = length_unique)
    epoc_by_obs$Observateur <- factor(epoc_by_obs$Observateur, levels = epoc_by_obs$Observateur[order(epoc_by_obs$ID_liste,decreasing = T)])
    epoc_by_obs <- epoc_by_obs[order(epoc_by_obs$ID_liste,decreasing = T),]
    
    vec.col <- c("red","purple","green","brown","blue",rep("grey50",length((6:100))))
    
    
    
    epoc_by_obs <- epoc_by_obs[order(epoc_by_obs$ID_liste,decreasing = T),]  
    
    list.champ <- as.character(epoc_by_obs[1:5,"Observateur"])
    
    list.champ.loc <- epoc.envi.liste[which(epoc.envi.liste$Observateur %in% list.champ == TRUE),"ID_liste"]
    
    list.champ.loc <- bary.list_sf[which(bary.list_sf$ID_liste %in% list.champ.loc == T),]
    list.champ.loc <- left_join(list.champ.loc,epoc.envi.liste[,c("ID_liste","Observateur")])
    
    list.champ <- data.frame(as.character(epoc_by_obs[1:5,"Observateur"]))
    colnames(list.champ)[1]  <- "Observateur"
    
    list.champ$couleur <- vec.col[1:5]
    
    list.champ.loc <- left_join(list.champ.loc,list.champ)
    
    ggplot() +
      geom_sf(data = fra.adm2.l93) +
      geom_sf(data = list.champ.loc,aes(color = as.factor(couleur)),alpha = 0.2) +
      theme(legend.position = "none") +
      annotation_scale(location = "bl",width_hint = 0.25) +
      annotation_north_arrow(location = "bl", which_north = "true", 
                             pad_x = unit(1.1, "cm"), pad_y = unit(0.7, "cm"),
                             style = north_arrow_fancy_orienteering) #+
     # ggtitle("Carte des grands contributeurs")
    
    
# graphs théoriques proba/part communs/rares -------    
  # plot proba
    tab.qt.global.part$indice <- 1:nrow(tab.qt.global.part)
    tab.qt.global.prob$indice <- 1:nrow(tab.qt.global.prob)
    
    
    ggplot(tab.qt.global.part,aes(x = indice,y=mediane*100,ymin=0.75*100)) + geom_point(alpha = 0) +
      geom_smooth(level = 0.95) +
      geom_ribbon(aes(x=indice,ymin=min,ymax=max),alpha=0) +
      #scale_y_continuous(labels = "percent") +
      xlab("Nombre d'especes par EPOCs") + ylab("Part en %") +
      theme(axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.25)),
            axis.text.x = element_text(size = rel(1.25)))
    
  # plot part
    ggplot(tab.qt.global.prob,aes(x = indice,y=mediane,ymin=0.75)) + 
      geom_point(alpha = 0) +
      geom_smooth(level = 0.75) +
      #geom_line(aes(x = indice,y=mediane)) +
      #ggtitle("Probabilité d'avoir au moins une espece communes dans les EPOCs") + 
      xlab("Nombre d'especes par EPOCs") + ylab("Probabilité") +
      theme(axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.25)),
            axis.text.x = element_text(size = rel(1.25)))
    

    
# Representation ecoregions ------
  eco.reg.fra <- st_intersection(eco.reg,fra.adm0.l93)
  eco.reg.v3.fra <- st_intersection(eco.reg.v3,fra.adm0.l93) 
  
  eco.reg.v3.fra$ECO_NAME <- gsub("Alps conifer and mixed forests","Montagnes",eco.reg.v3.fra$ECO_NAME)
  
  
  
  ggplot() + 
    geom_sf(data=fra.adm2.l93,alpha=0.25) +
    geom_sf(data = eco.reg.fra, aes(fill=ECO_NAME),alpha=0.85) +
    ggtitle("Avant aggreg")
    
  ggplot() + 
    geom_sf(data=fra.adm2.l93,alpha=0.25) +
    geom_sf(data = eco.reg.v3.fra, aes(fill=ECO_NAME),alpha=0.85)  +
    annotation_scale(location = "bl",width_hint = 0.25) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.35, "cm"), pad_y = unit(0.7, "cm"),
                           style = north_arrow_fancy_orienteering) + 
    theme(legend.background = element_rect(fill = "ivory2",             # design encadre de legende
                                           size = 0.35, 
                                           linetype = "solid",
                                           colour = "sienna4")) +
    labs(fill = "Ecorégions") #+
    #ggtitle("Carte des écorégions")
    
    
  # cartes des epocs hautes altitudes -----
    # recup' des epocs de hautes altitudes
      loc.high.alt <- bary.reg[which(bary.reg$`Hautes altitudes` == 1),]
      loc.high.alt_sf <- st_as_sf(loc.high.alt, coords = c("X_barycentre_L93","Y_barycentre_L93"),crs=2154)

          
      ggplot() + 
        geom_sf(data=fra.adm2.l93,alpha=0.05) +
        geom_sf(data = eco.reg.v3.fra, aes(fill=ECO_NAME),alpha=0.70)  +
        geom_sf(data=loc.high.alt_sf,alpha=0.80) +
        annotation_scale(location = "bl",width_hint = 0.25) +
        annotation_north_arrow(location = "bl", which_north = "true", 
                               pad_x = unit(1.1, "cm"), pad_y = unit(0.7, "cm"),
                               style = north_arrow_fancy_orienteering) + 
        theme(legend.position = "none") #+
        #ggtitle("Localisation des EPOCs réalisés à plus de 1200m")
    
  # table nb liste par écoregions (non modifie) ----
      eco.reg <- st_transform(eco.reg,crs=2154)
      
      # exploration preleminaire (-> table)
      epoc.envi.obs_sf.preli <- epoc.envi.obs_sf[,]
      
      table.preli <- st_intersects(x=eco.reg,y=epoc.envi.obs_sf.preli,sparse = F)
      table.preli <- t(table.preli)
      table.preli.res <- colSums(table.preli) ; table.preli.res
      
      eco.nam <- as.character(eco.reg$ECO_NAME)
      names(table.preli.res) <- eco.nam
      
  # carte zones intersecté ----
    eco.reg.v3.fra.buff <- st_buffer(eco.reg.v3.fra,dist = 25000)

    j <- st_intersection(eco.reg.v3.fra.buff) %>%
      mutate(nb_intersection = n.overlaps)
    
    j <- st_intersection(j,fra.adm0.l93)
    
    f <- which(j$nb_intersection != 1)
    
    j[f,"nb_intersection2"] <- st_drop_geometry(j[f,"nb_intersection"])
    j[-f,"nb_intersection2"] <- 1
    
    
        ggplot() +
          geom_sf(data=eco.reg.v3.fra) +
          geom_sf(data = j,aes(fill=as.factor(nb_intersection2)),alpha=0.75) +
          annotation_scale(location = "bl",width_hint = 0.25) +
          annotation_north_arrow(location = "bl", which_north = "true", 
                                 pad_x = unit(0.75, "cm"), pad_y = unit(0.7, "cm"),
                                 style = north_arrow_fancy_orienteering) + 
          theme(legend.background = element_rect(fill = "ivory2",             # design encadre de legende
                                                 size = 0.35, 
                                                 linetype = "solid",
                                                 colour = "sienna4")) +
          labs(fill = "Nombre d'écorégions\npris en compte") #+
          #ggtitle("Zones tampon")
    
    
    
    
# Plot de l'évolution de la RMSE -----
  load("C:/git/epoc/10_load_for_rmse_plot_4000.RData")
        
  table.rmse <- data.frame(c(1:4000),c(0))
  colnames(table.rmse) <- c("indice","rmse")

  for(i in 2015:4000){
    
    rf.1_0 <- ranger(formula = formul.model,
                     data = data_split$train,
                     importance = "impurity",
                     probability = TRUE,
                     replace = TRUE,
                     #sample.fraction = freq.detect,
                     case.weights = 1/sqrt(data_split$train$densite),
                     #classification = T,
                     #mtry = 3,
                     seed = 123,
                     num.trees = i)
    
    table.rmse[i,"rmse"] <- rf.1_0$prediction.error
    
    cat(i,"\n")
    }
  
  ggplot(table.rmse,aes(x=indice,y=rmse)) + 
    geom_line() +
    geom_vline(xintercept = which.min(table.rmse$rmse),color="red") +
    xlab("Nombre d'arbres du randomForest") + ylab('RMSE (Root Mean Squared Error)')

  
  
# graph des correlations ACP ----
  library(ade4)
  # changement de nom des variables
 
    names(list.all.var)[grep("SpBioC18",colnames(list.all.var))] <- "Precipitation_trimestre_chaud"
    names(list.all.var)[grep("SpBioC10",colnames(list.all.var))] <- "Temp_moyenne_trimestre_chaud"
    names(list.all.var)[grep("SpBioC11",colnames(list.all.var))] <- "Temp_moyenne_trimestre_froid"
    names(list.all.var)[grep("SpBioC12",colnames(list.all.var))] <- "Precipitation_annuelle"
    names(list.all.var)[grep("SpBioC13",colnames(list.all.var))] <- "Precipitation_mois_humide"
    names(list.all.var)[grep("SpBioC14",colnames(list.all.var))] <- "Precipitation_mois_sec"
    names(list.all.var)[grep("SpBioC15",colnames(list.all.var))] <- "Saisonnalite_precipitation"
    names(list.all.var)[grep("SpBioC16",colnames(list.all.var))] <- "Precipitation_trimestre_humide"
    names(list.all.var)[grep("SpBioC17",colnames(list.all.var))] <- "Precipitation_trimestre_sec"
    names(list.all.var)[grep("SpBioC19",colnames(list.all.var))] <- "Precipitation_trimestre_froid"
    
    names(list.all.var)[grep("SpBioC2",colnames(list.all.var))] <- "Amplitude_thermique"
    names(list.all.var)[grep("SpBioC4",colnames(list.all.var))] <- "Saisonnalite_temp" 
    names(list.all.var)[grep("SpBioC1",colnames(list.all.var))] <- "Temp_annuelle"
    names(list.all.var)[grep("SpBioC3",colnames(list.all.var))] <- "Isotherme"
    names(list.all.var)[grep("SpBioC5",colnames(list.all.var))] <- "Temp_max_mois_chaud"
    names(list.all.var)[grep("SpBioC6",colnames(list.all.var))] <- "Temp_min_mois_froid"
    names(list.all.var)[grep("SpBioC7",colnames(list.all.var))] <- "Etendue_thermique"
    names(list.all.var)[grep("SpBioC8",colnames(list.all.var))] <- "Temp_moyenne_trimestre_humide"
    names(list.all.var)[grep("SpBioC9",colnames(list.all.var))] <- "Temp_moyenne_trimestre_sec"
    
  
    
  # ACP
    acp.bioclim <- dudi.pca(df = list.all.var[, grep("trimestre|Precipitation|Saisonnalite|thermique|Temp|Isotherme", names(list.all.var))], 
                            center = T, scale = T, scannf = FALSE, nf = 3)
    
    s.corcircle(acp.bioclim$co,clabel = 0.96,full=T,
                xax = 1,yax =2)
    s.corcircle(acp.bioclim$co,clabel = 0.96,full=T,
                xax = 1,yax =3)
    s.corcircle(acp.bioclim$co,clabel = 0.96,full=T,
                xax = 2,yax =3)
    
    
  # matrice de correlation variable GAM ------
    mat.cor <- list.all.var2[,grep("CLCM|BioClim_ACP|WGS84",colnames(list.all.var2))]
      
    corrplot::corrplot(cor(mat.cor,method = "spearman"),method = "number")
  
  
# Resultats : biais temporel d'echantillonnage -----
  # boxplot des tps d'ecoute selon les mois/annees -----
    
    epoc.envi.obs$Tps_ecoute2 <- as.numeric(gsub("(0.0)([1-9]{1})(*)","\\2",as.character(round(epoc.envi.obs$Tps_ecoute,2))))
    
    
    effort_bymonth <- aggregate(Tps_ecoute2 ~ Annee + Mois + ID_liste, epoc.envi.obs,
                                FUN = min)
    
    effort_bymonth <- aggregate(Tps_ecoute2 ~ Annee + Mois, effort_bymonth,
                                FUN = sum)
    
    effort_bymonth$Tps_ecoute3 <- effort_bymonth$Tps_ecoute2/60
    effort_bymonth$Tps_ecoute4 <- effort_bymonth$Tps_ecoute3/24
    
    
    
    effort_bymonth %>%
      ggplot(aes(x = Mois, y = Tps_ecoute3)) +
      #geom_jitter(aes(group = Mois), colour = "darkolivegreen3") +
      geom_line(colour = "darkolivegreen3",size = 0.75,linetype=1,alpha=0.75) +
      facet_wrap(~ Annee, ncol = 3) +
      labs(title = "Repartition annuelle du temps d'écoute",
           y = "Effort (en heure)",
           x = "Mois") + theme_bw(base_size = 15)
    
    
    
  # GAM heure de début -----
    # conversion horaire
      epoc.court.in$Heure_debut2 <- gsub("[.]",":",epoc.court.in$Heure_debut)
    
      epoc.court.in$Heure_debut2 <- time_to_decimal(epoc.court.in$Heure_debut2)
    
    
    
    mod.tp.ab <- mgcv::gam(Abondance_liste ~ s(Heure_debut2), data=epoc.court.in, family="nb",method="REML") # gam abondance
    plot(mod.tp.ab, rug = T,xlab = "Heure de début d'écoute",ylab = "Variation de l'abondance", ylim = c(-3,0.2))
    
    mod.tp.dv <- mgcv::gam(Diversite_liste ~ s(Heure_debut2), data=epoc.court.in, family="nb",method="REML") # gam diversite
    plot(mod.tp.dv, rug = T,xlab = "Heure de début d'écoute",ylab = "Variation de la diversite")


    
# Resultats : Graphs théoriques -----
    tab.qt.global.part$indice <- 1:nrow(tab.qt.global.part)
    tab.qt.global.prob$indice <- 1:nrow(tab.qt.global.prob)
    
 
    ggplot(tab.qt.global.part,aes(x = indice,y=mediane*100,ymin=0.75*100)) + 
      geom_point() + 
      geom_line(aes(x = indice,y=mediane*100)) +
      geom_ribbon(aes(x=indice,ymin=borne_inf*100,ymax=borne_sup*100),alpha=0.5) +
      ggtitle("Part des espèces communes dans les EPOCs (95%)") + 
      #geom_ribbon(aes(x=indice,ymin=min*100,ymax=max*100),alpha=0.15)+
      xlab("Nombre d'espèces par EPOC") + ylab("Part en pourcentage")
    
    
    ggplot(tab.qt.global.prob,aes(x = indice,y=mediane,ymin=0.75)) +
      geom_point() + 
      geom_line() +
      geom_ribbon(aes(x=indice,ymin=borne_inf,ymax=borne_sup),alpha=0.5) + 
      geom_vline(xintercept = 5,color="red",size=1.1) +
      ggtitle("Probabilité d'avoir au moins une espèce commune dans les EPOCs") + 
      xlab("Nombre d'espèces par EPOC") + ylab("Probabilité")
    
# Résultats : Graphiques recapitulatifs des flags -------
  # changement de couleur des observateurs suspectee
    observateur.flag.nochamp$couleur <- "Non suspecté"
    observateur.flag.nochamp[observateur.flag.nochamp$Observateur %in% bad.observateur,"couleur"] <- "Suspecté"
    
    
    
    flag1 <- ggplot(observateur.flag.nochamp,aes(x =nb_liste,y= part_many_rare, color = couleur)) +
      geom_jitter(alpha = 0.65) + 
      scale_colour_manual(values=c("black","red")) +
      geom_hline(yintercept = max.champ.flag.many.rare,color="red") +
      ylab("Proportion d'EPOC hors-normes - Cas C") +
      xlab("Nombre d'EPOC") +
      labs(color = "Observateurs") +
      theme(axis.title.x = element_text(size = rel(1.25)),
            axis.title.y = element_text(size = rel(1.25)),
            axis.text.y = element_text(size = rel(1.1)),
            axis.text.x = element_text(size = rel(1.1)))
    
    flag2 <- ggplot(observateur.flag.nochamp,aes(x = nb_liste,y= part_only_rare, color = couleur)) +
      geom_jitter(alpha = 0.65) + 
      scale_colour_manual(values=c("black","red")) +
      geom_hline(yintercept = max.champ.flag.only.rare,color="red") +
      ylab("Proportion d'EPOC hors-normes - Cas B") +
      xlab("Nombre d'EPOC") +
      labs(color = "Observateurs") +
      theme(axis.title.x = element_text(size = rel(1.25)),
            axis.title.y = element_text(size = rel(1.25)),
            axis.text.y = element_text(size = rel(1.1)),
            axis.text.x = element_text(size = rel(1.1)))
        
    flag3 <- ggplot(observateur.flag.nochamp,aes(x = nb_liste,y= part_scarce_commun, color = couleur)) +
      geom_jitter(alpha = 0.65) + 
      scale_colour_manual(values=c("black","red")) +
      geom_hline(yintercept = max.champ.flag.scarce.communs,color="red") +
      ylab("Proportion d'EPOC hors-normes - Cas A") +
      xlab("Nombre d'EPOC") +
      labs(color = "Observateurs") +
      theme(axis.title.x = element_text(size = rel(1.25)),
            axis.title.y = element_text(size = rel(1.25)),
            axis.text.y = element_text(size = rel(1.1)),
            axis.text.x = element_text(size = rel(1.1)))
    
    flag4 <- ggplot(observateur.flag.nochamp,aes(x = nb_liste,y= part_first_obs_unusual, color = couleur)) +
      geom_jitter(alpha = 0.65) +
      scale_colour_manual(values=c("black","red")) +
      geom_hline(yintercept = max.champ.flag.first.obs,color="red") +
      ylab("Proportion d'EPOC hors-normes - Cas D") +
      xlab("Nombre d'EPOC") +
      labs(color = "Observateurs") +
      theme(axis.title.x = element_text(size = rel(1.25)),
            axis.title.y = element_text(size = rel(1.25)),
            axis.text.y = element_text(size = rel(1.1)),
            axis.text.x = element_text(size = rel(1.1)))
    
    # plot groupe des flags
    ggpubr::ggarrange(flag3,flag2,flag1,flag4,legend = "none")
    
    

# Résultats : Boxplot prob occurrence estimée selon code atlas -------
  # load carte prob de l'espece !!!
    
  #load("C:/git/epoc/data/table_atlas.Rda")
    
  table_atlas <- read.csv(file = "C:/git/epoc/data/maille_atlas/data_atlas_france_abondance_2009_2012.csv",header=T,sep=";",dec=".")
  
  # manipulation des caracteres (soucis vis-a-vis des accents)
    table_atlas$Nom_commun <- gsub(" ","_",table_atlas$Nom_commun)
    table_atlas$Nom_commun <- gsub(",","e",table_atlas$Nom_commun)

  #table_atlas.sf <- st_transform(st_read(dsn = "C:/git/epoc/data/France_EBBA2_grid/France_EBBA2_grid.shp"),crs=2154)
  table_atlas.sf <- st_transform(st_read(dsn = "C:/git/epoc/data/maille_atlas/L93_10x10_TerreMer.shp"),crs=2154)
  fra.adm0 <- st_transform(st_read(dsn = "C:/Users/Travail/Desktop/Ressource QGis/france/adm/FRA_adm0.shp"),crs=2154)
  
  # manipulation spatiale de la grille 10x10 ------
    table_atlas.sf <- st_crop(table_atlas.sf,fra.adm0)
    
  colnames(table_atlas.sf)[2] <- colnames(table_atlas)[1] # harmonisation des noms de colonnes
    
  # subsetting de la table avec le code atlas selon une espece ----
    #table_atlas_bysp <- table_atlas[which(table_atlas$Species_scientific_name == sp),]
    table_atlas_bysp <- table_atlas[which(table_atlas$Nom_commun == "Fauvette_melanocephale"),]
  
    table_atlas_bysp.sf <- full_join(table_atlas.sf,table_atlas_bysp)
    
    
  # Code atlas ----
    #table_atlas_bysp.sf$Avis_niche <- ifelse(table_atlas_bysp.sf$Expert_breeding_assessment == "A", "Possible",
    #                                         ifelse(table_atlas_bysp.sf$Expert_breeding_assessment == "B", "Probable","Certain"))
    #table_atlas_bysp.sf[which(is.na(table_atlas_bysp.sf$Expert_breeding_assessment)),"Avis_niche"] <- "Aucun"
    
    table_atlas_bysp.sf$Avis_niche <- ifelse(table_atlas_bysp.sf$Indice_nidification == "1", "Possible",
                                             ifelse(table_atlas_bysp.sf$Indice_nidification == "2", "Probable","Certain"))
    table_atlas_bysp.sf[which(is.na(table_atlas_bysp.sf$Indice_nidification)),"Avis_niche"] <- "Aucun"
    
    
    #table_atlas_bysp.sf <- table_atlas_bysp.sf[which(is.na(table_atlas_bysp.sf$Species_scientific_name) == FALSE),]
  
 
    
  # carte indice de nidification ----
      table_atlas_bysp.sf$Avis_niche <- factor(table_atlas_bysp.sf$Avis_niche, levels = c("Aucun","Possible","Probable","Certain"))
      
      table_atlas_bysp.sf[which(table_atlas_bysp.sf$ESPACE == "Marin"),"Avis_niche"] <- NA
      
      #table_atlas_bysp.sf$Avis_niche_colour <- ifelse(table_atlas_bysp.sf$Avis_niche == "Possible","yellow",
      #                                                ifelse(table_atlas_bysp.sf$Avis_niche == "Probable","orange",
      #                                                       ifelse(table_atlas_bysp.sf$Avis_niche == "Certain","red","grey")))
      
  
      table_atlas_bysp.sf1 <- table_atlas_bysp.sf[which(is.na(table_atlas_bysp.sf$Avis_niche) == F),]
      
      ggplot() +
        geom_sf(data = table_atlas_bysp.sf1, aes(fill = Avis_niche)) + 
        scale_fill_manual(values=c("grey","yellow","orange","red"),na.translate=F) +
        labs(fill = "Indice \nde nidification")
      ggsave(filename = "Carte_code_atlas.png", path = paste0(species.path),
             device="png")
      
  # détection des points intersectant la grille atlas 50x50km -----
    grid.predict.sp_sfL93 <- st_transform(grid.predict.sp_sf,crs=2154)
    atlas.grid.sp <- st_intersection(table_atlas_bysp.sf,grid.predict.sp_sfL93)
  
    atlas.grid.sp_code <- st_drop_geometry(atlas.grid.sp)
    #atlas.grid.sp_code <- atlas.grid.sp_code[,c("X50X50_square","MaxYear","Last_Year","Species_scientific_name","Expert_breeding_assessment",
    #                                        "Prob_presence","Avis_niche","Abondance1","Abondance_corrige2")]
    atlas.grid.sp_code <- atlas.grid.sp_code[,c("Maille","Nom_commun","Indice_nidification","ESPACE",
                                                "Prob_presence","Avis_niche")]
    
    atlas.grid.sp_code$Avis_niche <- factor(atlas.grid.sp_code$Avis_niche, levels = c("Aucun","Possible","Probable","Certain"))
    
    
    atlas.grid.sp_code[which(atlas.grid.sp_code$ESPACE == "Marin"),"Avis_niche"] <- NA
    
    atlas.grid.sp_code <- atlas.grid.sp_code[which(atlas.grid.sp_code$ESPACE != "Marin"),]
    
    atlas.grid.sp_code$Nb_maille_avis <- ifelse(atlas.grid.sp_code$Avis_niche == "Possible", length(which(table_atlas_bysp.sf1$Avis_niche == "Possible")),
                                                ifelse(atlas.grid.sp_code$Avis_niche == "Probable",length(which(table_atlas_bysp.sf1$Avis_niche == "Probable")),
                                                       ifelse(atlas.grid.sp_code$Avis_niche == "Certain", length(which(table_atlas_bysp.sf1$Avis_niche == "Certain")),length(which(table_atlas_bysp.sf1$Avis_niche == "Aucun")))))
    
    
    
  # visualisation -----
    table(atlas.grid.sp_code$ESPACE)
    
    ggplot(atlas.grid.sp_code) +
      geom_boxplot(aes(x = Avis_niche, y = Prob_presence), outlier.shape = NA) +
      scale_y_continuous(limits = quantile(atlas.grid.sp_code$Prob_presence, c(0.0, 0.99))) + 
      geom_text(data = atlas.grid.sp_code, aes(x = Avis_niche, y = min(Prob_presence), label = Nb_maille_avis),
                size = 5,col="red",vjust=1.1) +
      xlab("Indices de nidification") + ylab("Probabilité d'occurrence estimée")
      
      ggsave(filename = "Boxplot_occurrence_atlas.png", path = paste0(species.path),
             device="png")
        
# DISCUSSION : seuil de determination communs/non-commmuns -----
    h <- list.flag[list.flag$Observateur == "Loïc Jomat (lpo)",]  # phillipe jourde (charentes-maritime)/ loic jomat (rosseliere) / xavier birot colomb (montagnes)
    h1 <- epoc.envi.obs[epoc.envi.obs$ID_liste %in% h$ID_liste,]
    
    h2 <- left_join(h1[,c("ID_liste","Nom_espece","Nom_latin")],oiso.reg.all[,c("Nom_latin","communs")])
    h2[is.na(h2$communs),"communs"] <- 0
    h3 <- h2[which(h2$communs == 0),]
    h3$Nom_espece <- as.character(h3$Nom_espece)
    h4 <- table(h3$Nom_espece)[which(table(h3$Nom_espece) > 100)]
    
    
    
    
# not run -----
  hydro <- foreign::read.dbf(file = "C:/Users/Travail/Desktop/Ressource QGis/france/hydrographie/TronconHydrograElt_FXX.dbf")












