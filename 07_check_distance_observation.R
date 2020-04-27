# chemin
  setwd("C:/git/epoc/data")
# packages
  library(sf)
  library(ggplot2)
  library(dplyr)

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
  

  # exploration des data
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



























