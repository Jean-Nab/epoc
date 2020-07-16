# chemin
  setwd("C:/git/epoc/data")

  
library(dplyr)
library(ade4)
  

# import data ----
  #load("C:/git/epoc/08_prep_save1_mars_juillet.RData")
  
  list.all.var <- read.csv("C:/git/epoc/data/Donnees_Yves/GI_Coordonnee_listes_EPOC.csv")
  grid.predict <- read.csv("C:/git/epoc/data/Donnees_Yves/GI_SysGrid__3e+05.csv")

  
# nettoyage du jeu de data envi (noms de colonnes) ----
  del.var <- grep("[.]y|[.]1|optional|V1",colnames(list.all.var))
  list.all.var <- list.all.var[,-del.var]
  
  mod.var <- grep("[.]x",colnames(list.all.var))
  colnames(list.all.var) <- gsub("[.x]","",colnames(list.all.var))
  
# nettoyage et harmonisation du jeu de data de la grille de prediction -----
  del.var1 <- grep("[.]y|[.]x|optional",colnames(grid.predict))
  grid.predict <- grid.predict[,-del.var1]
  
  colnames(grid.predict)[grep("Group.1.1",colnames(grid.predict))] <- "X_barycentre_L93"
  colnames(grid.predict)[grep("Group.2.1",colnames(grid.predict))] <- "Y_barycentre_L93"
  colnames(grid.predict)[grep("Group.1",colnames(grid.predict))] <- "Lon_WGS84_bary"
  colnames(grid.predict)[grep("Group.2",colnames(grid.predict))] <- "Lat_WGS84_bary"
  
  
# rm de variable communes -----
  del.var <- grep("SpPenS|SpPenM|SpPenL|SpNorS|SpEasS|SpSumS|SpSumM|SpSumL|SpAltiM",colnames(list.all.var))
  list.all.var <- list.all.var[,-del.var]
  
  del.var1 <- grep("SpPenS|SpPenM|SpPenL|SpNorS|SpEasS|SpSumS|SpSumM|SpSumL|SpAltiM",colnames(grid.predict))
  grid.predict <- grid.predict[,-del.var1]
  
  
# Ajout de coordonnees spatial supplementaire ------
  library(spdep)

  nb.rot <- 40 # Nombre de projection de coordonnees (divisable par 360)
  
for(i in 1:(nb.rot-1)){ # boucle de 40 iterations
  
  Coord.rotate <- Rotation(xy = list.all.var[,c("Lon_WGS84_bary","Lat_WGS84_bary")],
                           angle = pi*i/nb.rot)
  Coord.rotate2 <- Rotation(xy = grid.predict[,c("Lon_WGS84_bary","Lat_WGS84_bary")],
                           angle = pi*i/nb.rot)
  
  list.all.var <- cbind(list.all.var,Coord.rotate[,1])
  grid.predict <- cbind(grid.predict,Coord.rotate2[,1])
  
  
  names(list.all.var)[ncol(list.all.var)] <- paste0("CoordXRotate_",i)
  names(grid.predict)[ncol(grid.predict)] <- paste0("CoordXRotate_",i)
  
}
  
  
# sauvegarde sur disque -----  
  
  write.csv(list.all.var,row.names = F,file="C:/git/epoc/data/Donnees_Yves/GI_Coordonnee_listes_EPOC_cleaned.csv")
  write.csv(grid.predict,row.names = F, file = "C:/git/epoc/data/Donnees_Yves/GI_SysGrid__3e+05_cleaned.csv")
  
  
# Aggregation des variables de Corine Land Cover (spHCXX ------)
  # list.all.var ----
    list.all.var$row_id <- c(rep(1:nrow(list.all.var)))
    # niveau 2 CLC (11,12,13 --> CLC_Batis_Industriel) ----
      # 500m
          j <- grep("row_id|SpHC11M|SpHC12M|SpHC13M",colnames(list.all.var))
          dtf.tmp <- list.all.var[,j]
          
          dtf.tmp$CLCM_Batis_Industriel <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
          
          list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
      # 5000m
          j <- grep("row_id|SpHC11L|SpHC12L|SpHC13L",colnames(list.all.var))
          dtf.tmp <- list.all.var[,j]
          
          dtf.tmp$CLCL_Batis_Industriel <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
          
          list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
  
    # niveau 2 CLC (14 --> CLC_Espaces_verts_articialises) -----
          # 500m
            j <- grep("row_id|SpHC14M",colnames(list.all.var))
            dtf.tmp <- list.all.var[,j]
            
            dtf.tmp$CLCM_Espaces_verts_articialises <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
            
            list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
          # 5000m
            j <- grep("row_id|SpHC14L",colnames(list.all.var))
            dtf.tmp <- list.all.var[,j]
            
            dtf.tmp$CLCL_Espaces_verts_articialises <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
            
            list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
  
    # niveau (21 --> CLC_Terres_arables) ------
            # 500m
              j <- grep("row_id|SpHC21M",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLCM_Terres_arables <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
            # 5000m
              j <- grep("row_id|SpHC21L",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLCL_Terres_arables <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
            
    # niveau (22 --> CLC_Cultures_permanentes) ------
            # 500m
              j <- grep("row_id|SpHC22M",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLCM_Cultures_permanentes <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
            # 5000m
              j <- grep("row_id|SpHC22L",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLCL_Cultures_permanentes <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
  
    # niveau (23,24 --> CLC_Cultures_heterogenes) -----
            # 500m
              j <- grep("row_id|SpHC23M|SpHC24M",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLCM_Cultures_heterogenes <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
            # 5000m
              j <- grep("row_id|SpHC23L|SpHC24L",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLCL_Cultures_heterogenes <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
  
    # niveau (31,32  --> CLC_Forets) -----
            # 500m
              j <- grep("row_id|SpHC31M|SpHC32M",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLCM_Forets <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
            # 5000m
              j <- grep("row_id|SpHC31L|SpHC32L",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLCL_Forets <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
  
    # niveau (34 --> CLC_Espaces_ouverts) -----
            # 500m
              j <- grep("row_id|SpHC33M",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLCM_Espaces_ouverts <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
            # 5000m
              j <- grep("row_id|SpHC33L",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLCL_Espaces_ouverts <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
  
    # niveau (41,42 --> CLC_Zones_humides) -----
            # 500m
              j <- grep("row_id|SpHC41M|SpHC42M",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLCM_Zones_humides <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
            # 5000m
              j <- grep("row_id|SpHC41L|SpHC42L",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLCL_Zones_humides <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
  
    # niveau (51,52 --> CLC_Surfaces_eaux) ----
            # 500m
              j <- grep("row_id|SpHC51M|SpHC52M",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLCM_Surfaces_eaux <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
            # 5000m
              j <- grep("row_id|SpHC51L|SpHC52L",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLCL_Surfaces_eaux <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])



    # rm de la variable de join (row_id) -----
      list.all.var <- list.all.var[,-grep("row",colnames(list.all.var))]
  
  # grid.predict ----
              grid.predict$row_id <- c(rep(1:nrow(grid.predict)))
              # niveau 2 CLC (11,12,13 --> CLC_Batis_Industriel) ----
              # 500m
              j <- grep("row_id|SpHC11M|SpHC12M|SpHC13M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCM_Batis_Industriel <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC11L|SpHC12L|SpHC13L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCL_Batis_Industriel <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              # niveau 2 CLC (14 --> CLC_Espaces_verts_articialises) -----
              # 500m
              j <- grep("row_id|SpHC14M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCM_Espaces_verts_articialises <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC14L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCL_Espaces_verts_articialises <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              # niveau (21 --> CLC_Terres_arables) ------
              # 500m
              j <- grep("row_id|SpHC21M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCM_Terres_arables <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC21L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCL_Terres_arables <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              # niveau (22 --> CLC_Cultures_permanentes) ------
              # 500m
              j <- grep("row_id|SpHC22M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCM_Cultures_permanentes <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC22L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCL_Cultures_permanentes <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              # niveau (23,24 --> CLC_Cultures_heterogenes) -----
              # 500m
              j <- grep("row_id|SpHC23M|SpHC24M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCM_Cultures_heterogenes <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC23L|SpHC24L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCL_Cultures_heterogenes <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              # niveau (31,32  --> CLC_Forets) -----
              # 500m
              j <- grep("row_id|SpHC31M|SpHC32M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCM_Forets <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC31L|SpHC32L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCL_Forets <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              
              # niveau (34 --> CLC_Espaces_ouverts) -----
              # 500m
              j <- grep("row_id|SpHC33M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCM_Espaces_ouverts <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC33L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCL_Espaces_ouverts <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              # niveau (41,42 --> CLC_Zones_humides) -----
              # 500m
              j <- grep("row_id|SpHC41M|SpHC42M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCM_Zones_humides <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC41L|SpHC42L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCL_Zones_humides <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              # niveau (51,52 --> CLC_Surfaces_eaux) ----
              # 500m
              j <- grep("row_id|SpHC51M|SpHC52M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCM_Surfaces_eaux <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC51L|SpHC52L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLCL_Surfaces_eaux <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              
              
              # rm de la variable de join (row_id) -----
              grid.predict <- grid.predict[,-grep("row",colnames(grid.predict))]
              
              
  # Rename des variables bioclimatiques interessantes -----
    # list.all.var ----
      names(list.all.var)[grep("SpBioC2",colnames(list.all.var))] <- "BioClim_Amplitude_thermique"
      names(list.all.var)[grep("SpBioC4",colnames(list.all.var))] <- "BioClim_Temp_seasonality"  
      names(list.all.var)[grep("SpBioC18",colnames(list.all.var))] <- "BioClim_Precipitation_warmest_quarter"

    # grid.predict ----
      names(grid.predict)[grep("SpBioC2",colnames(grid.predict))] <- "BioClim_Amplitude_thermique"
      names(grid.predict)[grep("SpBioC4",colnames(grid.predict))] <- "BioClim_Temp_seasonality"  
      names(grid.predict)[grep("SpBioC18",colnames(grid.predict))] <- "BioClim_Precipitation_warmest_quarter"

# sauvegarde sur disque -----
  write.csv(list.all.var,row.names = F,file="C:/git/epoc/data/Donnees_Yves/GI_Coordonnee_listes_EPOC_cleaned_v2.csv")
  write.csv(grid.predict,row.names = F, file = "C:/git/epoc/data/Donnees_Yves/GI_SysGrid__3e+05_cleaned_v2.csv")

  
  
# Aggregation des variables bioclimatiques selon les axes d'une ACP ------ 
  # list.all.var -----
    list.group.bio <- list.all.var[,grep("SpBio|BioClim",colnames(list.all.var))]
    
    list.acp <- dudi.pca(list.group.bio,center=T,scale = T,nf = 3,scannf = F) # ACP selon les 3 premiers axes
  
    list.all.var <- list.all.var %>%
      bind_cols(list.acp$li) # binding des résidus de l'ACP sur les 3 premiers axes
    
    # Rename des axes de l'ACP ----
      colnames(list.all.var) <- gsub("Axis","BioClim_ACP_Axis",colnames(list.all.var))
  
  
  # grid.predict -----
    grid.group.bio <- grid.predict[,grep("SpBio|BioClim",colnames(grid.predict))]
    
    grid.acp <- dudi.pca(grid.group.bio,center=T,scale = T,nf = 3,scannf = F) # ACP selon les 3 premiers axes
    
    grid.predict <- grid.predict %>%
      bind_cols(grid.acp$li) # binding des résidus de l'ACP sur les 3 premiers axes
    
    # Rename des axes de l'ACP ----
      colnames(grid.predict) <- gsub("Axis","BioClim_ACP_Axis",colnames(grid.predict))
    
    
  # sauvegarde sur disque -----
    write.csv(list.all.var,row.names = F,file="C:/git/epoc/data/Donnees_Yves/GI_Coordonnee_listes_EPOC_cleaned_v2.1.csv")
    write.csv(grid.predict,row.names = F, file = "C:/git/epoc/data/Donnees_Yves/GI_SysGrid__3e+05_cleaned_v2.1.csv")
    
  
  
# Formation d'une version avec un retrait des habitats principaux (vs format de la donnee) -----
  # list.all.var -----
    # 500m
      dtf.aggr <- list.all.var[,grep("ID_liste|CLCM",colnames(list.all.var))] # formation d'un dtf regroupant uniquement les habitats CLC du buffer 500m
      max.hab <- max.col(dtf.aggr[-1])+1 # detection du numero des colonnes ayant la valeur max (= )
      
  
      for(i in 1:nrow(dtf.aggr)){
        
        dtf.aggr[i,max.hab[i]] <- 0
        
      }    
      
      list.all.var[,grep("CLCM",colnames(list.all.var))] <- NULL
      list.all.var <- left_join(list.all.var,dtf.aggr)

    # 5000m
      dtf.aggr <- list.all.var[,grep("WGS84|CLCL",colnames(list.all.var))]
      max.hab <- max.col(dtf.aggr[-1])+1
      
      
      for(i in 1:nrow(dtf.aggr)){
        
        dtf.aggr[i,max.hab[i]] <- 0
        
      }    
      
      list.all.var[,grep("CLCL",colnames(list.all.var))] <- NULL
      list.all.var <- left_join(list.all.var,dtf.aggr)

  # grid.predict -----
    # 500m
      dtf.aggr <- grid.predict[,grep("WGS84|CLCM",colnames(grid.predict))]
      max.hab <- max.col(dtf.aggr[-c(1,2)])+2
      
      
      for(i in 1:nrow(dtf.aggr)){
        
        dtf.aggr[i,max.hab[i]] <- 0
        
      }    
      
      grid.predict[,grep("CLCM",colnames(grid.predict))] <- NULL
      grid.predict <- left_join(grid.predict,dtf.aggr)
      
    # 5000m
      dtf.aggr <- grid.predict[,grep("WGS84|CLCL",colnames(grid.predict))]
      max.hab <- max.col(dtf.aggr[-c(1,2)])+2
      
      
      for(i in 1:nrow(dtf.aggr)){
        
        dtf.aggr[i,max.hab[i]] <- 0
        
      }    
      
      grid.predict[,grep("CLCL",colnames(grid.predict))] <- NULL
      grid.predict <- left_join(grid.predict,dtf.aggr)

      
# sauvegarde sur disque -----
  write.csv(list.all.var,row.names = F,file="C:/git/epoc/data/Donnees_Yves/GI_Coordonnee_listes_EPOC_cleaned_v3.csv")
  write.csv(grid.predict,row.names = F, file = "C:/git/epoc/data/Donnees_Yves/GI_SysGrid__3e+05_cleaned_v3.csv")








