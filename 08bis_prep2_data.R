# chemin
  setwd("C:/git/epoc/data")

  
library(dplyr)
  

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
          
          dtf.tmp$CLC_Batis_IndustrielM <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
          
          list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
      # 5000m
          j <- grep("row_id|SpHC11L|SpHC12L|SpHC13L",colnames(list.all.var))
          dtf.tmp <- list.all.var[,j]
          
          dtf.tmp$CLC_Batis_IndustrielL <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
          
          list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
  
    # niveau 2 CLC (14 --> CLC_Espaces_verts_articialises) -----
          # 500m
            j <- grep("row_id|SpHC14M",colnames(list.all.var))
            dtf.tmp <- list.all.var[,j]
            
            dtf.tmp$CLC_Espaces_verts_articialisesM <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
            
            list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
          # 5000m
            j <- grep("row_id|SpHC14L",colnames(list.all.var))
            dtf.tmp <- list.all.var[,j]
            
            dtf.tmp$CLC_Espaces_verts_articialisesL <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
            
            list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
  
    # niveau (21 --> CLC_Terres_arables) ------
            # 500m
              j <- grep("row_id|SpHC21M",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLC_Terres_arablesM <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
            # 5000m
              j <- grep("row_id|SpHC21L",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLC_Terres_arablesL <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
            
    # niveau (22 --> CLC_Cultures_permanentes) ------
            # 500m
              j <- grep("row_id|SpHC22M",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLC_Cultures_permanentesM <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
            # 5000m
              j <- grep("row_id|SpHC22L",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLC_Cultures_permanentesL <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
  
    # niveau (23,24 --> CLC_Cultures_heterogenes) -----
            # 500m
              j <- grep("row_id|SpHC23M|SpHC24M",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLC_Cultures_heterogenesM <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
            # 5000m
              j <- grep("row_id|SpHC23L|SpHC24L",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLC_Cultures_heterogenesL <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
  
    # niveau (31,32  --> CLC_Forets) -----
            # 500m
              j <- grep("row_id|SpHC31M|SpHC32M",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLC_ForetsM <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
            # 5000m
              j <- grep("row_id|SpHC31L|SpHC32L",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLC_ForetsL <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
  
    # niveau (34 --> CLC_Espaces_ouverts) -----
            # 500m
              j <- grep("row_id|SpHC33M",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLC_Espaces_ouvertsM <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
            # 5000m
              j <- grep("row_id|SpHC33L",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLC_Espaces_ouvertsL <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
  
    # niveau (41,42 --> CLC_Zones_humides) -----
            # 500m
              j <- grep("row_id|SpHC41M|SpHC42M",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLC_Zones_humidesM <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
            # 5000m
              j <- grep("row_id|SpHC41L|SpHC42L",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLC_Zones_humidesL <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
  
    # niveau (51,52 --> CLC_Surfaces_eaux) ----
            # 500m
              j <- grep("row_id|SpHC51M|SpHC52M",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLC_Surfaces_eauxM <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
            # 5000m
              j <- grep("row_id|SpHC51L|SpHC52L",colnames(list.all.var))
              dtf.tmp <- list.all.var[,j]
              
              dtf.tmp$CLC_Surfaces_eauxL <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              list.all.var <- left_join(list.all.var,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])



    # rm de la variable de join (row_id) -----
      list.all.var <- list.all.var[,-grep("row",colnames(list.all.var))]
  
  # grid.predict ----
              grid.predict$row_id <- c(rep(1:nrow(grid.predict)))
              # niveau 2 CLC (11,12,13 --> CLC_Batis_Industriel) ----
              # 500m
              j <- grep("row_id|SpHC11M|SpHC12M|SpHC13M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_Batis_IndustrielM <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC11L|SpHC12L|SpHC13L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_Batis_IndustrielL <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              # niveau 2 CLC (14 --> CLC_Espaces_verts_articialises) -----
              # 500m
              j <- grep("row_id|SpHC14M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_Espaces_verts_articialisesM <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC14L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_Espaces_verts_articialisesL <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              # niveau (21 --> CLC_Terres_arables) ------
              # 500m
              j <- grep("row_id|SpHC21M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_Terres_arablesM <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC21L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_Terres_arablesL <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              # niveau (22 --> CLC_Cultures_permanentes) ------
              # 500m
              j <- grep("row_id|SpHC22M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_Cultures_permanentesM <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC22L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_Cultures_permanentesL <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              # niveau (23,24 --> CLC_Cultures_heterogenes) -----
              # 500m
              j <- grep("row_id|SpHC23M|SpHC24M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_Cultures_heterogenesM <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC23L|SpHC24L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_Cultures_heterogenesL <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              # niveau (31,32  --> CLC_Forets) -----
              # 500m
              j <- grep("row_id|SpHC31M|SpHC32M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_ForetsM <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC31L|SpHC32L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_ForetsL <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              
              # niveau (34 --> CLC_Espaces_ouverts) -----
              # 500m
              j <- grep("row_id|SpHC33M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_Espaces_ouvertsM <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC33L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_Espaces_ouvertsL <- dtf.tmp[,-grep("row",colnames(dtf.tmp))]
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              # niveau (41,42 --> CLC_Zones_humides) -----
              # 500m
              j <- grep("row_id|SpHC41M|SpHC42M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_Zones_humidesM <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC41L|SpHC42L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_Zones_humidesL <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              # niveau (51,52 --> CLC_Surfaces_eaux) ----
              # 500m
              j <- grep("row_id|SpHC51M|SpHC52M",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_Surfaces_eauxM <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              # 5000m
              j <- grep("row_id|SpHC51L|SpHC52L",colnames(grid.predict))
              dtf.tmp <- grid.predict[,j]
              
              dtf.tmp$CLC_Surfaces_eauxL <- rowSums(dtf.tmp[,-grep("row",colnames(dtf.tmp))])
              
              grid.predict <- left_join(grid.predict,dtf.tmp[,grep("row|CLC",colnames(dtf.tmp))])
              
              
              
              # rm de la variable de join (row_id) -----
              grid.predict <- grid.predict[,-grep("row",colnames(grid.predict))]


# sauvegarde sur disque -----
  write.csv(list.all.var,row.names = F,file="C:/git/epoc/data/Donnees_Yves/GI_Coordonnee_listes_EPOC_cleaned_v2.csv")
  write.csv(grid.predict,row.names = F, file = "C:/git/epoc/data/Donnees_Yves/GI_SysGrid__3e+05_cleaned_v2.csv")


















