library(terra)
library(exactextractr)
library(sf)
library(dplyr)
library(units)

################################################################################
# Polygons with reference areas
################################################################################
# load TDF limits---------------------------------------------------------------
area_lim1_path <- "../../../data/maps/IAvH/BST/riesgo_etter/EcosistOrigBsTaeEtter_dissolve.gpkg"#"./00_rawdata/Study_areas/EcosistOrigBsTaeEtter_dissolve.gpkg"#"./00_rawdata/Study_areas/Colombia_borders_gadm.gpkg"# 
sf_tdf_lim <- st_read(area_lim1_path)
# sf_tdf_lim_srs <- sf_tdf_lim |> st_transform(st_crs(9377))
sf_tdf_lim_srs2 <- sf_tdf_lim |> st_transform(st_crs(3116))
# st_write(sf_tdf_lim_srs2,"../../data/maps/IAvH/BST/BST_dissolve_3116.gpkg")
# sf_tdf_lim_buffer1km <- st_read("../../data/maps/IAvH/BST/BST_dissolve_3116_buffer-1km.gpkg")
sf_tdf_lim_buffer100m <- sf_tdf_lim_srs2 |> st_buffer(-100)
  
# # Colombia regions------------------------------------------------------------
# sf_col_reg <- st_read("../../data/maps/IGAC/RegionFis/RegionFis_dissolve6.gpkg")
# sf_col_reg_srs2 <- sf_col_reg |> st_transform(st_crs(3116))
# sf_col_reg_nat_srs2 <- sf_col_reg_srs2 |> st_transform(st_crs(3116))

# Colombia subregions
sf_col_subreg_polyg <- st_read("../../../../../../../Users/mariaisabelarceplata/HikaruMac/Doctorado/data/maps/SubRegCol_BST.gpkg")
sf_col_subreg_polyg_srs <- sf_col_subreg_polyg |> st_transform(st_crs(3116))

# Intersection between subregions and Potential area of TDF
sf_tdf_lim_srs2_subreg <- st_intersection(sf_tdf_lim_srs2,sf_col_subreg_polyg_srs) 
sf_tdf_lim_srs2_subreg |> st_make_valid()
# st_write(sf_tdf_lim_srs2_subreg |> st_make_valid(),"../../data/maps/IAvH/BST/BST_CorzoEtAl_subreg.gpkg")

# conservation initiatives------------------------------------------------------
# RUNAP
sf_runap <- st_read("../../../data/maps/RUNAP/RUNAP_2023/runap2Polygon.shp")
sf_runap_srs2 <- sf_runap |> st_transform(st_crs(3116))
# st_write(sf_runap_srs2,"../../data/maps/RUNAP/RUNAP_2023/runap2Polygon_3116.gpkg")

sf_runap_tdf_srs2 <- sf_runap_srs2 |> st_intersection(sf_tdf_lim_srs2_subreg)
# get list of protected areas that overlap with TDF even within a -100 buffer
sf_runap_tdf_buff_srs2 <- sf_runap_srs2 |> st_intersection(sf_tdf_lim_buffer100m)

# get protected area size
sf_runap_tdf_srs2$size_pa_in_tdf_ha <- as.numeric(set_units(st_area(sf_runap_tdf_srs2),"ha"))

# filter the list of protected areas with TDF within a -100 buffer
sf_runap_tdf_srs2_filter <- sf_runap_tdf_srs2 |> filter(id_pnn %in% sf_runap_tdf_buff_srs2$id_pnn)
write.csv(sf_runap_tdf_srs2_filter |> st_drop_geometry(), "../03_data/outputs/1stBatch/summary_data/df_runap_tdf_srs2_filter.csv")

runap_filter_count <- sf_runap_tdf_srs2_filter |> group_by(NomSubReg,categoria) |> count()

################################################################################
# Rasters with information and reference areas
################################################################################
# load weighted habitat rasters
r_weig_hab_1990 <- rast("../../../../../../3yOfLife/Species_Habitat_Index_Colombia/02_outdata/summary/wmean_habitat1990.tif")
r_weig_hab_2020 <- rast("../../../../../../3yOfLife/Species_Habitat_Index_Colombia/02_outdata/summary/wmean_habitat2020.tif")
# get area raster
r_areas_tdf <- cellSize(r_weig_hab_1990,unit="ha")

# get habitat area values for hypothetical year
r_weig_hab_1990_area <- r_weig_hab_1990 * r_areas_tdf
# plot(r_weig_hab_1990_area)
# get habitat area values for 2020
r_weig_hab_2020_area <- r_weig_hab_2020 * r_areas_tdf

################################################################################
# Extract areas
################################################################################

#-------------------------------------------------------------------------------
# General values
#-------------------------------------------------------------------------------
# Habitat area for TDF by 2020 and Hypothetical year----------------------------
v_weig_hab_1990_area <- exact_extract(r_weig_hab_1990_area,sf_tdf_lim_srs2,fun='sum');v_weig_hab_1990_area
v_weig_hab_2020_area <- exact_extract(r_weig_hab_2020_area,sf_tdf_lim_srs2,fun='sum');v_weig_hab_2020_area

# Habitat area by Regions-------------------------------------------------------
v_weig_hab_1990_area_reg <- exact_extract(r_weig_hab_1990_area,sf_col_subreg_polyg_srs,fun='sum');v_weig_hab_1990_area_reg
sum(v_weig_hab_1990_area_reg)
v_weig_hab_2020_area_reg <- exact_extract(r_weig_hab_2020_area,sf_col_subreg_polyg_srs,fun='sum');v_weig_hab_2020_area_reg
sum(v_weig_hab_2020_area_reg)

# get mean values for the proportion of habitat available by pixel by region
v_weig_hab_1990_mean_hab_reg <- exact_extract(r_weig_hab_1990,sf_col_subreg_polyg_srs,fun='mean');v_weig_hab_1990_mean_hab_reg
v_weig_hab_2020_mean_hab_reg <- exact_extract(r_weig_hab_2020,sf_col_subreg_polyg_srs,fun='mean');v_weig_hab_2020_mean_hab_reg

# add column with TDF areas to polygon of Natural regions
sf_col_subreg_polyg_srs2 <- sf_col_subreg_polyg_srs |> 
  mutate(TDF_HypY_ha_sum=round(v_weig_hab_1990_area_reg),
         TDF_2020_ha_sum=round(v_weig_hab_2020_area_reg),
         TDF_HypY_mean=round(v_weig_hab_1990_mean_hab_reg,2),
         TDF_2020_mean=round(v_weig_hab_2020_mean_hab_reg,2))

sum(sf_col_subreg_polyg_srs2$TDF_HypY_ha_sum,na.rm=T)
sum(sf_col_subreg_polyg_srs2$TDF_2020_ha_sum,na.rm=T)

sf_col_subreg_polyg_srs2 |> group_by(NomSubReg) |> summarise(total=sum(TDF_2020_ha_sum))

#-------------------------------------------------------------------------------
# Protected areas
#-------------------------------------------------------------------------------
# TDF in Runap------------------------------------------------------------------
r_weig_hab_2020_ap <- mask(r_weig_hab_2020,sf_runap_tdf_srs2_filter)

v_weig_hab_2020_area_pa <- exact_extract(r_weig_hab_2020_area,sf_runap_tdf_srs2_filter,fun='sum')
sum(v_weig_hab_2020_area_pa)

# create column in PA polygon with TDF area by 2020
sf_runap_tdf_srs2_filter <- sf_runap_tdf_srs2_filter |> 
  mutate(hab_area_in_pa_ha=v_weig_hab_2020_area_pa,
         prop_hab_in_pa=hab_area_in_pa_ha/size_pa_in_tdf_ha)

# check TDF polygon area in Protected area
summary(sf_runap_tdf_srs2_filter$size_pa_in_tdf_ha)
# check proportion of TDF polygon inside PA
summary(sf_runap_tdf_srs2_filter$prop_hab_in_pa)

# Group by protected area category and filter by area
df_tdf_in_runap <- sf_runap_tdf_srs2_filter |> st_drop_geometry() |> 
  group_by(NomSubReg,categoria,nombre) |> 
  summarise(hab_tdf_sum=round(sum(hab_area_in_pa_ha))) |> arrange(desc(hab_tdf_sum)) 

# check duplicated areas
temp <- sf_runap_tdf_srs2_filter |> count(NomSubReg,categoria,nombre)
sf_runap_tdf_srs2_filter$nombre %in%df_tdf_in_runap$nombre

df_tdf_in_runap |> ungroup() |> count(NomSubReg)
df_tdf_in_runap |> ungroup() |> summarise(hab_tdf_sum_sum=sum(hab_tdf_sum))
df_tdf_in_runap |> ungroup() |> group_by(NomSubReg) |> summarise(hab_tdf_sum_sum=sum(hab_tdf_sum))

df_tdf_in_runap_categories <- df_tdf_in_runap |> ungroup() |> 
  group_by(NomSubReg,categoria) |> summarise(n=n(),Habitat_area_ha=sum(hab_tdf_sum)) |> 
  mutate(PA_by_subreg=sum(Habitat_area_ha), prop_protected_hab=Habitat_area_ha/PA_by_subreg)

sum(df_tdf_in_runap_categories$n)

write.csv2(df_tdf_in_runap_categories,paste("../03_data/outputs/1stBatch/summary_data/df_tdf_in_runap_categories_",Sys.Date(),".csv"))

################################################################################
# Connectivity
################################################################################

#-------------------------------------------------------------------------------
# GISFrag
#-------------------------------------------------------------------------------
mean_gisfrag_1990 <- rast("../../../../../../3yOfLife/Species_Habitat_Index_Colombia/02_outdata/summary/mean_GISfrag1990.tif")
mean_gisfrag_2000 <- rast("../../../../../../3yOfLife/Species_Habitat_Index_Colombia/02_outdata/summary/mean_GISfrag2000.tif")
mean_gisfrag_2010 <- rast("../../../../../../3yOfLife/Species_Habitat_Index_Colombia/02_outdata/summary/mean_GISfrag2010.tif")
mean_gisfrag_2020 <- rast("../../../../../../3yOfLife/Species_Habitat_Index_Colombia/02_outdata/summary/mean_GISfrag2020.tif")

# GISFrag by subregion-----------
v_mean_gisfrag_1990_subreg <- exact_extract(mean_gisfrag_1990,sf_tdf_lim_srs2_subreg,fun='mean')
v_mean_gisfrag_1990_subreg

v_mean_gisfrag_2020_subreg <- exact_extract(mean_gisfrag_2020,sf_tdf_lim_srs2_subreg,fun='mean')
v_mean_gisfrag_2020_subreg

#-------------------------------------------------------------------------------
# Omniscape
#-------------------------------------------------------------------------------
mean_omnisc_1990 <- rast("../../../../../../3yOfLife/Species_Habitat_Index_Colombia/02_outdata/summary/mean_Omnisc1990.tif")
mean_omnisc_2000 <- rast("../../../../../../3yOfLife/Species_Habitat_Index_Colombia/02_outdata/summary/mean_Omnisc2000.tif")
mean_omnisc_2010 <- rast("../../../../../../3yOfLife/Species_Habitat_Index_Colombia/02_outdata/summary/mean_Omnisc2010.tif")
mean_omnisc_2020 <- rast("../../../../../../3yOfLife/Species_Habitat_Index_Colombia/02_outdata/summary/mean_Omnisc2020.tif")

# omnisc by subregion-----------
v_mean_omnisc_1990_subreg <- exact_extract(mean_omnisc_1990,sf_tdf_lim_srs2_subreg,fun='mean')
v_mean_omnisc_1990_subreg

v_mean_omnisc_2020_subreg <- exact_extract(mean_omnisc_2020,sf_tdf_lim_srs2_subreg,fun='mean')
v_mean_omnisc_2020_subreg


temp <- sf_tdf_lim_srs2_subreg |> 
  mutate(gisfrag_1990=v_mean_gisfrag_1990_subreg, gisfrag_2020=v_mean_gisfrag_2020_subreg,
         omnisca_1990=v_mean_omnisc_1990_subreg, omnisca_2020=v_mean_omnisc_2020_subreg)

temp |> select(NomSubReg, gisfrag_1990:omnisca_2020)
