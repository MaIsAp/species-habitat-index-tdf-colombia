library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(units)
library(gdalUtilities)

# sp list
df_sp_info <- read_csv("./00_rawdata/tables/df_tdf_sp_inf.csv")

# study area limit -TDF
study_area_lim_path <- "/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/BST/riesgo_etter/EcosistOrigBsTaeEtter_dissolve.gpkg"#"./00_rawdata/Study_areas/Colombia_borders_gadm.gpkg"# "/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/BST/riesgo_etter/EcosistOrigBsTaeEtter_dissolve.gpkg"#
sf_study_area <- st_read(study_area_lim_path) 

# Colombia limits
colombia_lim <- "/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/Colombia_boundaries/Colombia_borders_gadm.gpkg"
sf_colombia_lim <- st_read(colombia_lim)

#-------------------------------------------------------------------------------
# IUCN rangemaps
#-------------------------------------------------------------------------------
# complete extension of range maps that overlap with col
sf_rangemaps_col <- st_read("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IUCN/RangeMapsColombia/Range_maps_Colombia.gpkg")
# filter to list of TDF species
sf_rangemaps_col_tdf <- sf_rangemaps_col |> filter(sci_name %in% df_sp_info$sci_name)
# save to file
st_write(sf_rangemaps_col_tdf,"/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IUCN/RangeMapsColombia/Range_maps_Colombia_TDF_splist.gpkg")

# create dataframe with counts by groups
df_rangemaps_col_tdf_count_pol <- sf_rangemaps_col_tdf |> st_drop_geometry() |> count(class,sci_name) |> arrange(desc(n)) 
# get count by class
df_rangemaps_col_tdf_count_pol |> count(class)

#-------------------------------------------------------------------------------
# Load clipped range maps in QGIS for TDF sp list 
#-------------------------------------------------------------------------------
# clipped to Colombia limits
sf_rangemaps_col_lim_tdf_list <- st_read("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IUCN/RangeMapsColombia/TDF/Range_maps_Col_limits_TDF_splist.gpkg")
# clipped to TDF limits converted to MULTILINESTRING to avoid issues with MULTISURFACE
sf_rangemaps_col_tdf_lim_list_lines <- st_read("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IUCN/RangeMapsColombia/TDF/Range_maps_Col_TDF_limits_splist_lines.gpkg")
# convert to MULTIPOLYGON
sf_rangemaps_col_tdf_lim_list_polyg <- sf_rangemaps_col_tdf_lim_list_lines |> st_cast("MULTILINESTRING") |> st_cast("MULTIPOLYGON")
# transform to planar projection that allows to run
sf_rangemaps_col_tdf_lim_list_crs <- sf_rangemaps_col_tdf_lim_list_polyg |> st_make_valid() |> st_transform(st_crs(3116))
# group polygons by species
sf_rangemaps_col_tdf_lim_list_union <- sf_rangemaps_col_tdf_lim_list_crs |> group_by(sci_name,class,category) |> 
  summarise(geom = st_union(geom)) 

# check that species are not repeated 
count_sp <- sf_rangemaps_col_tdf_lim_list_union |> st_drop_geometry() |> count(sci_name) # 9 less than in df_range_maps_col_tdf_count_pol

sp_difference <- df_rangemaps_col_tdf_count_pol |> filter(!sci_name %in% count_sp$sci_name) # the range maps do not overlap with Colombia

#check geometries---------------------
l_test_geometry <- map(count_sp$sci_name, ~sf_rangemaps_col_tdf_lim_list_union |> filter(sci_name==.x) |> st_geometry() |> class())
v_test_geometry <- map(l_test_geometry,1) |> unlist()
sum(grepl("sfc_MULTIPOLYGON",v_test_geometry))
#-------------------------------------

# save polygons
st_write(sf_rangemaps_col_tdf_lim_list_union,"/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IUCN/RangeMapsColombia/TDF/Range_maps_Col_TDF_limits_united.gpkg")
