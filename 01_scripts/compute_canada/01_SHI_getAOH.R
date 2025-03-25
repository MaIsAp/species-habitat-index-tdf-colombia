# This script takes a species name from a list in a csv file and produces the
# Area of Habitat for the species at an specified location with path 'area_lim1_path'
# by downloading range maps from IUCN, loading an SDM
# filtering by elevation ranges for the species according to IUCN
# Author: Maria Isabel Arce Plata
# Date: September 2023
#-------------------------------------------------------------------------------
.libPaths("/home/maisap/R/x86_64-pc-linux-gnu-library/4.3/")
.libPaths()

packages <- c("unix","sf","dplyr","terra","geodata","purrr","readr","tidyr","units","stars")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
new.packages

# if(length(new.packages)) install.packages(new.packages, repos='https://muug.ca/mirror/cran/')
lapply(packages,require,character.only=T)

mem <- (as.numeric(Sys.getenv("SLURM_MEM_PER_NODE")))*0.90 #reduce mem with reference to memory limit
print(mem)
unix::rlimit_as(mem*1000000,mem*1000000) # convert megabytes to bytes
rlimit_all()

mem_gb <- mem**0.001 # convert megabyes to gigabytes
terraOptions(memmax=mem_gb)

i <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
print(i)

# Parameters--------------------------------------------------------------------

# spatial resolution
spat_res <- 100
# srs
sf_srs <- st_crs(3116)
srs_ras <- crs("EPSG:3116")

# species list
sp_file <- "./00_rawdata/tables/df_sp_forSHI.csv"#"./00_rawdata/tables/df_spVasquez.csv"#

# range maps path
range_map_path <- "./00_rawdata/Range_maps/Range_maps_Col_TDF_limits_united.gpkg"#"/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IUCN/RangeMapsColombia/Range_maps_Colombia.gpkg"#
# study area limit
area_lim1_path <- "./00_rawdata/Study_areas/EcosistOrigBsTaeEtter_dissolve.gpkg"#"./00_rawdata/Study_areas/Colombia_borders_gadm.gpkg"# "/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/BST/riesgo_etter/EcosistOrigBsTaeEtter_dissolve.gpkg"#
# elevation map path
dem_path <- "./00_rawdata/DEM/r_DEMCol_100m.tif"#"/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/PlanetaryComputer/cop-dem-glo-90/r_DEMCol_100m.tif"# 
# binary sdm
bin_sdm_path <- "./00_rawdata/SDMs/binarios"#"/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/Specific_authors/Vasquez_SDM/binary"#

# outputFolder
outputFolder0 <- "./02_outdata/species"#file.path("./Connectivity/layers_for_omniscape")#
getwd()

#-------------------------------------------------------------------------------
# Load inputs
#-------------------------------------------------------------------------------
sp_list <- read.csv(sp_file)
sp <- sp_list$sci_name[i] # assigns species i from list
print(sp)

outputFolder <- file.path(outputFolder0,gsub(" ","_",sp))#file.path(outputFolder,sp,sp)#
print(outputFolder)

if (!dir.exists(file.path(outputFolder))){
  dir.create(file.path(outputFolder))
}else{
  print("dir exists")
}

# Study area
sf_area_lim1 <- st_read(area_lim1_path)
sf_area_lim1_srs <- sf_area_lim1 |> st_transform(sf_srs)
area_study_a <- sf_area_lim1_srs |> st_area()

print(sf_area_lim1)

print("========== Study area successfully loaded ==========")

# range map
sf_range_maps_colombia <- st_read(range_map_path)
sf_range_map <- sf_range_maps_colombia |> dplyr::filter(sci_name==sp) # load range map species i
print(sf_range_map)

sf_range_map_crs <- sf_range_map |> st_transform(st_crs(sf_srs))
area_range_map <- sf_range_map_crs |> st_area()

rm(sf_range_maps_colombia)
print("========== Expert range map successfully loaded ==========")

# check if model file exists
sdm_file <- list.files(bin_sdm_path,pattern = gsub(" ","_",sp),full.names=TRUE)
if(length(sdm_file)==0){
  sdm_file <- list.files(bin_sdm_path,pattern = sp,full.names=TRUE)
}
print(sdm_file)

model_avail <- length(sdm_file)>0
if(model_avail==T){
  # if model file exists, load SDM
  r_sdm <- terra::rast(sdm_file)
  non_empty <- global(r_sdm,max,na.rm=T)$max
  cat("Maximum value in raster is:",non_empty)
  if(non_empty!=0){
    r_sdm <- terra::ifel(r_sdm==1,1,NA) # only consider 1s and convert 0 to NA
  }else{
    model_avail <- FALSE
    print("=========Empty model===========")
  }
}
  
#-------------------------------------------------------------------------------
# Get AOH
#-------------------------------------------------------------------------------
# define base area according to availability of IUCN range map or Species Distribution Model
range_map_source <- data.frame(range_map=nrow(sf_range_map)>0,
                               sdm_file=model_avail) |> 
  mutate(source_opt= case_when(
    range_map & sdm_file  ~1,
    range_map & !sdm_file ~2,
    !range_map & sdm_file ~3,
    !range_map & !sdm_file ~4
    ))

print(range_map_source)

# If a range map is available it continues with the process, but if there is no data 
# on the species distribution it should throw a message for the user to know the issue 
# and if there is a SDM available it takes two possible paths, overlap range map 
# and SDM, but if there is no overlap give priority to SDM or if there is no range map use SDM
if(range_map_source$source_opt==4){
  print("No range map or SDM found")
}else{
  if(range_map_source$source_opt!=2){
    # if just SDM is available that will be the range map
    if(range_map_source$source_opt==3){
      r_range_map2 <- r_sdm
    }
    
    # if range map and sdm are available crop SDM with range map
    if(range_map_source$source_opt==1){
      # load range map
      r_range_map2 <- project(r_sdm, crs(sf_range_map), method="near")
      r_range_map2 <- mask(crop(r_range_map2, sf_range_map), sf_range_map)
      
      # for cases when range map and SDM do not overlap (length(r_range_map2)==0), use SDM
      if(length(r_range_map2)==0){
        cat("Species distribution and study area do not overlap \n")
        r_range_map2 <- r_sdm
      }
    }
    # convert range map to polygon for cases where SDM was used
    sf_range_map <<- terra::as.polygons(r_range_map2) |> st_as_sf()
    print(sf_range_map)
  }
}

# # check polygon created previously or the range map from IUCN if it was the only source available --- !!Commented because geometry issues are supposed to be fixed in this new polygons file
# geometry_test <- class(st_geometry(sf_range_map))[1]
# print(geometry_test)
# 
# # convert polygons to multipolygon in case there is a curvepolygon
# if(geometry_test=="sfc_CURVEPOLYGON"){
#   sf_range_map <- st_cast(sf_range_map, "GEOMETRYCOLLECTION") %>%
#     st_collection_extract("LINESTRING") |> st_cast("POLYGON") |> st_cast("MULTIPOLYGON")
# }

# align range map projection to study area reference system
sf_area_lim2 <- sf_range_map |> st_make_valid() |> st_transform(st_crs(sf_area_lim1))
# assign desired srs of study to range map
sf_area_lim2_srs <- sf_area_lim2 |> st_transform(sf_srs)

# get area of range map
area_range <- sf_area_lim2_srs |> st_combine() |> st_area() # 

# range map for study area
sf_area_lim <- st_intersection(sf_area_lim2,sf_area_lim1) |> 
  st_make_valid()
sf_area_lim_srs <- st_intersection(sf_area_lim2_srs,sf_area_lim1_srs) |> 
  st_make_valid()

# get area of range map inside study area
area_aoh <- sf_area_lim_srs |> st_area()

# Buffer size for bbox
sf_bbox_aoh <- sf_area_lim_srs |> st_bbox() |> st_as_sfc()
area_bbox <- sf_bbox_aoh |> st_area()
buff_size <- round(sqrt(area_aoh)/2)
st_write(sf_bbox_aoh,file.path(outputFolder, paste0(sp,"_st_bbox_aoh.gpkg")),append=F) # for a bbox without the buffer

sf_ext_srs <- st_bbox(sf_area_lim_srs |> st_buffer(buff_size))

# Create bounding box
sf_bbox <- st_as_sfc(sf_ext_srs)
print(sf_bbox)
bbox_path <- file.path(outputFolder, paste0(sp,"_st_bbox.gpkg"))
st_write(sf_bbox,bbox_path,append=F)

sf_bbox_nosrs <- sf_bbox |> st_transform(st_crs(sf_area_lim))
sf_ext <- st_bbox(sf_bbox_nosrs)
bbox_nosrs_path <- file.path(outputFolder, paste0(sp,"_st_bbox_nosrs.gpkg"))
st_write(sf_ext|> st_as_sfc(),bbox_nosrs_path,append=F)

# Bounding box raster
r_frame <- terra::rast(terra::ext(sf_ext_srs),resolution=spat_res)
terra::crs(r_frame) <- srs_ras
terra::values(r_frame) <- 1
r_frame_path <- file.path(outputFolder, paste0(sp,"_r_frame.tif"))
terra::writeRaster(r_frame,r_frame_path,overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")
r_range_map <- terra::mask(r_frame,terra::vect(sf_area_lim_srs)) # Mask to study area

gc(T)
terra::mem_info(r_range_map)
free_RAM()

print("========== 1st version of map of area of habitat generated ==========")

# Load elevation preferences
df_IUCN_sheet <- sp_list |> filter(sci_name==sp)#rredlist::rl_search(sp, key = token)$result

df_IUCN_sheet_condition <- df_IUCN_sheet |> dplyr::mutate(
  min_elev= case_when( #evaluate if elevation ranges exist and add margin if included
    is.na(elevation_lower) ~ NA_real_,
    !is.na(elevation_lower) ~ as.numeric(elevation_lower)),
  max_elev= case_when(
    is.na(elevation_upper) ~ NA_real_,
    !is.na(elevation_upper) ~ as.numeric(elevation_upper))
) |> select(sci_name,elevation_lower, elevation_upper,min_elev,max_elev) |> 
  mutate(elev_opt = case_when(!is.na(min_elev) & !is.na(max_elev) ~ 1, # to filter elevation range by df_IUCN_sheet_condition$min_elev and max_elev values
                                !is.na(min_elev) &  is.na(max_elev) ~ 2, # to filter elevation range just for max_elev 
                                is.na(min_elev) & !is.na(max_elev) ~ 3, # to filter elevation range just for df_IUCN_sheet_condition$min_elev
                                is.na(min_elev) & is.na(max_elev) ~ 4)) # to keep r_range_map without elevation filter

# create table of data source availability
df_data_avail <- df_IUCN_sheet_condition
df_data_avail <- df_data_avail |> bind_cols(range_map_source) |> mutate(elev_overlap=FALSE)

# Create elevation layer that will be used in script 07 to create slope
# Load elevation map and filter by elevation preferences
r_DEM <- terra::rast(dem_path)
r_DEM_ext <- terra::crop(r_DEM, sf_ext_srs)
# save layer to create source and resistance layers for omniscape
writeRaster(r_DEM_ext,file.path(outputFolder,paste0(sp,"_r_DEM_ext.tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")

# loads elevation map if there is elevation data and when there is no elevation info available it just keeps using the map before this
if(df_IUCN_sheet_condition$elev_opt!=4){
  if(df_IUCN_sheet_condition$elev_opt==1){
    r_DEM_range <- terra::ifel(r_DEM_ext >= df_IUCN_sheet_condition$min_elev & r_DEM_ext <= df_IUCN_sheet_condition$max_elev,1,0) 
  }
  if(df_IUCN_sheet_condition$elev_opt==2){
    r_DEM_range <- terra::ifel(r_DEM_ext >= df_IUCN_sheet_condition$min_elev,1,0)
  }
  if(df_IUCN_sheet_condition$elev_opt==3){
    r_DEM_range <- terra::ifel(r_DEM_ext <= df_IUCN_sheet_condition$max_elev,1,0)
  }
  r_DEM_range_res <- terra::resample(r_DEM_range, r_range_map, method="near") # resample to raster of SDM
  
  print(r_DEM_range_res)
  # save layer to create source and resistance layers for omniscape in script 07
  terra::writeRaster(r_DEM_range_res,file.path(outputFolder,paste0(sp,"_r_DEM_range_res.tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")
  
  # evaluate if there is no overlap or the raster is empty
  r_DEM_range_overlap <- r_DEM_range_res + r_range_map
  
  df_data_avail$elev_overlap <- terra::global(r_DEM_range_overlap, max, na.rm=T)$max ==2
  print(df_data_avail$elev_overlap)
  
  if(df_data_avail$elev_overlap){
    r_range_map <- terra::mask(r_range_map,r_DEM_range_res,maskvalues=0) #Mask range map to elevation ranges
  }
}

print(df_data_avail)

write_csv(df_data_avail, file.path(outputFolder,paste0(sp,"_df_data_avail.csv")))

r_range_map_path <- file.path(outputFolder, paste0(sp,"_r_range_map.tif"))
terra::writeRaster(r_range_map, r_range_map_path, overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")

print(r_range_map)

print("========== Map of area of habitat generated ==========")

# RASTER OF "Variability of dat sources" OF Area of Habitat

r_range_map_area <- terra::cellSize(r_range_map,unit="m")#create raster of areas by pixel
area_aoh <- terra::global(r_range_map_area*r_range_map,sum)$sum 

#create dataframe with area values
df_aoh_areas <- tibble(sci_name=sp, area_range = area_range, area_bbox=area_bbox,
                       buff_size=buff_size, area_aoh=area_aoh ,area_study_a=area_study_a)
write_csv(df_aoh_areas,file.path(outputFolder,paste0(sp,"_df_aoh_areas.csv")))

print("========== Table with all area values produced ==========")

# for areas where there is a range map available there will be 1s, for SDMs 10s
# and if there was information about elevation used, the areas inside the range will have a 100

if(range_map_source$source_opt==1 | range_map_source$source_opt==2){
  r_range_source1 <- st_rasterize(sf_range_map_crs, st_as_stars(r_frame)) |> terra::rast()
  r_range_source1 <- terra::ifel(is.na(r_range_source1),0,1)
  cat("========== Raster of range map generated ==========")
  if(range_map_source$source_opt==1){
    r_sdm_crs <- terra::project(r_sdm,srs_ras,method="near")
    r_range_source2 <- terra::resample(r_sdm_crs, r_range_map, method="near")
    r_range_source2 <- terra::ifel(is.na(r_range_source2),0,10)
    # r_range_source <- r_range_source1 + r_range_source2
    r_range_source <- app(c(r_range_source1,r_range_source2),fun=sum)
    cat("========== Raster of SDM & range map generated ==========")
  }
  if(df_IUCN_sheet_condition$elev_opt!=4){
    r_range_source3 <- terra::ifel(is.na(r_DEM_range_res),0,100)
    # r_range_source <- r_range_source1 + r_range_source3
    r_range_source <- app(c(r_range_source1,r_range_source3),fun=sum)
  }else{
    r_range_source <- r_range_source1
  }
}else{
  if(range_map_source$source_opt==3){
    r_sdm_crs <- project(r_sdm,srs_ras,method="near")
    r_range_source1 <- terra::resample(r_sdm_crs, r_range_map, method="near")
    r_range_source1 <- terra::ifel(is.na(r_range_source1),0,10)
    cat("========== Raster of SDM generated ==========")
    if(df_IUCN_sheet_condition$elev_opt!=4){
      r_range_source3 <- terra::ifel(is.na(r_DEM_range_res),0,100)
      # r_range_source <- r_range_source1 + r_range_source3
      r_range_source <- app(c(r_range_source1,r_range_source3),fun=sum)
    }else{
      r_range_source <- r_range_source1
    }
  }
}
print("========== Map for range source availability generated ==========")

# Range map source availability approximation
r_source_map_path <- file.path(outputFolder, paste0(sp,"_r_source_map.tif"))
terra::writeRaster(r_range_source, r_source_map_path, overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")



# gc(T)
# 
# terra::mem_info(r_range_map)
# free_RAM()
# 
# rm(r_DEM, r_DEM_ext, r_DEM_range, r_DEM_range_res, r_bin_sdm, r_bin_sdm_frame, r_bin_sdm_frame_srs, r_bin_sdm_frame_res)
