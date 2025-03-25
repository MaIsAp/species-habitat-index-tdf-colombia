# This script takes the species name and calculates source and resistance files 
# to enter Omniscape
# Author: Maria Isabel Arce Plata
# Date: May 2024
#-------------------------------------------------------------------------------
.libPaths("/home/maisap/R/x86_64-pc-linux-gnu-library/4.3/")
.libPaths()

packages <- c("unix","terra","sf","dplyr","tidyr","readr","purrr","stringr")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
# 
# # if(length(new.packages)) install.packages(new.packages, repos='https://muug.ca/mirror/cran/')
lapply(packages,require,character.only=T)

mem <- (as.numeric(Sys.getenv("SLURM_MEM_PER_NODE")))*0.95 #reduce mem with reference to memory limit
print(mem)
unix::rlimit_as(mem*1000000,mem*1000000) # convert megabytes to bytes
rlimit_all()

mem_gb <- mem*0.001 # convert megabyes to gigabytes
terraOptions(memmax=mem_gb)

i <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
print(i)


# Parameters and paths----------------------------------------------------------
# species list
sp_file <- "./00_rawdata/tables/df_sp_forSHI.csv"#"./sp_forSHI.csv"#

# outputFolder
outputFolder0 <- "./02_outdata/species"#"/Volumes/Extreme Pro/Hikaru/Doctorado/Documento/Capitulo_01/03_data/outputs/CorridasSHI/species_Vasquez_SHIvs1/species/"#

sp_list <- read.csv(sp_file)
sp <- sp_list$sci_name[i] # assigns species i from list "Astronium graveolens"#
aerial <- sp_list$aerial[i]
# aerial <- 0

outputFolder <- file.path(outputFolder0,gsub(" ","_",sp))#file.path(outputFolder,sp,sp)#
print(outputFolder)

getwd()

#define time steps
t_0 <- 2000
t_n <- 2020 # should be larger than t_0 at least 2 years needed
time_step <- 10
t_range <- ((t_n - t_0)/time_step)
v_time_steps <- seq(t_0,t_n,time_step)
v_time_steps_all <-  c(v_time_steps[1]-time_step,v_time_steps)
time_ranges <- paste(v_time_steps[-length(v_time_steps)],v_time_steps[-1],sep="_")

# Continous sdm path
sdm_path <- "./00_rawdata/SDMs/continuos/"#"./model_output.tif"#"/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/Specific_authors/Vasquez_SDM/continuous/"# 

# habitat layer without mask--
habitat_nomask_files <- list.files(outputFolder,pattern="_habitat_nomask_layer_",full.names = T)

# bounding box not projected
bbox_nosrs_path <- file.path(outputFolder, paste0(sp,"_st_bbox_nosrs.gpkg"))

# habitat layer with cutoff
habitat_over_cutoff_files <- list.files(outputFolder,pattern="_hab_cutoff_",full.names = T)

# Elevation
df_data_avail <- read_csv(file.path(outputFolder,paste0(sp,"_df_data_avail.csv")))

bin_elev_path <- file.path(outputFolder,paste0(sp,"_r_DEM_range_res.tif")) # elevation range according to species for the complete area
elev_path <- file.path(outputFolder,paste0(sp,"_r_DEM_ext.tif"))

# Human footprint
HFP_files <- list.files("./00_rawdata/IHEH",pattern="tif$",full.names = T) #list.files("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/HuellaHumana",pattern="tif$",full.names = T)#
HFP_years <- substr(HFP_files,24,27) #,72,75)# extract years of HFP ###!!!!!! DEPENDS ON FOLDER LOCATION
HFP_years2 <- as.numeric(gsub("2015","2010",gsub("2018","2020",HFP_years))) # set HFP from 2015 for 2010 and 2018 for 2020

# Load reference layers------------------------------------------------------------
r_frame_path <- file.path(outputFolder, paste0(sp,"_r_frame.tif"))
r_frame <- terra::rast(r_frame_path)

country_lim_path <- "./00_rawdata/Study_areas/Colombia_borders_gadm.gpkg"#"/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/Colombia_borders_gadm.gpkg"#
sf_country_lim <- st_read(country_lim_path)

outputFolder <- file.path(outputFolder0,gsub(" ","_",sp))#file.path(outputFolder,sp,sp)#
print(outputFolder)

getwd()

# Load functions----------------------------------------------------------------
#each year
funProp <- function(x,y) x/y
#-------------------------------------------------------------------------------
# 2. Resistance Layer
#-------------------------------------------------------------------------------

# 2.1. Human Footprint (r1)-----------------------------------------------------
l_HFP <- map(HFP_files[HFP_years2%in%v_time_steps], ~terra::rast(.x))
l_HFP_croped <- map(l_HFP, ~terra::crop(.x,r_frame))

l_HFP_pr <- map(l_HFP_croped, ~terra::project(.x,crs("EPSG:3116"),method="near"))# HFP has discrete values
l_HFP_resamp <- map(l_HFP_pr, ~terra::resample(.x,r_frame, method="bilinear"))

s_HFP_res <- terra::rast(l_HFP_resamp) # to check alignment with other inputs

max_HFP <- max(global(s_HFP_res,max,na.rm=T)$max)

s_HFP_res <- terra::app(s_HFP_res, funProp, y=max_HFP)

map2(as.list(s_HFP_res), HFP_years[HFP_years2%in%v_time_steps] , ~terra::writeRaster(.x,file.path(outputFolder, paste0(sp,"_HFP_",.y,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))

# # load pre-calculated values
# l_HFP_res <- map(c("2000","2015","2018"), ~terra::rast(file.path(outputFolder, paste0(sp,"_HFP_",.x,".tif"))))
# s_HFP_res <- rast(l_HFP_res)

rm(l_HFP, l_HFP_pr, l_HFP_resamp)
gc(T)

print(s_HFP_res)
print("==================== r1: HFP layer loaded ==============================")

# 2.2. Distance to habitat (r2)-------------------------------------------------
#Load layers with cutoff for distance
l_habitat_over_cutoff <- map(habitat_over_cutoff_files, ~terra::rast(.x))
l_dist_HabitatArea <- map(l_habitat_over_cutoff, ~terra::gridDist(.x, target=1))
s_dist_HabitatArea <- terra::rast(l_dist_HabitatArea)

rm(l_habitat_over_cutoff, l_dist_HabitatArea)
gc(T)

df_aoh_areas <- read_csv(file.path(outputFolder,paste0(sp,"_df_aoh_areas.csv")))

# set maximum distance as the buffer size for the bounding box
# the buffer size is the sqrt value of the area of habitat
# everything above the buffer size (>1) will be made 1 (specially for the hypothetical year)
s_dist_HabitatAreaProp <- terra::app(s_dist_HabitatArea, funProp, y=df_aoh_areas$buff_size) |>
  setNames(v_time_steps_all)

s_dist_HabitatAreaProp <- terra::ifel(s_dist_HabitatAreaProp>1, 1,s_dist_HabitatAreaProp)

# to avoid setting the maximum to 1, use the highest value from the proportion
# between the maximum distance to habitat between layers for 2000 to 2020
max_dist <- global(s_dist_HabitatAreaProp,max,na.rm=T)$max
# get the maximum proportion from
max_dist_value <- max(max_dist[-1])
# set this proportion of the maximum distance as the
s_dist_HabitatAreaProp <- terra::classify(s_dist_HabitatAreaProp, rcl=cbind(max_dist_value,Inf,max_dist_value)) # turn max dist from other years as reference so the weight is not higher than the other years
s_dist_HabitatAreaProp <- terra::ifel(s_dist_HabitatAreaProp>max_dist_value, max_dist_value,s_dist_HabitatAreaProp) # turn max dist from other years as reference so the weight is not higher than the other years

map2(as.list(s_dist_HabitatAreaProp), v_time_steps_all,~writeRaster(.x,file.path(outputFolder, paste0(sp,"_Dist_HabitatAreaProp_",.y,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))

# # load pre-calculated values
# l_dist_HabitatAreaProp <- map(v_time_steps_all, ~terra::rast(file.path(outputFolder, paste0(sp,"_Dist_HabitatAreaProp_",.x,".tif"))))
# s_dist_HabitatAreaProp <- rast(l_dist_HabitatAreaProp)

gc(T)

print(s_dist_HabitatAreaProp)
print("============== r2: Distance to habitat layer created =====================")

if(df_data_avail$elev_overlap & df_data_avail$source_opt!=4){
  # 2.3. Elevation layer (r3)-----------------------------------------------------
  r_DEM_range_res_s <- rast(file.path(outputFolder, paste0(sp,"_r_DEM_range_res_s.tif")))
  r_DEM_range_bin_r <- 1- r_DEM_range_res_s
  terra::writeRaster(r_DEM_range_bin_r,file.path(outputFolder, paste0(sp,"_r_DEM_range_res_r.tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")
  # r_DEM_range_bin_r <- rast(file.path(outputFolder, paste0(sp,"_r_DEM_range_res_r.tif")))
  
  print(r_DEM_range_bin_r)
  print("============== r3: Elevation range layer loaded =========================")
}

# 2.4. Slope layer (r4)---------------------------------------------------------
r_slope_prop_r <- rast(file.path(outputFolder, paste0(sp,"_r_slope_prop_r.tif")))
print(r_slope_prop_r)
print("============== r4: Slope proportion layer created ======================")

# 2.5. Create layer-------------------------------------------------------------
# Sum layers
r1 <- 1
r2 <- 1
r3 <- 0.5
r4 <- 0.5
if(aerial==1){
  if(!df_data_avail$elev_overlap | df_data_avail$source_opt==4){
    s_resistance_layer <- ((r1*s_HFP_res) + (r2*s_dist_HabitatAreaProp[[-1]]))/(r1 + r2 + r3)
    s_resistance_baselayer <- ((r2*s_dist_HabitatAreaProp[[1]]))/(r1 + r2 + r3)
  } else{
    s_resistance_layer <- ((r1*s_HFP_res) + (r2*s_dist_HabitatAreaProp[[-1]]) + (r3*r_DEM_range_bin_r))/(r1 + r2 + r3 )
    s_resistance_baselayer <- ((r2*s_dist_HabitatAreaProp[[1]]) + (r3*r_DEM_range_bin_r))/(r1 + r2 + r3)
  }
}else{
  if(!df_data_avail$elev_overlap | df_data_avail$source_opt==4){
    s_resistance_layer <- ((r1*s_HFP_res) + (r2*s_dist_HabitatAreaProp[[-1]]) + (r4*r_slope_prop_r))/(r1 + r2 + r3 + r4)
    s_resistance_baselayer <- ((r2*s_dist_HabitatAreaProp[[1]]) + (r4*r_slope_prop_r))/(r1 + r2 + r3 + r4)
    
  }else{
    s_resistance_layer <- ((r1*s_HFP_res) + (r2*s_dist_HabitatAreaProp[[-1]]) + (r3*r_DEM_range_bin_r) + (r4*r_slope_prop_r))/(r1 + r2 + r3 + r4)
    s_resistance_baselayer <- ((r2*s_dist_HabitatAreaProp[[1]]) + (r3*r_DEM_range_bin_r) + (r4*r_slope_prop_r))/(r1 + r2 + r3 + r4)
  }
}

sf_country_lim_srs <- sf_country_lim |> st_transform(st_crs(s_resistance_layer))

# Turn values under 1 into 1 to avoid error in omniscape and NAs to the max resistance value in layers
s_resistance_layer100 <- terra::classify(s_resistance_layer*100, rcl=cbind(0,1,1),include.lowest=T)#,NA)) # 
max_res <- max(global(s_resistance_layer100,"max")$max)
s_resistance_layer100 <- ifel(is.na(s_resistance_layer100),max_res,s_resistance_layer100) # avoid NAs in the middle
# base layer
s_resistance_baselayer <- terra::classify(s_resistance_baselayer*100, rcl=cbind(0,1,1),include.lowest=T)#,NA)) 
max_res_base <- global(s_resistance_baselayer,"max")$max
s_resistance_baselayer <- ifel(is.na(s_resistance_baselayer),max_res_base,s_resistance_baselayer)

s_resistance_layer1 <- c(s_resistance_baselayer,s_resistance_layer100)
s_resistance_layer <- mask(s_resistance_layer1,sf_country_lim_srs) # crop to Colombia limits

rm(s_HFP_res, s_dist_HabitatAreaProp)

map2(as.list(s_resistance_layer),v_time_steps_all,~terra::writeRaster(.x,paste0(outputFolder,"/",sp,"_resist_layer_",.y,".tif"),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))

print(s_resistance_layer)
print("==================== Resistance Layer Saved ============================")
