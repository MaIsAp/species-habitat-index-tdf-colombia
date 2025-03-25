# This script takes a species name from a list in a csv file and produces the
# time series of changes in forest for the area of habitat of the species produced 
# in '01_SHI_getAOH' by using the maps from IDEAM for forest - non forest
# Author: Maria Isabel Arce Plata
# Date: January 2024
#-------------------------------------------------------------------------------
.libPaths("/home/maisap/R/x86_64-pc-linux-gnu-library/4.3/")
.libPaths()

packages <- c("unix","sf","dplyr","terra","geodata","purrr","readr","tidyr","stringr")#,"tmap")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
new.packages

# if(length(new.packages)) install.packages(new.packages, repos='https://muug.ca/mirror/cran/')
lapply(packages,require,character.only=T)

mem <- (as.numeric(Sys.getenv("SLURM_MEM_PER_NODE")))*0.90 #reduce mem with reference to memory limit
print(mem)
unix::rlimit_as(mem*1000000,mem*1000000) # convert megabytes to bytes
rlimit_all()

mem_gb <- mem*0.001 # convert megabyes to gigabytes
terraOptions(memmax=mem_gb)

i <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
print(i)

# Parameters--------------------------------------------------------------------
# spatial resolution
spat_res <- 100
# srs
sf_srs <- st_crs(3116)# 2138#

#define time steps
t_0 <- 2000
t_n <- 2020 # should be larger than t_0 at least 2 years needed
time_step <- 10
t_range <- ((t_n - t_0)/time_step)
v_time_steps <- seq(t_0,t_n,time_step)
v_time_steps_all <-  c(v_time_steps[1]-time_step,v_time_steps)
time_ranges <- paste(v_time_steps[-length(v_time_steps)],v_time_steps[-1],sep="_")

# species list
sp_file <- "./00_rawdata/tables/df_sp_forSHI.csv"#"./sp_forSHI.csv"#

# outputFolder
outputFolder0 <- "./02_outdata/species"#file.path("./Connectivity/layers_for_omniscape")#
getwd()

# Bosque no Bosque path
forest_path <- "./00_rawdata/IDEAM_categories"# "/run/media/miap/C084AA1284AA0ACC/Hikaru/Doctorado/data/maps/IDEAM/Cambio_Cobertura_Bosque_No_Bosque"#

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

r_frame <- rast(file.path(outputFolder, paste0(sp,"_r_frame.tif")))
r_range_map <- rast(file.path(outputFolder, paste0(sp,"_r_range_map",".tif")))

#-------------------------------------------------------------------------------
# Habitat change
#-------------------------------------------------------------------------------
# Load COLOMBIA DATA IDEAM forest-non forest and crop to r_frame
bnb_cover_files <- list.files(path=file.path(forest_path,"Cover"),pattern="_25m.tif",full.names = T)#######"BnB_ActualCover"
bnb_cover_files2 <- list.files(path=file.path(forest_path,"Cover"),pattern="_25m.tif",full.names = F)#
bnb_cover_files_years <- as.numeric(substr(bnb_cover_files2,start=7,stop=10)) # extract years from names
bnb_cover_files2 <- bnb_cover_files[bnb_cover_files_years %in% v_time_steps] # filter to time steps
print(bnb_cover_files2)

l_bnb_cove <- map(bnb_cover_files2, ~rast(.x))
s_bnb_cove <- rast(l_bnb_cove)
s_bnb_cove_range <- terra::crop(s_bnb_cove,r_frame)

bnb_loss_files <- list.files(path=file.path(forest_path,"Loss"),pattern="_25m.tif",full.names = T)#######"BnB_Loss_periods"
bnb_loss_files2 <- list.files(path=file.path(forest_path,"Loss"),pattern="_25m.tif",full.names = F)#######"BnB_Loss_periods"
bnb_loss_files_years <- substr(bnb_loss_files2,start=6,stop=14) # extract years from names
bnb_loss_files2 <- bnb_loss_files[bnb_loss_files_years %in% time_ranges] # filter to time steps
print(bnb_loss_files2)

l_bnb_loss <- map(bnb_loss_files2, ~rast(.x))
s_bnb_loss <- rast(l_bnb_loss)
s_bnb_loss_range <- terra::crop(s_bnb_loss,r_frame)

bnb_gain_files <- list.files(path=file.path(forest_path,"Gain"),pattern="_25m.tif",full.names = T)######"BnB_Gain_periods"
bnb_gain_files2 <- list.files(path=file.path(forest_path,"Gain"),pattern="_25m.tif",full.names = F)######"BnB_Gain_periods"
bnb_gain_files_years <- substr(bnb_gain_files2,start=6,stop=14) # extract years from names
bnb_gain_files2 <- bnb_gain_files[bnb_gain_files_years %in% time_ranges] # filter to time steps
print(bnb_gain_files2)

l_bnb_gain <- map(bnb_gain_files2, ~rast(.x))
s_bnb_gain <- rast(l_bnb_gain)
s_bnb_gain_range <- terra::crop(s_bnb_gain,r_frame)

terra::mem_info(s_bnb_gain_range)
free_RAM()

print("-----------------Forest-Non Forest layers loaded-----------------------")

scale_fact <- round(spat_res/res(l_bnb_cove[[1]]))

l_bnb_cove_rescale <- list(NA)
for(i in 1:length(l_bnb_cove)){
  l_bnb_cove_rescale[[i]] <- terra::aggregate(s_bnb_cove_range[[i]], fact= scale_fact[1] , fun= mean , na.rm=T)
  # terra::writeRaster(l_bnb_cove_rescale[[i]],file.path(outputFolder,paste0(sp,"_rescaled_cove_bnb_",v_time_steps[i],".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")
}

l_bnb_loss_rescale <- list(NA)
l_bnb_gain_rescale <- list(NA)
for(i in 1:length(l_bnb_loss)){
  l_bnb_loss_rescale[[i]] <- terra::aggregate(s_bnb_loss_range[[i]], fact= scale_fact[1] , fun= mean , na.rm=T)
  # terra::writeRaster(l_bnb_loss_rescale[[i]],file.path(outputFolder,paste0(sp,"_rescaled_loss_bnb_",time_ranges[i],".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")
  l_bnb_gain_rescale[[i]] <- terra::aggregate(s_bnb_gain_range[[i]], fact= scale_fact[1] , fun= mean , na.rm=T)
  # terra::writeRaster(l_bnb_gain_rescale[[i]],file.path(outputFolder,paste0(sp,"_rescaled_gain_bnb_",time_ranges[i],".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")
}

print("-----------------Forest-Non Forest layers rescaled-----------------------")

fun_standardize_layers <- function(x,range,names){
  x <- rast(x)
  rsmple <<- terra::resample(x,range)
  masked <- terra::mask(rsmple,range)
  print(names(masked))
  names(masked) <- names
  return(masked)
}

# save forest cover layers
s_habitat_bnb_nomask <- terra::resample(rast(l_bnb_cove_rescale),r_range_map)
print(names(s_habitat_bnb_nomask))
names(s_habitat_bnb_nomask) <- v_time_steps

map2(as.list(s_habitat_bnb_nomask), v_time_steps, ~terra::writeRaster(.x,file.path(outputFolder,paste0(sp,"_cove_bnb_",.y,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))

# save cover loss layers
s_habitat_bnb_loss <- fun_standardize_layers(l_bnb_loss_rescale, r_range_map ,time_ranges)
map2(as.list(s_habitat_bnb_loss), time_ranges , ~terra::writeRaster(.x,file.path(outputFolder,paste0(sp,"_loss_bnb_",.y,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))

# save cover gain layers
s_habitat_bnb_gain <- fun_standardize_layers(l_bnb_gain_rescale, r_range_map ,time_ranges)
map2(as.list(s_habitat_bnb_gain), time_ranges, ~terra::writeRaster(.x,file.path(outputFolder,paste0(sp,"_gain_bnb_",.y,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))

# rm(l_bnb_cove, s_bnb_cove, s_bnb_cove_range,
#    l_bnb_loss, s_bnb_loss, s_bnb_loss_range,
#    l_bnb_gain, s_bnb_gain, s_bnb_gain_range,
#    l_bnb_cove_rescale, l_bnb_loss_rescale, l_bnb_gain_rescale)
# gc(T)

