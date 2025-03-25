# This script takes the species name and calculates SHI for that species
# Author: Maria Isabel Arce Plata
# Date: September 2023
#-------------------------------------------------------------------------------
.libPaths("/home/maisap/R/x86_64-pc-linux-gnu-library/4.3/")
.libPaths()

packages <- c("unix","sf","dplyr","terra","geodata","purrr","readr","tidyr")#,"tmap")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
new.packages

# if(length(new.packages)) install.packages(new.packages, repos='https://muug.ca/mirror/cran/')
lapply(packages,require,character.only=T)

mem <- (as.numeric(Sys.getenv("SLURM_MEM_PER_NODE")))*0.95 #reduce mem with reference to memory limit
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
v_time_steps <- seq(t_0,t_n,time_step)
v_time_steps_all <-  c(v_time_steps[1]-time_step,v_time_steps)
time_ranges <- paste(v_time_steps[-length(v_time_steps)],v_time_steps[-1],sep="_")

# species list
sp_file <- "./00_rawdata/tables/df_sp_forSHI.csv"#"./sp_forSHI.csv"#

# outputFolder
outputFolder0 <- "./02_outdata/species"#file.path("./Connectivity/layers_for_omniscape")#
getwd()

#-------------------------------------------------------------------------------
# Load inputs
#-------------------------------------------------------------------------------
sp_list <- read.csv(sp_file)
sp <- sp_list$sci_name[i] # assigns species i from list

outputFolder <- file.path(outputFolder0,gsub(" ","_",sp))#file.path(outputFolder,sp,sp)#
print(outputFolder)

if (!dir.exists(file.path(outputFolder))){
  dir.create(file.path(outputFolder))
}else{
  print("dir exists")
}

r_frame <- rast(file.path(outputFolder, paste0(sp,"_r_frame.tif")))
r_range_map <- rast(file.path(outputFolder, paste0(sp,"_r_range_map",".tif")))

#****
clc_cover25m_files <- list.files(outputFolder,pattern="_25m",full.names = T)
print(clc_cover25m_files)
l_clc_cove <- map(clc_cover25m_files, ~rast(.x))
s_clc_cove <- rast(l_clc_cove)

rm(l_clc_cove)
#****

#-------------------------------------------------------------------------------
# Habitat change
#-------------------------------------------------------------------------------
# Corine Land Cover (IDEAM)-----------------------------------------------------
#3.2.2. Measure loss and gain, rescale and save---------------------------------
r_clc_change_00_10 <- s_clc_cove[[2]] - s_clc_cove[[1]]
r_clc_change_10_20 <- s_clc_cove[[3]] - s_clc_cove[[2]]

s_clc_change <- c(r_clc_change_00_10,r_clc_change_10_20)
s_clc_loss <- as.numeric(s_clc_change<0)
s_clc_gain <- as.numeric(s_clc_change>0)

rm(r_clc_change_00_10, r_clc_change_10_20,
   s_clc_change)

gc(T)

#3.2.3. rescale cover and save--------------------------------------------------
scale_fact <- round(spat_res/res(s_clc_cove[[1]]))
time_ranges_clc <- time_ranges[-1]

l_clc_cove_rescale <- list(NA)
for(i in 1:length(v_time_steps)){
  l_clc_cove_rescale[[i]] <- terra::aggregate(s_clc_cove[[i]], fact= scale_fact[1] , fun= mean , na.rm=T)
  # writeRaster(l_clc_cove_rescale[[i]],file.path(outputFolder,paste0(sp,"_rescaled_cove_clc_",v_time_steps[i],".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")
}

l_clc_loss_rescale <- list(NA)
l_clc_gain_rescale <- list(NA)
for(i in 1:length(time_ranges_clc)){
  l_clc_loss_rescale[[i]] <- terra::aggregate(s_clc_loss[[i]], fact= scale_fact[1] , fun= mean , na.rm=T)
  # writeRaster(l_clc_loss_rescale[[i]],file.path(outputFolder,paste0(sp,"_rescaled_loss_clc_",time_ranges_clc[i],".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")
  l_clc_gain_rescale[[i]] <- terra::aggregate(s_clc_gain[[i]], fact= scale_fact[1] , fun= mean , na.rm=T)
  # writeRaster(l_clc_gain_rescale[[i]],file.path(outputFolder,paste0(sp,"_rescaled_gain_clc_",time_ranges_clc[i],".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")
}

rm(s_clc_cove,s_clc_loss,s_clc_gain)
gc(T)

print("-----------------------CLC layers rescaled------------------------------")

fun_standardize_layers <- function(x,range,names){
  x <- rast(x)
  rsmple <<- terra::resample(x,range)
  masked <- terra::mask(rsmple,range)
  names(masked) <- names
  return(masked)
}

s_habitat_clc_nomask <- terra::resample(rast(l_clc_cove_rescale),r_range_map)
names(s_habitat_clc_nomask) <- v_time_steps
s_habitat_clc_loss <- fun_standardize_layers(l_clc_loss_rescale, r_range_map ,time_ranges_clc)
s_habitat_clc_gain <- fun_standardize_layers(l_clc_gain_rescale, r_range_map ,time_ranges_clc)

rm(l_clc_cove_rescale, l_clc_loss_rescale, l_clc_gain_rescale)
gc(T)

map2(as.list(s_habitat_clc_nomask), v_time_steps, ~terra::writeRaster(.x,file.path(outputFolder,paste0(sp,"_cove_clc_",.y,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))
map2(as.list(s_habitat_clc_loss), time_ranges_clc , ~terra::writeRaster(.x,file.path(outputFolder,paste0(sp,"_loss_clc_",.y,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))
map2(as.list(s_habitat_clc_gain), time_ranges_clc, ~terra::writeRaster(.x,file.path(outputFolder,paste0(sp,"_gain_clc_",.y,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))

print("-----------------------CLC layers saved------------------------------")
