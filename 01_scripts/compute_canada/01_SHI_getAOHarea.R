# This script calculates the area of the habitat of the species for the TDF
# and replaces it on the previously done table
# Author: Maria Isabel Arce Plata
# Date: August 2024
#-------------------------------------------------------------------------------
.libPaths("/home/maisap/R/x86_64-pc-linux-gnu-library/4.3/")
.libPaths()

packages <- c("unix","sf","dplyr","terra","geodata","purrr","readr","tidyr","units")
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

r_range_map_path <- file.path(outputFolder, paste0(sp,"_r_range_map.tif"))
r_range_map <- rast(r_range_map_path)

r_range_map_area <- terra::cellSize(r_range_map,unit="m")#create raster of areas by pixel
area_aoh <- terra::global(r_range_map_area*r_range_map,sum,na.rm=T)$sum 

df_aoh_areas <- read_csv(file.path(outputFolder,paste0(sp,"_df_aoh_areas.csv")))

#create dataframe with area values
df_aoh_areas$area_aoh <- area_aoh
write_csv(df_aoh_areas,file.path(outputFolder,paste0(sp,"_df_aoh_areas.csv")))
