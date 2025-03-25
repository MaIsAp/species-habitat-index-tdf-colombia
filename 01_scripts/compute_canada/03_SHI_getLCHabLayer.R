# This script takes the species name and calculates SHI for that species
# Author: Maria Isabel Arce Plata
# Date: September 2023
#-------------------------------------------------------------------------------
.libPaths("/home/maisap/R/x86_64-pc-linux-gnu-library/4.4/")
.libPaths()

packages <- c("unix","sf","dplyr","terra","geodata","purrr","readr","tidyr")#,"tmap")
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
# IUCN species info path
habitat_inf_path <- "./00_rawdata/tables/df_tdf_sp_habitat_inf.csv"#"./df_habitat.csv"#
iucn_lc_cat_path <- "./00_rawdata/tables/IUCN_to_LC_categories.csv"#"./PhD_Proposal/SHI/InfoHabSuit/IUCN_to_LC_categories.csv"#

# Land cover
lc_path <- "./00_rawdata/IDEAM_corine"#"/run/media/miap/C084AA1284AA0ACC/Hikaru/Doctorado/data/maps/IDEAM/Cobertura_tierra/Rasters/25m"#

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

#-------------------------------------------------------------------------------
# Habitat change
#-------------------------------------------------------------------------------
# Corine Land Cover (IDEAM)-----------------------------------------------------
files_cobs <- list.files(file.path(lc_path),pattern="_25m.tiff$",full.names = T)
print(files_cobs)

l_clc <- map(files_cobs, ~rast(.x))
print(l_clc)

# crop to area of species
l_clc_crop <- map(l_clc, ~terra::crop(.x,r_frame))
clc_names <- paste0("clc_",c("00_02","10_12","18"))
print(l_clc_crop)

s_clc <- rast(l_clc_crop) |> setNames(clc_names)
print(s_clc)

terra::mem_info(s_clc)
free_RAM()

print("-----------------Corine Land Cover layers loaded-----------------------")

df_IUCN_habitat_cat <- read_csv(habitat_inf_path) |> filter(sci_name==sp & suitability=="Suitable") |>
  mutate(code=as.character(code))# Load habitat preferences

#Load table with land cover equivalences needs to be updated with Jung et al
df_IUCN_to_LC_categories <- read.csv(iucn_lc_cat_path,colClasses = "character") # PENDING PUT 0.5 TO MARGINAL HABITATS
df_IUCN_habitat_LC_cat <- left_join(df_IUCN_habitat_cat,df_IUCN_to_LC_categories, by="code")
LC_codes <- as.numeric(unique(df_IUCN_habitat_LC_cat$CORINE_N3))
print(LC_codes)

# create table with suitability by land cover category
df_LC_label <- df_IUCN_to_LC_categories |> dplyr::select(CORINE_N3,CORINE_HAB_N3) |> 
  dplyr::distinct() |> mutate(CORINE_N3=as.numeric(CORINE_N3)) |> mutate(Suitability= if_else(CORINE_N3 %in% LC_codes,1,0)) |>  # assign 1 to land cover categories that correspond to IUCN habitat
  bind_rows(tibble(CORINE_N3=c(99,20,522), # NA and cloud categories
                   Suitability=c(NA,NA,NA)))

print(df_LC_label)

rm(l_clc, l_clc_crop)
gc(T)

#substitute values in raster based on column from df_LC_label
s_clc_cove <- terra::classify(s_clc,df_LC_label |> dplyr::select(-CORINE_HAB_N3)) 
# s_clc_cove <- trim(s_clc_cove)

print("-----------------CLC layers reclassified-----------------------")

map2(as.list(s_clc_cove), clc_names, ~writeRaster(.x,file.path(outputFolder, paste0(sp,"_",.y,"_25m.tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))

print("-----------------CLC layers saved-----------------------")
