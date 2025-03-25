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

# Parameters and paths----------------------------------------------------------

#define time steps
t_0 <- 2000
t_n <- 2020 # should be larger than t_0 at least 2 years needed
time_step <- 10
t_range <- ((t_n - t_0)/time_step)
v_time_steps <- seq(t_0,t_n,time_step)
v_time_steps_all <-  c(v_time_steps[1]-time_step,v_time_steps)

# proportion of habitat to consider for the habitat map (at least 1000m2 - stepping stone patches)
habitat_cutoff <- 0.1

# species list
sp_file <- "./00_rawdata/tables/df_sp_forSHI.csv"#"./sp_forSHI.csv"#

# outputFolder
outputFolder0 <- "./02_outdata/species"
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
# r_range_map <- rast(file.path(outputFolder, paste0(sp,"_r_range_map_",".tif")))
# r_range_map

#****
habitat_files <- list.files(outputFolder,pattern="habitat_layer",full.names = T)
l_habitat <- map(habitat_files, ~rast(.x))
s_habitat <- rast(l_habitat)
#****

#****
habitat_nomask_files <- list.files(outputFolder,pattern="_habitat_nomask_layer",full.names = T)
l_habitat_nomask <- map(habitat_nomask_files , ~rast(.x))
s_habitat_nomask <- rast(l_habitat_nomask)
#****

# #-------------------------------------------------------------------------------
# # 5. Measure scores
# #-------------------------------------------------------------------------------

# Area Score--------------------------------------------------------------------
r_areas <- terra::cellSize(s_habitat[[2]],unit="ha")#create raster of areas by pixel

s_habitat_area <- s_habitat * r_areas
habitat_area <- terra::global(s_habitat_area, sum, na.rm=T)

df_area_score <- tibble(sci_name=sp, Year= v_time_steps_all, Area=units::set_units(habitat_area$sum,"ha")) |>
  dplyr::mutate(ref_area=first(Area)) |> dplyr::group_by(Year) |> mutate(diff=ref_area-Area, percentage=as.numeric(Area*100/ref_area),score="AS")

write_csv(df_area_score,file.path(outputFolder,paste0(sp,"_df_area_score.csv")))

df_area_score2 <- tibble(sci_name=sp, Year= v_time_steps, Area=units::set_units(habitat_area$sum[-1],"ha")) |>
  dplyr::mutate(ref_area=first(Area)) |> dplyr::group_by(Year) |> mutate(diff=ref_area-Area, percentage=as.numeric(Area*100/ref_area),score="AS")

write_csv(df_area_score2,file.path(outputFolder,paste0(sp,"_df_area_score2.csv")))
print("-----------------Area Score Measured-----------------------")

# Connectivity score 1 ---------------------------------------------------------
mean_per_hab_cov <- global(s_habitat,mean,na.rm=T) # mean value of percentage of habitat cover by pixel by year
print(mean_per_hab_cov)
s_habitat_over_cutoff <- ifel(s_habitat_nomask>=habitat_cutoff,1,0) # set cutoff for what could be used by the species
map2(as.list(s_habitat_over_cutoff),v_time_steps_all,~writeRaster(.x,file.path(outputFolder,paste0(sp,"_hab_cutoff_",.y,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))

l_habitat_dist <- map(as.list(s_habitat_over_cutoff), ~gridDist(.x, target=0)) # calculate distance to edge
gc(T)
map2(as.list(l_habitat_dist), v_time_steps_all,~writeRaster(.x,file.path(outputFolder,paste0(sp,"_dist_to_edge_",.y,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))

s_habitat_dist <- mask(rast(l_habitat_dist),s_habitat>=habitat_cutoff,maskvalues=1,inverse=T) # crop to habitat larger than cutoff, the minimum distance is 100 (size of the pixel)
df_habitat_dist <- global(s_habitat_dist,mean,na.rm=T)

df_conn_score <- tibble(sci_name=sp, Year=v_time_steps_all, value=df_habitat_dist$mean) |> mutate( ref_value=first(value)) |>
  dplyr::group_by(Year) |> mutate(diff=ref_value-value, percentage=(value*100)/ref_value,score="CS")
df_conn_score

write_csv(df_conn_score,file.path(outputFolder,paste0(sp,"_df_conn_scoreGISfrag.csv")))

df_conn_score2 <- tibble(sci_name=sp, Year=v_time_steps, value=df_habitat_dist$mean[-1]) |> mutate( ref_value=first(value)) |>
  dplyr::group_by(Year) |> mutate(diff=ref_value-value, percentage=(value*100)/ref_value,score="CS")
df_conn_score2

write_csv(df_conn_score2,file.path(outputFolder,paste0(sp,"_df_conn_scoreGISfrag2.csv")))
print("-----------------Connectivity Score Measured-----------------------")

#----------------------------- 3.2.3. SHS --------------------------------------
df_SHS <- data.frame(sci_name=sp, AS=as.numeric(df_area_score$percentage),CS=df_conn_score$percentage)
df_SHS <- df_SHS  |>  mutate(SHS=((AS+CS)/2),Year=v_time_steps_all)
df_SHS

df_SHS_tidy <- df_SHS |> tidyr::pivot_longer(AS:SHS,names_to="Score",values_to="Values") |> mutate(Version="HypBase_year_GISFrag")

write_csv(df_SHS_tidy,file.path(outputFolder,paste0(sp,"_df_SHS_GISfrag.csv")))

df_SHS2 <- data.frame(sci_name=sp, AS=as.numeric(df_area_score2$percentage),CS=df_conn_score2$percentage)
df_SHS2 <- df_SHS2  |>  mutate(SHS=((AS+CS)/2),Year=v_time_steps)
df_SHS2

df_SHS2_tidy <- df_SHS2 |> tidyr::pivot_longer(AS:SHS,names_to="Score",values_to="Values") |> mutate(Version="t0Base_year_GISFrag")

write_csv(df_SHS2_tidy,file.path(outputFolder,paste0(sp,"_df_SHS_GISfrag2.csv")))
print("-----------------Basic SHS Measured-----------------------")

