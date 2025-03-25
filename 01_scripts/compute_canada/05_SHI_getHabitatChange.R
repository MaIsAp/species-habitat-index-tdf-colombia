# This script needs sf_ext and sf_ext to load values for the area of interest
.libPaths("/home/maisap/R/x86_64-pc-linux-gnu-library/4.3/")
.libPaths()

packages <- c("unix","terra","sf","dplyr","tidyr","readr","purrr","tmap")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(packages,require,character.only=T)

i <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
print(i)

mem <- (as.numeric(Sys.getenv("SLURM_MEM_PER_NODE")))*0.95 #reduce mem with reference to memory limit
print(mem)
unix::rlimit_as(mem*1000000,mem*1000000) # convert megabytes to bytes
rlimit_all()

mem_gb <- mem*0.001 # convert megabyes to gigabytes
terraOptions(memmax=mem_gb)


# Parameters -------------------------------------------------------------------
#define time steps
t_0 <- 2000
t_n <- 2020 # should be larger than t_0 at least 2 years needed
time_step <- 10
t_range <- ((t_n - t_0)/time_step)
v_time_steps <- seq(t_0,t_n,time_step)
v_time_steps_all <-  c(v_time_steps[1]-time_step,v_time_steps)
time_ranges <- paste(v_time_steps[-length(v_time_steps)],v_time_steps[-1],sep="_")

# cut off value for transparency in SHS figure
fig_cutoff <- 0.1

# species list
sp_file <- "./00_rawdata/tables/df_sp_forSHI.csv"#"./sp_forSHI.csv"#

# study area limit
area_lim1_path <- "./00_rawdata/Study_areas/EcosistOrigBsTaeEtter_dissolve.gpkg"#"/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/BST/riesgo_etter/EcosistOrigBsTaeEtter_dissolve.gpkg"#"./00_rawdata/Study_areas/Colombia_borders_gadm.gpkg"# 

# country limit
country_lim_path <- "./00_rawdata/Study_areas/Colombia_borders_gadm.gpkg"#"/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/Colombia_borders_gadm.gpkg"#

# world limit
world_lim_path <- "./00_rawdata/Study_areas/world-administrative-boundaries.shp"#"/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/Administrative_boundaries/world-administrative-boundaries/world-administrative-boundaries.shp"#

# outputFolder
outputFolder0 <- "./02_outdata/species"#file.path("./Connectivity/layers_for_omniscape")#

#-------------------------------------------------------------------------------
# Load inputs and paths
#-------------------------------------------------------------------------------

sp_list <- read.csv(sp_file)
sp <- sp_list$sci_name[i] # assigns species i from list
outputFolder <- file.path(outputFolder0,gsub(" ","_",sp))#file.path(outputFolder,sp,sp)#
print(outputFolder)

getwd()

# # habitat layer without mask--
# habitat_nomask_files <- list.files(outputFolder,pattern="_habitat_nomask_layer_",full.names = T)


# Load reference layers------------------------------------------------------------
r_frame <- rast(file.path(outputFolder, paste0(sp,"_r_frame.tif")))
r_range_map <- rast(file.path(outputFolder, paste0(sp,"_r_range_map",".tif")))
r_range_map

#****
bnb_cover_files <- list.files(outputFolder,pattern="cove_bnb",full.names = T)
cat("Forest cover files:",bnb_cover_files,"\n")
l_habitat_bnb_nomask <- map(bnb_cover_files, ~rast(.x))

bnb_loss_files <- list.files(outputFolder,pattern="loss_bnb",full.names = T)
cat("Forest loss files:", bnb_loss_files,"\n")
l_habitat_bnb_loss <- map(bnb_loss_files, ~rast(.x))

bnb_gain_files <- list.files(outputFolder,pattern="gain_bnb",full.names = T)
cat("Forest gain files:",bnb_gain_files,"\n")
l_habitat_bnb_gain <- map(bnb_gain_files, ~rast(.x))
#****
#****
clc_cover_files <- list.files(outputFolder,pattern="cove_clc",full.names = T)
cat("Land cover files:",clc_cover_files,"\n")
l_habitat_clc_nomask <- map(clc_cover_files, ~rast(.x))

clc_loss_files <- list.files(outputFolder,pattern="loss_clc",full.names = T)
cat("Land cover loss files:",clc_loss_files,"\n")
l_habitat_clc_loss <- map(clc_loss_files, ~rast(.x))

clc_gain_files <- list.files(outputFolder,pattern="gain_clc",full.names = T)
cat("Land cover gain files:",clc_gain_files,"\n")
l_habitat_clc_gain <- map(clc_gain_files, ~rast(.x))
#****

#-------------------------------------------------------------------------------
# 4. Join land cover outputs
#-------------------------------------------------------------------------------
s_habitat_bnb_nomask <- rast(l_habitat_bnb_nomask)
s_habitat_bnb_loss <- rast(l_habitat_bnb_loss)
s_habitat_bnb_gain <- rast(l_habitat_bnb_gain)

s_habitat_clc_nomask <- rast(l_habitat_clc_nomask)
s_habitat_clc_loss <- rast(l_habitat_clc_loss)
s_habitat_clc_gain <- rast(l_habitat_clc_gain)

rm(l_habitat_bnb_nomask, l_habitat_bnb_loss, l_habitat_bnb_gain,
   l_habitat_clc_nomask, l_habitat_clc_loss, l_habitat_clc_gain)

gc(T)

# for omniscape
s_habitat_bnb_nomask_filled <- s_habitat_bnb_nomask
s_habitat_bnb_nomask_filled[[1]] <- terra::cover(s_habitat_bnb_nomask[[1]],s_habitat_clc_nomask[[1]]) # fill NAs of BnB with data from clc
s_habitat_bnb_nomask_filled[[2]] <- terra::cover(s_habitat_bnb_nomask[[2]],s_habitat_clc_nomask[[2]]) # fill NAs of BnB with data from clc
s_habitat_bnb_nomask_filled[[3]] <- terra::cover(s_habitat_bnb_nomask[[3]],s_habitat_clc_nomask[[3]])# fill NAs of BnB with data from clc

print("=================== NA's from forest layer filled with LC================")

# create base layer with rangemap, fill the rest of the area with 0 and mask ocean and other areas not considered
base_year <- mask(ifel(is.na(r_range_map),0,1),s_habitat_bnb_nomask_filled[[1]])

s_habitat_nomask0 <- (s_habitat_bnb_nomask_filled + s_habitat_clc_nomask)/2 # calc mean values between land cover sources
s_habitat_nomask <- c(base_year,s_habitat_nomask0)
names(s_habitat_nomask) <- v_time_steps_all

rm(s_habitat_bnb_nomask, s_habitat_clc_nomask,
   s_habitat_bnb_nomask_filled, s_habitat_nomask0)
gc(T)

map2(as.list(s_habitat_nomask),v_time_steps_all,~writeRaster(.x,file.path(outputFolder,paste0(sp,"_habitat_nomask_layer_",.y,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))
#****
# comb_habt_files <- list.files(outputFolder,pattern="habitat_nomask_layer",full.names = T)
# l_habitat_nomask <- map(comb_habt_files, ~rast(.x))
# s_habitat_nomask <- rast(l_habitat_nomask)
#****

print("-----------------No masked habitat layers produced-----------------------")

s_habitat <- terra::mask(s_habitat_nomask,r_range_map)
names(s_habitat) <- v_time_steps_all

map2(as.list(s_habitat),v_time_steps_all,~writeRaster(.x,file.path(outputFolder,paste0(sp,"_habitat_layer_",.y,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))

s_habitat_loss_filled <- cover(s_habitat_bnb_loss[[-1]] , s_habitat_clc_loss)

s_habitat_loss0 <- (s_habitat_loss_filled + s_habitat_clc_loss)/2
s_habitat_loss <- c(s_habitat_bnb_loss[[1]],s_habitat_loss0)

map2(as.list(s_habitat_loss),time_ranges,~writeRaster(.x,file.path(outputFolder,paste0(sp,"_loss_comb_",.y,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))

#****
# comb_loss_files <- list.files(outputFolder,pattern="loss_comb",full.names = T)
# l_comb_loss_rescale <- map(comb_loss_files, ~rast(.x))
# s_habitat_loss <- rast(l_comb_loss_rescale)
#****

s_habitat_bnb_gain_filled <- cover(s_habitat_bnb_gain[[-1]] , s_habitat_clc_gain)

s_habitat_gain0 <- (s_habitat_bnb_gain_filled + s_habitat_clc_gain)/2
s_habitat_gain <- c(s_habitat_bnb_gain[[1]],s_habitat_gain0)

map2(as.list(s_habitat_gain),time_ranges,~writeRaster(.x,file.path(outputFolder,paste0(sp,"_gain_comb_",.y,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))

#****
# comb_gain_files <- list.files(outputFolder,pattern="gain_comb",full.names = T)
# l_comb_gain_rescale <- map(comb_gain_files, ~rast(.x))
# s_habitat_gain <- rast(l_comb_gain_rescale)
#****
rm(s_habitat_bnb_loss,s_habitat_clc_loss,s_habitat_loss_filled,s_habitat_loss0,
   s_habitat_bnb_gain,s_habitat_clc_gain,s_habitat_bnb_gain_filled,s_habitat_gain0)
gc(T)
print("-----------------Habitat cover, gain and loss layers produced-----------------------")

# 4.1. Reclassify to produce plot ----------------------------------------------
r_cove_last <- s_habitat[[dim(s_habitat)[3]]]
r_cove_last_plot <- terra::ifel(r_cove_last >= fig_cutoff, r_cove_last, NA)
s_habitat_loss_plot <- terra::ifel(s_habitat_loss >= fig_cutoff, s_habitat_loss, NA)
s_habitat_gain_plot <- terra::ifel(s_habitat_gain >= fig_cutoff, s_habitat_gain, NA)

print("-----------------Layers reclassified for plot-----------------------")


# tmap_mode("view")
tmap_mode("plot")
# # osm <- read_osm(sf_area_lim2, ext=1.1)

# Study area
sf_area_lim1 <- st_read(area_lim1_path)
# sf_area_lim1_srs <- sf_area_lim1 |> st_transform(sf_srs)
sf_country_lim <- st_read(country_lim_path)
#load world limits
sf_world_lim <- st_read(world_lim_path)

img_map_habitat_changes <- tm_shape(sf_world_lim,bbox=st_bbox(sf_area_lim1))+tm_polygons(col="#c2bbac",fill="#c2bbac")+
  tm_shape(sf_country_lim)+tm_polygons(fill="white")+
  tm_shape(sf_area_lim1)+tm_fill(fill="#E8E9EB")+
  tm_shape(s_habitat[[1]])+tm_raster(style="cat",palette = "darkgray", legend.show = FALSE)+
  tm_shape(s_habitat_loss_plot[[1]])+tm_raster(style="cont",palette = "Reds", legend.show = FALSE)+
  tm_shape(s_habitat_loss_plot[[2]])+tm_raster(style="cont",palette = "Reds", legend.show = FALSE)+
  # tm_shape(s_habitat_loss_plot[[3]])+tm_raster(style="cont",palette = "Reds", legend.show = FALSE)+
  tm_shape(s_habitat_gain_plot[[1]])+tm_raster(style="cont",alpha=0.8,palette = "YlGn", legend.show = FALSE)+
  tm_shape(s_habitat_gain_plot[[2]])+tm_raster(style="cont",alpha=0.8,palette = "YlGn", legend.show = FALSE)+
  # tm_shape(s_habitat_gain_plot[[3]])+tm_raster(style="cont",alpha=0.8,palette = "YlGn", legend.show = FALSE)+
  tm_shape(r_cove_last_plot)+tm_raster(style="cont",palette = "Blues", legend.show = FALSE)+
  tm_compass(position=c("right","bottom"))+tm_scale_bar(position=c("left","top"))+
  tm_add_legend(labels=c("No change","Loss","Gain","Potential AOH","Potential TDF"),
                col=c("darkblue","darkred","darkgreen","darkgray","#E8E9EB"),title=sp)+
  tm_layout(bg.color="lightblue", legend.bg.color = "white",legend.bg.alpha = 0.8,legend.outside = F,legend.position = c("right","top"), legend.title.fontface = 4)

# img_map_habitat_changes

tmap_save(img_map_habitat_changes, paste0(outputFolder,"/",sp,"_cover_change2.png"))

print("-----------------Plot generated and saved-----------------------")

# tm_shape(sf_country_lim,bbox=st_bbox(sf_area_lim1))+tm_fill(fill="white")+
#   tm_shape(sf_area_lim1)+tm_fill(fill="#f7fafa")+tm_compass(position=c("right","bottom"))+
#   tm_scale_bar(position=c("left","top"))+
#   tm_add_legend(labels=c("No change","Loss","Gain","AOH"),fill=c("darkblue","darkred","darkgreen","lightgray"),
#                 title=paste0("Species Habitat Score \n",sp))+
#   tm_layout(bg.color="#c2bbac", legend.title.fontface = 4)

                