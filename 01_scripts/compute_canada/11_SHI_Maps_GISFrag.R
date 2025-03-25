library(terra)
library(purrr)
library(readr)
library(sf)
library(unix)

mem <- (as.numeric(Sys.getenv("SLURM_MEM_PER_NODE")))*0.95 #reduce mem with reference to memory limit
print(mem)
unix::rlimit_as(mem*1000000,mem*1000000) # convert megabytes to bytes
rlimit_all()

mem_gb <- mem*0.001 # convert megabyes to gigabytes
terraOptions(memmax=mem_gb)


#-------------------------------------------------------------------------------
# Set parameters and paths
#-------------------------------------------------------------------------------

sp_file <- "./00_rawdata/tables/df_sp_forSHI.csv"#"./00_rawdata/tables/df_tdf_sp_for_omni_checked_batch1.csv"##"./00_rawdata/tables/df_spVasquez.csv"#

# outputFolder
outputFolder <- "./02_outdata/summary"

# study area limit
area_lim_path <- "./00_rawdata/Study_areas/EcosistOrigBsTaeEtter_dissolve.gpkg"#"./00_rawdata/Study_areas/Colombia_borders_gadm.gpkg"# "/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/BST/riesgo_etter/EcosistOrigBsTaeEtter_dissolve.gpkg"#

# spatial resolution
spat_res <- 100
# srs
sf_srs <- st_crs(3116)
srs_ras <- crs("EPSG:3116")

#define time steps
t_0 <- 2000
t_n <- 2020 # should be larger than t_0 at least 2 years needed
time_step <- 10
t_range <- ((t_n - t_0)/time_step)
v_time_steps <- seq(t_0,t_n,time_step)
v_time_steps_all <-  c(v_time_steps[1]-time_step,v_time_steps)

#-------------------------------------------------------------------------------
# Read files
#-------------------------------------------------------------------------------

# Study area
sf_area_lim <- st_read(area_lim_path)
sf_area_lim_srs <- sf_area_lim |> st_transform(sf_srs)

# Bounding box raster
r_frame <- terra::rast(terra::ext(sf_area_lim_srs),resolution=spat_res)
terra::crs(r_frame) <- srs_ras
terra::values(r_frame) <- 1

sp_list <- read_csv(sp_file)

# load areas files when all species with habitat have outputs for scp10
df_aoh_areas_sp <- read_csv(file.path(outputFolder,"df_aoh_areas_sp.csv"))
v_sp <- gsub(" ","_",df_aoh_areas_sp$sci_name)

head(df_aoh_areas_sp)


#-------------------------------------------------------------------------------
# # Summarise Habitat
# #-------------------------------------------------------------------------------
# 
# for(year in v_time_steps_all){
#   cat("Year:",year,"\n")
#   l_sp_habitat_files <- map(v_sp, ~list.files(file.path("./02_outdata/species",.x),
#                                                   pattern=paste0("_habitat_layer_",year,".tif$"),full.names=T))
#   
#   #-------------------------------------------------------------------------------
#   # Create sp list and load rasters
#   
#   l_sp_habitat_files2 <- l_sp_habitat_files |> setNames(v_sp) |> compact()
#   complete_sp <- names(l_sp_habitat_files2)
#   
#   print(complete_sp)
#   
#   write_csv(as.data.frame(complete_sp),paste0("./00_rawdata/tables/complete",year,"_sp.csv"))
#   
#   n <- length(l_sp_habitat_files2)
#   print(n)
#   
#   # filter area dataframe just for sp with data
#   # df_aoh_areas_sp <- read_csv(file.path(outputFolder,"df_aoh_areas_sp.csv"))
#   # 
#   # head(df_aoh_areas_sp)
#   df_aoh_areas_sp_hab <- df_aoh_areas_sp |> dplyr::filter(sci_name %in% gsub("_"," ",complete_sp))
#   head(df_aoh_areas_sp_hab)
#   
#   total_weights <- sum(df_aoh_areas_sp_hab$W_stewardship)
#   
#   # load rasters and resample to r_frame one by one
#   r_habitat_sum <- rast(l_sp_habitat_files2[[1]]) |> resample(r_frame,method="bilinear")
#   print(r_habitat_sum)
#   
#   r_habitat_sum <- ifel(is.na(r_habitat_sum),0,r_habitat_sum)
#   print(r_habitat_sum)
#   
#   r_habitat_sum_wgt <- r_habitat_sum * df_aoh_areas_sp_hab$W_stewardship[1]
#   
#   for(i in 2:n){
#     r_habitat <- rast(l_sp_habitat_files2[[i]]) |> resample(r_frame,method="bilinear")
#     print(names(l_sp_habitat_files2)[i])
#     r_habitat <- ifel(is.na(r_habitat),0,r_habitat)
#     cat("Raster",i,"for species",df_aoh_areas_sp_hab$sci_name[i],"loaded and resampled \n")
#     r_habitat_sum <- app(c(r_habitat_sum,r_habitat),fun=sum,na.rm=T)
#     print(r_habitat_sum)
#     cat("...Habitats summed... \n")
#     r_habitat_wgt <- r_habitat * df_aoh_areas_sp_hab$W_stewardship[i]
#     r_habitat_sum_wgt <- app(c(r_habitat_sum_wgt,r_habitat_wgt),fun=sum,na.rm=T)
#     print(r_habitat_sum_wgt)
#     cat("...Weighted Habitats summed... \n")
#     gc(T)
#   }
#   
#   print(r_habitat_sum)
#   writeRaster(r_habitat_sum,file.path(outputFolder,paste0("habitat_sum",year,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")
#   #-------------------------------------------------------------------------------
#   # Calculate weighted mean habitat layer
#   
#   r_mean_habitat <- r_habitat_sum_wgt/total_weights
#   print(r_mean_habitat)
#   
#   # mask to TDF limits
#   r_mean_habitat_tdf <- terra::mask(r_mean_habitat,terra::vect(sf_area_lim_srs)) # Mask to study area
#   
#   writeRaster(r_mean_habitat_tdf,file.path(outputFolder,paste0("weighted_mean_habitat",year,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")
# }

#-------------------------------------------------------------------------------
# Summarise GISfrag metric
#-------------------------------------------------------------------------------

for(year in v_time_steps_all){
  cat("Year:",year,"\n")
  l_sp_GISfrag_files <- map(v_sp, ~list.files(file.path("./02_outdata/species",.x),
                                              pattern=paste0("_dist_to_edge_",year,".tif$"),full.names=T))

  #-------------------------------------------------------------------------------
  # Create sp list and load rasters

  l_sp_GISfrag_files2 <- l_sp_GISfrag_files |> setNames(v_sp) |> compact()
  complete_sp <- names(l_sp_GISfrag_files2)

  print(complete_sp)

  write_csv(as.data.frame(complete_sp),paste0("./00_rawdata/tables/complete_GISfrag",year,"_sp.csv"))

  n <- length(l_sp_GISfrag_files2)
  print(n)
  
  # df_aoh_areas_sp_hab <- df_aoh_areas_sp |> dplyr::filter(sci_name %in% gsub("_"," ",complete_sp))
  # head(df_aoh_areas_sp_hab)
  # 
  # total_weights <- sum(df_aoh_areas_sp_hab$W_stewardship)

  # load rasters and resample to r_frame one by one
  r_GISfrag_sum <- rast(l_sp_GISfrag_files2[[1]]) |> resample(r_frame,method="bilinear")
  print(r_GISfrag_sum)

  r_GISfrag_sum <- ifel(is.na(r_GISfrag_sum),0,r_GISfrag_sum)
  print(r_GISfrag_sum)
  
  # r_GISfrag_sum_wgt <- r_GISfrag_sum * df_aoh_areas_sp_hab$W_stewardship[1]

  for(i in 2:n){
    r_GISfrag <- rast(l_sp_GISfrag_files2[[i]]) |> resample(r_frame,method="bilinear")
    print(names(l_sp_GISfrag_files2[[i]]))
    r_GISfrag <- ifel(is.na(r_GISfrag),0,r_GISfrag)
    cat("Raster",i,"for species",v_sp[i],"loaded and resampled \n")
    
    r_GISfrag_sum <- app(c(r_GISfrag_sum,r_GISfrag),fun=sum,na.rm=T)
    print(r_GISfrag_sum)
    cat("...Mean distances summed... \n")

    # r_habitat_wgt <- r_habitat * df_aoh_areas_sp_hab$W_stewardship[i]
    # r_habitat_sum_wgt <- app(c(r_habitat_sum_wgt,r_habitat_wgt),fun=sum,na.rm=T)
    # print(r_habitat_sum_wgt)
    # cat("...Weighted Habitats summed... \n")
    gc(T)
  }

  print(r_GISfrag_sum)
  writeRaster(r_GISfrag_sum,file.path(outputFolder,paste0("sum_GISfrag",year,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")

  #-------------------------------------------------------------------------------
  # Calculate mean GISfrag layer
  r_mean_GISfrag <- r_GISfrag_sum/n
  print(r_mean_GISfrag)

  # mask to TDF limits
  r_mean_GISfrag_tdf <- terra::mask(r_mean_GISfrag,terra::vect(sf_area_lim_srs)) # Mask to study area

  writeRaster(r_mean_GISfrag_tdf,file.path(outputFolder,paste0("mean_GISfrag",year,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")
}


# #-------------------------------------------------------------------------------
# #Summarise Omniscape metric
# #-------------------------------------------------------------------------------
# 
# for(year in v_time_steps_all){
#   cat("Year:",year,"\n")
#   l_sp_Omnisc_files <- map(v_sp, ~list.files(file.path("./02_outdata/species",.x),
#                                               pattern=paste0("_masked_con_",year,".tif$"),full.names=T))
# 
#   #-------------------------------------------------------------------------------
#   # Create sp list and load rasters
# 
#   l_sp_Omnisc_files2 <- l_sp_Omnisc_files |> setNames(v_sp) |> keep(~length(.x)>0)
#   complete_sp <- names(l_sp_Omnisc_files2)
# 
#   print(complete_sp)
# 
#   write_csv(as.data.frame(complete_sp),paste0("./00_rawdata/tables/complete_Omnisc",year,"_sp.csv"))
# 
#   n <- length(l_sp_Omnisc_files2)
#   print(n)
# 
#   # load rasters and resample to r_frame one by one
#   r_Omnisc_sum <- rast(l_sp_Omnisc_files2[[1]]) |> resample(r_frame,method="bilinear")
#   print(r_Omnisc_sum)
# 
#   r_Omnisc_sum <- ifel(is.na(r_Omnisc_sum),0,r_Omnisc_sum)
#   print(r_Omnisc_sum)
# 
#   for(i in 2:n){
#     r_Omnisc <- rast(l_sp_Omnisc_files2[[i]]) |> resample(r_frame,method="bilinear")
#     r_Omnisc <- ifel(is.na(r_Omnisc),0,r_Omnisc)
#     cat("Raster",i,"loaded and resampled \n")
#     r_Omnisc_sum <- app(c(r_Omnisc_sum,r_Omnisc),fun=sum,na.rm=T)
#     cat("...Current summed... \n")
#     print(r_Omnisc_sum)
#   }
# 
#   print(r_Omnisc_sum)
# 
#   #-------------------------------------------------------------------------------
#   # Calculate mean Omnisc layer
#   r_mean_Omnisc <- r_Omnisc_sum/n
#   print(r_mean_Omnisc)
# 
#   # mask to TDF limits
#   r_mean_Omnisc_tdf <- terra::mask(r_mean_Omnisc,terra::vect(sf_area_lim_srs)) # Mask to study area
# 
#   writeRaster(r_mean_Omnisc_tdf,file.path(outputFolder,paste0("mean_Omnisc",year,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")
# }