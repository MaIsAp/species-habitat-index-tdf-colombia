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
# Summarise Habitat
#-------------------------------------------------------------------------------

for(year in v_time_steps_all){
  cat("Year:",year,"\n")
  l_sp_habitat_files <- map(v_sp, ~list.files(file.path("./02_outdata/species",.x),
                                                  pattern=paste0("_habitat_layer_",year,".tif$"),full.names=T))

  #-------------------------------------------------------------------------------
  # Set names to raster path list by species and remove if any is empty
  l_sp_habitat_files2 <- l_sp_habitat_files |> setNames(v_sp) |> compact()
  # create complete sp list
  complete_sp <- names(l_sp_habitat_files2)

  print(complete_sp)

  # write list of complete species by year
  write_csv(as.data.frame(complete_sp),paste0(outputFolder,"/complete",year,"_sp.csv"))

  n <- length(l_sp_habitat_files2)
  print(n)

  # filter area dataframe just for sp with data (in case some are missing)
  df_aoh_areas_sp_hab <- df_aoh_areas_sp |> dplyr::filter(sci_name %in% gsub("_"," ",complete_sp))
  head(df_aoh_areas_sp_hab)

  # get the sum of all weights by species where weights are measured by dividing species range inside TDF with total area of potential TDF
  total_weights <- sum(df_aoh_areas_sp_hab$W_stewardship)

  # load rasters and resample to r_frame for the first species to set base
  r_habitat_sum <- rast(l_sp_habitat_files2[[1]]) |> resample(r_frame,method="bilinear")
  print(r_habitat_sum)

  # set NAs to 0 to allow sum between rasters
  r_habitat_sum <- ifel(is.na(r_habitat_sum),0,r_habitat_sum)
  print(r_habitat_sum)

  # multiply raster by species habitat weight
  r_habitat_sum_wgt <- r_habitat_sum * df_aoh_areas_sp_hab$W_stewardship[1]

  # load rasters and resample to r_frame one by one
  for(i in 2:n){
    # load rasters and resample to r_frame for species i
    r_habitat <- rast(l_sp_habitat_files2[[i]]) |> resample(r_frame,method="bilinear")
    print(names(l_sp_habitat_files2)[i])
    
    # set NAs to 0 to allow sum between rasters
    r_habitat <- ifel(is.na(r_habitat),0,r_habitat)
    cat("Raster",i,"for species",df_aoh_areas_sp_hab$sci_name[i],"loaded and resampled \n")
    
    # sum rasters
    r_habitat_sum <- app(c(r_habitat_sum,r_habitat),fun=sum,na.rm=T)
    print(r_habitat_sum)
    cat("...Habitats summed... \n")
    # multiply raster by species habitat weight
    r_habitat_wgt <- r_habitat * df_aoh_areas_sp_hab$W_stewardship[i]
    # sum rasters with weight to get weighted sum
    r_habitat_sum_wgt <- app(c(r_habitat_sum_wgt,r_habitat_wgt),fun=sum,na.rm=T)
    print(r_habitat_sum_wgt)
    cat("...Weighted Habitats summed... \n")
    gc(T)
  }

  print(r_habitat_sum)
  
  # save summed habitat map
  writeRaster(r_habitat_sum,file.path(outputFolder,paste0("habitat_sum",year,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")
  #-------------------------------------------------------------------------------
  # Calculate weighted mean habitat layer by dividing weighted raster sum to total weights
  r_mean_habitat <- r_habitat_sum_wgt/total_weights
  print(r_mean_habitat)

  # mask to TDF limits
  r_mean_habitat_tdf <- terra::mask(r_mean_habitat,terra::vect(sf_area_lim_srs)) # Mask to study area

  # save summed weighted habitat map
  writeRaster(r_mean_habitat_tdf,file.path(outputFolder,paste0("weighted_mean_habitat",year,".tif")),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG")
}