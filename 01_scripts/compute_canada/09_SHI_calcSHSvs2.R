# this script takes Omniscape outputs by species and measures the SHS
# Author: Maria Isabel Arce Plata
# Date: February 2024
#-------------------------------------------------------------------------------
.libPaths("/home/maisap/R/x86_64-pc-linux-gnu-library/4.3/")
.libPaths()

packages <- c("unix","dplyr","terra","geodata","purrr","readr","ggplot2","sf")
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

# time steps
t_0 <- 2000 
t_n <- 2020 # should be larger than t_0 at least 2 years needed
time_step <- 10
v_time_steps <- seq(t_0,t_n,time_step)
v_time_steps_all <-  c(v_time_steps[1]-time_step,v_time_steps)

# species list
sp_file <- "./00_rawdata/tables/df_sp_forSHI.csv"#"./00_rawdata/tables/df_spVasquez.csv"#

# outputFolder
outputFolder0 <- "./02_outdata/species"#"/Volumes/Extreme Pro/Hikaru/Doctorado/Documento/Capitulo_01/03_data/outputs/CorridasSHI/species"#
getwd()

# proportion of habitat to consider for the habitat map
habitat_cutoff <- 0.1

# study area limit
area_lim1_path <- "./00_rawdata/Study_areas/EcosistOrigBsTaeEtter_dissolve.gpkg"#"./00_rawdata/Study_areas/Colombia_borders_gadm.gpkg"# "/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/BST/riesgo_etter/EcosistOrigBsTaeEtter_dissolve.gpkg"#

#-------------------------------------------------------------------------------
# Load inputs
#-------------------------------------------------------------------------------
sp_list <- read.csv(sp_file)
sp <- sp_list$sci_name[i] # assigns species i from list
print(sp)

outputFolder <- file.path(outputFolder0,gsub(" ","_",sp))#file.path(outputFolder,sp,sp)#
print(outputFolder)

r_range_map <- rast(file.path(outputFolder, paste0(sp,"_r_range_map",".tif")))

# Study area
#sf_area_lim1 <- st_read(area_lim1_path)
#sf_area_lim1_srs <- sf_area_lim1 |> st_transform(sf_srs)

#****
habitat_files <- list.files(outputFolder,pattern="habitat_layer",full.names = T)
l_habitat <- map(habitat_files, ~rast(.x))
# s_habitat <- rast(l_habitat)
#****

# Load Omniscape outputs and mask
l_omniscape_output <- map(v_time_steps_all, ~rast(file.path(outputFolder,paste0("Omniscape",.x,"/cum_currmap.tif"))))
s_omniscape_output0 <- rast(l_omniscape_output)

# mask to area of habitat
s_omniscape_output <- mask(s_omniscape_output0,l_habitat[[1]]>=habitat_cutoff,maskvalues=1,inverse=T)
names(s_omniscape_output) <- v_time_steps_all

# plot(s_omniscape_output, breaks=seq(0,4,0.5),type="continuous")

# save masked omniscape output 
map2(as.list(s_omniscape_output),v_time_steps_all,~writeRaster(.x,paste0(outputFolder,"/",sp,"_masked_con_base",.y,".tif"),overwrite=T, gdal=c("COMPRESS=DEFLATE"), filetype="COG"))
## Load masked Omniscape outputs and mask
# l_omniscape_output0 <- map(v_time_steps_all, ~rast(file.path(outputFolder,paste0(sp,"_masked_con_",.x,".tif"))))
# s_omniscape_output <- rast(l_omniscape_output0)

#-------------------------------------------------------------------------------
# Measure difference to reference layer 
#-------------------------------------------------------------------------------
global_stats_prop_curr <- global(s_omniscape_output,fun=c("min","mean","max","sd","sum"),na.rm=T)
print(global_stats_prop_curr)

df_conn_data <- tibble(Year= v_time_steps_all, 
                       min_curr= global_stats_prop_curr$min,
                       sum_curr= global_stats_prop_curr$sum,
                       current = global_stats_prop_curr$mean,
                       max_curr= global_stats_prop_curr$max,
                       sd_curr= global_stats_prop_curr$sd) |>
  mutate(ref_curr=first(current),diff=ref_curr-current, 
         Values=current*100/ref_curr,sci_name=sp,Score="CS",Version="HypBase_year_Omniscape_base") |>
  relocate(Score,.after=Values) |>  relocate(sci_name,.before=Year)

print(df_conn_data )

write_csv(df_conn_data,paste0(outputFolder,"/",sp,"_df_conn_scoreOmni_base_HypBase.csv"))
# df_conn_data <- read.csv(paste0(outputFolder,"/",sp,"_df_conn_score2.csv"),row.names="X")

# SHS - base year ----------------------
# # load SHS from GISfrag to get Area Score
df_SHS_GISfrag <- read_csv(paste0(outputFolder,"/",sp,"_df_SHS_GISfrag.csv"))
print(df_SHS_GISfrag)

print("---------TS for GIS frag created----------")

# create table with SHS measured with Omniscape
df_AS <- df_SHS_GISfrag |> filter(Score=="AS") |> mutate(Version="HypBase_year_Omniscape_base")
df_CS <- df_conn_data |> select(names(df_AS))
df_SHS <- df_CS |> mutate(Values=(df_AS$Values+Values)/2,Score="SHS")

df_SHS_Omnis <- df_SHS |> bind_rows(df_AS,df_CS)
write_csv(df_SHS_Omnis,paste0(outputFolder,"/",sp,"_df_SHS_HypBase_year_Omniscape_base.csv"))
# df_SHS_Omnis <- read_csv(paste0(outputFolder,"/",sp,"_df_SHS_HypBase_year_Omniscape.csv"))

print(df_SHS_Omnis)

print("---------TS for Omniscape created----------")

gc(TRUE)

#-------------------------------------------------------------------------------
# Measure difference without base year 
#-------------------------------------------------------------------------------
# # mask to area of habitat
s_omniscape_output2 <- mask(s_omniscape_output0[[-1]],l_habitat[[2]]>=habitat_cutoff,maskvalues=1,inverse=T)
# 
global_stats_prop_curr2 <- global(s_omniscape_output2,c("min","mean","max","sd","sum"),na.rm=T)
print(global_stats_prop_curr2)

df_conn_data_t0 <- tibble(Year= v_time_steps, 
                          min_curr= global_stats_prop_curr2$min,
                          sum_curr= global_stats_prop_curr2$sum,
                          current = global_stats_prop_curr2$mean,
                          max_curr= global_stats_prop_curr2$max,
                          sd_curr= global_stats_prop_curr2$sd) |>
  mutate(ref_curr=first(current),diff=ref_curr-current, 
         Values=current*100/ref_curr,sci_name=sp,Score="CS",Version="t0Base_year_Omniscape_base") |> 
  relocate(Score,.after=Values) |>  relocate(sci_name,.before=Year)

print(df_conn_data_t0)

write_csv(df_conn_data_t0,paste0(outputFolder,"/",sp,"_df_conn_scoreOmni_base_t0Base.csv"))
# df_conn_data_t0 <- read.csv(paste0(outputFolder,"/",sp,"_df_conn_scoreOmni_t0Base.csv"))

#SHS - no base year ---------------------------------------
# load SHS from GISfrag to get Area Score
df_SHS_GISfrag2 <- read_csv(paste0(outputFolder,"/",sp,"_df_SHS_GISfrag2.csv"))
print(df_SHS_GISfrag2)

print("---------TS for GIS frag 2 created----------")

# create table with SHS measured with Omniscape
df_AS2 <- df_SHS_GISfrag2 |> filter(Score=="AS") |> mutate(Version="t0Base_year_Omniscape_base")
df_CS2 <- df_conn_data_t0 |> select(names(df_AS2))
df_SHS2 <- df_CS2 |> mutate(Values=(df_AS2$Values+Values)/2,Score="SHS") 

df_SHS_Omnis2 <- df_SHS2 |> bind_rows(df_AS2,df_CS2)
write_csv(df_SHS_Omnis2,paste0(outputFolder,"/",sp,"_df_SHS_t0Base_year_Omniscape_base.csv"))

print(df_SHS_Omnis2)

print("---------TS for Omniscape 2 created----------")

#-------------------------------------------------------------------------------
# SHS complete
#-------------------------------------------------------------------------------

df_SHS_all <- df_SHS_GISfrag |> bind_rows(df_SHS_GISfrag2,df_SHS_Omnis,df_SHS_Omnis2)
write_csv(df_SHS_all,paste0(outputFolder,"/",sp,"_df_SHS_base.csv"))
