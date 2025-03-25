library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(taxize)
library(stringr)
library(furrr)
library(parallel)
library(rredlist)

#-------------------------------------------------------------------------------
# Load TDF sp List from literature
#-------------------------------------------------------------------------------
df_litlist_sp_verif <- read_csv("./00_rawdata/tables/df_litlist_sp_verif.csv") 

################################################################################
# IUCN information
################################################################################
# token <- readLines("./00_rawdata/tokens/IUCN_API.txt")
# cores <- detectCores()

#-------------------------------------------------------------------------------
# IUCN information - Elevation 
#-------------------------------------------------------------------------------
# plan(multisession,workers=cores-1)
# l_sp_info <- future_map(df_litlist_sp_verif$sci_name, ~rl_search(.x,key=token)$result)
# # check for empty species
# test <- map_dbl(l_sp_info, function(x) length(x)>0)
# sum(test)

# df_sp_info <- l_sp_info |> setNames(df_litlist_sp_verif$sci_name) |> bind_rows(.id="sci_name") |> arrange(desc(kingdom))
# df_sp_info %>% count(kingdom)
# df_sp_info %>% count(class)
# 
# # write_csv(df_sp_info,"./00_rawdata/tables/df_tdf_sp_inf_befElev.csv")
# # df_sp_info <- read_csv("./00_rawdata/tables/df_tdf_sp_inf.csv")
# 
# temp <- df_sp_info %>% count(sci_name)
# df_sp_info %>% filter(sci_name %in% df_habitat$sci_name) %>% count(kingdom)
# 
# sum(!is.na(df_sp_info$elevation_lower))
# sum(!is.na(df_sp_info$elevation_upper))
# sum(!is.na(df_sp_info$elevation_lower) | !is.na(df_sp_info$elevation_upper))
# 
# # check for species with at least one elevation data
# df_sp_info_check <- df_sp_info %>% filter(!is.na(elevation_lower) | !is.na(elevation_upper)) %>%
#   group_by(kingdom) %>% count(sci_name)
# 
# df_sp_info_check %>% count(kingdom)

# load missing elevation data from Occurrence data (IAvH) ----------------------

# # load occurrence points with elevation data from DEM
# sf_tdf_occurrences_dem <- st_read("/Volumes/Extreme Pro/Hikaru/Doctorado/Documento/Capitulo_02/01_repositorios/Biotablero/02_outdata/TDF_sp_occurences_points_buffer_elev.gpkg")
# df_tdf_occurrences_dem <- sf_tdf_occurrences_dem |> st_drop_geometry() |> mutate(sci_name=paste(genus,sp_epithet))
# 
# # from all species with occurrence data filter the species included in list so far
# df_tdf_occurrences_dem_in_list <- df_tdf_occurrences_dem |> filter(sci_name %in% df_sp_info$sci_name)
# temp <- df_tdf_occurrences_dem_in_list %>% count(sci_name)
# 
# df_tdf_occurrences_when_nodata <- df_tdf_occurrences_dem |> filter(!sci_name %in% df_sp_info_check$sci_name)
# df_tdf_occurrences_when_nodata_sp <- df_tdf_occurrences_when_nodata |> count(sci_name)
# 
# library(plyr)
# # get minimum and maximum elevation from mean elevation values extracted from DEM for occurrences
# df_tdf_occurrences_dem_in_list_min_max <- df_tdf_occurrences_dem_in_list |> dplyr::group_by(sci_name) |>
#   dplyr::summarise(elevation_lower2=round_any(min(cop_dem_90_100mresmean),100,f=floor), # round to lower value multiple of 100
#                    elevation_upper2=round_any(max(cop_dem_90_100mresmean),100,f=ceiling)) # round to higher value multiple of 100
# unloadNamespace("plyr")
# 
# # replace empty values for minimum and maximum elevation when data is available from occurrence points
# df_sp_info_with_elev_from_occ <- df_sp_info |> left_join(df_tdf_occurrences_dem_in_list_min_max,by="sci_name") |>
#   mutate(elevation_lower=case_when(is.na(elevation_lower) & !is.na(elevation_lower2) ~ elevation_lower2,
#                                    !is.na(elevation_lower) ~ elevation_lower),
#          elevation_upper=case_when(is.na(elevation_upper) & !is.na(elevation_upper2) ~ elevation_upper2,
#                                    !is.na(elevation_upper) ~ elevation_upper))
# 
# # check again species with at least one elevation range
# df_sp_info_check2 <- df_sp_info_with_elev_from_occ %>% filter(!is.na(elevation_lower) | !is.na(elevation_upper)) %>%
#   group_by(kingdom) %>% count(sci_name)
# 
# df_sp_info_check2 %>% count(kingdom)
# 
# plant_sp_elev_added <- df_sp_info_check2 |> filter(!sci_name %in% df_sp_info_check$sci_name)
# 
# #update list with elevation data -----------------------------------------------
# df_tdf_sp_info <- df_sp_info_with_elev_from_occ#df_sp_info_with_elev_from_occ %>% filter(sci_name %in% df_sp_info_check2$sci_name)

# write_csv(df_tdf_sp_info ,"./00_rawdata/tables/df_tdf_sp_inf.csv")
df_tdf_sp_info <- read_csv("./00_rawdata/tables/df_tdf_sp_inf.csv")

# # check again species with at least one elevation range
# df_sp_info_elev_check3 <- df_tdf_sp_info %>% filter(!is.na(elevation_lower) | !is.na(elevation_upper)) %>%
#   group_by(kingdom) %>% count(sci_name)


#-------------------------------------------------------------------------------
# IUCN information - Habitat
#-------------------------------------------------------------------------------
# plan(multisession,workers=cores-1)
# 
# l_habitat <- future_map(df_litlist_sp_verif$sci_name, ~rl_habitats(.x,key=token)$result )
# # check for empty species
# test <- map_dbl(l_habitat, function(x) length(x)>0)
# sum(test)
# 
# #separate habitat levels
# df_habitat <- l_habitat |> setNames(df_litlist_sp_verif$sci_name) |> bind_rows(.id="sci_name")
# df_habitat <- df_habitat |> separate(code, into=c("n1","n2"),sep="\\.",remove=F)
# 
# write_csv(df_habitat,"./00_rawdata/tables/df_tdf_sp_habitat_inf.csv")
df_habitat <- read_csv("./00_rawdata/tables/df_tdf_sp_habitat_inf.csv")

# # Check data availability - Habitat
# df_litlist_in_iucn_habitat <- df_litlist_sp_verif |> dplyr::filter(sci_name %in% df_habitat$sci_name)
# df_litlist_in_iucn_habitat |> count(group)

################################################################################
# Distribution Data sources
################################################################################

#-------------------------------------------------------------------------------
# IUCN rangemaps 
#-------------------------------------------------------------------------------
# complete range maps that overlap with col after combined
sf_rangemaps_tdf_col_intersect_uni <- st_read("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IUCN/RangeMapsColombia/TDF/Range_maps_Col_TDF_limits_united.gpkg")
df_rangemaps_tdf_col_intersect_uni <- sf_rangemaps_tdf_col_intersect_uni |> st_drop_geometry()

temp <- df_rangemaps_tdf_col_intersect_uni |> count(sci_name)

#-------------------------------------------------------------------------------
# Biomodelos 
#-------------------------------------------------------------------------------
# old list
biomod_sp <- read_csv("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/Biomodelos/list.spp.models.csv")#/run/media/miap/C084AA1284AA0ACC/Hikaru
head(biomod_sp)

# model files-------------------
# load list of binary models
v_list_bin_mod <- list.files("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/Biomodelos/Random/binarios/",pattern = "tif$")
# load list of continuous models
v_list_con_mod <- list.files("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/Biomodelos/Random/continuos/",pattern = "tif$")

#get just species names
v_list_bin_mod <- gsub("\\.tif","",v_list_bin_mod);v_list_bin_mod <- gsub("_10_MAXENT","",v_list_bin_mod); v_list_bin_mod <- gsub("_pub$","",v_list_bin_mod); 
v_list_bin_mod <- gsub("[0-9]+_","",v_list_bin_mod); v_list_bin_mod <- gsub("_"," ",v_list_bin_mod)

v_list_con_mod <- gsub("_MAXENT\\.tif","",v_list_con_mod); v_list_con_mod <- gsub("_"," ",v_list_con_mod)

v_list_con_not_bin_mod <- v_list_con_mod[!v_list_con_mod %in% v_list_bin_mod] # can the be downloaded from Biomodelos
v_list_bin_not_con_mod <- v_list_bin_mod[!v_list_bin_mod %in% v_list_con_mod]

# #update biomod list with actual files
# biomod_sp <- data.frame(Especie=c(v_list_bin_mod,v_list_con_mod)) |> count(Especie)

#-------------------------------------------------------------------------------
# Vásquez-Peinado list 
#-------------------------------------------------------------------------------
# df_tdf_plant_sp <- read_csv("./00_rawdata/tables/old/jan2024/df_tdf_plant_sp_VasquezPeinado.csv")

# #verify names
# # df_tdf_plant_sp_check <- tnrs(query = df_tdf_plant_sp$sci_name, source = "iPlant_TNRS") # tnrs is not working anymore
# #get verified names from taxize package
# df_tdf_plant_sp_check <- gnr_resolve(sci = df_tdf_plant_sp$sci_name)
# 
# # select names without author
# df_tdf_plant_sp_verif <- df_tdf_plant_sp_check |> group_by(submitted_name) |> distinct(matched_name) |> 
#   mutate(checked_name=word(matched_name, 1,2, sep=" ")) |> distinct(checked_name) |> 
#   filter(!str_detect(checked_name,"\\(")) |> 
#   mutate(sci_name = case_when(str_detect(checked_name,"manilkara") ~ "Manilkara zapota",
#                               str_detect(checked_name,"Spondias") ~ "Spondias mombin",
#                               TRUE ~ checked_name)) |>
#   distinct(sci_name)
# 
# #check sp that still have two names
# # temp <- df_tdf_plant_sp_verif |> count(submitted_name) |> arrange(desc(n))
# 
# write_csv(df_tdf_plant_sp_verif, "./00_rawdata/tables/df_tdf_plant_sp_VasquezPeinado_verif.csv")
df_tdf_plant_sp_verif <- read_csv("./00_rawdata/tables/df_tdf_plant_sp_VasquezPeinado_verif.csv")


################################################################################
# Dispersion distances
################################################################################

#------------------------------------------------------------------------------
# Mammals - Diaz et al. 2023
#------------------------------------------------------------------------------
df_hr_diaz <- read_csv("/Volumes/Extreme Pro/Hikaru/Doctorado/data/numeric/Traits/HomeRange_Diaz2024/camila_data_dist.csv")
df_hr_diaz <- df_hr_diaz |> mutate(disp_dist_m= Distance_km*1000)

#------------------------------------------------------------------------------
# Birds - Avonet and Ocampo-Peña 2024
#------------------------------------------------------------------------------
df_avonet <- read_csv("/Volumes/Extreme Pro/Hikaru/Doctorado/data/numeric/Avonet/ELEData/TraitData/AVONET1_BirdLife.csv")
head(df_avonet)
names(df_avonet)

# Use equation from Ocampo-Peñuela 2024 used for EcoScape to measure dispersion distance
df_avonet <- df_avonet |> mutate(d_km= 0.734*`Hand-Wing.Index`-8.777,
                                 d_km= if_else(d_km<0, NA, d_km), # there are some species that give a negative value
                                 disp_dist_m= d_km*1000)

sum(is.na(df_avonet$d_km))

# write_csv(df_avonet, "/Volumes/Extreme Pro/Hikaru/Doctorado/data/numeric/Avonet/ELEData/TraitData/AVONET1_BirdLife_Dispersion.csv")

#------------------------------------------------------------------------------
# Plants - Dispersion Database D3
#------------------------------------------------------------------------------
df_d3 <- read_csv2("/Volumes/Extreme Pro/Hikaru/Doctorado/data/numeric/Traits/D3/1-s2.0-S1433831913000218-mmc1.txt")
df_d3 <- df_d3 |> separate(name,c("genus","epit","otro"),sep=" ",remove = F) |> mutate(family=toupper(family))
names(df_d3)
head(df_d3)

# diaspore mass-----------------------------------------------------------------
summary(df_d3$dia_mass)
plot(density(df_d3$dia_mass,na.rm=T))
hist(df_d3$dia_mass,breaks=seq(0,6000,100))
plot(density(log(df_d3$dia_mass),na.rm=T))

library(plyr)
df_dia_mass_percentiles <- round(quantile(df_d3$dia_mass,seq(0,1,0.20),na.rm=T),1) |> as_tibble() |> mutate(value2=round_any(value,accuracy=0.5,f=ceiling))
unloadNamespace("plyr")

# classify diaspore mass according to rounded percentiles
df_d3 <- df_d3 |> 
  mutate(dia_mass_class=cut(dia_mass,breaks = df_dia_mass_percentiles$value2[c(1,2,5,6)],labels = c("small","medium","large"),include.lowest = T),
         .after="dia_mass")

df_d3_dia_mass <- df_d3 |> group_by(family) |> count(dia_mass_class) |> na.omit()

# dispersion type---------------------------------------------------------------
df_d3 |> summarise_at(vars(citation_prop_ane:citation_prop_other),mean,na.rm=T)
summary(df_d3$citation_total)

# measure weighted mean to get proportion of times the dispersion type appeared by family
df_d3_citation_props <- df_d3 |> select(family,starts_with("citation")) |> group_by(family) |> 
  summarise_at(vars(citation_prop_ane:citation_prop_other), ~weighted.mean(., w=citation_total,na.rm=T)) |> 
  mutate(verif_total=rowSums(across(where(is.numeric)),na.rm=T)) |> 
  rename_at(vars(citation_prop_ane:citation_prop_other),~paste0("gral_",.))

# reclassify dispersion type to work with just 4 categories
df_d3_citation_props_long <- df_d3_citation_props |> group_by(family) |> 
  pivot_longer(starts_with("gral"),names_to="dispersion_type",values_to="proportion") |> 
  mutate(dispersion_type2=case_when(dispersion_type=="gral_citation_prop_ane"   ~ "Anemochory",
                                    dispersion_type=="gral_citation_prop_hydro" ~ "Hydrochory",
                                    dispersion_type=="gral_citation_prop_epi" |
                                      dispersion_type=="gral_citation_prop_endo" |
                                      dispersion_type=="gral_citation_prop_dyso" |
                                      dispersion_type=="gral_citation_prop_hem" ~ "Zoochory",
                                    dispersion_type=="gral_citation_prop_other" ~ "Other"))

# regroup proportion values based on new classification
df_d3_citation_props_long2 <- df_d3_citation_props_long |> group_by(family,dispersion_type2) |> 
  summarise(proportion=sum(proportion,na.rm=T)) |> ungroup() |>
  filter(proportion>0) # remove families without data (all values were 0), minimum value is 0.4

# combine mass and dispersion type----------------------------------------------
df_d3_DispMass <- df_d3_dia_mass |> left_join(df_d3_citation_props_long2,by="family",relationship = "many-to-many") |> 
  mutate(ind_dis_mass=proportion*n , disp_dist_class=case_when(dia_mass_class=="large" ~ "short",
                                                               dia_mass_class=="medium" ~ "medium",
                                                               dia_mass_class=="small" & (dispersion_type2=="Zoochory" | dispersion_type2=="Other")~ "medium",
                                                               dia_mass_class=="small" & (dispersion_type2=="Anemochory" | dispersion_type2=="Hydrochory")~ "long"),
         disp_dist_m = case_when(disp_dist_class=="short"~300, # add three possible dispersion distances according to matrix 
                                 disp_dist_class=="medium"~1000,
                                 disp_dist_class=="long"~3000))

# choose categories with 2 highest indicator values that combines number of registers for the diaspore mass and the proportion of dispersion type
df_d3_DispMass_max_ind <- df_d3_DispMass |> ungroup() |> slice_max(ind_dis_mass,by="family",with_ties=T)
test_dup <- df_d3_DispMass_max_ind |> count(family)

# leave longest distance
df_d3_DispMass_max_dist <- df_d3_DispMass_max_ind |> ungroup() |> slice_max(disp_dist_m,by="family") |> count(family,disp_dist_m)
test_dup <- df_d3_DispMass_max_dist |> count(family)

# assign to D3 database by family and save
df_d3_dist <- df_d3 |> left_join(df_d3_DispMass_max_dist,by="family")
write_csv2(df_d3_dist,"/Volumes/Extreme Pro/Hikaru/Doctorado/data/numeric/Traits/D3/df_d3_dispersion_dist.txt")
df_d3_dist <- read_csv2("/Volumes/Extreme Pro/Hikaru/Doctorado/data/numeric/Traits/D3/df_d3_dispersion_dist.txt")

#-------------------------------------------------------------------------------
# Final dispersion data
#-------------------------------------------------------------------------------
# mammals
df_tdf_sp_info_mam <- df_tdf_sp_info |> dplyr::filter(sci_name %in% df_hr_diaz$sci_name) |> 
  left_join(df_hr_diaz |> select(sci_name,disp_dist_m),by="sci_name") 

# birds
df_tdf_sp_info_brd <- df_tdf_sp_info |> dplyr::filter(sci_name %in% df_avonet_in_list$Species1) |> 
  left_join(df_avonet_in_list |> select(Species1,disp_dist_m),by=c("sci_name"="Species1")) 

# plants
df_tdf_sp_info_plt <- df_tdf_sp_info |> dplyr::filter(sci_name %in% df_family_in_d3$sci_name) |> 
  left_join(df_d3_in_list |> select(family,disp_dist_m),by=c("family")) 

df_tdf_sp_disp_info <- df_tdf_sp_info_mam |> bind_rows(df_tdf_sp_info_brd,df_tdf_sp_info_plt) |>
  mutate(rad=round(disp_dist_m/100))
  

################################################################################
# List of species with traits
################################################################################
df_tdf_sp_info_avail <- df_tdf_sp_info |> mutate(has_elev=if_else(sci_name %in% df_sp_info_elev_check$sci_name,1,0),
                                            has_habitat=if_else(sci_name %in% df_habitat$sci_name,1,0),
                                            has_ran_map=if_else(sci_name %in% df_litlist_in_iucn_range$sci_name,1,0),
                                            has_biomod=if_else(sci_name %in% biomod_sp$Especie,1,0),
                                            has_mod_con=if_else(sci_name %in% v_list_con_mod,1,0),
                                            has_mod_bin=if_else(sci_name %in% v_list_bin_mod,1,0),
                                            has_mod_vpe=if_else(sci_name %in% df_tdf_plant_sp_verif$sci_name,1,0),
                                            has_disp_dist=if_else(sci_name %in% df_tdf_sp_disp_info$sci_name, 1,0))

df_summary_info_avail <- df_tdf_sp_info_avail |> count(has_elev,has_habitat,
                                                       has_ran_map,has_biomod,
                                                       has_mod_con,has_mod_bin,
                                                       has_mod_vpe,
                                                       has_disp_dist)

v_bat_families <- c("Emballonuridae","Molossidae","Mormoopidae",
                    "Noctilionidae","Phyllostomidae","Vespertilionidae")

# join sp info dataframe with dispersion info data frame and filter by having continuous model, habitat and dispersion info availability
df_tdf_sp_for_omni_batch2 <- df_tdf_sp_info_avail |> filter(has_habitat==1,
                                                            (has_mod_con==1 | has_mod_vpe==1),
                                                            has_disp_dist==1) |> 
  left_join(df_tdf_sp_disp_info |> select(sci_name,disp_dist_m,rad),
            by="sci_name") |> mutate(aerial=case_when(class=="AVES" ~ 1,
                                                      family %in% v_bat_families~1,
                                                      TRUE ~0))

# filter and counts for decission tree
df_tdf_sp_info_avail |> group_by(kingdom) |> count(has_elev,has_habitat,has_ran_map,has_mod_con,has_mod_vpe)
df_tdf_sp_info_avail |> filter(has_habitat==1) |> group_by(kingdom) |> count(has_ran_map,has_mod_con,has_mod_vpe)
df_tdf_sp_info_avail |> filter(kingdom=="ANIMALIA") |> count(has_habitat,has_disp_dist,has_ran_map,has_mod_con,has_mod_vpe)

write_csv(df_tdf_sp_for_omni_batch2,"./00_rawdata/tables/df_tdf_sp_for_omni_batch2.csv")


#-------------------------------------------------------------------------------
# Remove species with conflicts
#-------------------------------------------------------------------------------
# speces that did not overlap with the Study area for the Potential TDF
scp1_conflicts <- c("Gonzalagunia ovatifolia","Salacia macrantha","Senna multijuga", # SDM did not overlap
                    "Setophaga pensylvanica","Proechimys semispinosus", # SDM and range map did not overlap
                    "Sittasomus griseicapillus","Transandinomys talamancae","Microsciurus flaviventer") # SDM and range map did not overlap

df_tdf_sp_for_omni_batch3 <- df_tdf_sp_for_omni_batch2 |> filter(!sci_name %in% scp1_conflicts)
df_tdf_sp_for_omni_batch3 <- df_tdf_sp_for_omni_batch3 |> filter(has_mod_con==1) # removed temporarily species from Vásquez-Peinado
write_csv(df_tdf_sp_for_omni_batch3,"./00_rawdata/tables/df_tdf_sp_for_omni_batch3.csv")
df_tdf_sp_for_omni_batch3 <- read_csv("./00_rawdata/tables/df_tdf_sp_for_omni_batch3.csv")

# block size--------------------------------------------------------------------
df_tdf_sp_for_omni_batch3_group <- df_tdf_sp_for_omni_batch3 |> 
  mutate(block_size=floor(rad/20),
         block_size=if_else(block_size<1,1,block_size),
         rad_block=rad/block_size,
         block_group=cut(block_size,breaks = c(0,1,10,1000),labels=paste0("b",1:3)),
         radiu_group=cut(rad,breaks = c(0,3,10,1000),include.lowest = T,labels=paste0("r",1:3)),
         job_group=case_when(radiu_group=="r1" & block_group=="b1" ~ "g1",
                             radiu_group=="r2" & block_group=="b1" ~ "g2",
                             radiu_group=="r3" & block_group=="b1" ~ "g3",
                             radiu_group=="r3" & block_group=="b2" ~ "g4",
                             radiu_group=="r3" & block_group=="b3" ~ "g5"),
         block_size=if_else(job_group=="g5",floor(rad/10),block_size)) # update g5 with block size equal to rad/10
write_csv(df_tdf_sp_for_omni_batch3_group,"./00_rawdata/tables/df_tdf_sp_for_omni_batch3_group.csv")

count_bsize <- df_tdf_sp_for_omni_batch3_group |> count(block_size)
count_rad <- df_tdf_sp_for_omni_batch3_group |> count(rad)

# all combinations for block and radius size groups
df_tdf_sp_for_omni_batch3_group |> count(radiu_group,block_group)