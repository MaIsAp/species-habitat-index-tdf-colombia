library(rredlist)
library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(units)
library(taxize)
library(stringr)
library(furrr)

token <- readLines("./00_rawdata/tokens/IUCN_API.txt")

#-------------------------------------------------------------------------------
# IUCN rangemaps
#-------------------------------------------------------------------------------
# complete range maps that overlap with col-------------------------------------
sf_rangemaps_tdf_col_intersect <- st_read("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IUCN/RangeMapsColombia/Range_maps_BST_Col_intersection.gpkg")#/run/media/miap/C084AA1284AA0ACC/Hikaru/Doctorado/data/maps/IUCN/RangeMapsColombia/Range_maps_BST_Col_intersection.gpkg")

sp_in_tdf <- sf_rangemaps_tdf_col_intersect |> st_drop_geometry() |> count(sci_name) |> arrange(desc(n)) 

#with unite or combine produces a group of polygons by species without repetition by combining the different polygons
# sf_rangemaps_tdf_col_intersect_uni <- sf_rangemaps_tdf_col_intersect |> group_by(sci_name,class,category) |> 
#   summarise(geom = st_combine(geom))
# 
# st_write(obj = sf_rangemaps_tdf_col_intersect_uni,
#          "/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IUCN/RangeMapsColombia/Range_maps_BST_Col_intersection_combine_sp.gpkg")
sf_rangemaps_tdf_col_intersect_uni <- st_read("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IUCN/RangeMapsColombia/Range_maps_BST_Col_intersection_combine_sp.gpkg")

# #verify number of sp after grouping
# temp <- sf_rangemaps_tdf_col_intersect_uni |> st_drop_geometry() |> count(sci_name) #|> arrange(desc(n))

# # measure area of rangemap inside Colombia, this is not working, but I measured the area with QGIS
# sf_rangemaps_tdf_col_intersect <- sf_rangemaps_tdf_col_intersect_uni |> mutate(area_in_col2=st_area(sf_rangemaps_tdf_col_intersect_uni))

temp <- df_rangemaps_tdf_col_intersect |> count(sci_name) |> arrange(desc(n))

df_rangemaps_tdf_col_intersect_uni <- sf_rangemaps_tdf_col_intersect_uni |> st_drop_geometry()
head(df_rangemaps_tdf_col_intersect_uni)

# rangemaps cropped at the TDF potential area-----------------------------------
sf_rangemaps_tdf_intersect <- st_read("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IUCN/RangeMapsColombia/Range_maps_BST_intersection.gpkg")#"/run/media/miap/C084AA1284AA0ACC/Hikaru/
# sf_rangemaps_tdf_intersect_uni <- sf_rangemaps_tdf_intersect |> group_by(sci_name,class,category) |> 
#   summarise(geom = st_union(geom))
# 
# temp <- sf_rangemaps_tdf_intersect |> count(sci_name) |> arrange(desc(n))

# st_write(obj = sf_rangemaps_tdf_intersect_uni,
#          "/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IUCN/RangeMapsColombia/Range_maps_BST_intersection_union_sp.gpkg",append=F)
sf_rangemaps_tdf_intersect_uni <- st_read("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IUCN/RangeMapsColombia/Range_maps_BST_intersection_union_sp.gpkg")

# measure area of rangemap inside TDF, but since it did not work for the complete range maps in Colombia will keep the measurements done in QGIS for consistency
# sf_rangemaps_tdf_intersect_sum <- sf_rangemaps_tdf_intersect_uni |> mutate(area_in_tdf2=st_area(sf_rangemaps_tdf_intersect_uni))
df_rangemaps_tdf_intersect_uni <- sf_rangemaps_tdf_intersect_uni |> st_drop_geometry()

head(df_rangemaps_tdf_intersect_uni)

# load map with area of potential TDF
sf_tdf_pot_area <- st_read("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/BST/riesgo_etter/EcosistOrigBsTaeEtter_fixed.gpkg")
tdf_area <- drop_units(sum(st_area(sf_tdf_pot_area))) # 112724,270521 Km2

# join dataframe with rangemap size for all Colombia to get proportion of area in TDF based on the area for Colombia
df_rangemaps_tdf_col_areas <- df_rangemaps_tdf_intersect_uni |> 
  left_join(df_rangemaps_tdf_col_intersect_uni , by=c("sci_name","class","category"))

df_rangemaps_tdf_col_areas <- df_rangemaps_tdf_col_areas |> mutate(tdf_area = tdf_area,
                                                                   Prop_area_in_col=area_in_bst/area_in_col,
                                                                   Prop_area_in_bst=area_in_bst/tdf_area)

# 75% of the 2068 species have 
summary(df_rangemaps_tdf_col_areas$Prop_area_in_col)

summary(df_rangemaps_tdf_col_areas$area_in_bst)
summary(df_rangemaps_tdf_col_areas$Prop_area_in_bst)

# df_tdf_rangemaps <- df_rangemaps_tdf_col_areas |> filter(Prop_area_in_bst>=0.1)
df_tdf_rangemaps <- df_rangemaps_tdf_col_areas |> filter(area_in_bst>=1e+10)

unique(df_tdf_rangemaps$sci_name)

head(df_tdf_rangemaps)

write_csv(df_tdf_rangemaps,"./00_rawdata/tables/df_tdf_sp_IUCN_1mil_ha_InTDF.csv")
df_tdf_rangemaps <- read_csv("./00_rawdata/tables/df_tdf_sp_IUCN_1mil_ha_InTDF.csv")

df_tdf_rangemaps |> count(class)

#-------------------------------------------------------------------------------
# Biomodelos
#-------------------------------------------------------------------------------
biomod_sp <- read_csv("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/Biomodelos/list.spp.models.csv")#/run/media/miap/C084AA1284AA0ACC/Hikaru

head(biomod_sp)

df_tdf_sp <- biomod_sp |> filter(Especie %in% df_tdf_rangemaps$sci_name)

#-------------------------------------------------------------------------------
# Literature list
#-------------------------------------------------------------------------------
litlist_sp <- read_csv("/Volumes/Extreme Pro/Hikaru/Doctorado/Documento/Capitulo_01/04_archivos/Capitulo1/tables/TDFsp_from_literature.csv")#/run/media/miap/C084AA1284AA0ACC/Hikaru
appendix_litlist_sp <- read_csv("/Volumes/Extreme Pro/Hikaru/Doctorado/data/numeric/BST_IAvH/all_pizano2014.csv")

litlist_sp <- litlist_sp |> bind_rows(appendix_litlist_sp) |> group_by(group) |> count(sci_name)

# #check duplicates
# temp <- litlist_sp |> count(sci_name) |> arrange(desc(n))
# 
# #get verified names from taxize package
# df_litlist_sp_check <- gnr_resolve(sci = litlist_sp$sci_name)
# 
# # select names without author
# df_litlist_sp_verif <- df_litlist_sp_check |> group_by(submitted_name) |> distinct(matched_name) |>
#   mutate(checked_name=word(matched_name, 1,2, sep=" ")) |> distinct(checked_name) |>
#   filter(!str_detect(checked_name,"\\(")) |>
#   mutate(sci_name = case_when(str_detect(checked_name,"fuliginoſa") ~ "Amphisbaena fuliginosa",
#                               checked_name == "boa constrictor" ~ "Boa constrictor",
#                               checked_name == "Carollia? perspicillata" ~ "Carollia perspicillata",
#                               checked_name == "Hemi-Dactylus frenatus" ~ "Hemidactylus frenatus",
#                               str_detect(checked_name,"Spondias") ~ "Spondias mombin",
#                               TRUE ~ checked_name)) |>
#   distinct(sci_name)
# 
# #check sp that still have two names
# temp <- df_litlist_sp_verif |> count(submitted_name) |> arrange(desc(n))
# 
# sum(df_litlist_sp_verif$submitted_name!=df_litlist_sp_verif$sci_name)
# df_litlist_sp_verif$sci_name[df_litlist_sp_verif$submitted_name!=df_litlist_sp_verif$sci_name]
# 
# df_litlist_sp_verif <- df_litlist_sp_verif %>%
#   left_join(litlist_sp  ,by=c("submitted_name"="sci_name")) %>%
#   select(-submitted_name)
# 
# # fixed one species left without group for caps difference
# df_litlist_sp_verif$group[grep("savagei",df_litlist_sp_verif$sci_name,value=F)] <- "Amphibians"
# 
# litlist_plant_sp <- df_litlist_sp_verif |> filter(group=="Plants")
# litlist_anima_sp <- df_litlist_sp_verif |> filter(group!="Plants")
# 
# write_csv(df_litlist_sp_verif, "./00_rawdata/tables/df_litlist_sp_verif.csv")
df_litlist_sp_verif<- "./00_rawdata/tables/df_litlist_sp_verif.csv" #------------

# check species included in range maps IUCN or biomodelos
df_litlist_sp_veri_in_iucn <- df_litlist_sp_verif |> filter(sci_name %in% df_tdf_rangemaps$sci_name)
df_litlist_sp_veri_in_biom <- df_litlist_sp_verif |> filter(sci_name %in% biomod_sp$Especie) #*

df_litlist_sp_veri_in_biom_iucn <- df_litlist_sp_veri_in_biom |> filter(sci_name %in% df_tdf_rangemaps$sci_name)

df_litlist_anima_sp_in_biom <- litlist_anima_sp |> filter(sci_name %in% biomod_sp$Especie) #*
df_litlist_plant_sp_in_biom <- litlist_plant_sp |> filter(sci_name %in% biomod_sp$Especie) #*

#-------------------------------------------------------------------------------
# Vásquez-Peinado list
#-------------------------------------------------------------------------------
df_tdf_plant_sp <- read_csv("./00_rawdata/tables/df_tdf_plant_sp_VasquezPeinado.csv")

# #verify names---------------------------
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
df_tdf_plant_sp_verif <- "./00_rawdata/tables/df_tdf_plant_sp_VasquezPeinado_verif.csv"#----------------

#check species that have Biomodelo and model with Vasquez
vasquez_in_biomod <- df_tdf_plant_sp_verif[df_tdf_plant_sp_verif$sci_name %in% biomod_sp$Especie,] #*
vasquez_no_biomod <- df_tdf_plant_sp_verif[!df_tdf_plant_sp_verif$sci_name %in% biomod_sp$Especie,]

vasquez_in_litlist <- df_tdf_plant_sp_verif |> filter(!sci_name %in% biomod_sp$Especie , sci_name %in% litlist_plant_sp$sci_name)

#----------------------------------------

#-------------------------------------------------------------------------------
# González-M Plant trait sp list
#-------------------------------------------------------------------------------
df_tdf_plant_trait_sp <- read_csv("/Volumes/Extreme Pro/Hikaru/Doctorado/Documento/Capitulo_02/01_repositorios/Biotablero/02_outdata/TDF_traits_sp_filtered.csv")#/run/media/miap/C084AA1284AA0ACC
df_tdf_plant_occur_sp <- read_csv("/Volumes/Extreme Pro/Hikaru/Doctorado/Documento/Capitulo_02/01_repositorios/Biotablero/02_outdata/TDF_traits_sp_occurences.csv")#/run/media/miap/C084AA1284AA0ACC

# plants with occurrences and traits
df_tdf_plant_trait_occur_sp <- df_tdf_plant_trait_sp |> filter(sci_name %in% df_tdf_plant_occur_sp$sci_name)
# plants with occurrences, traits and have SDMs (biomodelos or Vasquez-Peinado)
df_tdf_plant_trait_occur_biomod_sp <- df_tdf_plant_trait_occur_sp |> filter(sci_name %in% biomod_sp$Especie)
df_tdf_plant_trait_occur_vasquez_sp <- df_tdf_plant_trait_occur_sp |> filter(sci_name %in% df_tdf_plant_sp_verif$sci_name)
df_tdf_plant_trait_occur_only_vasquez_sp <- df_tdf_plant_trait_occur_vasquez_sp |> filter(!sci_name %in% biomod_sp$Especie)

# verify names--------------------------------------------------
# #get verified names from taxize package #------------
# df_tdf_plant_trait_sp_check <- gnr_resolve(sci = df_tdf_plant_trait_sp$sci_name)
# 
# # select names without author
# df_tdf_plant_trait_sp_verif <- df_tdf_plant_trait_sp_check |> group_by(submitted_name) |> distinct(matched_name) |> 
#   mutate(checked_name=word(matched_name, 1,2, sep=" ")) |> distinct(checked_name) |> 
#   filter(!str_detect(checked_name,"\\(")) |> 
#   mutate(sci_name = case_when(str_detect(checked_name,"Clusia") ~ "Clusia umbellata", # no clear reference found, kept the same
#                               str_detect(checked_name,"Spondias") ~ "Spondias mombin",
#                               TRUE ~ checked_name)) |>
#   distinct(sci_name)
# 
# #check sp that still have two names
# # temp <- df_tdf_plant_trait_sp_verif |> count(submitted_name) |> arrange(desc(n))
# 
# write_csv(df_tdf_plant_trait_sp_verif, "./00_rawdata/tables/df_tdf_plant_trait_sp_verif.csv")
df_tdf_plant_trait_sp_verif <- "./00_rawdata/tables/df_tdf_plant_trait_sp_verif.csv"#----

# #get verified names from taxize package --------
# df_tdf_plant_occur_sp_check <- gnr_resolve(sci = df_tdf_plant_occur_sp$sci_name)
# 
# # select names without author
# df_tdf_plant_occur_sp_verif <- df_tdf_plant_occur_sp_check |> group_by(submitted_name) |> distinct(matched_name) |> 
#   mutate(checked_name=word(matched_name, 1,2, sep=" ")) |> distinct(checked_name) |> 
#   filter(!str_detect(checked_name,"\\(")) |> 
#   mutate(sci_name = case_when(str_detect(checked_name,"Erythrina rubrivenium") ~ "Erythrina rubrivenium", # no clear reference found, kept the same but Tetrorchidium rubrivenium can also be
#                               str_detect(checked_name,"Spondias") ~ "Mimosa pudicavartetrandra", # no clear reference found, kept the same
#                               str_detect(checked_name,"Coursetia") ~ "Coursetia caribaeavar",# no clear reference found, kept the same
#                               TRUE ~ checked_name)) |>
#   distinct(sci_name)
# 
# #check sp that still have two names
# # temp <- df_tdf_plant_occur_sp_verif |> count(submitted_name) |> arrange(desc(n))
# 
# write_csv(df_tdf_plant_occur_sp_verif, "./00_rawdata/tables/df_tdf_plant_occur_sp_verif.csv")
df_tdf_plant_occur_sp_verif <- "./00_rawdata/tables/df_tdf_plant_occur_sp_verif.csv"#-----

#---------------------------------------------------------------

df_vasquez_sp_in_gonzalez_trait <- df_tdf_plant_trait_sp_verif |> filter(sci_name %in% df_tdf_plant_sp_verif$sci_name) |> 
  count(sci_name) |> arrange(desc(n))
df_vasquez_sp_in_gonzalez_occur <- df_tdf_plant_occur_sp_verif |> filter(sci_name %in% df_tdf_plant_sp_verif$sci_name) |> 
  count(sci_name) |> arrange(desc(n))

df_biomode_sp_in_gonzalez_trait <- df_tdf_plant_trait_sp_verif |> filter(sci_name %in% biomod_sp$Especie) |> 
  count(sci_name) |> arrange(desc(n))
df_biomode_sp_in_gonzalez_occur <- df_tdf_plant_occur_sp_verif |> filter(sci_name %in% biomod_sp$Especie) |> 
  count(sci_name) |> arrange(desc(n))

df_vasquez_sp_in_gonzalez_no_biomod_trait <- df_vasquez_sp_in_gonzalez_trait |> filter(!sci_name %in% biomod_sp$Especie) |> 
  count(sci_name) |> arrange(desc(n))
df_vasquez_sp_in_gonzalez_no_biomod_occur <- df_vasquez_sp_in_gonzalez_occur |> filter(!sci_name %in% biomod_sp$Especie) |> 
  count(sci_name) |> arrange(desc(n))

# None of these plants are in the IUCN rangemaps

#-------------------------------------------------------------------------------
# Total list of species with range maps or with SDM
#-------------------------------------------------------------------------------

# Animals-------------------------------------------------------------------
df_tdf_anim_sp <- rbind(df_tdf_rangemaps %>% select(sci_name), # in IUCN
                        litlist_anima_sp %>% ungroup() %>% select(sci_name)) |> # in list from literature
  count(sci_name) |> arrange(desc(n))

# has range map-----------
df_tdf_anim_sp_in_iucn <- df_tdf_anim_sp |> filter(sci_name %in% df_tdf_rangemaps$sci_name)

# biomodelos
df_tdf_anim_sp_in_iucn_in_biom <- df_tdf_anim_sp_in_iucn |> filter(sci_name %in% biomod_sp$Especie) %>% mutate(info="IUCN_&_BIOMOD")
df_tdf_anim_sp_in_iucn_no_biom <- df_tdf_anim_sp_in_iucn |> filter(!sci_name %in% biomod_sp$Especie)%>% mutate(info="IUCN")

# don't have range map----
df_tdf_anim_sp_no_iucn <- df_tdf_anim_sp |> filter(!sci_name %in% df_tdf_rangemaps$sci_name)

# biomodelos
df_tdf_anim_sp_no_iucn_in_biom <- df_tdf_anim_sp_no_iucn |> filter(sci_name %in% biomod_sp$Especie) %>% mutate(info="BIOMOD")
df_tdf_anim_sp_no_iucn_no_biom <- df_tdf_anim_sp_no_iucn |> filter(!sci_name %in% biomod_sp$Especie)

# total list--------------
df_tdf_prel_anim_sp <- rbind(df_tdf_anim_sp_in_iucn_in_biom,
                             df_tdf_anim_sp_in_iucn_no_biom,
                             df_tdf_anim_sp_no_iucn_in_biom) %>% select(-n)

temp <- df_tdf_prel_anim_sp %>% count(sci_name)

write_csv(df_tdf_prel_anim_sp, "./00_rawdata/tables/df_tdf_prel_anim_sp.csv")

# Plants-----------------------------------------------------------------------
df_tdf_plan_sp <- rbind(df_tdf_plant_sp_verif %>% ungroup() %>% select(sci_name), # in Vásquez-Peinado
                        litlist_plant_sp %>% ungroup() %>% select(sci_name), # in list from literature
                        df_tdf_plant_trait_sp_verif %>% ungroup() %>% select(sci_name), # González-M
                        df_tdf_plant_occur_sp_verif %>% ungroup() %>% select(sci_name)) |> # González-M
  count(sci_name) |> arrange(desc(n))

# have Biomodelo----------
df_tdf_plan_sp_in_biom <- df_tdf_plan_sp |> filter(sci_name %in% biomod_sp$Especie)

df_tdf_plan_sp_in_biom_in_vasq <- df_tdf_plan_sp_in_biom |> filter(sci_name %in% df_tdf_plant_sp_verif$sci_name)%>% mutate(info="BIOMOD_&_VASQUEZ")
df_tdf_plan_sp_in_biom_no_vasq <- df_tdf_plan_sp_in_biom |> filter(!sci_name %in% df_tdf_plant_sp_verif$sci_name)%>% mutate(info="BIOMOD")

# do not have Biomodelo---
df_tdf_plan_sp_no_biom <- df_tdf_plan_sp |> filter(!sci_name %in% biomod_sp$Especie)

df_tdf_plan_sp_no_biom_in_vasq <- df_tdf_plan_sp_no_biom |> filter(sci_name %in% df_tdf_plant_sp_verif$sci_name) %>% mutate(info="VASQUEZ")
df_tdf_plan_sp_no_biom_no_vasq <- df_tdf_plan_sp_no_biom |> filter(!sci_name %in% df_tdf_plant_sp_verif$sci_name)

# total list--------------
df_tdf_prel_plan_sp <- rbind(df_tdf_plan_sp_in_biom_in_vasq,
                             df_tdf_plan_sp_in_biom_no_vasq,
                             df_tdf_plan_sp_no_biom_in_vasq) %>% select(-n)

write_csv(df_tdf_prel_plan_sp, "./00_rawdata/tables/df_tdf_prel_plan_sp.csv")

temp <- df_tdf_prel_plan_sp %>% count(sci_name)

# plant and animal species list--------------
df_tdf_prel_sp <- rbind(df_tdf_prel_plan_sp,
                        df_tdf_prel_anim_sp)

write_csv(df_tdf_prel_sp, "./00_rawdata/tables/df_tdf_prel_sp.csv")

temp <- df_tdf_prel_sp %>% count(sci_name)

#-------------------------------------------------------------------------------
# IUCN information
#-------------------------------------------------------------------------------

# #get elevation info first------------------------------------------------------------
# l_sp_info <- map(df_tdf_prel_sp$sci_name, ~rl_search(.x,key=token)$result)
# df_sp_info <- l_sp_info |> setNames(df_tdf_prel_sp$sci_name) |> bind_rows(.id="sci_name")
# 
# test <- map_dbl(l_sp_info, function(x) length(x)>0)
# sum(test)
# 
# # write_csv(df_sp_info,"./00_rawdata/tables/df_sp_inf.csv")
# df_sp_info <- read_csv("./00_rawdata/tables/df_sp_inf.csv")
# 
# temp <- df_sp_info %>% count(sci_name)
# 
# sum(!is.na(df_sp_info$elevation_lower))
# sum(!is.na(df_sp_info$elevation_upper))
# sum(!is.na(df_sp_info$elevation_lower) | !is.na(df_sp_info$elevation_upper))
# 
# df_sp_info_check <- df_sp_info %>% filter(!is.na(elevation_lower) | !is.na(elevation_upper)) %>% 
#   group_by(kingdom) %>% count(sci_name)
# 
# df_sp_info_check %>% count(kingdom)
# 
# # 1. 5 Tropical/tropical dry forest ---
# df_habitat_dryforest <- df_habitat %>% filter(code=="1.5")
# temp <- df_habitat_dryforest %>% count(sci_name)
# 
# # get habitat info ---
# l_habitat <- map(df_sp_info_check$sci_name, ~rl_habitats(.x,key=token)$result )
# 
# test <- map_dbl(l_habitat, function(x) length(x)>0)
# sum(test)
# 
# #separate habitat levels
# df_habitat <- l_habitat |> setNames(df_sp_info_check$sci_name) |> bind_rows(.id="sci_name")
# df_habitat <- df_habitat |> separate(code, into=c("n1","n2"),sep="\\.",remove=F)
# 
# write_csv(df_habitat,"./00_rawdata/tables/df_sp_habitat_inf.csv")
# # df_habitat <- read_csv("./00_rawdata/tables/df_tdf_sp_habitat_inf.csv")
# 
# # verify number of species by habitat type
# temp <- df_habitat %>% count(sci_name)
# df_habitat_cat_all <- df_habitat |> group_by(n1,n2) |> count()

# filter species for the subtr

 # step changed to use the next part (get habitat first)
# get habitat info first--------------------------------------------------------
library(parallel)
detectCores()

plan(multisession,workers=10)
l_habitat_2 <- future_map(df_tdf_prel_sp$sci_name, ~rl_habitats(.x,key=token)$result )

test <- map_dbl(l_habitat_2, function(x) length(x)>0)
sum(test)

#separate habitat levels
df_habitat2 <- l_habitat_2 |> setNames(df_tdf_prel_sp$sci_name) |> bind_rows(.id="sci_name")
df_habitat2 <- df_habitat2 |> separate(code, into=c("n1","n2"),sep="\\.",remove=F)

write_csv(df_habitat2,"./00_rawdata/tables/df_tdf_sp_habitat_inf.csv")
# df_habitat2 <- read_csv("./00_rawdata/tables/df_tdf_sp_habitat_inf.csv")

# check by group
temp <- df_tdf_prel_plan_sp %>% filter(sci_name %in% df_habitat2$sci_name)
temp <- df_tdf_prel_anim_sp %>% filter(sci_name %in% df_habitat2$sci_name)

# verify number of species by habitat type
df_tdf_sp_habitat_inf <- df_habitat2 %>% count(sci_name)
df_habitat_cat_all <- df_habitat2 |> group_by(n1,n2) |> count()

# how many species are in Vasquez
df_tdf_sp_habitat_inf_vasquez_no_biomod <- df_tdf_sp_habitat_inf |> 
  filter(sci_name %in% df_tdf_plant_sp_verif$sci_name  & ! sci_name %in% biomod_sp$Especie)

write_csv(df_tdf_sp_habitat_inf_vasquez_no_biomod,"./00_rawdata/tables/df_tdf_sp_habitat_inf_vasquez_no_biomod.csv")

# check by group
temp <- df_tdf_prel_plan_sp %>% filter(sci_name %in% df_tdf_sp_habitat_inf$sci_name)
temp <- df_tdf_prel_anim_sp %>% filter(sci_name %in% df_tdf_sp_habitat_inf$sci_name)

# filter by habitat 1.5 for TDF in IUCN removed too because it removes species in TDF defined with local criteria using global criteria

#get elevation info ------------------------------------------------------------
plan(multisession,workers=10)
l_sp_info2 <- future_map(df_tdf_sp_habitat_inf$sci_name, ~rl_search(.x,key=token)$result)
df_sp_info2 <- l_sp_info2 |> setNames(df_tdf_sp_habitat_inf$sci_name) |> bind_rows(.id="sci_name") |> arrange(desc(kingdom))

# check for empty species
test <- map_dbl(l_sp_info2, function(x) length(x)>0)
sum(test)

# write_csv(df_sp_info2,"./00_rawdata/tables/df_tdf_sp_inf.csv")
df_sp_info2 <- read_csv("./00_rawdata/tables/df_tdf_sp_inf.csv")

temp <- df_sp_info2 %>% count(sci_name)
df_sp_info2 %>% filter(sci_name %in% df_tdf_sp_habitat_inf$sci_name) %>% count(kingdom)

sum(!is.na(df_sp_info2$elevation_lower))
sum(!is.na(df_sp_info2$elevation_upper))
sum(!is.na(df_sp_info2$elevation_lower) | !is.na(df_sp_info2$elevation_upper))

# check for species with at least one elevation data
df_sp_info_check2 <- df_sp_info2 %>% filter(!is.na(elevation_lower) | !is.na(elevation_upper)) %>% 
  group_by(kingdom) %>% count(sci_name)

df_sp_info_check2 %>% count(kingdom)

# load missing elevation data from Occurrence data (IAvH) ----------------------

# load occurrence points with elevation data from DEM
sf_tdf_occurrences_dem <- st_read("/Volumes/Extreme Pro/Hikaru/Doctorado/Documento/Capitulo_02/01_repositorios/Biotablero/02_outdata/TDF_sp_occurences_points_buffer_elev.gpkg")
# temp1 <- st_area(sf_tdf_occurrences_dem) # check size of buffer

df_tdf_occurrences_dem <- sf_tdf_occurrences_dem |> st_drop_geometry() |> mutate(sci_name=paste(genus,sp_epithet))
# from all species with occurrence data filter the species included in list so far
df_tdf_occurrences_dem_in_list <- df_tdf_occurrences_dem |> filter(sci_name %in% df_sp_info2$sci_name)
temp <- df_tdf_occurrences_dem_in_list %>% count(sci_name)

library(plyr)
# get minimum and maximum elevation from mean elevation values extracted from DEM for occurrences
df_tdf_occurrences_dem_in_list_min_max <- df_tdf_occurrences_dem_in_list |> dplyr::group_by(sci_name) |> 
  dplyr::summarise(elevation_lower2=round_any(min(cop_dem_90_100mresmean),100,f=floor), # round to lower value multiple of 100
            elevation_upper2=round_any(max(cop_dem_90_100mresmean),100,f=ceiling)) # round to higher value multiple of 100
unloadNamespace("plyr")

# replace empty values for minimum and maximum elevation when data is available from occurrence points
df_sp_info_with_elev_from_occ <- df_sp_info2 |> left_join(df_tdf_occurrences_dem_in_list_min_max,by="sci_name") |> 
  mutate(elevation_lower=case_when(is.na(elevation_lower) & !is.na(elevation_lower2) ~ elevation_lower2,
                                   !is.na(elevation_lower) ~ elevation_lower),
         elevation_upper=case_when(is.na(elevation_upper) & !is.na(elevation_upper2) ~ elevation_upper2,
                                   !is.na(elevation_upper) ~ elevation_upper))

# check again species with at least one elevation range
df_sp_info_check2 <- df_sp_info_with_elev_from_occ %>% filter(!is.na(elevation_lower) | !is.na(elevation_upper)) %>% 
  group_by(kingdom) %>% count(sci_name)

df_sp_info_check2 %>% count(kingdom)

#update list with elevation data -----------------------------------------------
df_tdf_sp_info <- df_sp_info_with_elev_from_occ#df_sp_info_with_elev_from_occ %>% filter(sci_name %in% df_sp_info_check2$sci_name)

# write_csv(df_tdf_sp_info ,"./00_rawdata/tables/df_tdf_sp_inf.csv")
df_tdf_sp_info <- read_csv("./00_rawdata/tables/df_tdf_sp_inf.csv")

df_tdf_family_lev <- df_tdf_sp_info %>% group_by(kingdom,phylum,class,order,family) %>% count(family)

df_tdf_order_lev <- df_tdf_sp_info %>% group_by(kingdom,phylum,class,order) %>% count(order)

df_tdf_class_lev <- df_tdf_sp_info %>% group_by(kingdom,phylum,class) %>% count(class)


#-------------------------------------------------------------------------------
#Check model data
#-------------------------------------------------------------------------------

df_tdf_sp_without_model <- df_tdf_sp_info |> filter(!sci_name %in% biomod_sp$Especie)
df_tdf_sp_in_biomod <- df_tdf_sp_info |> filter(sci_name %in% biomod_sp$Especie)

#check species with model with Vasquez and no Biomodelos
df_tdf_sp_mod_vasquez <- df_tdf_sp_info |> filter(sci_name %in% df_tdf_plant_sp_verif$sci_name & !sci_name %in% biomod_sp$Especie)
temp <- df_tdf_sp_info |> filter(!sci_name %in% df_tdf_plant_sp_verif$sci_name & !sci_name %in% biomod_sp$Especie) # number of species with just range map


#-------------------------------------------------------------------------------
# Check SDM area
#-------------------------------------------------------------------------------
# load list of binary models
v_list_bin_mod <- list.files("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/Biomodelos/Random/binarios/")
# load list of continuous models
v_list_con_mod <- list.files("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/Biomodelos/Random/continuos/")

#get just species names
v_list_bin_mod <- gsub("_10_MAXENT.tif","",v_list_bin_mod); v_list_bin_mod <- gsub("_"," ",v_list_bin_mod)
v_list_con_mod <- gsub("_MAXENT.tif","",v_list_con_mod); v_list_con_mod <- gsub("_"," ",v_list_con_mod)

# get number of binary models with continuous model
df_tdf_sp_biomod_bin_in_con <- v_list_bin_mod[v_list_bin_mod %in% v_list_con_mod] # all continuous are in binary

#get list of data according to type models available
df_tdf_sp_biomod_bin <- df_tdf_sp_info |> filter(sci_name %in% v_list_bin_mod)
df_tdf_sp_biomod_con <- df_tdf_sp_info |> filter(sci_name %in% v_list_con_mod)

df_tdf_sp_biomod_con |> group_by(kingdom,phylum,class) %>% count(class)

df_tdf_sp_biomod_bin |> group_by(kingdom) %>% count(kingdom)
df_tdf_sp_biomod_bin |> group_by(kingdom,phylum,class) %>% count(class)

library(terra)
library(exactextractr)
v_list_bin_mod_path <- list.files("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/Biomodelos/Random/binarios/",full.names = T)
# v_list_con_mod_path <- list.files("/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/Biomodelos/Random/continuos/",full.names = T)

# load binary mod
l_r_mod <- map(v_list_bin_mod, ~rast(grep(paste0(gsub(" ","_",.x),"_"),v_list_bin_mod_path,value = T))) # added _ to avoid detecting similar species like trigona and trigonata
# calculate total area by adding pixels with resolution 1km2
l_area_total <- map(l_r_mod, ~global(.x,sum,na.rm=T))
df_area_total <- map(l_area_total,1) |> unlist() |> tibble::as_tibble_col(column_name="total") |> mutate(sci_name=v_list_bin_mod, .before= "total")

temp <- df_area_total |> count(sci_name)

# calc area in tdf
l_area_in_tdf <- map(l_r_mod, ~sum(exact_extract(.x,sf_tdf_pot_area,fun="sum")))
df_area_in_tdf <- l_area_in_tdf |> unlist()#setNames(v_list_bin_mod) |> bind_cols(.id="sci_name")
# create dataframe with areas
df_areas_sdm <- df_area_total |> mutate(in_tdf=df_area_in_tdf, Prop_tdf_in_SDM=in_tdf/total, Prop_SDM_in_tdf=in_tdf/tdf_area)

summary(df_areas_sdm$Prop_SDM_in_tdf)

# get plant species from data of IAvH and general stats of proportion of area in TDF for criteria
df_areas_sdm_plants <- df_areas_sdm |> filter(sci_name %in% df_tdf_plant_occur_sp_verif$sci_name | sci_name %in% df_tdf_plant_trait_sp_verif$sci_name)#df_tdf_plan_sp$sci_name)

summary(df_areas_sdm_plants$total)
summary(df_areas_sdm_plants$in_tdf)
summary(df_areas_sdm_plants$Prop_SDM_in_tdf)
summary(df_areas_sdm_plants$Prop_tdf_in_SDM)

# filter species list by proportion of habitat inside the TDF
df_sp_with_sdm_in_tdf <- df_areas_sdm |> filter(Prop_SDM_in_tdf>=0.01)
df_sp_with_sdm_to_exc <- df_areas_sdm |> filter(Prop_SDM_in_tdf<0.01)

#-------------------------------------------------------------------------------
# get list of species information according to the latter filter
#-------------------------------------------------------------------------------
df_tdf_sp_biomod_bin_prophab <- df_tdf_sp_info |> filter(sci_name %in% df_sp_with_sdm_in_tdf$sci_name)  
df_tdf_sp_biomod_bin_prophab |> count(kingdom)

df_sp_forSHI <- df_tdf_sp_info |> filter(!sci_name %in% df_sp_with_sdm_to_exc$sci_name)
df_sp_forSHI |> count(kingdom)

write_csv(dt_sp_forSHI, "./00_rawdata/tables/df_sp_forSHI.csv")


#-------------------------------------------------------------------------------
# get home range
#-------------------------------------------------------------------------------

library(traitdataform) #--------------------------------------------------------

pulldata("amniota")
head(amniota)

df_amniota_inlist <- amniota |> mutate(sci_name= paste(genus,species)) |> dplyr::filter(sci_name %in% df_tdf_sp_info$sci_name)
df_amniota_inlist |> count(class)

pulldata("amphibio")
head(amphibio)

df_amphibio_inlist <- amphibio |> dplyr::filter(Species %in% df_tdf_sp_info$sci_name) |> rename(sci_name=Species)
df_amphibio_inlist |> count(Order)
df_amphibio_inlist |> count(Family)

pulldata("pantheria")
head(pantheria)

df_pantheria_inlist <- pantheria |> dplyr::filter(MSW05_Binomial %in% df_tdf_sp_info$sci_name) |> rename(sci_name=MSW05_Binomial)
df_pantheria_inlist |> count(MSW05_Order)
df_pantheria_inlist |> count(MSW05_Family)

df_pantheria_inlist |> count(X12.2_Terrestriality)
df_pantheria_inlist |> count(X6.2_TrophicLevel)

df_home_range_pantheria <- df_pantheria_inlist |> 
  dplyr::select(sci_name,home_range_km2=X22.1_HomeRange_km2) |> 
  na.omit()

df_pantheria_in_amniota <- df_amniota_inlist |> filter(sci_name %in% df_pantheria_inlist$sci_name)


library(traits) #---------------------------------------------------------------
l_ernest <- tr_ernest()
names(l_ernest)

df_ernest <- l_ernest$data
df_ernest_sp <- df_ernest |> mutate(sci_name=paste(genus,species)) |> filter(sci_name %in% df_tdf_sp_info$sci_name)

df_ernest_sp |> count(class)

names(df_ernest_sp)

library(rfutres) #--------------------------------------------------------------
l_futres_inlist <- map(df_tdf_sp_info$sci_name,  ~futres_data(scientificName = .x)) |> setNames(df_tdf_sp_info$sci_name)
l_futres_data_inlist <- map(l_futres_inlist , ~.x$data) |> compact()

sp_in_futres <- names(l_futres_data_inlist)

df_tdf_sp_info |> filter(sci_name %in% sp_in_futres) |> count(class)


#------------------------------------------------------------------------------
# Tables
#------------------------------------------------------------------------------

# Camila data---------------------------------
df_hr_diaz <- read_csv("/Volumes/Extreme Pro/Hikaru/Doctorado/data/numeric/Traits/HomeRange_Diaz2024/camila_data_dist.csv")

# Disperson Database D3----------------------
df_d3 <- read_csv2("/Volumes/Extreme Pro/Hikaru/Doctorado/data/numeric/Traits/D3/1-s2.0-S1433831913000218-mmc1.txt")
df_d3 <- df_d3 |> separate(name,c("genus","epit","otro"),sep=" ",remove = F)

d3_genus_in_splist <- unique(df_d3$genus[df_d3$genus %in% df_tdf_sp_info$genus])

splist_genus_in_d3 <- df_tdf_sp_info$genus[df_tdf_sp_info$genus %in% df_d3$genus]
splist_famil_in_d3 <- df_tdf_sp_info$family[df_tdf_sp_info$family %in% toupper(df_d3$family)]
unique(splist_famil_in_d3)

df_d3_in_list <- df_d3 |> filter(toupper(family) %in% df_tdf_sp_info$family)

df_d3_in_list |> group_by(family) |> count(family) # Anacardiacea only has One sp with NAs

df_d3_in_list_rankvals <- df_d3_in_list |> group_by(family) |> summarise(min_ane=min(rank_ane,na.rm = T),mean_ane=mean(rank_ane,na.rm = T),max_ane=max(rank_ane,na.rm = T),
                                                                         min_hyd=min(rank_hydro,na.rm = T),mean_hyd=mean(rank_hydro,na.rm = T),max_hyd=max(rank_hydro,na.rm = T),
                                                                         min_epi=min(rank_epi_wool,na.rm = T),mean_epi=mean(rank_epi_wool,na.rm = T),max_epi=max(rank_epi_wool,na.rm = T))

# Avonet ------------------------------------
df_avonet <- read_csv("/Volumes/Extreme Pro/Hikaru/Doctorado/data/numeric/Avonet/ELEData/TraitData/AVONET1_BirdLife.csv")
head(df_avonet)
names(df_avonet)
df_avonet_in_list <- df_avonet |> filter(Species1 %in% df_tdf_sp_info$sci_name)

unique(df_avonet_in_list$Species1)
summary(df_avonet_in_list$`Hand-Wing.Index`)
summary(df_avonet_in_list$Range.Size)

df_avonet_in_list |> count(Habitat)
df_avonet_in_list |> count(Trophic.Level)
df_avonet_in_list |> count(Trophic.Niche)
df_avonet_in_list |> count(Primary.Lifestyle)

