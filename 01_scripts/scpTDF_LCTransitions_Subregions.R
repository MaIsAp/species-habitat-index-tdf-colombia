library(sf)
library(terra)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(ggalluvial)
library(scales)

################################################################################
# Load layers and table with categories
################################################################################

# get labels for categories
df_IUCN_LC_association_gralcat <- read.csv2("../03_data/inputs/tables/df_IUCN_LC_association_gral.csv")

# Raster with transitions between 2000,2010,2018
v_r_subreg_files <- list.files("../../../data/maps/IAvH/BST/transiciones/subregions",pattern = "tif$",full.names = T)
v_subreg_names <- gsub("\\.tif","",gsub("^.*subreg\\_","",v_r_subreg_files))
v_subregion_en <- c("Caribbean","North Andean","Orinoco","Cauca V.","Magdalena V.","Patia V.")

# load subregion raster with land cover codes per year
l_r_tdf_subreg <- map(v_r_subreg_files,rast)

################################################################################
# Get transition dataframe recategorized by subregion
################################################################################

# get frequency table for each combination of codes per year
l_tdf_subreg_transitions_00_10_20 <- map(l_r_tdf_subreg,freq)
names(l_tdf_subreg_transitions_00_10_20) <- v_subregion_en
df_tdf_subreg_transitions_00_10_20 <- l_tdf_subreg_transitions_00_10_20 |> bind_rows(.id = "subregion")

# separate y3 from code by "_" and y1 and 2 from getting the first and last 3 elements 
df_tdf_subreg_transitions_00_10_20_sep <- df_tdf_subreg_transitions_00_10_20 |> separate(value,c("value","y3"),sep="_") |> 
  mutate(value=as.numeric(value),cats=sprintf("%06d",value),.before = "y3") |> 
  tidyr::separate_wider_position(cats, c(y1=3,y2=3)) |> select(-layer,-value) 

# df_IUCN_LC_association_gralcat$gral_categ <- gsub("Heavily degraded forest","Successional forest",df_IUCN_LC_association_gralcat$gral_categ)

v_corine_level <- df_IUCN_LC_association_gralcat$gral_categ
names(v_corine_level) <- df_IUCN_LC_association_gralcat$CORINE_N3

# assign labels to levels
df_tdf_subreg_tran_00_10_20_labs <- df_tdf_subreg_transitions_00_10_20_sep |>
  mutate(y1_lab=map_chr(as.numeric(y1), ~str_replace_all(.x,v_corine_level)),
         y2_lab=map_chr(as.numeric(y2), ~str_replace_all(.x,v_corine_level)),
         y3_lab=map_chr(as.numeric(y3), ~str_replace_all(.x,v_corine_level)),
         )

# check conversion from pasturelands to forests, to see proportion with open forest-----
df_tdf_tran_10_20_past_for <- df_tdf_subreg_tran_00_10_20_labs |> ungroup() |> select(y2,y3,count) |> 
  filter((str_detect(y2,"^23") | str_detect(y2,"241")) & 
           str_detect(y3,"^31") & !str_detect(y3,"315")) |> 
  group_by(y2,y3) |> summarise(count_sum= sum(count)) |> 
  mutate(area_ha=count_sum*25*25/10000)

df_tdf_subreg_tran_00_10_20_labs |> ungroup() |> select(y2,count) |> 
  filter(str_detect(y2,"^31")) |> 
  group_by(y2) |> summarise(count_sum= sum(count)) |> 
  mutate(area_ha=count_sum*25*25/10000)
df_tdf_subreg_tran_00_10_20_labs |> ungroup() |> select(y3,count) |> 
  filter(str_detect(y3,"^31")) |> 
  group_by(y3) |> summarise(count_sum= sum(count)) |> 
  mutate(area_ha=count_sum*25*25/10000)

df_tdf_tran_10_20_for <- df_tdf_subreg_tran_00_10_20_labs |> ungroup() |> select(y2,y3,count) |> 
  filter(str_detect(y2,"^31") & y2!="315") |> 
  group_by(y3) |> summarise(count_sum= sum(count)) |> 
  mutate(area_ha=count_sum*25*25/10000)
# --------------------------------------------------------------------------------------

# regroup year combinations by new land cover categories, sum pixel counts, 
# get code combinations ids and calculate approximate area by pixel size
df_tdf_subreg_tran_00_10_20_labs_count <- df_tdf_subreg_tran_00_10_20_labs |> 
  group_by(subregion,y1_lab,y2_lab,y3_lab) |> summarise(count_sum=sum(count)) |> 
  mutate(alluvium=row_number(), area_ha=count_sum*25*25/10000)

write_tsv(df_tdf_subreg_tran_00_10_20_labs_count,"../03_data/outputs/df_tdf_subreg_tran_00_10_20_labs_count.tsv")  

# get areas by categories for all TDF area
df_tdf_tran_00_10_20_labs_count <- df_tdf_subreg_tran_00_10_20_labs |> 
  group_by(y1_lab,y2_lab,y3_lab) |> summarise(count_sum=sum(count)) |> 
  mutate(alluvium=row_number(), area_ha=count_sum*25*25/10000)

write_tsv(df_tdf_tran_00_10_20_labs_count,"../03_data/outputs/df_tdf_tran_00_10_20_labs_count.tsv")  

# extract value of pasturelands between 2010 to 2018 for paragraph in results----------
df_tdf_tran_10_20_pastureland <- df_tdf_tran_00_10_20_labs_count |> ungroup() |> select(y2_lab,y3_lab, area_ha) |> 
  filter(y2_lab=="Pastureland") |> group_by(y2_lab,y3_lab) |> 
  summarise(total_trans_area_ha=sum(area_ha)) |> 
  mutate(prop= total_trans_area_ha/sum(total_trans_area_ha)); df_tdf_tran_10_20_pastureland

sum(df_tdf_tran_10_20_pastureland$total_trans_area_ha)
sum(df_tdf_tran_10_20_pastureland$total_trans_area_ha[-4])
sum(df_tdf_tran_10_20_pastureland$prop[-4])

#--------------------------------------------------------------------------------------

################################################################################
# Reshape table and create sankey
################################################################################

# order categories for diagram
v_category_order <- c("Forest", "Successional forest",
                     "Pastureland","Arable land and plantations",
                     "Shrubland and grassland", "Other")
# create vector with color for categories
v_colors <- c("#5c8132","#49b68a","#964627","#c8a130","#cf78c7","lightgray")

# reshape table for alluvial structure
# df_transitions_long <- df_tdf_subreg_tran_00_10_20_labs_count |> pivot_longer(y1_lab:y3_lab,names_to="x", values_to="stratum") |> 
#   mutate(stratum=factor(stratum,levels=v_category_order),x_num=factor(x,labels=c("2000","2010","2018"))) # this one is not working even with having the same structure than with function
df_transitions_long <- ggalluvial::to_lodes_form(df_tdf_subreg_tran_00_10_20_labs_count,
                                                 axes=c("y1_lab","y2_lab","y3_lab"),
                                                 site=subregion) |> 
  mutate(stratum=factor(stratum,levels=v_category_order),x_num=factor(x,labels=c("2000","2010","2018")))

# create diagram
img_tdf_transitions_gral <- 
  ggplot(df_transitions_long,
         aes(x= x_num, y = area_ha,
             stratum=stratum,alluvium=alluvium,
             fill= stratum, label=stratum)) +
  geom_flow()+ # to aggregate flows between adjacent axes
  scale_alpha_manual(values=c(1,0.1,0.1,0.1,0.1,0.1))+#,aesthetics="color")+
  geom_stratum(width = 1/3) +
  # geom_label(stat = "stratum", aes(label = str_wrap(after_stat(stratum),width=20)),fill="white",alpha=0.9,size=2.5) +
  scale_fill_manual(values=v_colors,name="") +
  scale_x_discrete(expand = c(.07, .07))+
  theme_bw()+ ylab("Hectares") + xlab("") + theme(legend.position = "right")+#guides(fill=guide_legend(ncol=2))+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),breaks = scales::breaks_extended(n = 6));img_tdf_transitions_gral

ggsave("../05_figures/img_tdf_transitions_gral.png",img_tdf_transitions_gral,width=6,height = 2.5,dpi=600)

# create diagram by subregion
img_tdf_transitions_subr_hor <- 
  ggplot(df_transitions_long,
         aes(x= x_num, y = area_ha,
             stratum=stratum,alluvium=alluvium,
             fill= stratum, label=stratum)) +
  geom_flow()+ # to aggregate flows between adjacent axes
  scale_alpha_manual(values=c(1,0.1,0.1,0.1,0.1,0.1))+#,aesthetics="color")+
  geom_stratum(width = 1/3) +
  #geom_label(stat = "stratum", aes(label = str_wrap(after_stat(stratum),width=20)),fill="white",alpha=0.9,size=2.5) +
  scale_fill_manual(values=v_colors,name="") +
  scale_x_discrete(expand = c(.07, .07))+
  theme_bw()+ ylab("Hectares") + xlab("") + theme(legend.position = "bottom")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),breaks = scales::breaks_extended(n = 6))+
  facet_wrap(~subregion, scales="free_y");img_tdf_transitions_subr_hor

img_tdf_transitions_subr_ver <- 
  ggplot(df_transitions_long,
         aes(x= x_num, y = area_ha,
             stratum=stratum,alluvium=alluvium,
             fill= stratum, label=stratum)) +
  geom_flow()+ # to aggregate flows between adjacent axes
  scale_alpha_manual(values=c(1,0.1,0.1,0.1,0.1,0.1))+#,aesthetics="color")+
  geom_stratum(width = 1/3) +
  #geom_label(stat = "stratum", aes(label = str_wrap(after_stat(stratum),width=20)),fill="white",alpha=0.9,size=2.5) +
  scale_fill_manual(values=v_colors,name="") +
  scale_x_discrete(expand = c(.07, .07))+
  theme_bw()+ ylab("Hectares") + xlab("") + theme(legend.position = "bottom")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),breaks = scales::breaks_extended(n = 6))+
  facet_wrap(~subregion, scales="free_y",ncol=2);img_tdf_transitions_subr_ver

ggsave("../05_figures/img_tdf_transitions_subreg_hor.png",img_tdf_transitions_subr_hor,width=8,height = 6,dpi=600)
ggsave("../05_figures/img_tdf_transitions_subreg_ver.png",img_tdf_transitions_subr_ver,width=6,height = 8,dpi=600)


################################################################################
# Sum total areas by categories
################################################################################
# sum areas by region per year--------------------------------------------------
df_subreg_categs_areas_long <- df_transitions_long |> group_by(subregion,x_num,stratum) |> 
  summarise(area_ha_sum=sum(area_ha))

df_subreg_categs_areas_long_total <- df_subreg_categs_areas_long |> ungroup() |> 
  group_by(subregion,x_num) |> summarise(area_ha_sum_gral=sum(area_ha_sum))

# calculate proportions per category by year
df_subreg_categs_areas_long_prop <- df_subreg_categs_areas_long |> 
  left_join(df_subreg_categs_areas_long_total,by=c("subregion","x_num")) |> 
  mutate(prop=area_ha_sum/area_ha_sum_gral)

# convert to wide format
df_subreg_categs_areas_wide <- df_subreg_categs_areas_long |> pivot_wider(names_from=x_num,values_from=area_ha_sum)

write_tsv(df_subreg_categs_areas_long_prop,"../03_data/outputs/df_subreg_tdf_lc_categs_areas_long_prop.tsv")
write_tsv(df_subreg_categs_areas_wide,"../03_data/outputs/df_subreg_tdf_lc_categs_areas_wide.tsv")

# sum areas per year for all TDF------------------------------------------------
df_categs_areas_long <- df_transitions_long |> group_by(x_num,stratum) |> 
  summarise(area_ha_sum=sum(area_ha))

df_categs_areas_long_total <- df_categs_areas_long |> ungroup() |> 
  group_by(x_num) |> summarise(area_ha_sum_gral=sum(area_ha_sum))

# calculate proportions per category by year
df_categs_areas_long_prop <- df_categs_areas_long |> 
  left_join(df_categs_areas_long_total,by=c("x_num")) |> 
  mutate(prop=area_ha_sum/area_ha_sum_gral)

# convert to wide format
df_categs_areas_wide <- df_categs_areas_long |> pivot_wider(names_from=x_num,values_from=area_ha_sum)

write_tsv(df_categs_areas_long_prop,"../03_data/outputs/df_tdf_lc_categs_areas_long_prop.tsv")
write_tsv(df_categs_areas_wide,"../03_data/outputs/df_tdf_lc_categs_areas_wide.tsv")


################################################################################
# Forest Areas
################################################################################

# load TDF limits
area_lim1_path <- "/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IAvH/BST/riesgo_etter/EcosistOrigBsTaeEtter_dissolve.gpkg"#"./00_rawdata/Study_areas/EcosistOrigBsTaeEtter_dissolve.gpkg"#"./00_rawdata/Study_areas/Colombia_borders_gadm.gpkg"# 
sf_tdf_lim <- st_read(area_lim1_path)
# sf_tdf_lim_srs <- sf_tdf_lim |> st_transform(st_crs(9377)) # national projection - did not use it because faced some errors
sf_tdf_lim_srs2 <- sf_tdf_lim |> st_transform(st_crs(3116))

# BnB layers--------------------------------------------------------------------
library(exactextractr)
years <- c(1990,2000,2010,2020)
l_bnb <- map(years, \(x) rast(paste0("../../../data/maps/IDEAM/Cambio_Cobertura_Bosque_No_Bosque/BnB_ActualCover/cover_",x,"_25m.tif")))
r_areas <- cellSize(l_bnb[[1]],unit="ha")
l_bnb_tdf_areas <- map(l_bnb, \(x) r_areas*x)
gc(TRUE)
l_v_tdf_areas <- map_dbl(l_bnb_tdf_areas, \(x) exact_extract(x,sf_tdf_lim_srs2,fun='sum'))

# forest non forest data - areas where tree cover is at least 30%
df_tdf_tran_90_20_bnb <- data.frame(year = c(1990,2000,2010,2020), source="Forest dataset",
                                    total_area_ha = round(unlist(l_v_tdf_areas))) 

write_csv(df_tdf_tran_90_20_bnb,"/Volumes/Extreme Pro/Hikaru/Doctorado/data/maps/IDEAM/Cambio_Cobertura_Bosque_No_Bosque/BnB_ActualCover/df_tdf_tran_90_20_bnb.csv")

# Both layers--------------------------------------------------------------------
# create vector with forest categories to compare with forest non forest
forest_categ <- c("Forest","Open forest")

df_tdf_tran_00_10_20_forest_areas <- df_transitions_long |> 
  filter(stratum== "Forest" ) |> 
  group_by(x_num) |> 
  summarise(total_area_ha=sum(area_ha)) |> 
  mutate(year=as.numeric(as.character(x_num)),
         source="Corine dataset",.before = "total_area_ha",.keep="unused") |> 
  bind_rows( df_tdf_tran_90_20_bnb)

img_CLC_forest_ts <- df_tdf_tran_00_10_20_forest_areas |> 
  ggplot(aes(x=year,y=total_area_ha,linetype=source))+#,color=label))+
  geom_line()+geom_point()+ 
  geom_text(aes(label=round(total_area_ha)),vjust = 0.5,hjust=1.4,size=2,show.legend = FALSE)+
  theme_bw()+scale_x_continuous(limits = c(1988,2020),breaks = c(1990,2000,2010,2018,2020))+
  xlab("Year")+ylab("Hectares")+ labs(linetype="Data source")+#,color="Cover category")+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),breaks = scales::breaks_extended(n = 6));img_CLC_forest_ts

ggsave("../05_figures/img_CLC_forest_ts.png",dpi = 300,width=5,height = 3)
ggsave("../05_figures/img_CLC_forest_ts.svg",dpi = 300,width=5,height = 3)
