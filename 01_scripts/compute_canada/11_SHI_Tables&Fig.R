library(dplyr)
library(purrr)
library(ggplot2)
library(sf)
library(tmap)
library(rredlist)

#--------------------------------------------------------------------------------
# Info especies
df_sp_iucn_biomodelos <- read_csv("./tables/df_sp_iucn_biomodelos.csv")
df_habitat <- read_csv("./tables/df_habitat.csv")
df_aoh_areas_sp <- read_csv("./outputs/species/df_aoh_areas_sp.csv")
sf_study_area <- st_read("/run/media/miap/C084AA1284AA0ACC/Hikaru/Doctorado/data/maps/IAvH/BST/riesgo_etter/EcosistOrigBsTaeEtter_dissolve.gpkg")
df_sp_all_info <- read_csv("./tables/df_sp_all_info.csv")

# general
df_sp <- read_csv("./tables/sp_forSHI.csv")
df_sp |> group_by(class) |> summarise(n=n())
df_sp |> group_by(category) |> summarise(n=n())

#-------------------------------pruebas------------------------------------------

sf_range_maps_colombia <- st_read("/run/media/miap/C084AA1284AA0ACC/Hikaru/Doctorado/data/maps/IUCN/RangeMapsColombia/Range_maps_BST_Col_biomodelos.gpkg")
sel_range_map <- sf_BST_rangemaps_biomodelos |> dplyr::filter(sci_name=="Andinobates dorisswansonae") # load range map species i

sel_range_map

class(st_geometry(sel_range_map))[1]

sel_range_map<-st_cast(sel_range_map, "GEOMETRYCOLLECTION") %>%
  st_collection_extract("LINESTRING") |> st_cast("POLYGON") |> st_cast("MULTIPOLYGON")

plot(sel_range_map)

osm <- read_osm(sel_range_map, ext=1.1)

tm_shape(osm) + tm_rgb()+ 
  tm_shape(sel_range_map)+tm_borders()+
  tm_shape(sf_study_area)+tm_polygons(col="green")

plot(sel_range_map)

rm(sf_range_maps_colombia)

# count removed
read_csv()
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# t0 as baseline
SHS_table_t0_baseline <- read_csv("./outputs/species/SHS_table_t0_baseline.csv")

SHS_table_t0_baseline <- SHS_table_t0_baseline |> filter(!is.na(Values))

#-------------------
# species information
included_sp <- unique(SHS_table_t0_baseline$sci_name)

l_sp_info <- purrr::map(included_sp, ~rl_search(.x,key=token)$result)
df_sp_info <- l_sp_info |> setNames(included_sp) |> bind_rows(.id="sci_name")

l_habitat <- purrr::map(included_sp, ~rl_habitats(.x,key=token)$result )
df_habitat <- l_habitat |> setNames(included_sp) |> bind_rows(.id="sci_name")
df_habitat <- df_habitat |> tidyr::separate(col=code,into=c("n1","n2"),sep="\\.",remove=F)

df_sp_all_info <- df_sp_info |> left_join(df_habitat, by="sci_name")

# summaries
# class
df_sp_info |> group_by(class) |> summarise(n=n())

# Categorías IUCN
df_sp_info |> group_by(category) |> summarise(n=n())

# habitat
df_habitat_list <- df_sp_all_info |> filter(suitability=="Suitable") |> distinct(sci_name,n1) |> group_by(n1) |> summarise(n=n())

df_habitat_category <- df_sp_all_info  |> distinct(sci_name,category,n1) |> group_by(category,n1)  |> summarise(n=n())

#---------------------

df_SHS_t0_baseline <- SHS_table_t0_baseline |> left_join(df_sp_info,by="sci_name")

df_SHS_t0_baseline <- df_SHS_t0_baseline |> mutate(IUCN_category=if_else(category=="LC","Least Concernt","Some Risk Level"))

#Categories
# imgRiskCat_1990base <- ggplot(df_SHS_t0_baseline |> filter(Score=="SHS" & category!="LC"),aes(x=Year,y=Values,group=sci_name)) + geom_line(alpha=0.5) + theme_classic() +
#   ylab("SHS (%)") + coord_cartesian(ylim=c(0,200))  + coord_cartesian(ylim=c(0,200)) + 
#   ggtitle("SHS for species under some risk level - 1990 as baseline")
# 
# ggsave("./outputs/img_RiskCat_1990_baseline.png",imgRiskCat_1990base,width=5,height = 3)
# 
# imgLC_1990base <- ggplot(df_SHS_t0_baseline |> filter(Score=="SHS" & category=="LC"),aes(x=Year,y=Values,group=sci_name)) + geom_line(alpha=0.5) + theme_classic() +
#   ylab("SHS (%)") + coord_cartesian(ylim=c(0,200))   + coord_cartesian(ylim=c(0,200)) + 
#   ggtitle("SHS for least concern species - 1990 as baseline")
# 
# ggsave("./outputs/img_LCCat_1990_baseline.png",imgLC_1990base,width=5,height = 3)

imgCateg_1990base <- ggplot(df_SHS_t0_baseline |> filter(Score=="SHS"),aes(x=Year,y=Values,group=sci_name,linetype=IUCN_category)) + geom_line(alpha=0.5) + theme_classic() +
  ylab("SHS (%)") + coord_cartesian(ylim=c(0,200)) + coord_cartesian(ylim=c(0,200)) + 
  ggtitle("SHS by IUCN category - 1990 as baseline")

ggsave("./outputs/imgCateg_1990base.png",imgCateg_1990base,width=5,height = 3)

#Scores
# imgAS_1990base <- ggplot(SHS_table_t0_baseline |> filter(Score=="AS"),aes(x=Year,y=Values,group=sci_name)) + geom_line() + theme_classic(alpha=0.5) +
#   scale_color_discrete(guide="none") + ylab("AS (%)") + coord_cartesian(ylim=c(0,200))+ 
#   ggtitle("Area Score - 1990 as baseline")
# 
# ggsave("./outputs/img_AS_1990_baseline.png",imgAS_1990base,width=5,height = 3)
# 
# imgCS_1990base <- ggplot(SHS_table_t0_baseline |> filter(Score=="CS"),aes(x=Year,y=Values,group=sci_name)) + geom_line() + theme_classic(alpha=0.5) +
#   scale_color_discrete(guide="none") + ylab("CS (%)") + coord_cartesian(ylim=c(0,200)) + 
#   ggtitle("Connectivity Score - 1990 as baseline")
# 
# ggsave("./outputs/img_CS_1990_baseline.png",imgCS_1990base,width=5,height = 3)

imgScores_1990base <- ggplot(SHS_table_t0_baseline |> filter(Score!="SHS"),aes(x=Year,y=Values,group=interaction(sci_name,Score),col=Score)) + geom_line() + theme_classic() +
  ylab("AS, CS (%)") + coord_cartesian(ylim=c(0,200)) +  scale_color_manual(values=c("#ffaa39ff","#d389d5ff")) +
  ggtitle("Scores - 1990 as baseline")

ggsave("./outputs/img_Scores_1990_baseline.png",imgScores_1990base,width=5,height = 3)

#SHS gral
imgSHS_1990base <- ggplot(SHS_table_t0_baseline |> filter(Score=="SHS"),aes(x=Year,y=Values,col=sci_name)) + geom_line(alpha=0.5) + theme_classic() +
  scale_color_discrete(guide="none") + ylab("SHS (%)") + coord_cartesian(ylim=c(0,100)) + 
  ggtitle("Species Habitat Score - 1990 as baseline")

ggsave("./outputs/img_SHS_1990_baseline.png",imgSHS_1990base,width=5,height = 3)


# shi
df_SHS1_aoh_areas_sp <- df_SHS_t0_baseline |> filter(Score=="SHS") |> left_join(df_aoh_areas_sp,by="sci_name",relationship="many-to-one")
print(df_SHS1_aoh_areas_sp)

df_SHI1_t0 <- df_SHS1_aoh_areas_sp |> group_by(Year) |> summarise(SHI=mean(Values,na.rm=T),Steward_SHI= weighted.mean(Values,W_stewardship,na.rm=T))
print(df_SHI1_t0)
write_csv(df_SHI1_0,file= "./outputs/SHI_table_t0_baseline.csv")

imgSHI_1990base <- ggplot(df_SHI1_t0,aes(x=Year,y=SHI)) + geom_line(alpha=0.5) + theme_classic() +
  scale_color_discrete(guide="none") + ylab("SHI (%)") + coord_cartesian(ylim=c(0,200)) + 
  ggtitle("Species Habitat Index - 1990 as baseline")

ggsave("./outputs/img_SHS_1990_baseline.png",imgSHS_1990base,width=5,height = 3)


#-------------------------------------------------------------------------------
# aoh as baseline
#-------------------------------------------------------------------------------

SHS_table_aoh_baseline <- read_csv("./outputs/species/SHS_table_aoh_baseline.csv")

SHS_table_aoh_baseline <- SHS_table_aoh_baseline |> filter(!is.na(Values))

df_SHS_aoh_baseline <- SHS_table_aoh_baseline |> left_join(df_sp_info,by="sci_name")

df_SHS_aoh_baseline <- df_SHS_aoh_baseline |> mutate(IUCN_category=if_else(category=="LC","Least Concernt","Some Risk Level"))

df_group_class <- df_SHS_aoh_baseline |> group_by(category) |> distinct(sci_name) |> summarise(n=n())

SHS_table_aoh_baseline |> group_by(Score,result) |> summarise(n=n())

# Categories----------------------------------
# imgRiskCat_aohbase <- ggplot(df_SHS_aoh_baseline |> filter(Score=="SHS" & category!="LC"),aes(x=Year,y=Values,group=sci_name)) + geom_line(alpha=0.5) + theme_classic() +
#   ylab("SHS (%)") + coord_cartesian(ylim=c(0,200)) + coord_cartesian(ylim=c(0,200)) + 
#   ggtitle("SHS for species under some risk level - AOH as baseline")
# 
# ggsave("./outputs/img_RiskCat_AOH_baseline.png",imgRiskCat_aohbase ,width=5,height = 3)
# 
# imgLC_aohbase <- ggplot(df_SHS_aoh_baseline |> filter(Score=="SHS" & category=="LC"),aes(x=Year,y=Values,group=sci_name)) + geom_line(alpha=0.5) + theme_classic() +
#   ylab("SHS (%)") + coord_cartesian(ylim=c(0,200)) + coord_cartesian(ylim=c(0,200)) + 
#   ggtitle("SHS for least concern species - AOH as baseline")
# 
# ggsave("./outputs/img_LCCat_AOH_baseline.png",imgLC_aohbase,width=5,height = 3)


imgCateg_aohbase <- ggplot(df_SHS_aoh_baseline |> filter(Score=="SHS"),aes(x=Year,y=Values,group=sci_name,linetype=IUCN_category)) + geom_line(alpha=0.5) + theme_classic() +
  ylab("SHS (%)") + coord_cartesian(ylim=c(0,200)) + coord_cartesian(ylim=c(0,200)) + 
  ggtitle("SHS by IUCN category - AOH as baseline")

ggsave("./outputs/imgCateg_aohbase.png",imgCateg_aohbase,width=5,height = 3)


#---------------------------------------------
imgAS_aohbase <- ggplot(SHS_table_aoh_baseline |> filter(Score=="AS"),aes(x=Year,y=Values,group=sci_name)) + geom_line(alpha=0.5) + theme_classic() +
  scale_color_discrete(guide="none") + ylab("AS (%)") + coord_cartesian(ylim=c(0,200)) + 
  ggtitle("Area Score - AOH as baseline")

ggsave("./outputs/img_AS_AOH_baseline.png",imgAS_aohbase,width=5,height = 3)

imgCS_aohbase <- ggplot(SHS_table_aoh_baseline |> filter(Score=="CS"),aes(x=Year,y=Values,group=sci_name)) + geom_line(alpha=0.5) + theme_classic() +
  scale_color_discrete(guide="none") + ylab("CS (%)") + coord_cartesian(ylim=c(0,200)) + 
  ggtitle("Connectivity Score - AOH as baseline")

ggsave("./outputs/img_CS_AOH_baseline.png",imgCS_aohbase,width=5,height = 3)

imgSHS_aohbase <- ggplot(SHS_table_aoh_baseline |> filter(Score=="SHS"),aes(x=Year,y=Values,col=sci_name)) + geom_line(alpha=0.5) + theme_classic() +
  scale_color_discrete(guide="none") + ylab("SHS (%)") + coord_cartesian(ylim=c(0,100)) + 
  ggtitle("Species Habitat Score - AOH as baseline")#labs(title="Species Habitat Score",subtitle="AOH as baseline")

ggsave("./outputs/img_SHS_AOH_baseline.png",imgSHS_aohbase,width=5,height = 3)

# shi
df_SHS1_aoh_areas_sp <- df_SHS_aoh_baseline |> filter(Score=="SHS") |> left_join(df_aoh_areas_sp,by="sci_name",relationship="many-to-one")
print(df_SHS1_aoh_areas_sp)

df_SHI1_aoh <- df_SHS1_aoh_areas_sp |> group_by(Year) |> summarise(SHI=mean(Values,na.rm=T),Steward_SHI= weighted.mean(Values,W_stewardship,na.rm=T))
print(df_SHI1_aoh)
write_csv(df_SHI1_aoh,file= "./outputs/SHI_table_aoh_baseline.csv")



