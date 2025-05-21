library(readr)
library(dplyr)
library(ggplot2)
library(purrr)
library(scico)
library(tidyr)
library(gridExtra)
library(ggpubr)
library(rphylopic)
library(viridis)
library(openxlsx)

################################################################################
# Load data and set parameters
################################################################################
# time steps
t_0 <- 2000 
t_n <- 2020 # should be larger than t_0 at least 2 years needed
time_step <- 10
v_time_steps <- seq(t_0,t_n,time_step)
v_time_steps_all <-  c(v_time_steps[1]-time_step,v_time_steps)

# outputFolder
outputFolder <- ".././05_figures"

# shs from scp10 for all CS with different mask for Omniscape outputs
df_SHS_allSp <- read_csv(".././01_repositorios/Species_Habitat_Index_Colombia/02_outdata/summary/df_SHS_allSp.csv")
df_SHS_base_allSp <- read_csv(".././01_repositorios/Species_Habitat_Index_Colombia/02_outdata/summary/df_SHS_base_allSp.csv")
df_SHS_tdf_allSp <- read_csv(".././01_repositorios/Species_Habitat_Index_Colombia/02_outdata/summary/df_SHS_tdf_allSp.csv")

# shi from scp10
df_SHI_table <- read_csv(".././01_repositorios/Species_Habitat_Index_Colombia/02_outdata/summary/df_SHI_table.csv")

# species information
df_tdf_sp_inf <- read_csv(".././03_data/outputs/1stBatch/df_tdf_sp_inf.csv")#".././01_repositorios/Species_Habitat_Index_Colombia/00_rawdata/tables/df_tdf_sp_for_omni_batch3_group.csv")
# iucn habitat codes and lables
df_iucn_habitat_categories <- read_csv2("../../../data/numeric/IUCN/IUCN_habitat_classif_scheme.csv")
# areas of habitat for each species and the stewardship
df_aoh_areas_sp <- read_csv(".././01_repositorios/Species_Habitat_Index_Colombia/02_outdata/summary/df_aoh_areas_sp.csv")
# information about species habitat from iucn
df_habitat <- read_csv(".././01_repositorios/Species_Habitat_Index_Colombia/00_rawdata/tables/df_tdf_sp_habitat_inf_filtered.csv")

################################################################################
# Define groups 
################################################################################

# # check setwardship distribution
# summary(df_aoh_areas_sp$W_stewardship)
# df_aoh_areas_groups <- df_aoh_areas_sp |> mutate(Stewardship_group=cut(W_stewardship,breaks=seq(0,1,0.01),include.lowest = T))
# 
# df_aoh_areas_groups |> count(Stewardship_group)

# join outputs from different Omniscape metric intergration options but avoid repeating GISFrag
df_SHS_metrics0 <- df_SHS_allSp |> rename(version=Version) |> 
  bind_rows(df_SHS_base_allSp |> rename(version=Version) |> filter(!grepl("GISFrag",version))) |> 
  bind_rows(df_SHS_tdf_allSp |> rename(version=Version) |> filter(!grepl("GISFrag",version)))

rm(df_SHS_allSp,df_SHS_base_allSp,df_SHS_tdf_allSp)

# check versions for baseline and connectivity metric
df_versions <- df_SHS_metrics0 |> count(version)

# get standard deviation of the Area Score
AS_sd <- df_SHS_metrics0 |> filter(Score=="AS") |> summarise(AS_sd=sd(Values),Q99=quantile(Values,0.99))

# remove outliers in species changes
v_sp_outliers <- df_SHS_metrics0 |> filter(Score=="AS", Values>(AS_sd$AS_sd*2)) |> count(sci_name) |> pull(sci_name)

# re-define df_SHS_allSp by removing sp_outliers and create base (year) and method columns
df_SHS_metrics <- df_SHS_metrics0 |> filter(!sci_name %in% v_sp_outliers) |> 
  tidyr::separate(version,c("base","method"),sep="_year_",remove = F)

rm(df_SHS_metrics0)

# define total species
v_total_sp_names <- unique(df_SHS_metrics$sci_name)

#-------------------------------------------------------------------------------
# IUCN category
#-------------------------------------------------------------------------------


unique(df_tdf_sp_inf$category)

# join table with SHS values by species with species info from IUCN
df_SHS_metrics_info <- df_SHS_metrics |> left_join(df_tdf_sp_inf,by="sci_name") |> 
  mutate(IUCN_category=case_when(threatMix=="LC" ~ "Least Concern",
                                 threatMix=="DD" ~ "Data Deficient",
                                 TRUE ~ "Some risk"))

df_SHS_metrics_info |> count(version,Score,Year,category)

# Get list of species by IUCN category group by Score and Method
df_SHS_IUCN_categ <- df_SHS_metrics_info |> count(version,Score,Year,IUCN_category);df_SHS_IUCN_categ
# Get total IUCN categories
IUCN_categ <- unique(df_SHS_IUCN_categ$IUCN_category)

# join area info to measure Steward SHI from all species in SHS
df_SHS_metrics_info_areas <- df_SHS_metrics_info |> filter(Score=="SHS") |> 
  left_join(df_aoh_areas_sp,by="sci_name",relationship="many-to-one")

glimpse(df_SHS_metrics_info_areas)

#-------------------------------------------------------------------------------
# Taxonomic group
#-------------------------------------------------------------------------------
# create new column with taxonomic group names
df_SHS_metrics_info_areas <- df_SHS_metrics_info_areas |> 
  mutate(tax_group=case_when(kingdom=="PLANTAE" ~ "Plants",
                             class  =="AVES"  ~ "Birds",
                             class  =="MAMMALIA" ~ "Mammals"))


df_SHS_metrics_info_areas |> count(IUCN_category,tax_group)

#-------------------------------------------------------------------------------
# Habitat 
#-------------------------------------------------------------------------------
# remove code number and dot to habitat category name
df_iucn_habitat_categories <- df_iucn_habitat_categories |> 
  mutate(n1_lab = gsub("^\\d*\\S? ","",n1_name))

df_iucn_habitat_categories |> count(n1_lab)

# create list with habitat categories from IUCN and regroup to natural and artificial----
df_iucn_habitat_cat_code <- df_iucn_habitat_categories |> 
  count(n1_lab,n1) |>
  mutate(n1_lab2=case_when(grepl("Artificial - ",n1_lab) ~ "Artificial",
                           grepl("Unknown" , n1_lab) | grepl("Other" , n1_lab) ~ "Other",
                           grepl("Introduced Vegetation",n1_lab) ~ "Other",
                           TRUE ~ "Natural")) |> 
  dplyr::select(-n)

# check classification
df_iucn_habitat_cat_code |> count(n1_lab2)

# now define general habitat category according to group of categories included for the species----
df_habitat_suit_single <- df_habitat |> filter(suitability=="Suitable") |> # filter to suitable habitats to avoid marginal areas
  # get cover equivalences from n1_lab1 to grouped categories for natural, artificial and introduced vegetation
  left_join(df_iucn_habitat_cat_code, by="n1") |> 
  # reduce categories to new groups
  group_by(sci_name) |> count(n1_lab2) |> 
  # get grouped land cover prescence  
  tidyr::pivot_wider(names_from="n1_lab2",values_from="n",values_fill=0) |> 
  mutate(Hab_comb=case_when(Natural>0 & Artificial==0 & Other==0 ~ "Natural", # exclusively natural
                            Artificial >0 ~ "Artificial", # Includes the artificial category
                            TRUE ~ "Other"))

df_habitat |> count(sci_name) |> count()
df_habitat_suit_single |> ungroup() |> count(Hab_comb)

# join table with SHS values by species with habitat info
df_SHS_Sp_Info_Habitat <- df_SHS_metrics_info_areas |>
  left_join(df_habitat_suit_single |> select(Hab_comb),by="sci_name", relationship="many-to-one")

#join to general species info
df_tdf_sp_inf_hab <- df_tdf_sp_inf |> 
  mutate(IUCN_category=case_when(threatMix=="LC" ~ "Least Concern",
                                 threatMix=="DD" ~ "Data Deficient",
                                 TRUE ~ "Some risk")) |>
  left_join(df_habitat_suit_single |> select(Hab_comb),by="sci_name", relationship="many-to-one")

# # join with table with labels for IUCN categories
# df_SHS_Sp_Info_Habitat_label <- df_SHS_Sp_Info_Habitat |> 
#   left_join(df_iucn_habitat_cat_code, by="n1") 

# # check species that include forest in their suitable habitat
# df_sp_hab_include_forest <- df_SHS_Sp_Info_Habitat |> filter(Hab_comb=="Natural") |> count(sci_name) # 686
# # species that do not include forest
# df_sp_hab_noinclu_forest <- df_SHS_metrics |> filter(!sci_name %in% df_sp_hab_include_forest$sci_name) |> count(sci_name)

df_SHS_Sp_Info_Habitat |> ungroup() |> count(Hab_comb)

# check species with NAs in Habitat label
df_SHS_Sp_Info_Habitat |> filter(is.na(Hab_comb)) |> count(sci_name) # Croton hibiscifolius only has one marginal habitat

glimpse(df_SHS_Sp_Info_Habitat)
glimpse(df_tdf_sp_inf_hab)

write.csv(df_SHS_Sp_Info_Habitat,".././03_data/outputs/df_SHS_Sp_Info_Habitat.csv")
write.csv(df_tdf_sp_inf_hab,".././03_data/outputs/df_tdf_sp_inf_hab.csv")
write.xlsx(df_tdf_sp_inf_hab,".././03_data/outputs/df_tdf_sp_inf_hab.xlsx")

df_tdf_sp_inf_hab |> count(IUCN_category,Hab_comb)
df_tdf_sp_inf_hab |> count(class,Hab_comb)
df_tdf_sp_inf_hab |> count(class,IUCN_category,Hab_comb)

################################################################################
# Histogram with SHS by species
################################################################################
#-------------------------------------------------------------------------------
# extract data for SHS with GISFrag by 2020
#-------------------------------------------------------------------------------
df_SHS_metrics_info_areas_plot <- df_SHS_Sp_Info_Habitat |> 
  filter(Score=="SHS", method=="GISFrag",Year=="2020",base=="t0Base") |> 
  arrange(desc(Values))

# reorder species names by index values
df_SHS_metrics_info_areas_plot$sci_name <- factor(df_SHS_metrics_info_areas_plot$sci_name,
                                                  levels=df_SHS_metrics_info_areas_plot$sci_name)

df_colors <- rev(viridis(3)) ; names(df_colors) <- IUCN_categ

# birds ------------------------------------------------------------------------
bird <- get_uuid(name = "Ortalis")

img_bird_scores_nat <- ggplot(df_SHS_metrics_info_areas_plot |> 
                                filter(tax_group=="Birds" & Hab_comb=="Natural"), 
                          aes(x=sci_name,y=Values,fill=IUCN_category)) +
  geom_bar(stat="identity")+ xlab("") + scale_y_continuous("Species Habitat Score",limits=c(0,190))+
  geom_hline(yintercept = 100, linetype="dotted", col="gray")+ ggtitle("Natural")+
  theme_classic()+coord_flip()+scale_fill_manual(values=df_colors)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5,hjust=1,size=4,face = "italic"),
        plot.title = element_text(size=10), legend.position = "none")+
  add_phylopic(uuid = bird,x=42,y=170,ysize=15)

img_bird_scores_art <- ggplot(df_SHS_metrics_info_areas_plot |> 
                                filter(tax_group=="Birds" & Hab_comb=="Artificial"), 
                              aes(x=sci_name,y=Values,fill=IUCN_category)) +
  geom_bar(stat="identity")+ xlab("") + scale_y_continuous("Species Habitat Score",limits=c(0,190))+
  geom_hline(yintercept = 100, linetype="dotted", col="gray")+ ggtitle("Artificial")+
  theme_classic()+coord_flip()+scale_fill_manual(values=df_colors)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5,hjust=1,size=4,face = "italic"),
        plot.title = element_text(size=10),
        legend.position = "none")+
  add_phylopic(uuid = bird,x=180,y=170,ysize=15)

img_bird_scores <- ggarrange(img_bird_scores_art,img_bird_scores_nat,
                             nrow=2, heights = c(3.5,1))

# mammals ----------------------------------------------------------------------
mamm <- get_uuid(name = "Saguinus",n = 3)[[3]]

img_mamm_scores_nat <- ggplot(df_SHS_metrics_info_areas_plot |> 
                                filter(tax_group=="Mammals" & Hab_comb=="Natural"), 
                          aes(x=sci_name,y=Values,fill=IUCN_category)) +
  geom_bar(stat="identity")+ xlab("") + scale_y_continuous("Species Habitat Score",limits=c(0,190))+
  geom_hline(yintercept = 100, linetype="dotted", col="gray")+ ggtitle("Natural")+
  theme_classic()+coord_flip()+scale_fill_manual(values=df_colors)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5,hjust=1,size=4,face = "italic"),
        plot.title = element_text(size=10),
        legend.position = "none")+
  add_phylopic(uuid = mamm,x=43,y=170,ysize=15)

img_mamm_scores_art <- ggplot(df_SHS_metrics_info_areas_plot |> 
                                filter(tax_group=="Mammals" & Hab_comb=="Artificial"), 
                              aes(x=sci_name,y=Values,fill=IUCN_category)) +
  geom_bar(stat="identity")+ xlab("") + scale_y_continuous("Species Habitat Score",limits=c(0,190))+
  geom_hline(yintercept = 100, linetype="dotted", col="gray")+ ggtitle("Artificial")+
  theme_classic()+coord_flip()+scale_fill_manual(values=df_colors)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5,hjust=1,size=4,face = "italic"),
        plot.title = element_text(size=10),
        legend.position = "none")+
  add_phylopic(uuid = mamm,x=11,y=170,ysize=15)

img_mamm_scores <- ggarrange(img_mamm_scores_art,img_mamm_scores_nat,nrow=2, heights = c(1.5,3))

# plants -----------------------------------------------------------------------
plan <- get_uuid(name = "Piper")
plot(get_phylopic(uuid=plan))

img_plan_scores_nat <- ggplot(df_SHS_metrics_info_areas_plot |> 
                                filter(tax_group=="Plants" & Hab_comb=="Natural"), 
                          aes(x=sci_name,y=Values,fill=IUCN_category)) +
  geom_bar(stat="identity",alpha=0.8)+ xlab("") + scale_y_continuous("Species Habitat Score",limits=c(0,190))+
  geom_hline(yintercept = 100, linetype="dotted", col="gray")+ ggtitle("Natural")+
  theme_classic()+coord_flip()+scale_fill_manual(values=df_colors)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5,hjust=1,size=4,face = "italic"),
        plot.title = element_text(size=10))+
  add_phylopic(uuid = plan,x=400,y=170,ysize=15)

img_plan_scores_art <- ggplot(df_SHS_metrics_info_areas_plot |> 
                                filter(tax_group=="Plants" & Hab_comb=="Artificial"), 
                              aes(x=sci_name,y=Values,fill=IUCN_category)) +
  geom_bar(stat="identity",alpha=0.8)+ xlab("") + scale_y_continuous("Species Habitat Score",limits=c(0,190))+
  geom_hline(yintercept = 100, linetype="dotted", col="gray")+ ggtitle("Artificial")+
  theme_classic()+coord_flip()+scale_fill_manual(values=df_colors)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5,hjust=1,size=4,face = "italic"),
        plot.title = element_text(size=10),
        legend.position = "none")+
  add_phylopic(uuid = plan,x=30,y=170,ysize=15)

img_plan_scores <- ggarrange(img_plan_scores_art,img_plan_scores_nat,nrow=2, heights = c(1,3))

# all groups -------------------------------------------------------------------
png(".././05_figures/img_species_scores.png",width = 28,height = 50,units = "cm",res=600)
ggarrange(ggarrange(img_bird_scores,img_mamm_scores,img_plan_scores_art,heights=c(5,2,1),ncol=1),
          img_plan_scores_nat,
          ncol=2,widths = c(1,1.5))
dev.off()


#-------------------------------------------------------------------------------
# extract data for SHS with Omsnicape by 2020
#-------------------------------------------------------------------------------
df_SHS_omnisc_info_areas_plot <- df_SHS_Sp_Info_Habitat |> 
  filter(Score=="SHS", method=="Omniscape",Year=="2020",base=="t0Base") |> 
  arrange(desc(Values))

# reorder species names by index values
df_SHS_omnisc_info_areas_plot$sci_name <- factor(df_SHS_omnisc_info_areas_plot$sci_name,
                                                  levels=df_SHS_omnisc_info_areas_plot$sci_name)

# birds ------------------------------------------------------------------------
bird <- get_uuid(name = "Ortalis")

img_bird_scores_nat2 <- ggplot(df_SHS_omnisc_info_areas_plot |> 
                                filter(tax_group=="Birds" & Hab_comb=="Natural"), 
                              aes(x=sci_name,y=Values,fill=IUCN_category)) +
  geom_bar(stat="identity")+ xlab("") + scale_y_continuous("Species Habitat Score",limits=c(0,190))+
  geom_hline(yintercept = 100, linetype="dotted", col="gray")+ ggtitle("Natural")+
  theme_classic()+coord_flip()+scale_fill_manual(values=df_colors)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5,hjust=1,size=4,face = "italic"),
        plot.title = element_text(size=10), legend.position = "none")+
  add_phylopic(uuid = bird,x=42,y=170,ysize=15)

img_bird_scores_art2 <- ggplot(df_SHS_omnisc_info_areas_plot |> 
                                filter(tax_group=="Birds" & Hab_comb=="Artificial"), 
                              aes(x=sci_name,y=Values,fill=IUCN_category)) +
  geom_bar(stat="identity")+ xlab("") + scale_y_continuous("Species Habitat Score",limits=c(0,190))+
  geom_hline(yintercept = 100, linetype="dotted", col="gray")+ ggtitle("Artificial")+
  theme_classic()+coord_flip()+scale_fill_manual(values=df_colors)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5,hjust=1,size=4,face = "italic"),
        plot.title = element_text(size=10),
        legend.position = "none")+
  add_phylopic(uuid = bird,x=180,y=170,ysize=15)

img_bird_scores2 <- ggarrange(img_bird_scores_art2,img_bird_scores_nat2,
                             nrow=2, heights = c(3.5,1))

# mammals ----------------------------------------------------------------------
mamm <- get_uuid(name = "Saguinus",n = 3)[[3]]

img_mamm_scores_nat2 <- ggplot(df_SHS_omnisc_info_areas_plot |> 
                                filter(tax_group=="Mammals" & Hab_comb=="Natural"), 
                              aes(x=sci_name,y=Values,fill=IUCN_category)) +
  geom_bar(stat="identity")+ xlab("") + scale_y_continuous("Species Habitat Score",limits=c(0,190))+
  geom_hline(yintercept = 100, linetype="dotted", col="gray")+ ggtitle("Natural")+
  theme_classic()+coord_flip()+scale_fill_manual(values=df_colors)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5,hjust=1,size=4,face = "italic"),
        plot.title = element_text(size=10),
        legend.position = "none")+
  add_phylopic(uuid = mamm,x=43,y=170,ysize=15)

img_mamm_scores_art2 <- ggplot(df_SHS_omnisc_info_areas_plot |> 
                                filter(tax_group=="Mammals" & Hab_comb=="Artificial"), 
                              aes(x=sci_name,y=Values,fill=IUCN_category)) +
  geom_bar(stat="identity")+ xlab("") + scale_y_continuous("Species Habitat Score",limits=c(0,190))+
  geom_hline(yintercept = 100, linetype="dotted", col="gray")+ ggtitle("Artificial")+
  theme_classic()+coord_flip()+scale_fill_manual(values=df_colors)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5,hjust=1,size=4,face = "italic"),
        plot.title = element_text(size=10),
        legend.position = "none")+
  add_phylopic(uuid = mamm,x=11,y=170,ysize=15)

img_mamm_scores2 <- ggarrange(img_mamm_scores_art2,img_mamm_scores_nat2,nrow=2, heights = c(1.5,3))

# plants -----------------------------------------------------------------------
plan <- get_uuid(name = "Piper")
plot(get_phylopic(uuid=plan))

img_plan_scores_nat2 <- ggplot(df_SHS_omnisc_info_areas_plot |> 
                                filter(tax_group=="Plants" & Hab_comb=="Natural"), 
                              aes(x=sci_name,y=Values,fill=IUCN_category)) +
  geom_bar(stat="identity",alpha=0.8)+ xlab("") + scale_y_continuous("Species Habitat Score",limits=c(0,190))+
  geom_hline(yintercept = 100, linetype="dotted", col="gray")+ ggtitle("Natural")+
  theme_classic()+coord_flip()+scale_fill_manual(values=df_colors)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5,hjust=1,size=4,face = "italic"),
        plot.title = element_text(size=10))+
  add_phylopic(uuid = plan,x=400,y=170,ysize=15)

img_plan_scores_art2 <- ggplot(df_SHS_omnisc_info_areas_plot |> 
                                filter(tax_group=="Plants" & Hab_comb=="Artificial"), 
                              aes(x=sci_name,y=Values,fill=IUCN_category)) +
  geom_bar(stat="identity",alpha=0.8)+ xlab("") + scale_y_continuous("Species Habitat Score",limits=c(0,190))+
  geom_hline(yintercept = 100, linetype="dotted", col="gray")+ ggtitle("Artificial")+
  theme_classic()+coord_flip()+scale_fill_manual(values=df_colors)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5,hjust=1,size=4,face = "italic"),
        plot.title = element_text(size=10),
        legend.position = "none")+
  add_phylopic(uuid = plan,x=30,y=170,ysize=15)

img_plan_scores2 <- ggarrange(img_plan_scores_art2,img_plan_scores_nat2,nrow=2, heights = c(1,3))

# all groups -------------------------------------------------------------------
png(".././05_figures/img_species_scores_omnisc.png",width = 28,height = 50,units = "cm",res=600)
ggarrange(ggarrange(img_bird_scores2,img_mamm_scores2,img_plan_scores_art2,heights=c(5,2,1),ncol=1),
          img_plan_scores_nat2,
          ncol=2,widths = c(1,1.5))
dev.off()

################################################################################
# Sub-Scores
################################################################################

df_scores_methods <- df_SHS_metrics_info |> 
  mutate(scores=interaction(Score,method),
         scores2=if_else(grepl("AS",scores),"AS",scores)) |> 
  filter(Score!="SHS" & !(Score=="AS" & grepl("Omniscape",method)))

df_scores_methods |> count(scores,scores2)

img_SubScores <- df_scores_methods |> 
  ggplot(aes(x=Year,y=Values,group=interaction(sci_name,version,scores2)))+#,colour=Score)) + 
  geom_line(alpha=0.1) + theme_classic() + 
  facet_grid(scores2~base, 
             labeller=labeller(base=c(HypBase="Hypothetical base year",t0Base="2000 base year"),
                               scores2=c(AS="AS",
                               CS.GISFrag="GISFrag",CS.Omniscape="Omniscape (Suit.)",
                               CS.Omniscape_base="Omniscape (Base)",
                               CS.Omniscape_tdf="Omniscape (TDF)"))) +
  ylab("Score (%)")+theme_bw()+theme(legend.position="bottom")+ 
  # scale_x_continuous(labels=c(expression(B[0]),v_time_steps),breaks=v_time_steps_all)+ 
  coord_cartesian(ylim=c(0,110),xlim=c(2000,2020))+ scale_y_continuous(breaks=seq(0,125,25));img_SubScores

ggsave(".././05_figures/img_SubScores.png", img_SubScores ,dpi = 300,width=8,height = 7)

col_scores <- c(scico(3,palette="devon")[2],scico(4,palette="roma"))

img_SubScores_boxplots <- df_scores_methods |> 
  ggplot(aes(x=factor(Year),y=Values,group=interaction(Year,version,scores2),color=scores2)) + 
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = scores2, color = scores2),
    position = position_dodge(width = 0.7),linetype="dashed")+
  geom_boxplot(position = position_dodge(width = 0.7),width=0.6,alpha=0.8) + 
  theme_classic() + 
  facet_grid(base~., 
             labeller=labeller(base=c(HypBase="Hypothetical base year",t0Base="2000 base year"))) +
  ylab("Score (%)")+xlab("Year")+theme_bw()+
  theme(legend.position="bottom")+ 
  scale_color_manual(name="",labels=c("AS","GISFrag","Omniscape (Suit.)","Omniscape (Base)","Omniscape (All TDF)"),values = col_scores)+
  coord_cartesian(ylim=c(0,150))+ scale_x_discrete(breaks=c("2000","2010","2020"))+
  scale_y_continuous(breaks=seq(0,150,25));img_SubScores_boxplots

ggsave("../05_figures/img_SubScores_boxplots.png",img_SubScores_boxplots,width = 6,height = 7)
