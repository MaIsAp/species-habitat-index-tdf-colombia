# This script depends of outputs from scpSHIReadOutputs.R and produces the figures
# with the confidence intervals for the complete SHI and by groups

library(readr)
library(dplyr)
library(ggplot2)
library(purrr)
library(scico)
library(tidyr)
library(gridExtra)
library(ggpubr)

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
outputFolder <- "../05_figures"

# get species info dataframe
df_SHS_Sp_Info_Habitat <- read.csv("../03_data/outputs/df_SHS_Sp_Info_Habitat.csv")

#---------------------------------------------------------------
# define total species
v_total_sp_names <- unique(df_SHS_Sp_Info_Habitat$sci_name)

# check versions for baseline and connectivity metric
df_versions <- df_SHS_Sp_Info_Habitat |> count(version)

# Get total IUCN categories
IUCN_categ <- unique(df_SHS_Sp_Info_Habitat$IUCN_category)

################################################################################
# SHI - Bootstraping
################################################################################

# function to calculate SHI
get_shi <- as_mapper(function(shs){
  shs |> 
    group_by(version,Year) |> 
    summarise(SHI=mean(Values,na.rm=T),Steward_SHI= weighted.mean(Values,W_stewardship,na.rm=T))
})

#-------------------------------------------------------------------------------
# General Scores
#-------------------------------------------------------------------------------

# set base values --------------------------------------------------------------
df_SHI <- get_shi(df_SHS_Sp_Info_Habitat)

# bootstraping
samples <- 100

# function to sample species from SHS
sample_shs <- as_mapper(function(x,data,v_sp){
  set.seed(x)
  sample_sp_list <- sample(v_sp,length(v_sp),replace=T)
  df_SHS_sample <- data |> filter(sci_name %in% sample_sp_list)
  cat("Sample #:",x,"\n")
  return(list(sample_sp_list,df_SHS_sample))
})

# get samples from SHS----------------------------------------------------------
l_shs_samples <- map(1:samples,~sample_shs(.x,data=df_SHS_Sp_Info_Habitat,v_sp=v_total_sp_names)) |> 
  setNames(1:samples)

# convert to a df with group of sp by each iteration with information from df_SHS_Sp_Info_Habitat
l_df_shs_samples <- map(l_shs_samples,2)
df_shs_samples <- bind_rows(l_df_shs_samples,.id="sample_num")
write_csv(df_shs_samples,".././03_data/outputs/1stBatch/df_shs_samples_bootstraping.csv")

# get shi value for each sample
l_shi_samples <- map(l_shs_samples,2) |> map(~get_shi(.x))
df_shi_samples <- l_shi_samples |> bind_rows(.id="sample_num")

write_csv(df_shi_samples,".././03_data/outputs/1stBatch/df_shi_samples_bootstraping.csv")
df_shi_samples <- read_csv(".././03_data/outputs/1stBatch/df_shi_samples_bootstraping.csv")

# function to calc intervals
intervals <- as_mapper(function(boot_data,metric,lower=0.025,upper=0.975){
  # filter boot data to connectivity metric version
  boot_data_group <- boot_data |> filter(version==metric)
  # Get mean and 95% intervals grouped by year
  intervals <- boot_data_group |> ungroup() |> 
    nest_by(Year) |> 
    mutate(quantiles = list(
      quantile(data$Steward_SHI, probs = c(lower, upper)) |> 
        setNames(c("Lower_CI","Upper_CI")) |> bind_rows()), # get quantiles and set names
      mean_SHI = list(mean(data$SHI))) # measure mean within years
  # create dataframe with interval values and remove additional data
  df_index_intervals <- intervals |> 
    unnest(c(quantiles,mean_SHI)) |> 
    dplyr::select(-data) |> 
    data.frame() |> ungroup() #|> 
  # mutate(Index=data_group$Steward_SHI)
  return(df_index_intervals)
})

# get intervals ----------------------------------------------------------------
l_shi_intervals <- map(df_versions$version, \(metric) 
                       intervals(boot_data=df_shi_samples,metric=metric) |> 
                         mutate(Index=df_SHI |> filter(version==metric) |> pull(Steward_SHI))) |> # add index from data
  setNames(df_versions$version)
df_shi_intervals <- l_shi_intervals |> bind_rows(.id="version") |> 
  separate(version,c("base","method"),sep="_year_",remove = F)

write_tsv(df_shi_intervals,".././03_data/outputs/1stBatch/summary_data/df_shi_intervals.tsv")

################################################################################
# Plots
################################################################################

# SHI-----------------------------
img_SHI_interv <- df_shi_intervals |> 
  ggplot() + 
  geom_ribbon(aes(x=Year, y=Index, ymax = Upper_CI, ymin = Lower_CI,group=interaction(method,base),fill=method), alpha = 0.5) +
  geom_line(aes(x=Year, y=Index,group=interaction(method,base),colour=method,linetype=base), linewidth=0.5) + 
  ylab("Species Habitat Index (%)")+
  theme_bw() + theme(legend.position="right") + 
  scale_x_continuous(breaks=v_time_steps, limits = c(2000,2020)) + 
  scale_fill_scico_d(palette = "roma",labels=c("GISfrag","Omniscape (Suitable)","Omniscape (Base year)","Omniscape (All TDF)")) +
  scale_color_scico_d(palette = "roma",labels=c("GISfrag","Omniscape (Suitable)","Omniscape (Base year)","Omniscape (All TDF)")) +
  scale_y_continuous(breaks=seq(0,125,25))+
  scale_linetype_discrete(name="base year",labels=c("Hypothetical","2000"));img_SHI_interv # +
# facet_grid(.~base, labeller=labeller(base=c(HypBase="Hypothetical base year",t0Base="2000 base year")));img_SHI_interv

# ggsave(".././03_data/outputs/1stBatch/img_SHI_interv.png", img_SHI_interv ,dpi = 300,width=6,height = 3)
ggsave(".././05_figures/img_SHI_interv.png", img_SHI_interv ,dpi = 300,width=6,height = 3)

#-------------------------------------------------------------------------------
# IUCN category
#-------------------------------------------------------------------------------
glimpse(df_SHS_Sp_Info_Habitat)

# get shi from data by IUCN category---------------------------
df_shi_IUCN <- df_SHS_Sp_Info_Habitat |> filter(IUCN_category!="Data Deficient") |> 
  group_by(IUCN_category) |> nest() |>  
  mutate(shi_data=map(data,~get_shi(.x))) |> dplyr::select(-data)

# get group quantities
df_SHS_IUCN_cat_count <- df_SHS_Sp_Info_Habitat |> group_by(version,IUCN_category,Year) |> count() 
df_SHS_IUCN_cat <- df_SHS_IUCN_cat_count |> filter(version=="HypBase_year_GISFrag" & Year=="1990") 

# nest SHS by IUCN category and sample ID
df_shs_samples_IUCN <- df_shs_samples |> group_by(IUCN_category,sample_num) |> nest()

# get SHI for samples by IUCN group ------------------------------------------
df_shi_samples_IUCN <- df_shs_samples_IUCN |> mutate(shi_iucn=map(data,~get_shi(.x))) |> 
  dplyr::select(shi_iucn) |> unnest(shi_iucn) #|> ungroup() |> nest_by(IUCN_category)

# get SHI intervals for samples
df_shi_intervals_IUCN <- map(IUCN_categ[1:2], \(group) map(df_versions$version , 
                                                      \(metric) intervals(boot_data=df_shi_samples_IUCN |> 
                                                                            filter(IUCN_category==group),metric=metric) |> 
                                                        mutate(Index=df_shi_IUCN |> 
                                                                 filter(IUCN_category==group) |> unnest(shi_data) |> 
                                                                 filter(version==metric) |> pull(Steward_SHI))) |> # add index from data
                               setNames(df_versions$version) |> bind_rows(.id="version") |> mutate(IUCN_category=group)) |> 
  bind_rows() |> 
  separate(version,c("base","method"),sep="_year_",remove = F)

# both
img_SHI_interv_IUCN <- df_shi_intervals_IUCN |> 
  ggplot() + 
  geom_ribbon(aes(x=Year, y=Index, ymax = Upper_CI, ymin = Lower_CI,group=interaction(method,base),fill=method), alpha = 0.5) +
  geom_line(aes(x=Year, y=Index,group=interaction(method,base),colour=method,linetype=base), linewidth=0.5) + 
  ylab("Species Habitat Index (%)")+
  theme_bw() + theme(legend.position="none") +# theme(legend.position="right") + 
  scale_x_continuous(breaks=v_time_steps, limits = c(2000,2020)) + 
  scale_fill_scico_d(palette = "roma") +
  scale_color_scico_d(palette = "roma") +
  scale_y_continuous(breaks=seq(0,125,25)) +
  facet_grid(.~IUCN_category,
             labeller=labeller(IUCN_category=c('Least Concern'=paste0("Least Concern (n=",df_SHS_IUCN_cat |> filter(IUCN_category=="Least Concern") |> pull(n),")"),
                                               'Some risk'=paste0("Some Risk (n=",df_SHS_IUCN_cat |> filter(IUCN_category=="Some risk") |> pull(n),")"))))+
  scale_linetype_discrete(name="base year",labels=c("Hypothetical","2000"));img_SHI_interv_IUCN
# facet_grid(IUCN_category~base,
#            labeller=labeller(base=c(HypBase="Hypothetical base year",t0Base="2000 base year"),
#                              IUCN_category=c('Not Threatened'="Not Threatened (n=492)",'Threatened/DD'="Threatened/DD (n=12)")));img_SHI_interv_IUCN

# ggsave(".././03_data/outputs/1stBatch/img_SHI_interv_IUCN.png", img_SHI_interv_IUCN ,dpi = 300,width=4,height = 4)
ggsave(".././05_figures/img_SHI_interv_IUCN.png", img_SHI_interv_IUCN ,dpi = 300,width=8,height = 3)


#-------------------------------------------------------------------------------
# Taxonomic group 
#-------------------------------------------------------------------------------
# get shi from data by taxonomic group------------------
df_shi_group <- df_SHS_Sp_Info_Habitat |> group_by(tax_group) |> nest() |>  
  mutate(shi_data=map(data,~get_shi(.x))) |> dplyr::select(-data)

# count groups for graph
df_SHS_tax_groups_count <- df_SHS_Sp_Info_Habitat |> group_by(version,tax_group,Year) |> count() 
df_SHS_tax_groups <- df_SHS_tax_groups_count |> filter(version=="HypBase_year_GISFrag" & Year=="1990") 

# nest SHS by taxonomic group and sample ID
df_shs_samples_group <- df_shs_samples |> group_by(tax_group,sample_num) |> nest()

# get SHI for samples---------------------------
df_shi_samples_group <- df_shs_samples_group |> mutate(shi_group=map(data,~get_shi(.x))) |> 
  dplyr::select(shi_group) |> unnest(shi_group) #|> ungroup() |> nest_by(tax_group)

# get SHI intervals for samples
df_shi_intervals_group <- map(unique(df_SHS_Sp_Info_Habitat$tax_group), \(taxon) map(df_versions$version , 
                                                                                        \(metric) intervals(boot_data=df_shi_samples_group |> filter(tax_group==taxon),metric=metric) |> 
                                                                                          mutate(Index=df_shi_group |> filter(tax_group==taxon) |> unnest(shi_data) |> filter(version==metric) |> 
                                                                                                   pull(Steward_SHI))) |> # add index from data
                                setNames(df_versions$version) |> bind_rows(.id="version") |> mutate(tax_group=taxon)) |> 
  bind_rows() |> 
  separate(version,c("base","method"),sep="_year_",remove = F)

# both
img_SHI_interv_group <- df_shi_intervals_group |> 
  ggplot() + 
  geom_ribbon(aes(x=Year, y=Index, ymax = Upper_CI, ymin = Lower_CI,group=interaction(method,base),fill=method), alpha = 0.5) +
  geom_line(aes(x=Year, y=Index,group=interaction(method,base),colour=method,linetype=base), linewidth=0.5) + 
  ylab("Species Habitat Index (%)")+
  theme_bw() + theme(legend.position="none") + # theme(legend.position="right") +
  scale_x_continuous(breaks=v_time_steps, limits = c(2000,2020)) + 
  scale_fill_scico_d(palette = "roma") +
  scale_color_scico_d(palette = "roma") +
  scale_y_continuous(breaks=seq(0,125,25)) +
  facet_grid(.~tax_group,
             labeller=labeller(tax_group=c(Birds= paste0("Birds (n=",df_SHS_tax_groups |> filter(tax_group=="Birds") |> pull(n),")"),
                                           Mammals= paste0("Mammals (n=",df_SHS_tax_groups |> filter(tax_group=="Mammals") |> pull(n),")"), 
                                           Plants= paste0("Plants (n=",df_SHS_tax_groups |> filter(tax_group=="Plants") |> pull(n),")"))))+
  scale_linetype_discrete(name="base year",labels=c("Hypothetical","2000"));img_SHI_interv_group

ggsave(".././05_figures/img_SHI_interv_tax_group.png", img_SHI_interv_group ,dpi = 300,width=8,height = 3)

#-------------------------------------------------------------------------------
# Habitat type
#-------------------------------------------------------------------------------
# get shi from data by habitat type - remove NAs and species with Other type of habitat ------------------
df_shi_cat_hab <- df_SHS_Sp_Info_Habitat |> 
  group_by(Hab_comb)|> filter(!is.na(Hab_comb) & Hab_comb!="Other") |> nest() |>  
  mutate(shi_data=map(data,~get_shi(.x))) |> dplyr::select(-data)

# count groups for graphs
df_SHS_Sp_Info_Habitat_count <- df_SHS_Sp_Info_Habitat |> 
  filter(!is.na(Hab_comb) & Hab_comb!="Other")|> group_by(version,Hab_comb,Year) |> count() 
df_SHS_Habitat_groups <- df_SHS_Sp_Info_Habitat_count |> 
  filter(version=="HypBase_year_GISFrag" & Year=="1990") # filter or just one metric to avoid repetition

# nest SHS by habitat type and sample ID
df_shs_samples_cat_hab <- df_shs_samples |> 
  filter(!is.na(Hab_comb) & Hab_comb!="Other") |> 
  group_by(Hab_comb,sample_num) |> nest()

# get SHI for samples
df_shi_samples_cat_hab <- df_shs_samples_cat_hab |> mutate(shi_group=map(data,~get_shi(.x))) |> 
  dplyr::select(shi_group) |> unnest(shi_group) #|> ungroup() |> nest_by(tax_group)

# get SHI intervals for samples
df_shi_intervals_hab <- map(df_SHS_Habitat_groups$Hab_comb, \(hab) map(df_versions$version , 
                                                                            \(metric) intervals(boot_data=df_shi_samples_cat_hab |> filter(Hab_comb==hab), metric=metric) |> 
                                                                              mutate(Index=df_shi_cat_hab |> filter(Hab_comb==hab) |> unnest(shi_data) |> filter(version==metric) |> 
                                                                                       pull(Steward_SHI))) |> # add index from data
                              setNames(df_versions$version) |> bind_rows(.id="version") |> mutate(Hab_comb=hab)) |> 
  bind_rows() |> 
  separate(version,c("base","method"),sep="_year_",remove = F)


# figure for habitat
img_SHI_interv_hab <- df_shi_intervals_hab |> 
  ggplot() + 
  geom_ribbon(aes(x=Year, y=Index, ymax = Upper_CI, ymin = Lower_CI,group=interaction(method,base),fill=method), alpha = 0.5) +
  geom_line(aes(x=Year, y=Index,group=interaction(method,base),colour=method,linetype=base), linewidth=0.5) + 
  ylab("Species Habitat Index (%)")+
  theme_bw() + theme(legend.position="none") + # theme(legend.position="right") +
  scale_x_continuous(breaks=v_time_steps, limits = c(2000,2020))+ 
  scale_fill_scico_d(palette = "roma",labels=c("GISfrag","Omniscape (Suitable)","Omniscape (Base year)","Omniscape (All TDF)")) +
  scale_color_scico_d(palette = "roma",labels=c("GISfrag","Omniscape (Suitable)","Omniscape (Base year)","Omniscape (All TDF)"))  +
  scale_y_continuous(breaks=seq(0,125,25)) +
  facet_grid(.~Hab_comb,
             labeller=labeller(Hab_comb=c(Artificial= paste0("Artificial (n=", df_SHS_Habitat_groups |> filter(Hab_comb=="Artificial") |> pull(n),")"),
                                          Natural = paste0("Natural (n=", df_SHS_Habitat_groups |> filter(Hab_comb=="Natural") |> pull(n),")"))))+
  scale_linetype_discrete(name="base year",labels=c("Hypothetical","2000"));img_SHI_interv_hab

ggsave(".././05_figures/img_SHI_interv_habitat.png", img_SHI_interv_hab ,dpi = 300,width=8,height = 3)

