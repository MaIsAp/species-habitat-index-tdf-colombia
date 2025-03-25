library(readr)
library(purrr)
library(dplyr)
library(tibble)
library(terra)
library(tidyr)
library(ggplot2)
library(writexl)

# sp_file <- "./01_repositorios/Species_Habitat_Index_Colombia/00_rawdata/tables/df_tdf_sp_for_omni_batch3.csv"##"./00_rawdata/tables/df_spVasquez.csv"#
sp_file <- "./00_rawdata/tables/df_sp_forSHI.csv"#

sp_list <- read_csv(sp_file)
v_sp <- gsub(" ","_",sp_list$sci_name)

# save species list with metadata for paper
writexl::write_xlsx(sp_list |> select(sci_name,kingdom:family,elevation_lower,elevation_lower2,elevation_upper,elevation_upper2,has_elev,disp_dist_m,rad,aerial,block_size),"./02_outdata/species/SpeciesListSHITDFCol.xlsx")

# get summary stats from Habitat percentage--------
l_sp_habitat_prop <- map(v_sp, ~list.files(file.path("./02_outdata/species",.x),pattern="habitat_layer",full.names=T))
# keep just sp with files
l_sp_habitat_prop <- l_sp_habitat_prop |> setNames(v_sp) |> keep(~length(.x)>0)
sp_with_hab <- names(l_sp_habitat_prop)

l_df_summary <- list()
for(i in 1:length(v_sp)){
  l_r_habitat_files <- map(l_sp_habitat_prop[[i]],~rast(.x))
  r_sp_habitat <- rast(l_r_habitat_files) |> setNames(v_time_steps_all)
  df_summary <- as.data.frame(summary(r_sp_habitat)) |> tidyr::separate(Freq,c("Stat","Val"),sep=":") |> mutate(Year=gsub("X","",Var2),.after="Var2")
  l_df_summary[[i]] <- df_summary 
  cat("Species ", i)
}
l_df_summary <- l_df_summary |> setNames(sp_with_hab)
df_summary_sp <- l_df_summary |> bind_rows(.id="sci_name")

write_csv(df_summary_sp,"./02_outdata/summary/df_summary_stats_hab_per20241209.csv")
df_summary_sp <- read_csv("/Volumes/Extreme Pro/Hikaru/Doctorado/Documento/Capitulo_01/03_data/outputs/1stBatch/df_summary_stats_hab_per.csv")

# data for 757 species for 7 statistics (1er Q, 3er Q, min, max, mean, median, NAs) for 4 years should have 28 rows
df_sum_stat_year <- df_summary_sp  |> group_by(Stat,Year)   |> summarise(min=min(Val,na.rm=T))
df_summary_sp |> count(sci_name)

# plot values
relevant_stats <- c("1st Qu.","3rd Qu.","Max.   ","Mean   ","Median ") # Min always 0

img_SumStatsHabPercQ1Q3 <- ggplot(df_summary_sp |> filter(Stat %in% relevant_stats),aes(x=Year,y=Val,group=sci_name))+
  geom_point(alpha=0.2)+geom_line(alpha=0.2)+facet_wrap(.~Stat,scales="free")+
  theme_bw(); img_SumStatsHabPercQ1Q3

ggsave("./02_outdata/summary/img_SumStatsHabPercQ1Q3.png",img_SumStatsHabPercQ1Q3)

ggplot(df_summary_sp |> filter(Stat %in% relevant_stats),aes(x=Year,y=Val,group=sci_name))+
  geom_point(alpha=0.2)+geom_line(alpha=0.2)+facet_wrap(.~Stat,scales="free")+
  theme_bw()+coord_cartesian(ylim=c(0,0.5))

df_summary_sp |> group_by(Stat, Var2) |> count()

df_summary_sp |> filter(Stat=="1st Qu." & Val>0.1) |> group_by(Var2) |> count()
df_summary_sp |> filter(Stat=="3rd Qu." & Val>0.1) |> group_by(Var2) |> count()
df_summary_sp |> filter(Stat=="3rd Qu." & Val<=0.1) |> group_by(Var2) |> count()
df_summary_sp |> filter(Stat=="Median " & Val>0.1) |> group_by(Var2) |> count()
df_summary_sp |> filter(Stat=="Mean   " & Val>0.1) |> group_by(Var2) |> count()

df_summary_sp |> filter(Stat=="3rd Qu.") |> 
  filter(Val>0.1) |> group_by(Var2) |> count()
df_summary_sp |> filter(Stat=="3rd Qu.") |> 
  filter(Val<=0.1) |> group_by(Var2) |> count()
