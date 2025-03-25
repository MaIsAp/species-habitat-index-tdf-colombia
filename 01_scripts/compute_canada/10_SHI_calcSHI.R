# This script takes the species name and calculates SHI for that species
# Author: Maria Isabel Arce Plata
# Date: September 2023
#-------------------------------------------------------------------------------
.libPaths("/home/maisap/R/x86_64-pc-linux-gnu-library/4.3/")
.libPaths()

packages <- c("dplyr","purrr","readr","tidyr","ggplot2")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
new.packages

# if(length(new.packages)) install.packages(new.packages, repos='https://muug.ca/mirror/cran/')
lapply(packages,require,character.only=T)


# Parameters and paths----------------------------------------------------------

# species list
sp_file <- "./00_rawdata/tables/df_sp_forSHI.csv"#"./sp_forSHI.csv"#

# time steps
t_0 <- 2000 
t_n <- 2020 # should be larger than t_0 at least 2 years needed
time_step <- 10
v_time_steps <- seq(t_0,t_n,time_step)
v_time_steps_all <-  c(v_time_steps[1]-time_step,v_time_steps)

# outputFolder
outputFolder <- "./02_outdata"
getwd()


#-------------------------------------------------------------------------------
# Load all tables
#-------------------------------------------------------------------------------
sp_list <- read.csv(sp_file)
sp <- sp_list$sci_name

# Load score tables
l_files <- map(sp, ~list.files(file.path(outputFolder,"species",gsub(" ","_",.x)),pattern=paste0(.x,"_df_SHS.csv"),full.names=T)) |> setNames(sp)
# keep just sp with files
l_files_nonempty <- l_files |> compact()
l_df_scores_sp <- map(l_files_nonempty, ~read_csv(.x))
df_scores_sp <- bind_rows(l_df_scores_sp) |> mutate(Version=case_when(Version=="Base_year_GISFrag"~ "HypBase_year_GISFrag",
                                                                      Version=="NoBase_year_GISFrag"~ "t0Base_year_GISFrag",
                                                                      Version=="Base_year_Omniscape"~"HypBase_year_Omniscape",
                                                                      Version=="NoHypBase_year_Omniscape"~ "t0Base_year_Omniscape",
                                                                      TRUE~Version))

write_csv(df_scores_sp,file= file.path(outputFolder,"summary","df_SHS_allSp.csv"))

print(head(df_scores_sp))

sp_complete <- names(l_files_nonempty)
print(sp_complete)
length(sp_complete)

# Load area tables
l_df_aoh_areas_sp <- map(sp_complete, ~read_csv(file.path(outputFolder,"species",gsub(" ","_",.x),paste0(.x,"_df_aoh_areas.csv"))))
df_aoh_areas_sp <- bind_rows(l_df_aoh_areas_sp) |> mutate(W_stewardship=area_aoh/area_range)

write_csv(df_aoh_areas_sp,file.path(outputFolder,"summary","df_aoh_areas_sp.csv"))

print(head(df_aoh_areas_sp))


#-------------------------------------------------------------------------------
# Join tables
#-------------------------------------------------------------------------------
df_SHS_aoh_areas_sp <- df_scores_sp |> filter(Score=="SHS") |> 
  left_join(df_aoh_areas_sp,by="sci_name",relationship="many-to-one")

df_SHI <- df_SHS_aoh_areas_sp |> group_by(Version,Year) |> 
  summarise(SHI=mean(Values,na.rm=T),Steward_SHI= weighted.mean(Values,W_stewardship,na.rm=T))

path_SHI <- file.path(outputFolder,"summary","df_SHI_table.csv")
print(df_SHI)

write_csv(df_SHI,file= path_SHI)

#Plot - With aoh as base year---------------------------------------------------
df_SHI_base <- df_SHI |> filter(grepl("^HypBase_year",Version))

#Plot
img_SHI1_timeseries <- ggplot(df_SHI_base , aes(x=Year,y=Steward_SHI,linetype=Version))+
  geom_line(linewidth=1) + geom_point(size=2)+
  scale_linetype_manual(values=c("solid","dotted"),labels=c("GISfrag","Omniscape")) +
  ylab("Species Habitat Index (%)")+theme_bw()+theme(legend.position="bottom")+ 
  ggtitle("Hypothetical Base year") + scale_x_continuous(labels=c("Ini",v_time_steps),breaks=v_time_steps_all)+ 
  coord_cartesian(ylim=c(0,110),xlim=c(2000,2020))+ scale_y_continuous(breaks=seq(0,125,25))

path_img_SHI1_timeseries <- file.path(outputFolder,"SHI_timeseries_hyp_baseline.png")
ggsave(path_img_SHI1_timeseries, img_SHI1_timeseries ,dpi = 300,width=8,height = 5)

#Plot - No base year---------------------------------------------------
df_SHI_nobase <- df_SHI |> filter(grepl("^t0Base_year",Version))

#Plot
img_SHI2_timeseries <- ggplot(df_SHI_nobase , aes(x=Year,y=Steward_SHI,lty=Version))+
  geom_line(linewidth=1) + geom_point(size=2)+
  scale_linetype_manual(values=c("solid","dotted"),labels=c("GISfrag","Omniscape")) +
  ylab("Species Habitat Index (%)")+theme_bw()+theme(legend.position="bottom")+ 
  ggtitle("2000 - Base year") + scale_x_continuous(labels=v_time_steps,breaks=v_time_steps)+ 
  coord_cartesian(ylim=c(0,110),xlim=c(2000,2020)) + scale_y_continuous(breaks=seq(0,125,25))

path_img_SHI2_timeseries <- file.path(outputFolder,"SHI_timeseries_t0_baseline.png")
ggsave(path_img_SHI2_timeseries, img_SHI2_timeseries ,dpi = 300,width=8,height = 5)



#-------------------------------------------------------------------------------
# Create SHS integrated file for Omniscape value measured for all TDF area
#-------------------------------------------------------------------------------
# Load score tables
l_files_tdf <- map(sp, ~list.files(file.path(outputFolder,"species",gsub(" ","_",.x)),pattern=paste0(.x,"_df_SHS_tdf.csv"),full.names=T)) |> setNames(sp)
# keep just sp with files
l_files_nonempty <- l_files_tdf |> compact()
l_df_scores_sp <- map(l_files_nonempty, ~read_csv(.x))
df_scores_sp <- bind_rows(l_df_scores_sp) |> mutate(Version=case_when(Version=="Base_year_GISFrag"~ "HypBase_year_GISFrag",
                                                                      Version=="NoBase_year_GISFrag"~ "t0Base_year_GISFrag",
                                                                      TRUE~Version))

write_csv(df_scores_sp,file= file.path(outputFolder,"summary","df_SHS_tdf_allSp.csv"))

print(head(df_scores_sp))

sp_complete <- names(l_files_nonempty)
print(sp_complete)
length(sp_complete)

#-------------------------------------------------------------------------------
# Join tables
#-------------------------------------------------------------------------------
df_SHS_aoh_areas_sp <- df_scores_sp |> filter(Score=="SHS") |> 
  left_join(df_aoh_areas_sp,by="sci_name",relationship="many-to-one")

df_SHI <- df_SHS_aoh_areas_sp |> group_by(Version,Year) |> 
  summarise(SHI=mean(Values,na.rm=T),Steward_SHI= weighted.mean(Values,W_stewardship,na.rm=T))

path_SHI <- file.path(outputFolder,"summary","df_SHI_tdf_table.csv")
print(df_SHI)

write_csv(df_SHI,file= path_SHI)


#-------------------------------------------------------------------------------
# Create SHS integrated file for Omniscape value measured for base year area
#-------------------------------------------------------------------------------
# Load score tables
l_files_base <- map(sp, ~list.files(file.path(outputFolder,"species",gsub(" ","_",.x)),pattern=paste0(.x,"_df_SHS_base.csv"),full.names=T)) |> setNames(sp)
# keep just sp with files
l_files_nonempty <- l_files_base |> compact()
l_df_scores_sp <- map(l_files_nonempty, ~read_csv(.x))
df_scores_sp <- bind_rows(l_df_scores_sp) |> mutate(Version=case_when(Version=="Base_year_GISFrag"~ "HypBase_year_GISFrag",
                                                                      Version=="NoBase_year_GISFrag"~ "t0Base_year_GISFrag",
                                                                      TRUE~Version))

write_csv(df_scores_sp,file= file.path(outputFolder,"summary","df_SHS_base_allSp.csv"))

print(head(df_scores_sp))

sp_complete <- names(l_files_nonempty)
print(sp_complete)
length(sp_complete)

#-------------------------------------------------------------------------------
# Join tables
#-------------------------------------------------------------------------------
df_SHS_aoh_areas_sp <- df_scores_sp |> filter(Score=="SHS") |> 
  left_join(df_aoh_areas_sp,by="sci_name",relationship="many-to-one")

df_SHI <- df_SHS_aoh_areas_sp |> group_by(Version,Year) |> 
  summarise(SHI=mean(Values,na.rm=T),Steward_SHI= weighted.mean(Values,W_stewardship,na.rm=T))

path_SHI <- file.path(outputFolder,"summary","df_SHI_base_table.csv")
print(df_SHI)

write_csv(df_SHI,file= path_SHI)