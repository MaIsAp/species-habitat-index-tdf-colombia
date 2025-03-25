library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(taxize)
library(stringr)

#-------------------------------------------------------------------------------
# Literature list
#-------------------------------------------------------------------------------
litlist_sp <- read_csv("./00_rawdata/tables/df_litlist_sp.csv") 
litlist_sp |> count(group)

appendix_litlist_sp <- read_csv("/Volumes/Extreme Pro/Hikaru/Doctorado/data/numeric/BST_IAvH/all_pizano2014.csv")
appendix_litlist_sp |> dplyr::count(group)

litlist_sp <- litlist_sp |> dplyr::bind_rows(appendix_litlist_sp) |> dplyr::group_by(group) |> dplyr::count(sci_name) |> mutate(source="Pizano2014")

litlist_sp |> dplyr::count(group)

#-------------------------------------------------------------------------------
# González-M Plant trait sp list (IAvH)
#-------------------------------------------------------------------------------
df_tdf_plant_trait_sp <- read_csv("/Volumes/Extreme Pro/Hikaru/Doctorado/Documento/Capitulo_02/01_repositorios/Biotablero/02_outdata/TDF_traits_sp_filtered.csv")#/run/media/miap/C084AA1284AA0ACC
df_tdf_plant_occur_sp <- read_csv("/Volumes/Extreme Pro/Hikaru/Doctorado/Documento/Capitulo_02/01_repositorios/Biotablero/02_outdata/TDF_traits_sp_occurences.csv")#/run/media/miap/C084AA1284AA0ACC

df_tdf_plant_iavh <- df_tdf_plant_trait_sp |> dplyr::bind_rows(df_tdf_plant_occur_sp) |> dplyr::count(sci_name) |> mutate(group="Plants",.before="sci_name", source="González-M")

litlist_sp <- litlist_sp |> dplyr::bind_rows(df_tdf_plant_iavh) |> dplyr::group_by(group) |> dplyr::count(sci_name)

litlist_sp |> dplyr::count(group)

#-------------------------------------------------------------------------------
# Verify taxonomy
#-------------------------------------------------------------------------------

#check duplicates
temp <- litlist_sp |> dplyr::count(sci_name) |> dplyr::arrange(desc(n))

#get verified names from taxize package
df_litlist_sp_check <- gnr_resolve(sci = litlist_sp$sci_name)
write_csv(df_litlist_sp_check,"../../03_data/df_litlist_sp_check.csv")

# select names without author
df_litlist_sp_compare <- df_litlist_sp_check |> dplyr::group_by(submitted_name) |> dplyr::distinct(matched_name) |>
  dplyr::mutate(checked_name=word(matched_name, 1,2, sep=" ")) |> dplyr::distinct(checked_name) |>
  dplyr::filter(!str_detect(checked_name,"\\(")) |> #dplyr::rename(sci_name=checked_name) |> 
    mutate(sci_name = case_when(str_detect(checked_name,"fuliginoſa") ~ "Amphisbaena fuliginosa",
                                checked_name == "boa constrictor" ~ "Boa constrictor",
                                checked_name == "Carollia? perspicillata" ~ "Carollia perspicillata",
                                checked_name == "Scinax xsignata" ~ "Scinax x-signatus",
                                checked_name == "Hemi-Dactylus frenatus" ~ "Hemidactylus frenatus",
                                checked_name == "Pandion haliætus" ~ "Pandion haliaetus", 
                                checked_name == "Aïphanes horrida" ~ "Aiphanes horrida",
                                checked_name == "Capsicum annuum'" ~ "Capsicum annuum",
                                checked_name == "Elæis guineënsis" ~ "Elaeis guineensis",
                                checked_name == "Heliconia osaënsis" ~ "Heliconia osaensis",
                                checked_name == "manilkara zapota" ~ "Manilkara zapota",
                                checked_name == "Pitcairnia maïdifolia" ~ "Pitcairnia maidifolia",
                                checked_name == "Spondias ×" ~ "Spondias mombin",
                                submitted_name == "Mimosa pudicavartetrandra" ~ "Mimosa pudicavartetrandra",
                                submitted_name == "Annona quinduenses" ~ "Annona quinduensis",
                                submitted_name == "Erythrina rubrivenium" ~ "Erythrina rubrivenium",
                                submitted_name == "Callinandra purdiaei" ~ "Calliandra purdiaei",
                                submitted_name == "Clusia umbellata" ~ "Clusia umbellata",
                                submitted_name == "Coursetia caribaeavar" ~ "Coursetia caribaea",
                                submitted_name == "Acalypha macrostachia" ~ "Acalypha macrostachya",
                                submitted_name == "Chloroleucon mangensevar" ~ "Chloroleucon mangense",
                                str_detect(submitted_name,"klattii") ~ "Steiractinia klattii",
                                str_detect(checked_name,"Citrus") ~ "Citrus limon",
                                str_detect(checked_name,"Eucharis") & checked_name != "Eucharis caucana" ~ "Eucharis grandiflora",
                                str_detect(checked_name,"Musa") & checked_name != "Musa velutina" ~ "Musa paradisiaca",
                                str_detect(checked_name,"Wikispecies") ~ "Argemone mexicana",
                                TRUE ~ checked_name)) |>
  dplyr::distinct(sci_name) |> 
  ungroup()

#check sp that still have two names
temp <- df_litlist_sp_compare |> dplyr::count(submitted_name) |> dplyr::arrange(desc(n))
temp <- df_litlist_sp_compare |> dplyr::count(sci_name) |> dplyr::arrange(desc(n))

# check for differences between submitted names and verified
sum(df_litlist_sp_compare$submitted_name!=df_litlist_sp_compare$sci_name)
df_litlist_sd_diff_compare <- df_litlist_sp_compare |> dplyr::filter(submitted_name!=sci_name)

df_litlist_sp_verif <- df_litlist_sp_compare %>%
  dplyr::left_join(litlist_sp  ,by=c("submitted_name"="sci_name")) %>%
  dplyr::select(-submitted_name) |> count(group,sci_name)

# fixed one species left without group for caps difference
df_litlist_sp_verif$group[grep("Protium tenuifolium",df_litlist_sp_verif$sci_name,value=F)] <- "Plants"

df_litlist_sp_verif |> count(group)

litlist_plant_sp <- df_litlist_sp_verif |> filter(group=="Plants")
litlist_anima_sp <- df_litlist_sp_verif |> filter(group!="Plants")

# write_csv(df_litlist_sp_verif, "./00_rawdata/tables/df_litlist_sp_verif.csv")
df_litlist_sp_verif<- read_csv("./00_rawdata/tables/df_litlist_sp_verif.csv") 

#-------------------------------------------------------------------------------
# Verif Salinas data (Not included)
#-------------------------------------------------------------------------------
df_sp_list_salinas <- read_csv("/Volumes/Extreme Pro/Hikaru/Doctorado/data/numeric/Traits/Salinas_V/sp_list_salinas.csv",col_names = F)

v_sp_list_salinas <- df_sp_list_salinas$X1[!str_detect(df_sp_list_salinas$X1," sp")]

sum(df_sp_list_salinas$X1 %in% df_litlist_sp_verif$sci_name)
sum(v_sp_list_salinas %in% df_litlist_sp_verif$sci_name)

sum(df_sp_list_salinas$X1 %in% df_litlist_in_biomodelos$sci_name)
sum(v_sp_list_salinas %in% df_litlist_in_biomodelos$sci_name)

sum(df_sp_list_salinas$X1 %in% df_litlist_in_vasquezpe$sci_name)
sum(v_sp_list_salinas %in% df_litlist_in_vasquezpe$sci_name)

sum(df_sp_list_salinas$X1 %in% df_litlist_in_vasquezpe$sci_name & !df_sp_list_salinas$X1 %in% df_litlist_in_vasquezpe$sci_name) # there is no sp from Vasquez that is not in Biomodelos
sum(v_sp_list_salinas %in% df_litlist_in_vasquezpe$sci_name)


#get verified names from taxize package
df_salinas_sp_check <- gnr_resolve(sci = v_sp_list_salinas$X1)

# select names without author
df_salinas_sp_compare <- df_salinas_sp_check |> dplyr::group_by(submitted_name) |> dplyr::distinct(matched_name) |>
  dplyr::mutate(checked_name=word(matched_name, 1,2, sep=" ")) |> dplyr::distinct(checked_name) |>
  dplyr::filter(!str_detect(checked_name,"\\(")) |> dplyr::rename(sci_name=checked_name) #|> 
  mutate(sci_name = case_when(str_detect(checked_name,"fuliginoſa") ~ "Amphisbaena fuliginosa",
                              checked_name == "boa constrictor" ~ "Boa constrictor",
                              checked_name == "Carollia? perspicillata" ~ "Carollia perspicillata",
                              checked_name == "Scinax xsignata" ~ "Scinax x-signatus",
                              checked_name == "Hemi-Dactylus frenatus" ~ "Hemidactylus frenatus",
                              checked_name == "Pandion haliætus" ~ "Pandion haliaetus", 
                              checked_name == "Aïphanes horrida" ~ "Aiphanes horrida",
                              checked_name == "Capsicum annuum'" ~ "Capsicum annuum",
                              checked_name == "Elæis guineënsis" ~ "Elaeis guineensis",
                              checked_name == "Heliconia osaënsis" ~ "Heliconia osaensis",
                              checked_name == "manilkara zapota" ~ "Manilkara zapota",
                              checked_name == "Pitcairnia maïdifolia" ~ "Pitcairnia maidifolia",
                              checked_name == "Spondias ×" ~ "Spondias mombin",
                              submitted_name == "Mimosa pudicavartetrandra" ~ "Mimosa pudicavartetrandra",
                              submitted_name == "Annona quinduenses" ~ "Annona quinduensis",
                              submitted_name == "Erythrina rubrivenium" ~ "Erythrina rubrivenium",
                              submitted_name == "Callinandra purdiaei" ~ "Calliandra purdiaei",
                              submitted_name == "Clusia umbellata" ~ "Clusia umbellata",
                              submitted_name == "Coursetia caribaeavar" ~ "Coursetia caribaea",
                              submitted_name == "Acalypha macrostachia" ~ "Acalypha macrostachya",
                              submitted_name == "Chloroleucon mangensevar" ~ "Chloroleucon mangense",
                              str_detect(submitted_name,"klattii") ~ "Steiractinia klattii",
                              str_detect(checked_name,"Citrus") ~ "Citrus limon",
                              str_detect(checked_name,"Eucharis") & checked_name != "Eucharis caucana" ~ "Eucharis grandiflora",
                              str_detect(checked_name,"Musa") & checked_name != "Musa velutina" ~ "Musa paradisiaca",
                              str_detect(checked_name,"Wikispecies") ~ "Argemone mexicana",
                              TRUE ~ checked_name)) |>
  dplyr::distinct(sci_name) |> 
  ungroup()
