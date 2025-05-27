## Scripts to update and add data from sampling years 2023 and onward.
## Creating objects needed for fpr_table_fish_site(), fpr_table_fish_density() and fpr_plot_fish_box()
## so that we can add tables with most recent data to the report.
## copy select 2023 photo directories from shared server to repo so we can include in report. photo file name changes
## occurring on shared server only

# 2023 Data ------------------
source("scripts/packages.R")

year = "2023"
file <- "habitat_confirmations.xls"


## Prep needed to make objects
####-------------- habitat and fish data------------------
habitat_confirmations <- fpr_import_hab_con(path = paste0("data/", year, "/habitat_confirmations.xls"),
                                            col_filter_na = T,
                                            row_empty_remove = T,
                                            backup = FALSE)

## to test with old repo data
# habitat_confirmations <- fpr_import_hab_con(col_filter_na = T, row_empty_remove = T, backup = FALSE)

hab_loc <- habitat_confirmations %>%
  purrr::pluck("step_1_ref_and_loc_info") %>%
  dplyr::filter(!is.na(site_number))%>%
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date)))


##add the species code
hab_fish_codes <- fishbc::freshwaterfish %>%
  select(species_code = Code, common_name = CommonName) %>%
  tibble::add_row(species_code = 'NFC', common_name = 'No Fish Caught') %>%
  mutate(common_name = case_when(common_name == 'Cutthroat Trout' ~ 'Cutthroat Trout (General)', T ~ common_name))


## Needed for fpr_table_fish_density() and fpr_plot_fish_box()
## fish densities ----------------------------------------------------------

hab_fish_indiv_prep <- habitat_confirmations %>%
  purrr::pluck("step_3_individual_fish_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  select(-gazetted_names:-site_number)

hab_fish_indiv_prep2 <- left_join(
  hab_fish_indiv_prep,
  hab_loc,
  by = 'reference_number'
)

hab_fish_indiv_prep3 <- left_join(
  hab_fish_indiv_prep2,
  select(hab_fish_codes, common_name:species_code),
  by = c('species' = 'common_name')
) %>%
  dplyr::select(reference_number,
                alias_local_name,
                site_number,
                sampling_method,
                method_number,
                haul_number_pass_number,
                species_code,
                length_mm,
                weight_g) ##added method #

hab_fish_collect_info <- habitat_confirmations %>%
  purrr::pluck("step_2_fish_coll_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  # select(-gazetted_name:-site_number) %>%
  dplyr::distinct(reference_number, sampling_method, method_number, haul_number_pass_number, .keep_all = T)

# join the indiv fish data to existing site info
hab_fish_indiv <- full_join(
  select(hab_fish_indiv_prep3,
         reference_number,
         sampling_method,
         method_number,
         haul_number_pass_number,
         species_code,
         length_mm,
         weight_g),
  select(hab_fish_collect_info,
         reference_number,
         local_name,
         temperature_c:model, ##added date_in:time_out
         comments
  ),
  by = c(
    "reference_number",
    # 'alias_local_name' = 'local_name',
    "sampling_method",
    "method_number",
    "haul_number_pass_number")
) %>%
  mutate(species_code = as.character(species_code)) %>%
  mutate(species_code = case_when(
    is.na(species_code) ~ 'NFC',
    T ~ species_code)
  ) %>%
  mutate(species_code = as.factor(species_code)) %>%
  mutate(life_stage = case_when(  ##this section comes from the histogram below - we include here so we don't need to remake the df
    length_mm <= 65 ~ 'fry',
    length_mm > 65 & length_mm <= 110 ~ 'parr',
    length_mm > 110 & length_mm <= 140 ~ 'juvenile',
    length_mm > 140 ~ 'adult',
    T ~ NA_character_
  ),
  life_stage = case_when(
    stringr::str_detect(species_code, 'L|SU|LSU') ~ NA_character_,
    TRUE ~ life_stage
  ))%>%
  mutate(life_stage = fct_relevel(life_stage,
                                  'fry',
                                  'parr',
                                  'juvenile',
                                  'adult')) %>%
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(site_id = paste0(site, '_', location))




# make a summary table for fish sampling data
tab_fish_summary <- hab_fish_indiv %>%
  group_by(site_id,
           ef,
           sampling_method,
           species_code) %>% ##added sampling method!
  summarise(count_fish = n()) %>%
  arrange(site_id, species_code, ef)

#rename so we can use with updated data
tab_fish_summary_2023 <- tab_fish_summary


# this will be joined to the abundance estimates and the confidence intervals
fish_abund_prep <- hab_fish_indiv %>%
  group_by(local_name,
           site_id,
           ef,
           sampling_method,
           haul_number_pass_number,
           species_code,
           life_stage,
           ef_seconds) %>% ##added sampling method!
  filter(sampling_method == 'electrofishing') %>%
  summarise(catch = n()) %>%
  arrange(site_id, species_code, ef) %>%
  # ungroup() %>%
  mutate(catch = case_when(
    species_code == 'NFC' ~ 0L,
    T ~ catch),
    # effort = catch/ef_seconds,
    id = paste0(local_name, '_', species_code, '_', life_stage)) %>%
  ungroup() %>%
  arrange(id)

# join the total number of passes to each event so that we know if it is a higher number than the pass of the catch
fish_abund_prep2 <- left_join(
  fish_abund_prep,

  fish_abund_prep %>%
    group_by(local_name) %>%
    summarise(pass_total = max(haul_number_pass_number)),
  by = 'local_name'
)

# make a dat to indicate if the nfc in the set for each species
fish_nfc_tag<- fish_abund_prep2 %>%
  mutate(nfc_pass = case_when(
    # species_code != 'NFC' &
    haul_number_pass_number == pass_total ~ F,
    T ~ T),
    nfc_pass = case_when(
      species_code == 'NFC' ~ T,
      T ~ nfc_pass)
  ) %>%
  select(local_name, species_code, life_stage, haul_number_pass_number, pass_total, nfc_pass) %>%
  arrange(desc(haul_number_pass_number)) %>%
  # filter(nfc_pass == T) %>%
  distinct(local_name, species_code, life_stage, .keep_all = T) %>%
  select(-haul_number_pass_number, -pass_total)

# calculate abundance for each site regardless of whether a nfc_pass occurred.
fish_abund_prep3 <- left_join(
  fish_abund_prep2 %>%
    group_by(local_name, species_code, life_stage) %>%
    summarise(catch = sum(catch)),

  fish_nfc_tag,

  by = c('local_name', 'species_code', 'life_stage')
)


# add back the size of the sites so we can do a density
fish_abund <- left_join(
  fish_abund_prep3,

  hab_fish_collect_info %>%
    select(local_name,
           # sampling_method,
           # haul_number_pass_number,
           ef_seconds:enclosure) %>%
    distinct(local_name, ef_length_m, .keep_all = T),

  by = c('local_name')
) %>%
  mutate(area_m2 = round(ef_length_m * ef_width_m,1),
         density_100m2 = round(catch/area_m2 * 100,1)) %>%
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F)

# Rename so we can call fpr_table_fish_density() and fpr_plot_fish_box() with updated data
fish_abund_2023 <- fish_abund



## Needed for fpr_table_fish_site()
### density results -----------------------------------------------------------

# need to summarize just the sites
tab_fish_sites_sum <- left_join(
  fish_abund_prep2 %>%
    select(local_name, pass_total) %>%
    distinct(),


  hab_fish_collect_info %>%
    select(local_name,
           ef_length_m:enclosure) %>%
    distinct(),

  by = 'local_name'
) %>%
  mutate(area_m2 = round(ef_length_m * ef_width_m,1)) %>%
  select(site = local_name, passes = pass_total, ef_length_m, ef_width_m, area_m2, enclosure)

rm(fish_abund_prep,
  fish_abund_prep2,
  fish_abund_prep3,
  fish_nfc_tag
)

# Rename so we can call fpr_table_fish_site() with updated data
tab_fish_sites_sum_2023 <- tab_fish_sites_sum

#-----photos------------------------------------------------------------------------------------------

# This only needs to be done if new photos are added, turning off for now since no new photos.

## copy select 2023 photo directories from shared server to repo so we can include in report. photo file name changes
# make list of directories to transfer

# dir_2023_photos_stub = "~/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/skeena/photos/"
# dir_2023_photos <- c("198225", "198217", "198215")
#
# dir_repo_photos_stub = "data/2023/photos/"
#
# # create the directory
# fs::dir_create(dir_repo_photos_stub)
#
# # copy the directories with purrr::map
# purrr::map(dir_2023_photos,
#            ~fs::dir_copy(paste0(dir_2023_photos_stub, .x),
#                          paste0(dir_repo_photos_stub, .x),
#            overwrite = TRUE))



# 2024 data --------------------------------------------------------------

## Monitoring --------------------------------------------------------------

### Load form_monitoring_2024  --------------------------------------------------------------

# Only run first time or if we have updated the form
# path_form_monitoring_2024 <- fs::path_expand(fs::path("~/Projects/gis/", params$gis_project_name, "/data_field/2024/form_monitoring_2024.gpkg"))
#
# form_monitoring_2024 <- fpr::fpr_sp_gpkg_backup(
#     path_gpkg = path_form_monitoring_2024,
#     dir_backup = "data/backup/",
#     update_utm = TRUE,
#     update_site_id = TRUE,
#     write_back_to_path = FALSE,
#     return_object = TRUE,
#     write_to_csv = FALSE,
#     write_to_rdata = FALSE,
#     col_easting = "utm_easting",
#     col_northing = "utm_northing")
#
#
#   # Now burn to the sqlite
#   conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
#   # won't run on first build if the table doesn't exist
#   readwritesqlite::rws_drop_table("form_monitoring_2024", conn = conn)
#   readwritesqlite::rws_write(form_monitoring_2024, exists = F, delete = TRUE,
#                              conn = conn, x_name = "form_monitoring_2024")
#   readwritesqlite::rws_disconnect(conn)

### Read form_monitoring_2024 --------------------------
conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
form_monitoring_2024 <- readwritesqlite::rws_read_table("form_monitoring_2024", conn = conn)
readwritesqlite::rws_disconnect(conn)


### Clean form_monitoring_2024 --------------------------

# clean up the monitoring form so we can display it in a table
tab_monitoring <- form_monitoring_2024 |>
  sf::st_drop_geometry() |>
  dplyr::select(
    pscis_crossing_id,
    stream_name,
    road_name,
    crossing_subtype,
    `span` = diameter_or_span_meters,
    `width` = length_or_width_meters,
    assessment_comment,
    dplyr::matches("_notes$"),
    -condition_notes,
    -climate_notes,
    -priority_notes
  ) |>
  janitor::clean_names(case = "title")



## Load form_fiss_site_2024 --------------------------
conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
form_fiss_site_2024 <- readwritesqlite::rws_read_table("form_fiss_site_2024", conn = conn)
readwritesqlite::rws_disconnect(conn)

# Only run first time or if we have updated the form
# This is needed to build the `tab_fish_sites_sum` object for `fpr_table_fish_site()`

# path_form_fiss_site_2024 <- fs::path_expand(fs::path("~/Projects/gis/", params$gis_project_name, "/data_field/2024/form_fiss_site_2024.gpkg"))
#
#
#   form_fiss_site_2024 <- fpr::fpr_sp_gpkg_backup(
#     path_gpkg = path_form_fiss_site_2024,
#     dir_backup = "data/backup/",
#     update_utm = TRUE,
#     update_site_id = FALSE,
#     write_back_to_path = FALSE,
#     return_object = TRUE,
#     write_to_csv = FALSE,
#     write_to_rdata = FALSE,
#     col_easting = "utm_easting",
#     col_northing = "utm_northing") |>
#     sf::st_drop_geometry()
#
#
#   # Now burn to the sqlite
#   conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
#   # won't run on first build if the table doesn't exist
#   readwritesqlite::rws_drop_table("form_fiss_site_2024", conn = conn)
#   readwritesqlite::rws_write(form_fiss_site_2024, exists = F, delete = TRUE,
#                              conn = conn, x_name = "form_fiss_site_2024")
#   readwritesqlite::rws_disconnect(conn)


## Load 2024 fish data--------------------------

# path to the fish data with the pit tags joined.
path_fish_tags_joined <-  fs::path_expand('~/Projects/repo/fish_passage_skeena_2022_reporting/data/2024_fish_data_tags_joined.csv')

# specify which project data we want. for this case `2024-073-sern-peace-fish-passage`
project = "2024-072-sern-skeena-fish-passage"

fish_data_2024 <- readr::read_csv(file = path_fish_tags_joined) |>
  janitor::clean_names() |>
  dplyr::filter(project_name == project)


### Fish sampling results condensed ----------------------------------------------
# tab_fish_summary
tab_fish_summary_2024 <- fish_data_2024 |>
  # exclude visual observations
  dplyr::filter(sampling_method == "electrofishing") |>
  tidyr::separate(local_name, into = c("site_id", "location", "ef")) |>
  dplyr::mutate(site_id = paste0(site_id, "_", location)) |>
  dplyr::group_by(site_id,
                  ef,
                  sampling_method,
                  species) |>
  dplyr::summarise(count_fish = n()) |>
  dplyr::arrange(site_id, species, ef)



### Fish sampling site summary ------------------------------
# `tab_fish_sites_sum` object for `fpr_table_fish_site()`
tab_fish_sites_sum_2024 <- dplyr::left_join(fish_data_2024 |>
                                              dplyr::group_by(local_name) |>
                                              dplyr::mutate(pass_total = max(pass_number)) |>
                                              dplyr::ungroup() |>
                                              dplyr::select(local_name, pass_total, enclosure),
                                            form_fiss_site_2024 |>
                                              dplyr::filter(!is.na(ef)) |>
                                              dplyr::select(local_name, gazetted_names, site_length, avg_wetted_width_m) |>
                                              dplyr::mutate(gazetted_names = stringr::str_trim(gazetted_names),
                                                            gazetted_names = stringr::str_to_title(gazetted_names)) ,
                                            by = "local_name"

) |>
  dplyr::distinct(local_name, .keep_all = TRUE) |>
  dplyr::rename(ef_length_m = site_length, ef_width_m = avg_wetted_width_m) |>
  dplyr::mutate(area_m2 = round(ef_length_m * ef_width_m,1)) |>
  dplyr::select(site = local_name, stream = gazetted_names, passes = pass_total, ef_length_m, ef_width_m, area_m2, enclosure)


### Fish sampling density results ------------------------------
# `fish_abund` object for `fpr_table_fish_density()` and `fpr_plot_fish_box()`
fish_abund_2024 <- dplyr::left_join(
  fish_data_2024 |>
    # exclude visual observations
    dplyr::filter(sampling_method == "electrofishing") |>
    # Add life_stage and pass_total
    dplyr::mutate(
      life_stage = case_when(
        length <= 65 ~ 'fry',
        length > 65 & length <= 110 ~ 'parr',
        length > 110 & length <= 140 ~ 'juvenile',
        length > 140 ~ 'adult',
        TRUE ~ NA_character_
      ),
      life_stage = case_when(
        stringr::str_like(species, '%sculpin%') ~ NA_character_,
        TRUE ~ life_stage
      ),
      # Add pass_total here
      pass_total = max(pass_number)
    ) |>
    # Group and summarize
    dplyr::group_by(local_name, species, life_stage, pass_number,pass_total) |>
    dplyr::summarise(
      catch = n(),
      .groups = "drop" # Ensures the grouping is removed after summarizing
    ) |>
    # Add nfc_pass
    dplyr::mutate(
      catch = case_when(species == 'NFC' ~ 0L, TRUE ~ catch),
      nfc_pass = case_when(
        species != 'NFC' & pass_number == pass_total ~ FALSE,
        TRUE ~ TRUE
      ),
      nfc_pass = case_when(
        species == 'NFC' ~ TRUE,
        TRUE ~ nfc_pass
      )),

  form_fiss_site_2024 |>
    dplyr::filter(!is.na(ef)) |>
    dplyr::select(local_name, site, location, site_length, avg_wetted_width_m),

  by = "local_name"
) |>

  dplyr::rename(ef_length_m = site_length, ef_width_m = avg_wetted_width_m, species_code = species) |>
  dplyr::mutate(area_m2 = round(ef_length_m * ef_width_m,1),
                density_100m2 = round(catch/area_m2 * 100,1)) |>
  dplyr::select(local_name, site, location, species_code, life_stage, catch, density_100m2, nfc_pass)

