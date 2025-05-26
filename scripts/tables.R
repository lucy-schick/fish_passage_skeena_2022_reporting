# this file imports our data and builds the tables we need for our reporting




# add project specific variables ------------------------------------------
filename_html <- 'Skeena2022'
repo_name <- 'fish_passage_skeena_2022_reporting'
maps_location <- 'https://hillcrestgeo.ca/outgoing/fishpassage/projects/bulkley/archive/2022-05-02/'
maps_location_zip <- 'https://hillcrestgeo.ca/outgoing/fishpassage/projects/bulkley/archive/2022-05-02/bulkley_2022-05-02.zip'


pscis_list <- fpr::fpr_import_pscis_all()
pscis_phase1 <- pscis_list %>% pluck('pscis_phase1')
pscis_phase2 <- pscis_list %>% pluck('pscis_phase2') %>%
  arrange(pscis_crossing_id)
pscis_reassessments <- pscis_list %>% pluck('pscis_reassessments')
pscis_all_prep <- pscis_list %>%
  bind_rows()

# for now convert to pscis_all since we are not ready for the wshds yet

# this is a workaround for the wshd area since it is now dropped from the crossings table
# wshds <- sf::read_sf('data/fishpass_mapping/fishpass_mapping.gpkg', layer = 'hab_wshds')

##lets add in the xref pscis id info - this is made from 01_prep_data/0140-extract-crossings-xref.R
# xref_pscis_my_crossing_modelled <- readr::read_csv(
#   file = paste0(getwd(),
#                 '/data/inputs_extracted/xref_pscis_my_crossing_modelled.csv'))
  # mutate(external_crossing_reference = as.numeric(external_crossing_reference)) %>%
  # rename(my_crossing_reference = external_crossing_reference)



# import data from sqlite -------------------------------------------------


#this is our new db made from 0282-extract-bcfishpass2-crossing-corrections.R and 0290
conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
readwritesqlite::rws_list_tables(conn)
bcfishpass_phase2 <- readwritesqlite::rws_read_table("bcfishpass", conn = conn) %>%
  dplyr::filter(stream_crossing_id %in%
           (pscis_phase2 %>%
              pull(pscis_crossing_id))) %>%
  dplyr::filter(!is.na(stream_crossing_id))

bcfishpass <- readwritesqlite::rws_read_table("bcfishpass", conn = conn) %>%
  mutate(ch_cm_co_pk_sk_network_km = round(ch_cm_co_pk_sk_network_km,2))

#load bcfishpass_2025 object - bcfishpass modelling updated in 2025
bcfishpass_2025 <- readwritesqlite::rws_read_table("bcfishpass_2025", conn = conn)

# bcfishpass_archive <- readwritesqlite::rws_read_table("bcfishpass_archive_2022-03-02-1403", conn = conn)
bcfishpass_column_comments <- readwritesqlite::rws_read_table("bcfishpass_column_comments", conn = conn)
# bcfishpass_archived <- readwritesqlite::rws_read_table("bcfishpass_morr_bulk_archive", conn = conn) %>%
#   mutate(downstream_route_measure = as.integer(downstream_route_measure))
# pscis_historic_phase1 <- readwritesqlite::rws_read_table("pscis_historic_phase1", conn = conn)
bcfishpass_spawn_rear_model <- readwritesqlite::rws_read_table("bcfishpass_spawn_rear_model", conn = conn)
# tab_cost_rd_mult <- readwritesqlite::rws_read_table("rd_cost_mult", conn = conn)
rd_class_surface_prep <- readwritesqlite::rws_read_table("rd_class_surface", conn = conn)
xref_pscis_my_crossing_modelled <- readwritesqlite::rws_read_table("xref_pscis_my_crossing_modelled", conn = conn)
#
wshds <- readwritesqlite::rws_read_table("wshds", conn = conn) %>%
   mutate(aspect = as.character(aspect))

photo_metadata <- readwritesqlite::rws_read_table("photo_metadata", conn = conn)
# # fiss_sum <- readwritesqlite::rws_read_table("fiss_sum", conn = conn)
readwritesqlite::rws_disconnect(conn)

tab_cost_rd_mult <- readr::read_csv(paste0(getwd(), '/data/inputs_raw/tab_cost_rd_mult.csv'))

# this doesn't work till our data loads to pscis
pscis_all <- left_join(
  pscis_all_prep,
  xref_pscis_my_crossing_modelled,
  by = c('my_crossing_reference' = 'external_crossing_reference')
) %>%
  mutate(pscis_crossing_id = case_when(
    is.na(pscis_crossing_id) ~ as.numeric(stream_crossing_id),
    T ~ pscis_crossing_id
  )) %>%
  # mutate(amalgamated_crossing_id = case_when(
  #   !is.na(my_crossing_reference) ~ my_crossing_reference,
  #   T ~ pscis_crossing_id
  # )) %>%
  # select(-stream_crossing_id) %>%
  arrange(pscis_crossing_id)

# HACK!!!!!!!!!!! - replace pscis_all_prep with pscis_all
pscis_all_sf <- pscis_all %>%
  # distinct(.keep_all = T) %>%
  sf::st_as_sf(coords = c("easting", "northing"),
               crs = 26909, remove = F) %>% ##don't forget to put it in the right crs buds
  sf::st_transform(crs = 3005) ##convert to match the bcfishpass format

# here is a spot to burn this to a file to try to figure out wtf is going on with these IDs
# pscis_all_sf %>% sf::write_sf('data/inputs_extracted/pscis_all_2022.gpkg')

# add the elevations for our pscis crossing locations
# pscis_all_sf <- poisspatial::ps_elevation_google(pscis_all_sf,
#                                                  key = google_api_key,
#                                                  Z = 'elev') %>%
#   mutate(elev = round(elev, 0))
# looks like the api maxes out at 220 queries and we have 223.  As a work around lets make a function then split by source
fpr_get_elev <- function(dat){
  poisspatial::ps_elevation_google(dat,
                                   key = Sys.getenv('GOOG_API_KEY'),
                                   Z = 'elev') %>%
  mutate(elev = round(elev, 0))
}

pscis_all_sf <- pscis_all_sf %>%
  dplyr::group_split(source) %>%
  purrr::map(fpr_get_elev) %>%
  dplyr::bind_rows()



##this is not working or needed yet
# bcfishpass_rd <- bcfishpass %>%
#   select(pscis_crossing_id = stream_crossing_id, my_crossing_reference, crossing_id, distance, road_name_full,
#          road_class, road_name_full, road_surface, file_type_description, forest_file_id,
#          client_name, client_name_abb, map_label, owner_name, admin_area_abbreviation,
#          steelhead_network_km, steelhead_belowupstrbarriers_network_km, distance) %>%
#   # dplyr::filter(distance < 100) %>% ## we need to screen out the crossings that are not matched well
#   select(pscis_crossing_id, my_crossing_reference:admin_area_abbreviation, steelhead_network_km, steelhead_belowupstrbarriers_network_km)



####-----------report table--------------------
#  HACK hashout for now!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! becasue some columns are now missing from bcfishpass.crossings

tab_cost_rd_mult_report <- tab_cost_rd_mult %>%
  mutate(cost_m_1000s_bridge = cost_m_1000s_bridge * 10) %>%
  rename(
    Class = my_road_class,
    Surface = my_road_surface,
    `Class Multiplier` = road_class_mult,
    `Surface Multiplier` = road_surface_mult,
    `Bridge $K/10m` = cost_m_1000s_bridge,
    `Streambed Simulation $K` = cost_embed_cv
  ) %>%
  dplyr::filter(!is.na(Class)) %>%
  mutate(Class =case_when(
    Class == 'fsr' ~ str_to_upper(Class),
    T ~ stringr::str_to_title(Class)),
    Surface = stringr::str_to_title(Surface)
  )


# we are not doing this right now becasue we have PSCIS Ids for everything
# pscis_rd <- left_join(
#   rd_class_surface,
#   xref_pscis_my_crossing_modelled,
#   by = c('my_crossing_reference' = 'external_crossing_reference')
# ) %>%
#   mutate(stream_crossing_id = as.numeric(stream_crossing_id)) %>% #should be able to remove this after we have the data in?
#   mutate(pscis_crossing_id = case_when(!is.na(stream_crossing_id) ~ stream_crossing_id,
#                                        T ~ pscis_crossing_id)) %>%
#   select(-stream_crossing_id)
#   # dplyr::filter(distance < 100)
#  HACK bottom hashout for now!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# priorities phase 1 ------------------------------------------------------
##uses habitat value to initially screen but then refines based on what are likely not barriers to most most the time
phase1_priorities <- pscis_all %>%
  dplyr::filter(!source %ilike% 'phase2') %>%
  select(aggregated_crossings_id, pscis_crossing_id, my_crossing_reference, utm_zone:northing, habitat_value, barrier_result, source) %>%
  mutate(priority_phase1 = case_when(habitat_value == 'High' & barrier_result != 'Passable' ~ 'high',
                                     habitat_value == 'Medium' & barrier_result != 'Passable' ~ 'mod',
                                     habitat_value == 'Low' & barrier_result != 'Passable' ~ 'low',
                                     T ~ NA_character_)) %>%
  mutate(priority_phase1 = case_when(habitat_value == 'High' & barrier_result == 'Potential' ~ 'mod',
                                     T ~ priority_phase1)) %>%
  mutate(priority_phase1 = case_when(habitat_value == 'Medium' & barrier_result == 'Potential' ~ 'low',
                                     T ~ priority_phase1)) %>%
  # mutate(priority_phase1 = case_when(my_crossing_reference == 99999999999 ~ 'high', ##this is where we can make changes to the defaults
  #                                    T ~ priority_phase1)) %>%
  dplyr::rename(utm_easting = easting, utm_northing = northing)


##turn spreadsheet into list of data frames
pscis_phase1_for_tables <- pscis_all %>%
  dplyr::filter(source %ilike% 'phase1') %>%
  arrange(pscis_crossing_id)


pscis_split <- pscis_phase1_for_tables  %>% #pscis_phase1_reassessments
  # sf::st_drop_geometry() %>%
  # mutate_if(is.numeric, as.character) %>% ##added this to try to get the outlet drop to not disapear
  # tibble::rownames_to_column() %>%
  dplyr::group_split(pscis_crossing_id) %>%
  purrr::set_names(pscis_phase1_for_tables$pscis_crossing_id)

##make result summary tables for each of the crossings
tab_summary <- pscis_split %>%
  purrr::map(fpr::fpr_table_cv_detailed)

tab_summary_comments <- pscis_split %>%
  purrr::map(fpr::fpr_table_cv_detailed_comments)

##had a hickup where R cannot handle the default size of the integers we used for numbers so we had to change site names!!
tab_photo_url <- list.files(path = paste0(getwd(), '/data/photos/'), full.names = T) %>%
  basename() %>%
  as_tibble() %>%
  mutate(value = as.integer(value)) %>%  ##need this to sort
  dplyr::arrange(value)  %>%
  mutate(photo = paste0('![](data/photos/', value, '/crossing_all.JPG)')) %>%
  dplyr::filter(value %in% pscis_phase1_for_tables$my_crossing_reference)  %>% ##we don't want all the photos - just the phase 1 photos for this use case!!!
  left_join(., xref_pscis_my_crossing_modelled, by = c('value' = 'external_crossing_reference'))  %>% ##we need to add the pscis id so that we can sort the same
  arrange(stream_crossing_id) %>%
  select(-value) %>%
  # pull(photo)
  dplyr::group_split(stream_crossing_id)
  # purrr::set_names(nm = . %>% bind_rows() %>% arrange(value) %>% pull(stream_crossing_id)) %>%
  # bind_rows()
  # arrange(stream_crossing_id) %>%
  # dplyr::group_split(value)


# html tables
tabs_phase1 <- mapply(
  fpr::fpr_table_cv_detailed_print,
  tab_sum = tab_summary,
  comments = tab_summary_comments,
  photos = tab_photo_url)


# html tables for the pdf version
# tabs_phase1_pdf <- mapply(
#   fpr::fpr_table_cv_detailed_print,
#   tab_sum = tab_summary,
#   comments = tab_summary_comments,
#   photos = tab_photo_url,
#   gitbook_switch = FALSE
#   ) %>%
#   head()
# fpr_print_tab_summary_all_pdf <- function(tab_sum, comments, photos){
#   kable(tab_sum, booktabs = T) %>%
#     kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
#     kableExtra::add_footnote(label = paste0('Comments: ', comments[[1]]), notation = 'none') %>% #this grabs the comments out
#     kableExtra::add_footnote(label = paste0('Photos: PSCIS ID ', photos[[2]],
#                                             '. From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
#                                             photos[[1]]), notation = 'none') %>%
#     kableExtra::add_footnote(label = '<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>', escape = F, notation = 'none')
# }

tabs_phase1_pdf <- mapply(
  fpr::fpr_table_cv_detailed_print,
  tab_sum = tab_summary,
  comments = tab_summary_comments,
  photos = tab_photo_url,
  gitbook_switch = FALSE)

# tabs_phase1_pdf <- mapply(fpr_print_tab_summary_all_pdf, tab_sum = tab_summary, comments = tab_summary_comments, photos = tab_photo_url)

####-------------- habitat and fish data------------------
habitat_confirmations <- fpr_import_hab_con(col_filter_na = T, row_empty_remove = T)

hab_site_prep <-  habitat_confirmations %>%
  purrr::pluck("step_4_stream_site_data") %>%
  # tidyr::separate(local_name, into = c('site', 'location'), remove = F) %>%
  mutate(average_gradient_percent = round(average_gradient_percent * 100, 1)) %>%
  mutate_if(is.numeric, round, 1) %>%
  select(-gazetted_names:-site_number, -feature_type:-utm_method) %>%   ##remove the feature utms so they don't conflict with the site utms
  distinct(reference_number, .keep_all = T) ##since we have features we need to filter them out


hab_loc <- habitat_confirmations %>%
  purrr::pluck("step_1_ref_and_loc_info") %>%
  dplyr::filter(!is.na(site_number))%>%
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date)))

hab_site <- left_join(
  hab_loc,
  hab_site_prep,
  by = 'reference_number'
) %>%
  tidyr::separate(alias_local_name, into = c('site', 'location'), remove = F) %>%
  mutate(site = as.numeric(site)) %>%
  dplyr::filter(!alias_local_name %like% '_ef') ##get rid of the ef sites

hab_fish_collect_map_prep <- habitat_confirmations %>%
  purrr::pluck("step_2_fish_coll_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(site_id = paste0(site, location)) %>%
  distinct(local_name, species, .keep_all = T) %>% ##changed this to make it work as a feed for the extract-fish.R file
  mutate(across(c(date_in,date_out), janitor::excel_numeric_to_date)) %>%
  mutate(across(c(time_in,time_out), chron::times))


##prep the location info so it is ready to join to the fish data
hab_loc2 <- hab_loc %>%
  tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(site_id = paste0(site, location)) %>%
  dplyr::filter(str_detect(alias_local_name, 'ef|198222|mt')) ##filter ef and mt sites


# test to see what we get at each site
test <- hab_fish_collect_map_prep %>%
  distinct(local_name, species)



##join the tables together
hab_fish_collect_map_prep2 <- right_join(
  # distinct to get rid of lots of sites
  select(hab_loc2, reference_number, alias_local_name, utm_zone:utm_northing) %>% distinct(alias_local_name, .keep_all = T),
  select(hab_fish_collect_map_prep %>% distinct(local_name, species, .keep_all = T), local_name, species),
  by = c('alias_local_name' = 'local_name')
)


##add the species code
hab_fish_codes <- fishbc::freshwaterfish %>%
  select(species_code = Code, common_name = CommonName) %>%
  tibble::add_row(species_code = 'NFC', common_name = 'No Fish Caught') %>%
  mutate(common_name = case_when(common_name == 'Cutthroat Trout' ~ 'Cutthroat Trout (General)', T ~ common_name))

# this is the table to burn to geojson for mapping
# we are just going to keep 1 site for upstream and downstream because more detail won't show well on the map anyway
# purpose is to show which fish are there vs. show all the sites and what was caught at each. TMI

hab_fish_collect_map_prep3 <- left_join(
  hab_fish_collect_map_prep2 %>%
    mutate(species = as.factor(species)),  ##just needed to do this b/c there actually are no fish.

  select(hab_fish_codes, common_name, species_code),
  by = c('species' = 'common_name')
)
  # this time we ditch the nfc because we don't want it to look like sites are non-fish bearing.  Its a multipass thing
  # WATCH THIS IN THE FUTURE
  # dplyr::filter(species_code != 'NFC')

# need to make an array for mapping the hab_fish_collect files
# this gives a list column vs an array.  prob easier to use postgres and postgis to make the array
hab_fish_collect <- left_join(
  hab_fish_collect_map_prep3 %>%
    select(alias_local_name:utm_northing) %>%
    distinct(),

  hab_fish_collect_map_prep3 %>%
    select(-species, -reference_number, -utm_zone:-utm_northing) %>%
    pivot_wider(names_from = 'alias_local_name', values_from = "species_code") %>%
    pivot_longer(cols = '8530_ds_ef2':'8525_ds_mt') %>%
    rename(alias_local_name = name,
             species_code = value),

    by = 'alias_local_name'
) %>%
  rowwise() %>%
  mutate(species_code = toString(species_code),
         species_code = stringr::str_replace_all(species_code, ',', ''))


rm(hab_fish_collect_map_prep, hab_fish_collect_map_prep2)

hab_fish_collect_prep1 <- habitat_confirmations %>%
  purrr::pluck("step_2_fish_coll_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  select(-gazetted_name:-site_number)

hab_features <- left_join(
  habitat_confirmations %>%
    purrr::pluck("step_4_stream_site_data") %>%
    select(reference_number,local_name, feature_type:utm_northing) %>%
    dplyr::filter(!is.na(feature_type)),

  fpr::fpr_xref_obstacles,

  by = c('feature_type' = 'spreadsheet_feature_type')
)


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
    species_code %in% c('L', 'SU', 'LSU') ~ NA_character_,
    T ~ life_stage
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
  dplyr::filter(sampling_method == 'electrofishing') %>%
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
  # dplyr::filter(nfc_pass == T) %>%
  distinct(local_name, species_code, life_stage, .keep_all = T) %>%
  select(-haul_number_pass_number, -pass_total)

# dat to show sites  for those that have a pass where no fish of those species were captured
# nfc_pass tag used to indicate that this is an abundance estimate
# fish_nfc_tag <- left_join(
#   fish_abund_prep2,
#
#   fish_nfc_prep,
#   by = c('local_name','species_code', 'life_stage', 'haul_number_pass_number', 'pass_total')
# ) %>%
#   tidyr::fill(nfc_pass, .direction = 'up')

  # dplyr::filter(!is.na(nfc_pass)) %>%

  # mutate(nfc_pass = case_when(
  #   species_code != 'NFC' ~ 'TRUE',
  #   T ~ NA_character_))


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


### depletion estimates -----------------------------------------------------

# only run depletion estimates when there are site/species events with more than 1 pass and all passes have fish.
# otherwise just add the counts together for the abundance
# fish_deplet_prep <- fish_abund_prep3 %>%
#   dplyr::filter(is.na(abundance)) %>%
#   # at least three passes
#   group_by(id) %>%
#   dplyr::filter( n() > 2 )
#
#
# fish_abund_prep_ls <-  fish_deplet_prep %>%
#   ungroup() %>%
#   dplyr::group_split(id) %>%
#   purrr::set_names(nm = unique(fish_deplet_prep$id))
#
# fpr_fish_depletion <- function(dat, ...){
#   ls <- FSA::depletion(dat$catch, dat$ef_seconds, Ricker.mod = F)
#   out <-  summary(ls) %>%
#       as_tibble() %>%
#       slice(1)
#   out
#   }
#
#
# fish_abund_prep_ls %>%
#   purrr::map(fpr_fish_depletion) %>%
#   map(plot)
#
# fish_abund_calc <- fish_abund_prep_ls %>%
#   purrr::map(fpr_fish_depletion) %>%
#   bind_rows(.id = 'id') %>%
#   janitor::clean_names()

# well 2 events did not have a negative slope and 12 more were suspect as model
#  did not show a significant slope so this is crap.


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
) |>
  mutate(area_m2 = round(ef_length_m * ef_width_m,1)) %>%
  select(site = local_name, passes = pass_total, ef_length_m, ef_width_m, area_m2, enclosure)

rm(
  fish_abund_nfc_prep,
  fish_abund_prep,
  fish_abund_prep2,
  fish_abund_prep3,
  fish_abund_prep4,
  fish_nfc_tag
)


# # table to summarize ef passes done in a site
# tab_fish_sites <- hab_fish_collect_info %>%
#   select(local_name, haul_number_pass_number, ef_seconds:enclosure) %>%
#   distinct() %>%
#   mutate(area_m2 = round(ef_length_m * ef_width_m,1)) %>%
#   tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F)



# hab_fish_dens <- hab_fish_indiv %>%
#   dplyr::filter(sampling_method == 'electrofishing') %>% ##added this since we now have mt data as well!!
#   mutate(area = round(ef_length_m * ef_width_m),0) %>%
#   group_by(local_name, method_number, haul_number_pass_number, ef_length_m, ef_width_m, ef_seconds, area, species_code, life_stage) %>%
#   summarise(fish_total = length(life_stage)) %>%
#   ungroup() %>%
#   mutate(density_100m2 = round(fish_total/area * 100, 1)) %>%
#   tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) %>%
#   mutate(site_id = paste0(site, location),
#          location = case_when(location == 'us' ~ 'Upstream',
#                               T ~ 'Downstream'),
#          life_stage = factor(life_stage, levels = c('fry', 'parr', 'juvenile', 'adult')))

# priorities phase 2--------------------------------------------------------------
#load priorities
habitat_confirmations_priorities <- readr::read_csv(
  file = "./data/habitat_confirmations_priorities.csv",
  #this is not necessary but we will leave.
  locale = readr::locale(encoding = "UTF-8")) %>%
  dplyr::filter(!alias_local_name %like% 'ef' &
           ##ditch the ef sites and the toboggan site that is passable
           !alias_local_name %like% '198042') %>% ##ditch the ef sites and the toboggan site that is passable
  # tidyr::separate(local_name, into = c('site', 'location'), remove = F) %>%
  mutate(upstream_habitat_length_km = round(upstream_habitat_length_m/1000,1)) %>%
  rename(local_name = alias_local_name) #did this to stay consistent for later



hab_site_priorities_prep <- left_join(
  select(habitat_confirmations_priorities, reference_number, local_name, priority),
  select(hab_site, reference_number, alias_local_name, site, utm_zone:utm_northing),
  by = 'reference_number'
) %>%
  dplyr::filter(!local_name %like% '_ds' &
           # ends in a number
           !local_name %like% '\\d$') %>%
  select(-local_name) %>%
  dplyr::filter(!is.na(priority))  ##this is how we did it before.  changed it to get a start on it

hab_site_priorities <- left_join(
  hab_site_priorities_prep %>%
    tidyr::separate(alias_local_name, into = c('alias_local_name', 'location'), remove = T),

  pscis_phase2 %>% select(pscis_crossing_id, barrier_result) %>% mutate(pscis_crossing_id = as.character(pscis_crossing_id)),

  by = c('alias_local_name' = 'pscis_crossing_id')
) %>%
  select(-location)


# bcfishpass modelling table setup for reporting --------------------------


# When we need to update our column names according to the new output from bcfishpass.crossings...
bcfishpass_names_updated_prep <- names(bcfishpass) %>%
  tibble::as_tibble() %>%
  rename(bcfishpass = value)

# join to the comments
bcfishpass_names_updated <- left_join(
  bcfishpass_names_updated_prep,
  bcfishpass_column_comments,
  by = c('bcfishpass' = 'column_name')
)



#this is how we line up our new column names and put things in order for reporting on the fish habitat modeling
# we need to update this sometimes.  When we do we update 02_prep_reporting/0160-load-bcfishpass-data.R,
# get the data from  rename the xref_bcfishpass_names tribble to xref_bcfishpass_names_old  and go through the following procedure
# xref_bcfishpass_names_old <- xref_bcfishpass_names


# ## join the new with the old so you can kable(xref_bcfishpass_names_prep) then run in Rmd chunk and copy paste tribble yo
# xref_bcfishpass_names_prep <- left_join(
#   bcfishpass_names_updated,
#   xref_bcfishpass_names_old, #, -column_comment
#   by = c('bcfishpass')
# ) %>%
#     mutate(report = stringr::str_replace_all(bcfishpass, '_', ' ') %>%
#              stringr::str_to_title() %>%
#              stringr::str_replace_all('Km', '(km)') %>%
#              stringr::str_replace_all('Ha', '(ha)') %>%
#              stringr::str_replace_all('Lakereservoir', 'Lake Reservoir') %>%
#              stringr::str_replace_all('Co ', 'CO ') %>%
#              stringr::str_replace_all('Ch', 'CH ') %>%
#              stringr::str_replace_all('St ', 'ST ') %>%
#              stringr::str_replace_all('Sk ', 'SK ') %>%
#              stringr::str_replace_all('Bt ', 'BT ') %>%
#              stringr::str_replace_all('Wct ', 'WCT ') %>%
#              stringr::str_replace_all('Pscis', 'PSCIS') %>%
#              stringr::str_replace_all('Spawningrearing', 'Spawning Rearing') %>%
#              stringr::str_replace_all('Betweenbarriers', 'Between Barriers') %>%
#              stringr::str_replace_all('Belowupstrbarriers', 'Below Barriers')) %>%
#   select(bcfishpass, report, id_join, id_side, column_comment)

xref_bcfishpass_names <- tibble::tribble(
                                                    ~bcfishpass,                                                        ~report, ~id_join, ~id_side,                                                                                                                                                                                                                                          ~column_comment,
                                      "aggregated_crossings_id",                                      "Aggregated Crossings Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                            "all_rearing_belowupstrbarriers_km",                              "All Rearing Below Barriers (km)",     NA,       NA,                                                                                                             "Length of stream upstream of point and below any additional upstream barriers, modelled as potential rearing habitat (all CH,CO,SK,ST,WCT)",
                                               "all_rearing_km",                                             "All Rearing (km)",     NA,       NA,                                                                          "Length of stream upstream of point and below any additional upstream barriers, modelled as potential spawning habitat for all modelled species (currently BT,CH,CO,SK,ST,WCT)",
                           "all_spawning_belowupstrbarriers_km",                             "All Spawning Below Barriers (km)",     NA,       NA,                                                                                                                       "Length of stream upstream of point modelled as potential rearing habitat for all modelled species (currently BT,CH,CO,SK,ST,WCT)",
                                              "all_spawning_km",                                            "All Spawning (km)",     NA,       NA,                                                                                                                      "Length of stream upstream of point modelled as potential spawning habitat for all modelled species (currently BT,CH,CO,SK,ST,WCT)",
                    "all_spawningrearing_belowupstrbarriers_km",                     "All Spawning Rearing Below Barriers (km)",     NA,       NA,                                                                                                                                                                                           "Length of all spawning and rearing habitat upstream of point",
                                       "all_spawningrearing_km",                                    "All Spawning Rearing (km)",     NA,       NA,                                                                                                                                                                                           "Length of all spawning and rearing habitat upstream of point",
                              "all_spawningrearing_per_barrier",                             "All Spawning Rearing Per Barrier",       NA,       NA, "If the given barrier and all barriers downstream were remediated, the amount of connected spawning/rearing habitat that would be added, per barrier. (ie the sum of all_spawningrearing_belowupstrbarriers_km for all barriers, divided by n barriers)",
                                               "barrier_status",                                               "Barrier Status",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                 "barriers_anthropogenic_dnstr",                                 "Barriers Anthropogenic Dnstr",       NA,       NA,                                                                                                                                  "List of the aggregated_crossings_id values of barrier crossings downstream of the given crossing, in order downstream",
                           "barriers_anthropogenic_dnstr_count",                           "Barriers Anthropogenic Dnstr Count",       NA,       NA,                                                                                                                                                                                      "A count of the barrier crossings downstream of the given crossing",
                                 "barriers_anthropogenic_upstr",                                 "Barriers Anthropogenic Upstr",       NA,       NA,                                                                                                                                                         "List of the aggregated_crossings_id values of barrier crossings upstream of the given crossing",
                           "barriers_anthropogenic_upstr_count",                           "Barriers Anthropogenic Upstr Count",       NA,       NA,                                                                                                                                                                                        "A count of the barrier crossings upstream of the given crossing",
                                            "barriers_bt_dnstr",                                            "Barriers BT Dnstr",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                "barriers_ch_cm_co_pk_sk_dnstr",                               "Barriers CH  Cm CO Pk SK Dnstr",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                            "barriers_st_dnstr",                                            "Barriers ST Dnstr",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                           "barriers_wct_dnstr",                                           "Barriers WCT Dnstr",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                "blue_line_key",                                                "Blue Line Key",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                       "bt_belowupstrbarriers_lakereservoir_ha",                        "BT Below Barriers Lake Reservoir (ha)",       NA,       NA,                                                                                                                  "Bull Trout model, total area lakes and reservoirs potentially accessible upstream of point and below any additional upstream barriers",
                             "bt_belowupstrbarriers_network_km",                               "BT Below Barriers Network (km)",       NA,       NA,                                                                                                                   "Bull Trout model, total length of stream network potentially accessible upstream of point and below any additional upstream barriers",
                        "bt_belowupstrbarriers_slopeclass03_km",                          "BT Below Barriers Slopeclass03 (km)",       NA,       NA,                                                                                                                "Bull Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
            "bt_belowupstrbarriers_slopeclass03_waterbodies_km",              "BT Below Barriers Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                    "Bull Trout model, length of stream connectors (in waterbodies) potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                        "bt_belowupstrbarriers_slopeclass05_km",                          "BT Below Barriers Slopeclass05 (km)",       NA,       NA,                                                                                                                "Bull Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 3-5%",
                        "bt_belowupstrbarriers_slopeclass08_km",                          "BT Below Barriers Slopeclass08 (km)",       NA,       NA,                                                                                                                "Bull Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 5-8%",
                        "bt_belowupstrbarriers_slopeclass15_km",                          "BT Below Barriers Slopeclass15 (km)",       NA,       NA,                                                                                                               "Bull Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 8-15%",
                        "bt_belowupstrbarriers_slopeclass22_km",                          "BT Below Barriers Slopeclass22 (km)",       NA,       NA,                                                                                                              "Bull Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 15-22%",
                        "bt_belowupstrbarriers_slopeclass30_km",                          "BT Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                                              "Bull Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 22-30%",
                              "bt_belowupstrbarriers_stream_km",                                "BT Below Barriers Stream (km)",       NA,       NA,                                                            "Bull Trout model, total length of streams and rivers potentially accessible upstream of point and below any additional upstream barriers (does not include network connectors in lakes etc)",
                             "bt_belowupstrbarriers_wetland_ha",                               "BT Below Barriers Wetland (ha)",       NA,       NA,                                                                                                                              "Bull Trout model, total area wetlands potentially accessible upstream of point and below any additional upstream barriers",
                                          "bt_lakereservoir_ha",                                       "BT Lake Reservoir (ha)",       NA,       NA,                                                                                                                                                            "Bull Trout model, total area lakes and reservoirs potentially accessible upstream of point ",
                                                "bt_network_km",                                              "BT Network (km)",       NA,       NA,                                                                                                                                                              "Bull Trout model, total length of stream network potentially accessible upstream of point",
                             "bt_rearing_belowupstrbarriers_km",                               "BT Rearing Below Barriers (km)",       NA,       NA,                                                                                                                        "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Bull Trout rearing habitat",
                                                "bt_rearing_km",                                              "BT Rearing (km)",       NA,       NA,                                                                                                                                                                    "Length of stream upstream of point modelled as potential Bull Trout rearing habitat",
                                           "bt_slopeclass03_km",                                         "BT Slopeclass03 (km)",       NA,       NA,                                                                                                                                                            "Bull Trout model, length of stream potentially accessible upstream of point with slope 0-3%",
                               "bt_slopeclass03_waterbodies_km",                             "BT Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                                                "Bull Trout model, length of stream connectors (in waterbodies) potentially accessible upstream of point with slope 0-3%",
                                           "bt_slopeclass05_km",                                         "BT Slopeclass05 (km)",       NA,       NA,                                                                                                                                                            "Bull Trout model, length of stream potentially accessible upstream of point with slope 3-5%",
                                           "bt_slopeclass08_km",                                         "BT Slopeclass08 (km)",       NA,       NA,                                                                                                                                                            "Bull Trout model, length of stream potentially accessible upstream of point with slope 5-8%",
                                           "bt_slopeclass15_km",                                         "BT Slopeclass15 (km)",       NA,       NA,                                                                                                                                                           "Bull Trout model, length of stream potentially accessible upstream of point with slope 8-15%",
                                           "bt_slopeclass22_km",                                         "BT Slopeclass22 (km)",       NA,       NA,                                                                                                                                                          "Bull Trout model, length of stream potentially accessible upstream of point with slope 15-22%",
                                           "bt_slopeclass30_km",                                         "BT Slopeclass30 (km)",       NA,       NA,                                                                                                                                                          "Bull Trout model, length of stream potentially accessible upstream of point with slope 22-30%",
                            "bt_spawning_belowupstrbarriers_km",                              "BT Spawning Below Barriers (km)",       NA,       NA,                                                                                                                       "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Bull Trout spawning habitat",
                                               "bt_spawning_km",                                             "BT Spawning (km)",       NA,       NA,                                                                                                                                                                   "Length of stream upstream of point modelled as potential Bull Trout spawning habitat",
                                                 "bt_stream_km",                                               "BT Stream (km)",       NA,       NA,                                                                                                      "Bull Trout model, total length of streams and rivers potentially accessible upstream of point  (does not include network connectors in lakes etc)",
                                                "bt_wetland_ha",                                              "BT Wetland (ha)",       NA,       NA,                                                                                                                                                                        "Bull Trout model, total area wetlands potentially accessible upstream of point ",
           "ch_cm_co_pk_sk_belowupstrbarriers_lakereservoir_ha",           "CH  Cm CO Pk SK Below Barriers Lake Reservoir (ha)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                 "ch_cm_co_pk_sk_belowupstrbarriers_network_km",                  "CH  Cm CO Pk SK Below Barriers Network (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
            "ch_cm_co_pk_sk_belowupstrbarriers_slopeclass03_km",             "CH  Cm CO Pk SK Below Barriers Slopeclass03 (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
"ch_cm_co_pk_sk_belowupstrbarriers_slopeclass03_waterbodies_km", "CH  Cm CO Pk SK Below Barriers Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
            "ch_cm_co_pk_sk_belowupstrbarriers_slopeclass05_km",             "CH  Cm CO Pk SK Below Barriers Slopeclass05 (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
            "ch_cm_co_pk_sk_belowupstrbarriers_slopeclass08_km",             "CH  Cm CO Pk SK Below Barriers Slopeclass08 (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
            "ch_cm_co_pk_sk_belowupstrbarriers_slopeclass15_km",             "CH  Cm CO Pk SK Below Barriers Slopeclass15 (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
            "ch_cm_co_pk_sk_belowupstrbarriers_slopeclass22_km",             "CH  Cm CO Pk SK Below Barriers Slopeclass22 (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
            "ch_cm_co_pk_sk_belowupstrbarriers_slopeclass30_km",             "CH  Cm CO Pk SK Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                  "ch_cm_co_pk_sk_belowupstrbarriers_stream_km",                   "CH  Cm CO Pk SK Below Barriers Stream (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                 "ch_cm_co_pk_sk_belowupstrbarriers_wetland_ha",                  "CH  Cm CO Pk SK Below Barriers Wetland (ha)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                              "ch_cm_co_pk_sk_lakereservoir_ha",                          "CH  Cm CO Pk SK Lake Reservoir (ha)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                    "ch_cm_co_pk_sk_network_km",                                 "CH  Cm CO Pk SK Network (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                               "ch_cm_co_pk_sk_slopeclass03_km",                            "CH  Cm CO Pk SK Slopeclass03 (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                   "ch_cm_co_pk_sk_slopeclass03_waterbodies_km",                "CH  Cm CO Pk SK Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                               "ch_cm_co_pk_sk_slopeclass05_km",                            "CH  Cm CO Pk SK Slopeclass05 (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                               "ch_cm_co_pk_sk_slopeclass08_km",                            "CH  Cm CO Pk SK Slopeclass08 (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                               "ch_cm_co_pk_sk_slopeclass15_km",                            "CH  Cm CO Pk SK Slopeclass15 (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                               "ch_cm_co_pk_sk_slopeclass22_km",                            "CH  Cm CO Pk SK Slopeclass22 (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                               "ch_cm_co_pk_sk_slopeclass30_km",                            "CH  Cm CO Pk SK Slopeclass30 (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                     "ch_cm_co_pk_sk_stream_km",                                  "CH  Cm CO Pk SK Stream (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                    "ch_cm_co_pk_sk_wetland_ha",                                 "CH  Cm CO Pk SK Wetland (ha)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                             "ch_rearing_belowupstrbarriers_km",                              "CH  Rearing Below Barriers (km)",     120L,       2L,                                                                                                                           "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Chinook rearing habitat",
                                                "ch_rearing_km",                                             "CH  Rearing (km)",     120L,       1L,                                                                                                                                                                       "Length of stream upstream of point modelled as potential Chinook rearing habitat",
                            "ch_spawning_belowupstrbarriers_km",                             "CH  Spawning Below Barriers (km)",     110L,       2L,                                                                                                                          "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Chinook spawning habitat",
                                               "ch_spawning_km",                                            "CH  Spawning (km)",     110L,       1L,                                                                                                                                                                      "Length of stream upstream of point modelled as potential Chinook spawning habitat",
                            "cm_spawning_belowupstrbarriers_km",                              "Cm Spawning Below Barriers (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                               "cm_spawning_km",                                             "Cm Spawning (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                             "co_rearing_belowupstrbarriers_ha",                               "CO Rearing Below Barriers (ha)",     145L,       2L,                                                                                                                              "Area of wetlands upstream of point and below any additional upstream barriers, modelled as potential Coho rearing habitat",
                             "co_rearing_belowupstrbarriers_km",                               "CO Rearing Below Barriers (km)",     140L,       2L,                                                                                                                              "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Coho rearing habitat",
                                                "co_rearing_ha",                                              "CO Rearing (ha)",     145L,       1L,                                                                                                                                                                          "Area of wetlands upstream of point modelled as potential Coho rearing habitat",
                                                "co_rearing_km",                                              "CO Rearing (km)",     140L,       1L,                                                                                                                                                                          "Length of stream upstream of point modelled as potential Coho rearing habitat",
                            "co_spawning_belowupstrbarriers_km",                              "CO Spawning Below Barriers (km)",     130L,       2L,                                                                                                                             "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Coho spawning habitat",
                                               "co_spawning_km",                                             "CO Spawning (km)",     130L,       1L,                                                                                                                                                                         "Length of stream upstream of point modelled as potential Coho spawning habitat",
                                        "crossing_feature_type",                                        "Crossing Feature Type",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                              "crossing_source",                                              "Crossing Source",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                        "crossing_subtype_code",                                        "Crossing Subtype Code",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                           "crossing_type_code",                                           "Crossing Type Code",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                              "crossings_dnstr",                                              "Crossings Dnstr",       NA,       NA,                                                                                                                                          "List of the aggregated_crossings_id values of crossings downstream of the given crossing, in order downstream",
                                                   "dam_height",                                                   "Dam Height",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                       "dam_id",                                                       "Dam Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                     "dam_name",                                                     "Dam Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                         "dam_operating_status",                                         "Dam Operating Status",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                    "dam_owner",                                                    "Dam Owner",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                      "dam_use",                                                      "Dam Use",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                             "dbm_mof_50k_grid",                                             "Dbm Mof 50k Grid",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                     "downstream_route_measure",                                     "Downstream Route Measure",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                             "ften_client_name",                                             "Ften Client Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                           "ften_client_number",                                           "Ften Client Number",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                   "ften_file_type_description",                                   "Ften File Type Description",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                          "ften_forest_file_id",                                          "Ften Forest File Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                  "ften_life_cycle_status_code",                                  "Ften Life Cycle Status Code",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                             "gnis_stream_name",                                             "Gnis Stream Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                     "gradient",                                                     "Gradient",       NA,       NA,                                                                                                                                                                                                                                  "Stream slope at point",
                                            "linear_feature_id",                                            "Linear Feature Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                              "localcode_ltree",                                              "Localcode Ltree",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                         "modelled_crossing_id",                                         "Modelled Crossing Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                "modelled_crossing_type_source",                                "Modelled Crossing Type Source",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                            "observedspp_dnstr",                                            "Observedspp Dnstr",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                            "observedspp_upstr",                                            "Observedspp Upstr",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                "ogc_proponent",                                                "Ogc Proponent",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                            "pk_spawning_belowupstrbarriers_km",                              "Pk Spawning Below Barriers (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                               "pk_spawning_km",                                             "Pk Spawning (km)",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                     "pscis_assessment_comment",                                     "PSCIS Assessment Comment",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                        "pscis_assessment_date",                                        "PSCIS Assessment Date",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                            "pscis_final_score",                                            "PSCIS Final Score",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                              "pscis_road_name",                                              "PSCIS Road Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                 "pscis_status",                                                 "PSCIS Status",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                            "pscis_stream_name",                                            "PSCIS Stream Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                   "rail_operator_english_name",                                   "Rail Operator English Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                              "rail_owner_name",                                              "Rail Owner Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                              "rail_track_name",                                              "Rail Track Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                             "sk_rearing_belowupstrbarriers_ha",                               "SK Rearing Below Barriers (ha)",     165L,       2L,                                                                                                                              "Area of lakes upstream of point and below any additional upstream barriers, modelled as potential Sockeye rearing habitat",
                             "sk_rearing_belowupstrbarriers_km",                               "SK Rearing Below Barriers (km)",     160L,       2L,                                                                                                                           "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Sockeye rearing habitat",
                                                "sk_rearing_ha",                                              "SK Rearing (ha)",     165L,       1L,                                                                                                                                                                          "Area of lakes upstream of point modelled as potential Sockeye rearing habitat",
                                                "sk_rearing_km",                                              "SK Rearing (km)",     160L,       1L,                                                                                                                                                                       "Length of stream upstream of point modelled as potential Sockeye rearing habitat",
                            "sk_spawning_belowupstrbarriers_km",                              "SK Spawning Below Barriers (km)",     150L,       2L,                                                                                                                          "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Sockeye spawning habitat",
                                               "sk_spawning_km",                                             "SK Spawning (km)",     150L,       1L,                                                                                                                                                                      "Length of stream upstream of point modelled as potential Sockeye spawning habitat",
                       "st_belowupstrbarriers_lakereservoir_ha",                        "ST Below Barriers Lake Reservoir (ha)",      20L,       2L,                                                                                                                   "Steelhead model, total area lakes and reservoirs potentially accessible upstream of point and below any additional upstream barriers",
                             "st_belowupstrbarriers_network_km",                               "ST Below Barriers Network (km)",      10L,       2L,                                                                                                                    "Steelhead model, total length of stream network potentially accessible upstream of point and below any additional upstream barriers",
                        "st_belowupstrbarriers_slopeclass03_km",                          "ST Below Barriers Slopeclass03 (km)",      50L,       2L,                                                                                                                 "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
            "st_belowupstrbarriers_slopeclass03_waterbodies_km",              "ST Below Barriers Slopeclass03 Waterbodies (km)",      40L,       2L,                                                                                     "Steelhead model, length of stream connectors (in waterbodies) potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                        "st_belowupstrbarriers_slopeclass05_km",                          "ST Below Barriers Slopeclass05 (km)",      60L,       2L,                                                                                                                 "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 3-5%",
                        "st_belowupstrbarriers_slopeclass08_km",                          "ST Below Barriers Slopeclass08 (km)",      70L,       2L,                                                                                                                 "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 5-8%",
                        "st_belowupstrbarriers_slopeclass15_km",                          "ST Below Barriers Slopeclass15 (km)",       NA,       NA,                                                                                                                "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 8-15%",
                        "st_belowupstrbarriers_slopeclass22_km",                          "ST Below Barriers Slopeclass22 (km)",       NA,       NA,                                                                                                               "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 15-22%",
                        "st_belowupstrbarriers_slopeclass30_km",                          "ST Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                                               "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 22-30%",
                              "st_belowupstrbarriers_stream_km",                                "ST Below Barriers Stream (km)",       NA,       NA,                                                             "Steelhead model, total length of streams and rivers potentially accessible upstream of point and below any additional upstream barriers (does not include network connectors in lakes etc)",
                             "st_belowupstrbarriers_wetland_ha",                               "ST Below Barriers Wetland (ha)",      30L,       2L,                                                                                                                               "Steelhead model, total area wetlands potentially accessible upstream of point and below any additional upstream barriers",
                                          "st_lakereservoir_ha",                                       "ST Lake Reservoir (ha)",      20L,       1L,                                                                                                                                                             "Steelhead model, total area lakes and reservoirs potentially accessible upstream of point ",
                                                "st_network_km",                                              "ST Network (km)",      10L,       1L,                                                                                                                                                               "Steelhead model, total length of stream network potentially accessible upstream of point",
                             "st_rearing_belowupstrbarriers_km",                               "ST Rearing Below Barriers (km)",      90L,       2L,                                                                                                                         "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Steelhead rearing habitat",
                                                "st_rearing_km",                                              "ST Rearing (km)",      90L,       1L,                                                                                                                                                                     "Length of stream upstream of point modelled as potential Steelhead rearing habitat",
                                           "st_slopeclass03_km",                                         "ST Slopeclass03 (km)",      50L,       1L,                                                                                                                                                             "Steelhead model, length of stream potentially accessible upstream of point with slope 0-3%",
                               "st_slopeclass03_waterbodies_km",                             "ST Slopeclass03 Waterbodies (km)",      40L,       1L,                                                                                                                                 "Steelhead model, length of stream connectors (in waterbodies) potentially accessible upstream of point with slope 0-3%",
                                           "st_slopeclass05_km",                                         "ST Slopeclass05 (km)",      60L,       1L,                                                                                                                                                             "Steelhead model, length of stream potentially accessible upstream of point with slope 3-5%",
                                           "st_slopeclass08_km",                                         "ST Slopeclass08 (km)",      70L,       1L,                                                                                                                                                             "Steelhead model, length of stream potentially accessible upstream of point with slope 5-8%",
                                           "st_slopeclass15_km",                                         "ST Slopeclass15 (km)",       NA,       NA,                                                                                                                                                            "Steelhead model, length of stream potentially accessible upstream of point with slope 8-15%",
                                           "st_slopeclass22_km",                                         "ST Slopeclass22 (km)",       NA,       NA,                                                                                                                                                           "Steelhead model, length of stream potentially accessible upstream of point with slope 15-22%",
                                           "st_slopeclass30_km",                                         "ST Slopeclass30 (km)",       NA,       NA,                                                                                                                                                           "Steelhead model, length of stream potentially accessible upstream of point with slope 22-30%",
                            "st_spawning_belowupstrbarriers_km",                              "ST Spawning Below Barriers (km)",      80L,       2L,                                                                                                                        "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Steelhead spawning habitat",
                                               "st_spawning_km",                                             "ST Spawning (km)",      80L,       1L,                                                                                                                                                                    "Length of stream upstream of point modelled as potential Steelhead spawning habitat",
                                                 "st_stream_km",                                               "ST Stream (km)",       NA,       NA,                                                                                                       "Steelhead model, total length of streams and rivers potentially accessible upstream of point  (does not include network connectors in lakes etc)",
                                                "st_wetland_ha",                                              "ST Wetland (ha)",      30L,       1L,                                                                                                                                                                         "Steelhead model, total area wetlands potentially accessible upstream of point ",
                                           "stream_crossing_id",                                           "Stream Crossing Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                             "stream_magnitude",                                             "Stream Magnitude",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                 "stream_order",                                                 "Stream Order",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                    "total_belowupstrbarriers_lakereservoir_ha",                     "Total Below Barriers Lake Reservoir (ha)",       NA,       NA,                                                                                                                                    "Total area lakes and reservoirs potentially accessible upstream of point and below any additional upstream barriers",
                          "total_belowupstrbarriers_network_km",                            "Total Below Barriers Network (km)",       NA,       NA,                                                                                                                                     "Total length of stream network potentially accessible upstream of point and below any additional upstream barriers",
                     "total_belowupstrbarriers_slopeclass03_km",                       "Total Below Barriers Slopeclass03 (km)",       NA,       NA,                                                                                                                            "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
         "total_belowupstrbarriers_slopeclass03_waterbodies_km",           "Total Below Barriers Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                "Total length of stream connectors (in waterbodies) potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                     "total_belowupstrbarriers_slopeclass05_km",                       "Total Below Barriers Slopeclass05 (km)",       NA,       NA,                                                                                                                            "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 3-5%",
                     "total_belowupstrbarriers_slopeclass08_km",                       "Total Below Barriers Slopeclass08 (km)",       NA,       NA,                                                                                                                            "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 5-8%",
                     "total_belowupstrbarriers_slopeclass15_km",                       "Total Below Barriers Slopeclass15 (km)",       NA,       NA,                                                                                                                           "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 8-15%",
                     "total_belowupstrbarriers_slopeclass22_km",                       "Total Below Barriers Slopeclass22 (km)",       NA,       NA,                                                                                                                          "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 15-22%",
                     "total_belowupstrbarriers_slopeclass30_km",                       "Total Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                                                          "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 22-30%",
                           "total_belowupstrbarriers_stream_km",                             "Total Below Barriers Stream (km)",       NA,       NA,                                                                              "Total length of streams and rivers potentially accessible upstream of point and below any additional upstream barriers (does not include network connectors in lakes etc)",
                          "total_belowupstrbarriers_wetland_ha",                            "Total Below Barriers Wetland (ha)",       NA,       NA,                                                                                                                                                "Total area wetlands potentially accessible upstream of point and below any additional upstream barriers",
                                       "total_lakereservoir_ha",                                    "Total Lake Reservoir (ha)",       NA,       NA,                                                                                                                                                                                                     "Total area lakes and reservoirs upstream of point ",
                                             "total_network_km",                                           "Total Network (km)",       NA,       NA,                                                                                                                                                                                                       "Total length of stream network upstream of point",
                                        "total_slopeclass03_km",                                      "Total Slopeclass03 (km)",       NA,       NA,                                                                                                                                                                        "Total length of stream potentially accessible upstream of point with slope 0-3%",
                            "total_slopeclass03_waterbodies_km",                          "Total Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                                                            "Total length of stream connectors (in waterbodies) potentially accessible upstream of point with slope 0-3%",
                                        "total_slopeclass05_km",                                      "Total Slopeclass05 (km)",       NA,       NA,                                                                                                                                                                        "Total length of stream potentially accessible upstream of point with slope 3-5%",
                                        "total_slopeclass08_km",                                      "Total Slopeclass08 (km)",       NA,       NA,                                                                                                                                                                        "Total length of stream potentially accessible upstream of point with slope 5-8%",
                                        "total_slopeclass15_km",                                      "Total Slopeclass15 (km)",       NA,       NA,                                                                                                                                                                       "Total length of stream potentially accessible upstream of point with slope 8-15%",
                                        "total_slopeclass22_km",                                      "Total Slopeclass22 (km)",       NA,       NA,                                                                                                                                                                      "Total length of stream potentially accessible upstream of point with slope 15-22%",
                                        "total_slopeclass30_km",                                      "Total Slopeclass30 (km)",       NA,       NA,                                                                                                                                                                      "Total length of stream potentially accessible upstream of point with slope 22-30%",
                                              "total_stream_km",                                            "Total Stream (km)",       NA,       NA,                                                                                                                                                "Total length of streams and rivers upstream of point (does not include network connectors in lakes etc)",
                                             "total_wetland_ha",                                           "Total Wetland (ha)",       NA,       NA,                                                                                                                                                                                                                 "Total area wetlands upstream of point ",
                             "transport_line_structured_name_1",                             "Transport Line Structured Name 1",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                           "transport_line_surface_description",                           "Transport Line Surface Description",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                              "transport_line_type_description",                              "Transport Line Type Description",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                "user_barrier_anthropogenic_id",                                "User Barrier Anthropogenic Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                  "utm_easting",                                                  "Utm Easting",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                 "utm_northing",                                                 "Utm Northing",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                     "utm_zone",                                                     "Utm Zone",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                         "watershed_group_code",                                         "Watershed Group Code",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                "watershed_key",                                                "Watershed Key",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                      "wct_belowupstrbarriers_lakereservoir_ha",                       "WCT Below Barriers Lake Reservoir (ha)",       NA,       NA,                                                                                                   "Westslope Cutthroat Trout model, total area lakes and reservoirs potentially accessible upstream of point and below any additional upstream barriers",
                            "wct_belowupstrbarriers_network_km",                              "WCT Below Barriers Network (km)",       NA,       NA,                                                                                                    "Westslope Cutthroat Trout model, total length of stream network potentially accessible upstream of point and below any additional upstream barriers",
                       "wct_belowupstrbarriers_slopeclass03_km",                         "WCT Below Barriers Slopeclass03 (km)",       NA,       NA,                                                                                                 "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
           "wct_belowupstrbarriers_slopeclass03_waterbodies_km",             "WCT Below Barriers Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                     "Westslope Cutthroat Trout model, length of stream connectors (in waterbodies) potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                       "wct_belowupstrbarriers_slopeclass05_km",                         "WCT Below Barriers Slopeclass05 (km)",       NA,       NA,                                                                                                 "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 3-5%",
                       "wct_belowupstrbarriers_slopeclass08_km",                         "WCT Below Barriers Slopeclass08 (km)",       NA,       NA,                                                                                                 "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 5-8%",
                       "wct_belowupstrbarriers_slopeclass15_km",                         "WCT Below Barriers Slopeclass15 (km)",       NA,       NA,                                                                                                "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 8-15%",
                       "wct_belowupstrbarriers_slopeclass22_km",                         "WCT Below Barriers Slopeclass22 (km)",       NA,       NA,                                                                                               "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 15-22%",
                       "wct_belowupstrbarriers_slopeclass30_km",                         "WCT Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                               "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 22-30%",
                             "wct_belowupstrbarriers_stream_km",                               "WCT Below Barriers Stream (km)",       NA,       NA,                                              "Westslope Cuthroat Trout model, total length of streams and rivers potentially accessible upstream of point and below any additional upstream barriers (does not include network connectors in lakes etc)",
                            "wct_belowupstrbarriers_wetland_ha",                              "WCT Below Barriers Wetland (ha)",       NA,       NA,                                                                                                               "Westslope Cutthroat Trout model, total area wetlands potentially accessible upstream of point and below any additional upstream barriers",
                               "wct_betweenbarriers_network_km",                            "WCT Between Barriers Network (km)",       NA,       NA,                                                                                                            "Westslope Cutthroat Trout model, total length of potentially accessible stream network between crossing and all in-stream adjacent barriers",
                                         "wct_lakereservoir_ha",                                      "WCT Lake Reservoir (ha)",       NA,       NA,                                                                                                                                              "Westslope Cuthroat Trout model, total area lakes and reservoirs potentially accessible upstream of point ",
                                               "wct_network_km",                                             "WCT Network (km)",       NA,       NA,                                                                                                                                                "Westslope Cuthroat Trout model, total length of stream network potentially accessible upstream of point",
                            "wct_rearing_belowupstrbarriers_km",                              "WCT Rearing Below Barriers (km)",       NA,       NA,                                                                                                               "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Westslope Cutthroat rearing habitat",
                               "wct_rearing_betweenbarriers_km",                            "WCT Rearing Between Barriers (km)",       NA,       NA,                                                                                                                                  "Westslope Cutthroat Trout model, total length of rearing habitat between crossing and all in-stream adjacent barriers",
                                               "wct_rearing_km",                                             "WCT Rearing (km)",       NA,       NA,                                                                                                                                                           "Length of stream upstream of point modelled as potential Westslope Cutthroat rearing habitat",
                                          "wct_slopeclass03_km",                                        "WCT Slopeclass03 (km)",       NA,       NA,                                                                                                                                             "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 0-3%",
                              "wct_slopeclass03_waterbodies_km",                            "WCT Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                                 "Westslope Cutthroat Trout model, length of stream connectors (in waterbodies) potentially accessible upstream of point with slope 0-3%",
                                          "wct_slopeclass05_km",                                        "WCT Slopeclass05 (km)",       NA,       NA,                                                                                                                                             "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 3-5%",
                                          "wct_slopeclass08_km",                                        "WCT Slopeclass08 (km)",       NA,       NA,                                                                                                                                             "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 5-8%",
                                          "wct_slopeclass15_km",                                        "WCT Slopeclass15 (km)",       NA,       NA,                                                                                                                                            "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 8-15%",
                                          "wct_slopeclass22_km",                                        "WCT Slopeclass22 (km)",       NA,       NA,                                                                                                                                           "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 15-22%",
                                          "wct_slopeclass30_km",                                        "WCT Slopeclass30 (km)",       NA,       NA,                                                                                                                                           "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 22-30%",
                           "wct_spawning_belowupstrbarriers_km",                             "WCT Spawning Below Barriers (km)",       NA,       NA,                                                                                                              "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Westslope Cutthroat spawning habitat",
                              "wct_spawning_betweenbarriers_km",                           "WCT Spawning Between Barriers (km)",       NA,       NA,                                                                                                                                 "Westslope Cutthroat Trout model, total length of spawning habitat between crossing and all in-stream adjacent barriers",
                                              "wct_spawning_km",                                            "WCT Spawning (km)",       NA,       NA,                                                                                                                                                          "Length of stream upstream of point modelled as potential Westslope Cutthroat spawning habitat",
                       "wct_spawningrearing_betweenbarriers_km",                   "WCT Spawning Rearing Between Barriers (km)",       NA,       NA,                                                                                                                     "Westslope Cutthroat Trout model, total length of spawning and rearing habitat between crossing and all in-stream adjacent barriers",
                                                "wct_stream_km",                                              "WCT Stream (km)",       NA,       NA,                                                                                        "Westslope Cuthroat Trout model, total length of streams and rivers potentially accessible upstream of point  (does not include network connectors in lakes etc)",
                                               "wct_wetland_ha",                                             "WCT Wetland (ha)",       NA,       NA,                                                                                                                                                          "Westslope Cuthroat Trout model, total area wetlands potentially accessible upstream of point ",
                                                 "wscode_ltree",                                                 "Wscode Ltree",       NA,       NA,                                                                                                                                                                                                                                                       NA

  )

####-----------overview table------------

tab_overview_prep1 <- pscis_phase2 %>%
  select(pscis_crossing_id, stream_name, road_name, road_tenure, easting, northing, habitat_value)

tab_overview_prep2 <- habitat_confirmations_priorities %>%
  dplyr::filter(location == 'us') %>%
  select(site, species_codes, upstream_habitat_length_m, priority, comments) %>%
  mutate(upstream_habitat_length_km = round(upstream_habitat_length_m/1000,1))

tab_overview <- left_join(
  tab_overview_prep1,
  tab_overview_prep2,
  by = c('pscis_crossing_id' = 'site')
) %>%
  mutate(utm = paste0(round(easting,0), ' ', round(northing,0))) %>%
  select(`PSCIS ID` = pscis_crossing_id,
         Stream = stream_name,
         Road = road_name,
         Tenure = road_tenure,
         `UTM (11U)` = utm,
         `Fish Species` = species_codes,
         `Habitat Gain (km)` = upstream_habitat_length_km,
         `Habitat Value` = habitat_value,
         Priority = priority,
         Comments = comments )
# mutate(test = paste0('[', Site, ']', '(Appendix 1 - Site Assessment Data and Photos)'))##hmm.. thought this worked
# %>%
#   replace(., is.na(.), "-")


rm(tab_overview_prep1, tab_overview_prep2)

####---------habitat summary--------------------------------

tab_hab_summary <- left_join(
  hab_site %>%
    #dplyr::filter out minnow trap sites because we did not do habitat surveys on these
    dplyr::filter(!alias_local_name %like% 'mt') %>%
    select(alias_local_name,
           site,
           location,
           avg_channel_width_m,
           avg_wetted_width_m,
           average_residual_pool_depth_m,
           average_gradient_percent,
           total_cover),

  habitat_confirmations_priorities %>%
    select(local_name,
           # site,
           # location,
           length_surveyed,
           hab_value),

  by = c('alias_local_name' = 'local_name') #c('site', 'location')
) %>%
  # mutate(location = case_when(
  #   location %ilike% 'us' ~ 'Upstream',
  #   T ~ 'Downstream'
  # )) %>%
  mutate(location = case_when(
    location %ilike% 'us' ~ stringr::str_replace_all(location, 'us', 'Upstream'),
    T ~ stringr::str_replace_all(location, 'ds', 'Downstream')
    )) %>%
  arrange(site, location) %>%
  select(Site = site,
         Location = location,
         `Length Surveyed (m)` = length_surveyed,
         `Channel Width (m)` = avg_channel_width_m,
         `Wetted Width (m)` = avg_wetted_width_m,
         `Pool Depth (m)` = average_residual_pool_depth_m,
         `Gradient (%)` = average_gradient_percent,
         `Total Cover` = total_cover,
         `Habitat Value` = hab_value)


# cost estimates ----------------------------------------------------------

## phase1 --------------------
#make the cost estimates
# HACK !!!!!!!!!!!!!!!!!!!!!!! turned off all cost estimate data for now


# # need to add the crossing fix, filter for our sites and customize for the waterfall sites
rd_class_surface <-  left_join(

  pscis_all %>%
    select(
      pscis_crossing_id,
      my_crossing_reference,
      aggregated_crossings_id,
      stream_name,
      road_name,
      downstream_channel_width_meters,
      barrier_result,
      fill_depth_meters,
      crossing_fix,
      habitat_value,
      recommended_diameter_or_span_meters,
      source),

  rd_class_surface_prep,

  by = c('pscis_crossing_id' = 'stream_crossing_id')
)
#   # here are the custom changes
#

tab_cost_est_prep <- left_join(
  rd_class_surface,
  select(tab_cost_rd_mult, my_road_class, my_road_surface, cost_m_1000s_bridge, cost_embed_cv),
  by = c('my_road_class','my_road_surface')
)

tab_cost_est_prep2 <- left_join(
  tab_cost_est_prep,
  select(fpr_xref_fix, crossing_fix, crossing_fix_code),
  by = c('crossing_fix')
) %>%
  mutate(cost_est_1000s = case_when(
    crossing_fix_code == 'SS-CBS' ~ cost_embed_cv,
    crossing_fix_code == 'OBS' ~ cost_m_1000s_bridge * recommended_diameter_or_span_meters)
  ) %>%
  mutate(cost_est_1000s = round(cost_est_1000s, 0))



##add in the model data.  This is a good reason for the data to be input first so that we can use the net distance!!
tab_cost_est_prep3 <- left_join(
  tab_cost_est_prep2,
  bcfishpass %>%
    select(stream_crossing_id,
           st_network_km,
           st_belowupstrbarriers_network_km),
  by = c('pscis_crossing_id' = 'stream_crossing_id')
) %>%
  mutate(cost_net = round(st_belowupstrbarriers_network_km * 1000/cost_est_1000s, 1),
         cost_gross = round(st_network_km * 1000/cost_est_1000s, 1),
         cost_area_net = round((st_belowupstrbarriers_network_km * 1000 * downstream_channel_width_meters * 0.5)/cost_est_1000s, 1), ##this is a triangle area!
         cost_area_gross = round((st_network_km * 1000 * downstream_channel_width_meters * 0.5)/cost_est_1000s, 1)) ##this is a triangle area!


# # ##add the xref stream_crossing_id
tab_cost_est_prep4 <- left_join(
  tab_cost_est_prep3,
  xref_pscis_my_crossing_modelled,
  by = c('my_crossing_reference' = 'external_crossing_reference')
) %>%
  mutate(stream_crossing_id = case_when(
    is.na(stream_crossing_id) ~ as.integer(pscis_crossing_id),
    T ~ stream_crossing_id
  ))

##add the priority info
tab_cost_est_phase1_prep <- left_join(
  phase1_priorities %>% select(pscis_crossing_id,
                               priority_phase1),
  tab_cost_est_prep4,
  by = 'pscis_crossing_id'
) %>%
  arrange(pscis_crossing_id) %>%
  select(pscis_crossing_id,
         my_crossing_reference,
         my_crossing_reference,
         stream_name,
         road_name,
         barrier_result,
         habitat_value,
         downstream_channel_width_meters,
         priority_phase1,
         crossing_fix_code,
         cost_est_1000s,
         st_network_km,
         cost_gross, cost_area_gross, source) %>%
  dplyr::filter(barrier_result != 'Unknown' & barrier_result != 'Passable')

# too_far_away <- tab_cost_est %>% filter(distance > 100) %>% ##after review all crossing match!!!!! Baren rail is the hwy but that is fine. added source, distance, crossing_id above
#   filter(source %like% 'phase2')

tab_cost_est_phase1 <- tab_cost_est_phase1_prep %>%
  rename(
    `PSCIS ID` = pscis_crossing_id,
    `External ID` = my_crossing_reference,
    Priority = priority_phase1,
    Stream = stream_name,
    Road = road_name,
    Result = barrier_result,
    `Habitat value` = habitat_value,
    `Stream Width (m)` = downstream_channel_width_meters,
    Fix = crossing_fix_code,
    `Cost Est ( $K)` =  cost_est_1000s,
    `Habitat Upstream (km)` = st_network_km,
    `Cost Benefit (m / $K)` = cost_gross,
    `Cost Benefit (m2 / $K)` = cost_area_gross) %>%
  dplyr::filter(!source %like% 'phase2') %>%
  select(-source)

## phase2 --------------------
tab_cost_est_prep4 <- left_join(
  tab_cost_est_prep3,
  select(
    dplyr::filter(habitat_confirmations_priorities, location == 'us'),

    site, upstream_habitat_length_m),

  by = c('pscis_crossing_id' = 'site')
) %>%
  # custom alteration to the cost estimate for 197379 owen trib
  mutate(cost_est_1000s = case_when(
    pscis_crossing_id == 197379 ~ 800,
    T ~ cost_est_1000s
  )) |>
  mutate(cost_net = round(upstream_habitat_length_m * 1000/(cost_est_1000s * 1000), 1),
         cost_area_net = round((upstream_habitat_length_m * 1000 * downstream_channel_width_meters * 0.5)/(cost_est_1000s * 1000), 1)) ##this is a triangle area!


tab_cost_est_prep5 <- left_join(
  tab_cost_est_prep4,
  select(hab_site %>% dplyr::filter(
    !alias_local_name %like% 'ds' &
      !alias_local_name %like% 'ef' &
      !alias_local_name %like% '\\d$'),
    site,
    avg_channel_width_m),
  by = c('pscis_crossing_id' = 'site')
)

##add the priority info
tab_cost_est_phase2 <- tab_cost_est_prep5 %>%
  dplyr::filter(source %like% 'phase2') %>%
  dplyr::filter(barrier_result != 'Unknown' & barrier_result != 'Passable') %>%
  select(pscis_crossing_id,
         stream_name,
         road_name,
         barrier_result,
         habitat_value,
         avg_channel_width_m,
         crossing_fix_code,
         cost_est_1000s,
         upstream_habitat_length_m,
         cost_net,
         cost_area_net,
         source) %>%
  mutate(upstream_habitat_length_m = round(upstream_habitat_length_m,0))

tab_cost_est_phase2_report <- tab_cost_est_phase2 %>%
  dplyr::arrange(pscis_crossing_id) %>%
  # dplyr::filter(source %like% 'phase2') %>%
  rename(`PSCIS ID` = pscis_crossing_id,
         Stream = stream_name,
         Road = road_name,
         Result = barrier_result,
         `Habitat value` = habitat_value,
         `Stream Width (m)` = avg_channel_width_m,
         Fix = crossing_fix_code,
         `Cost Est (in $K)` =  cost_est_1000s,
         `Habitat Upstream (m)` = upstream_habitat_length_m,
         `Cost Benefit (m / $K)` = cost_net,
         `Cost Benefit (m2 / $K)` = cost_area_net) %>%
  select(-source)


rm(tab_cost_est_prep, tab_cost_est_prep2,
   tab_cost_est_prep3, tab_cost_est_prep4, tab_cost_est_prep5)


# map tables --------------------------------------------------------------
hab_loc_prep <- left_join(
  hab_loc %>%
    tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
    dplyr::filter(!alias_local_name %ilike% 'ef' &
             location == 'us') %>%
    mutate(site = as.integer(site)),
  select(dplyr::filter(habitat_confirmations_priorities, location == 'us'),
         site, priority, comments),
  by = 'site'
)


#need to populate the coordinates before this will work
###please note that the photos are only in those files ecause they are referenced in other parts
#of the document
tab_hab_map <- left_join(
  tab_cost_est_phase2 %>% dplyr::filter(source %like% 'phase2'),
  hab_loc_prep %>% select(site, priority, utm_easting, utm_northing, comments),
  by = c('pscis_crossing_id' = 'site')
)  %>%
  sf::st_as_sf(coords = c("utm_easting", "utm_northing"),
               crs = 26909, remove = F) %>%
  sf::st_transform(crs = 4326) %>%
  ##changed this to docs .html from fig .png
  # mutate(data_link = paste0('<a href =',
  #                           'https://github.com/NewGraphEnvironment/fish_passage_bulkley_2020_reporting/tree/master/docs/sum/', pscis_crossing_id,
  #                           '.html', '>', 'data link', '</a>')) %>%
  mutate(data_link = paste0('<a href =', 'sum/cv/', pscis_crossing_id, '.html ', 'target="_blank">Culvert Data</a>')) %>%
  # mutate(photo_link = paste0('<a href =', 'data/photos/', pscis_crossing_id, '/crossing_all.JPG ',
  #                            'target="_blank">Culvert Photos</a>')) %>%
  mutate(model_link = paste0('<a href =', 'sum/bcfp/', pscis_crossing_id, '.html ', 'target="_blank">Model Data</a>')) %>%
  # mutate(photo_link = paste0('<a href =',
  #                            'https://github.com/NewGraphEnvironment/fish_passage_skeena_2021_reporting/tree/master/data/photos/', pscis_crossing_id,
  #                            '/crossing_all.JPG', '>', 'photo link', '</a>')) %>%
  mutate(photo_link = paste0('<a href =', 'https://raw.githubusercontent.com/NewGraphEnvironment/fish_passage_skeena_2022_reporting/master/data/photos/', pscis_crossing_id, '/crossing_all.JPG ',
                             'target="_blank">Culvert Photos</a>'))


# #--------------need to review if this is necessary
tab_map_prep <- left_join(
  pscis_all %>%
    sf::st_as_sf(coords = c("easting", "northing"),
                 crs = 26909, remove = F) %>% ##don't forget to put it in the right crs buds
    sf::st_transform(crs = 4326), ##convert to match the bcfishpass format,
  phase1_priorities %>% select(-utm_zone:utm_northing, -my_crossing_reference, priority_phase1, -habitat_value, -barrier_result), # %>% st_drop_geometry()
  by = 'pscis_crossing_id'
)


tab_map <- tab_map_prep %>%
  mutate(priority_phase1 = case_when(priority_phase1 == 'mod' ~ 'moderate',
                                     T ~ priority_phase1)) %>%
  mutate(data_link = paste0('<a href =', 'sum/cv/', pscis_crossing_id, '.html ', 'target="_blank">Culvert Data</a>')) %>%
  mutate(photo_link = paste0('<a href =', 'https://raw.githubusercontent.com/NewGraphEnvironment/', repo_name, '/master/data/photos/', my_crossing_reference, '/crossing_all.JPG ',
                             'target="_blank">Culvert Photos</a>')) %>%
  mutate(model_link = paste0('<a href =', 'sum/bcfp/', pscis_crossing_id, '.html ', 'target="_blank">Model Data</a>')) %>%
  dplyr::distinct(site_id, .keep_all = T) #just for now










