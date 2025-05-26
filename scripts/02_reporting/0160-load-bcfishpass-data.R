## we can switch this to pull from our own system but we will just use simons bcfishpass for now to save time
## simons db will not have the updated pscis names so we we include workflows to see that our new pscis crossings match our old modelled crossings

source('scripts/packages.R')
# source('R/functions.R')
#source('scripts/private_info.R')

# thinking we better use the remote database since my local version is outdated and not willing to risk a week of time to rebuild (might be fine in a day but never really know till we climb in)

# conn <- DBI::dbConnect(
#   RPostgres::Postgres(),
#   dbname = Sys.getenv('PG_DB_BCBARRIERS'),
#   host = Sys.getenv('PG_HOST_BCBARRIERS'),
#   port = Sys.getenv('PG_PORT_BCBARRIERS'),
#   user = Sys.getenv('PG_USER_BCBARRIERS'),
#   password = Sys.getenv('PG_PASS_BCBARRIERS')
# )


# these are the shared server params
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv('PG_DB_SHARE'),
  host = Sys.getenv('PG_HOST_SHARE'),
  port = Sys.getenv('PG_PORT_SHARE'),
  user = Sys.getenv('PG_USER_SHARE'),
  password = Sys.getenv('PG_PASS_SHARE')
)

# these are the production server params.  they could change but we may as well use the same env variable names
# conn <- DBI::dbConnect(
#   RPostgres::Postgres(),
#   dbname = Sys.getenv('PG_DB_DO'),
#   host = Sys.getenv('PG_HOST_DO'),
#   port = Sys.getenv('PG_PORT'),
#   user = Sys.getenv('PG_USER_DO'),
#   password = Sys.getenv('PG_PASS_DO')
# )

#
# ##listthe schemas in the database
dbGetQuery(conn,
           "SELECT schema_name
           FROM information_schema.schemata")
# #
# # # ##list tables in a schema
dbGetQuery(conn,
           "SELECT table_name
           FROM information_schema.tables
           WHERE table_schema='bcfishpass'") %>%
  filter(table_name %ilike% 'prec')
# # # # # #
# # # # # # ##list column names in a table
test <- dbGetQuery(conn,
           "SELECT column_name
           FROM information_schema.columns
           WHERE table_name='crossings'")


# get the crossings layer from aws until we get set up with a stable db.  file was downloaded from https://bcfishpass.s3.us-west-2.amazonaws.com/crossings.fgb (see dff-2022/background_layers.sh)
# crossings <- sf::st_read('~/Downloads/crossings.fgb',
#                      query = "select * from crossings WHERE watershed_group_code IN ('MORR', 'ZYMO', 'KISP')")
#
# bcfishpass <- crossings %>%
#   distinct(aggregated_crossings_id, .keep_all = T) %>%
#   sf::st_drop_geometry()

#
# dbGetQuery(conn,
#            "SELECT a.total_lakereservoir_ha
#            FROM bcfishpass.crossings a
#            WHERE stream_crossing_id IN (58159,58161,123446)")
#
# dbGetQuery(conn,
#            "SELECT o.observation_date, o.point_type_code  FROM whse_fish.fiss_fish_obsrvtn_pnt_sp o;") %>%
#   filter(observation_date > '1900-01-01' &
#          observation_date < '2021-02-01') %>%
#   group_by(point_type_code) %>%
#   summarise(min = min(observation_date, na.rm = T),
#             max = max(observation_date, na.rm = T))


#first thing we want to do is match up our pha

pscis_all <- bind_rows(fpr_import_pscis_all())

# n_distinct(pscis_all$aggregated_crossings_id)

dat <- pscis_all %>%
  sf::st_as_sf(coords = c("easting", "northing"),
               crs = 26909, remove = F) %>% ##don't forget to put it in the right crs buds
  sf::st_transform(crs = 3005) ##get the crs same as the layers we want to hit up


# add a unique id - we could just use the reference number
dat$misc_point_id <- seq.int(nrow(dat))

# dbSendQuery(conn, paste0("CREATE SCHEMA IF NOT EXISTS ", "test_hack",";"))
# load to database
sf::st_write(obj = dat, dsn = conn, Id(schema= "working", table = "misc"))


# sf doesn't automagically create a spatial index or a primary key
res <- dbSendQuery(conn, "CREATE INDEX ON working.misc USING GIST (geometry)")
dbClearResult(res)
res <- dbSendQuery(conn, "ALTER TABLE working.misc ADD PRIMARY KEY (misc_point_id)")
dbClearResult(res)

dat_info <- dbGetQuery(conn, "SELECT
  a.misc_point_id,
  b.*,
  ST_Distance(ST_Transform(a.geometry,3005), b.geom) AS distance
FROM
  working.misc AS a
CROSS JOIN LATERAL
  (SELECT *
   FROM bcfishpass.crossings
   ORDER BY
     a.geometry <-> geom
   LIMIT 1) AS b")

##get all the data and save it as an sqlite database as a snapshot of what is happening.  we can always hopefully update it
query <- "SELECT *
   FROM bcfishpass.crossings
   WHERE watershed_group_code IN ('MORR', 'ZYMO', 'KISP', 'KLUM', 'BABR')"



##import and grab the coordinates - this is already done
sf::st_read(conn, query =  query)
  sf::st_drop_geometry() %>%
  mutate(downstream_route_measure = as.integer(downstream_route_measure))
  # dplyr::distinct(.keep_all = T) #needed to do this because there are duplicated outputs



query <- "select col_description((table_schema||'.'||table_name)::regclass::oid, ordinal_position) as column_comment,
* from information_schema.columns
WHERE table_schema = 'bcfishpass'
and table_name = 'crossings';"

bcfishpass_column_comments <- st_read(conn, query =  query) %>%
  select(column_name, column_comment)

# porphyryr <- st_read(conn, query =
# "SELECT * FROM bcfishpass.crossings
#    WHERE stream_crossing_id = '124487'")

# get the pscis data
query <- "SELECT p.*, wsg.watershed_group_code
   FROM whse_fish.pscis_assessment_svw p
   INNER JOIN whse_basemapping.fwa_watershed_groups_poly wsg
ON ST_Intersects(wsg.geom,p.geom)
WHERE wsg.watershed_group_code IN ('MORR', 'ZYMO', 'KISP', 'KLUM');"

pscis <- st_read(conn, query =  query)

DBI::dbDisconnect(conn = conn)


##join the modelled road data to our pscis submission info

dat_joined <- left_join(
  dat %>% select(-aggregated_crossings_id),
  dat_info %>% select(-utm_zone),
  # select(dat_info,misc_point_id:fcode_label, distance, crossing_id), ##geom keep only the road info and the distance to nearest point from here
  by = "misc_point_id"
)

##lets simiplify dat_joined to have a look up
my_pscis_modelledcrossings_streams_xref <- dat_joined %>%
  select(aggregated_crossings_id, pscis_crossing_id, stream_crossing_id, modelled_crossing_id, site_id, source, distance) %>%
  st_drop_geometry()

# xref my_crossings pscis --------------------------------------------
# the following 2 sections on xref were moved here from 0100-extract-inputs script, makes more sense to have them here,
# the code to take xref data and burn to csv to match up with our templates can be found over there

get_this <- bcdata::bcdc_tidy_resources('pscis-assessments') %>%
  filter(bcdata_available == T)  %>%
  pull(package_id)

dat <- bcdata::bcdc_get_data(get_this)

## xref_pscis_my_crossing_modelled ----------------
xref_pscis_my_crossing_modelled <- dat %>%
  purrr::set_names(nm = tolower(names(.))) %>%
  dplyr::filter(funding_project_number == "skeena_2022_Phase1") %>%
  select(external_crossing_reference, stream_crossing_id) %>%
  dplyr::mutate(external_crossing_reference = as.numeric(external_crossing_reference)) %>%
  sf::st_drop_geometry()



# burn to sqlite------------------
##my time format format(Sys.time(), "%Y%m%d-%H%M%S")
# mydb <- DBI::dbConnect(RSQLite::SQLite(), "data/bcfishpass.sqlite")
conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
readwritesqlite::rws_list_tables(conn)
##archive the last version for now
bcfishpass_archive <- readwritesqlite::rws_read_table("bcfishpass", conn = conn)
# readwritesqlite::rws_drop_table("bcfishpass_archive", conn = conn) ##if it exists get rid of it - might be able to just change exists to T in next line
readwritesqlite::rws_write(bcfishpass_archive, exists = F, delete = TRUE,
          conn = conn, x_name = paste0("bcfishpass_archive_", format(Sys.time(), "%Y-%m-%d")))
readwritesqlite::rws_drop_table("bcfishpass", conn = conn) ##now drop the table so you can replace it
readwritesqlite::rws_write(bcfishpass, exists = F, delete = TRUE,
          conn = conn, x_name = "bcfishpass")
# write in the xref
readwritesqlite::rws_drop_table("xref_pscis_my_crossing_modelled", conn = conn) ##now drop the table so you can replace it
readwritesqlite::rws_write(xref_pscis_my_crossing_modelled, exists = F, delete = TRUE,
          conn = conn, x_name = "xref_pscis_my_crossing_modelled")
rws_write(pscis, exists = F, delete = TRUE,
          conn = conn, x_name = "pscis")
# add the comments
# bcfishpass_column_comments_archive <- readwritesqlite::rws_read_table("bcfishpass_column_comments", conn = conn)
# rws_write(bcfishpass_column_comments_archive, exists = F, delete = TRUE,
#           conn = conn, x_name = paste0("bcfishpass_column_comments_archive_", format(Sys.time(), "%Y-%m-%d-%H%m")))
readwritesqlite::rws_drop_table("bcfishpass_column_comments", conn = conn) ##now drop the table so you can replace it
readwritesqlite::rws_write(bcfishpass_column_comments, exists = F, delete = TRUE,
          conn = conn, x_name = "bcfishpass_column_comments")
readwritesqlite::rws_list_tables(conn)
readwritesqlite::rws_disconnect(conn)

##make a dataframe with our crossings that need a match
match_this <- dat_joined %>%
  st_drop_geometry() %>%
  select(pscis_crossing_id, stream_crossing_id, modelled_crossing_id, linear_feature_id, watershed_group_code) %>%
  mutate(reviewer = 'MW',
         notes = "Matched to closest stream model") %>%
  filter(!is.na(pscis_crossing_id) &
           is.na(stream_crossing_id))
# we manually compare results to the xref_pscis_my_crossing_phase2.csv we made and it is correct!

match_this_to_join <- match_this %>%
  select(-stream_crossing_id) %>%
  mutate(linear_feature_id = NA_integer_) %>%
  rename(stream_crossing_id = pscis_crossing_id) %>%
  mutate(across(c(stream_crossing_id:linear_feature_id), as.numeric))


##extra test to see if the match_this hits are already assigned in crossings
test <- bcfishpass %>%
  filter(stream_crossing_id %in% (match_this %>% pull(pscis_crossing_id)))


##!!!!!i believe this checks to make sure someone hasn't manually assigned our newly matched crossings....
## not sure why it would though so probalby not necessary - old script
##need to learn to move from the other fork for now rename and grab from there
# file.copy(from = "C:/scripts/bcfishpass/data/pscis_modelledcrossings_streams_xref.csv",
#             to = "C:/scripts/pscis_modelledcrossings_streams_xref.csv",
#           overwrite = T)
# #
# # ##get the crossing data from bcfishpass
# pscis_modelledcrossings_streams_xref <- readr::read_csv("C:/scripts/pscis_modelledcrossings_streams_xref.csv")
#
#
# ##check to make sure your match_this crossings aren't already assigned somehow
# pscis_modelledcrossings_streams_xref %>%
#   filter(stream_crossing_id %in% (match_this %>% pull(pscis_crossing_id)))
#
# ##nope - all good
#
# ##grab the bcfishpass spawning and rearing table and put in the database so it can be used to populate the methods and tie to the references table
urlfile="https://raw.githubusercontent.com/smnorris/bcfishpass/main/parameters/parameters_newgraph/habitat.csv"

bcfishpass_spawn_rear_model <- read_csv(url(urlfile))


# put it in the db
conn <- rws_connect("data/bcfishpass.sqlite")
rws_list_tables(conn)
# archive <- readwritesqlite::rws_read_table("bcfishpass_spawn_rear_model", conn = conn)
# rws_write(archive, exists = F, delete = TRUE,
#           conn = conn, x_name = paste0("bcfishpass_spawn_rear_model_archive_", format(Sys.time(), "%Y-%m-%d-%H%m")))
# rws_drop_table("bcfishpass_spawn_rear_model", conn = conn)
rws_write(bcfishpass_spawn_rear_model, exists = F, delete = TRUE,
          conn = conn, x_name = "bcfishpass_spawn_rear_model")
rws_list_tables(conn)
readwritesqlite::rws_disconnect(conn)


# Re-loaded the bcfishpass object in 2024 - used the updated code from 2024 reports
# lots of text in the memos is written/based off the of the bcfishpass modelling from 2022 and i dont think we want to go
# updating every memo with the 2025 modelling so only certain memos use the bcfishpass_2025 object, where I think its important that the modelling is updated

# name the watershed groups in our study area
wsg <- c('BULK', 'MORR', 'ZYMO', 'KISP', 'KLUM')

# this object should be called bcfishpass_crossings_vw or something that better reflects what it is
bcfishpass_2025 <- fpr::fpr_db_query(
  glue::glue(
    "SELECT * from bcfishpass.crossings_vw
  WHERE watershed_group_code IN (
  {glue::glue_collapse(glue::single_quote(wsg), sep = ', ')}
  );"
  )
) |>
  sf::st_drop_geometry()


# add to sqlite as bcfishpass_2024
conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
readwritesqlite::rws_list_tables(conn)

#add the new bcfishpass_2024 table
readwritesqlite::rws_drop_table("bcfishpass_2025", conn = conn) ##now drop the table so you can replace it
readwritesqlite::rws_write(bcfishpass_2025, exists = F, delete = TRUE,
                           conn = conn, x_name = "bcfishpass_2025")

readwritesqlite::rws_disconnect(conn)
