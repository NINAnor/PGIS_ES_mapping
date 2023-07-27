#install.packages("bigrquery")
library(bigrquery)
library(DBI)
library(dplyr)

bq_auth(path = "C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/eu-wendy_service_account.json")




con <- dbConnect(
  bigrquery::bigquery(),
  project = "eu-wendy",
  dataset = "testWendy",
  billing = "eu-wendy"
)


DBI::dbListTables(con)

#dplyr query read
tbl <- tbl(con, "es_descr")
es_all <- select(tbl, esID, esDESCR) %>%
  distinct() %>%
  collect()

available_teams$ID<-c("A","B","C")

## write with dbi
dbAppendTable(con, "test_table", available_teams)

insert_upload_job("rgee-381312", "data_base", "user_conf", user_conf)


user_table = bq_table(project = "rgee-381312", dataset = "data_base", table = 'user_conf')
bq_table_create(x = user_table, fields = as_bq_fields(user_conf))
bq_table_upload(x = user_table, values = user_conf)


### upload user_all table
user_all<-read.csv("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/setup_230710/user_all.csv")

user_table = bq_table(project = "rgee-381312", dataset = "data_base", table = 'user_all')
bq_table_create(x = user_table, fields = as_bq_fields(user_all))
bq_table_upload(x = user_table, values = user_all)

### upload es_pair table
es_pair<-read.csv("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/setup_230710/es_pair.csv")

user_table = bq_table(project = "rgee-381312", dataset = "data_base", table = 'es_pair')
bq_table_create(x = user_table, fields = as_bq_fields(es_pair))
bq_table_upload(x = user_table, values = es_pair)

### upload es_mapping1 table
es_map<-read.csv("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/setup_230710/es_mapping_R1.csv")

user_table = bq_table(project = "rgee-381312", dataset = "data_base", table = 'es_mappingR1')
bq_table_create(x = user_table, fields = as_bq_fields(es_map))
bq_table_upload(x = user_table, values = es_map)
