write_csv_here <- function(i){
  export_name <- deparse(substitute(i))
  write_csv(i, here("process_data", paste0(export_name, ".csv")))
}
write_shp_here <- function(i){
  export_name <- deparse(substitute(i))
  st_write(i, here("process_data", paste0(export_name, ".shp")))
}