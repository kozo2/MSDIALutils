importMsdialTable <- function(tablefile){
  tbl <- read_delim(tablefile, delim = "\t", escape_double = FALSE,
                col_names = FALSE, trim_ws = TRUE)
}
