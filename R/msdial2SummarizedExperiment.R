importMsdialTable <- function(tablefile){
    tbl <- readr::read_delim(tablefile, delim = "\t", escape_double = FALSE,
                             col_names = FALSE, trim_ws = TRUE)
    numof_peakmetadata_cols <- 29
    quantval_cols <- tbl %>% dplyr::select(numof_peakmetadata_cols:ncol(tbl))
    numof_sample_class <- length(na.omit(unique(unlist(quantval_cols[1,]))))
    numof_avgstdev_cols <- numof_sample_class * 2
    quantval_cols <- quantval_cols %>%
        dplyr::select(1:(ncol(quantval_cols)-numof_avgstdev_cols))

    sample_meta_tbl <- quantval_cols[c(1,2,3,4,5),]
    quantval_tbl <- quantval_cols[-c(1,2,3,4,5),]

}
