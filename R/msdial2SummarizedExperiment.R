library(magrittr)
library(dplyr)
library(SummarizedExperiment)
library(ggplot2)
library(tidyr)
library(tibble)

msdial2se <- function(tablefile){
    tbl <- readr::read_delim(tablefile, delim = "\t", escape_double = FALSE,
                             col_names = FALSE, trim_ws = TRUE)

    numof_peakmetadata_cols <- 29
    peakmetadata_cols <- tbl %>%
        dplyr::select(1:(numof_peakmetadata_cols-1)) %>%
        dplyr::slice(5:dplyr::n())
    colnames(peakmetadata_cols) <- as.character(unlist(peakmetadata_cols[1,]))
    row_tbl <- peakmetadata_cols[-1,]

    quantval_cols <- tbl %>% dplyr::select(numof_peakmetadata_cols:ncol(tbl))

    numof_sample_class <- length(na.omit(unique(unlist(quantval_cols[1,]))))
    numof_avgstdev_cols <- numof_sample_class * 2
    quantval_cols <- quantval_cols %>%
        dplyr::select(1:(ncol(quantval_cols)-numof_avgstdev_cols))

    sample_meta_tbl <- t(quantval_cols[c(1,2,3,4,5),])
    colnames(sample_meta_tbl) <- c("Class", "FileType",
                                   "InjectionOrder", "BatchId", "SampleId")
    col_tbl <- tibble::as_tibble(sample_meta_tbl)
    #col_data <- tibble::column_to_rownames(col_tbl, var = "SampleId")

    quantval_tbl <- quantval_cols[-c(1,2,3,4,5),] %>%
        mutate(across(where(is.character), as.numeric))

    se <- SummarizedExperiment::SummarizedExperiment(assays = quantval_tbl,
                                                     rowData = row_tbl,
                                                     colData = col_tbl)
}

save_plot4inchikey <- function(se, inchikey){
    tidytbl <- get_tidytbl4inchikey(se, inchikey)
    
    tidytbl2boxplot(tidytbl)
    ggsave(paste0("box_", inchikey, ".png"))
    
    tidytbl2barplot(tidytbl)
    ggsave(paste0("bar_", inchikey, ".png"))
    
}

get_tidytbl4inchikey <- function(se, inchikey){
    rowDFrame <- rowData(se)
    colDFrame <- colData(se)
    asyTbl <- assay(se, 1)
    
    rowDF4inchikey <- rowDFrame[rowDFrame$INCHIKEY==inchikey, ]
    asyTbl4inchikey <- asyTbl[rowDFrame$INCHIKEY==inchikey, ]
    
    tmp <- t(asyTbl4inchikey)
    colnames(tmp) <- rowDF4inchikey$`Alignment ID`
    tbl <- tibble::as_tibble(cbind(colDFrame, tmp))
    tidytbl <- tidyr::pivot_longer(tbl, cols=where(is.numeric))
    return(tidytbl)
}

tidytbl2boxplot <- function(tidytbl){
    ggplot(tidytbl, aes(x=Class, y=value)) + 
        geom_boxplot() +
        facet_wrap(~name)
}

tidytbl2barplot <- function(tidytbl){
    ggplot(tidytbl, aes(x=SampleId, y=value, fill=Class)) +
        geom_bar(stat = "identity") +
        facet_wrap(~name)
}