library(tidyverse)
library(here)
library(RSQLite)

# note: sample_df should be created from the nextflow samplesheet and include
# a column which will join back to the user's metadata (eg fastqFileNumber)
#
# TODO: make this sources in the (unique) sources in the status_df

CREATE = FALSE

createDatabase = function(){

  qc_data_paths = tools::file_path_sans_ext(
    basename(Sys.glob("data/multiqc_report_data/*txt")))
  qc_data_paths = qc_data_paths[
    str_detect(qc_data_paths, "^multiqc|.+featurecounts_biotype_.+|^tin")]
  qc_data_paths = qc_data_paths[
    str_detect(qc_data_paths,
               "warning|fastqc|sources|idxstats|picard",
               negate = TRUE)]
  qc_data_paths = as.list(qc_data_paths)
  names(qc_data_paths) = qc_data_paths



  # structure: Sample, id, audit, status, statusDecomp where Sample is from
  # the nextflow samplesheet, sample_id is a unique identifier for the same sample
  # which will allow join back to sample metadata in database, audit is a boolean,
  # status is the sum of bit statuses and statusDecomp is the list of powers of 2
  # to which the bit status decomposes
  #
  # there needs to be an id column in sample_df (eg make fastqFileNumber into id)

  sample_df = read_csv("data/all_yeast_nf_co_sample_sheet.csv", name_repair = "minimal") %>%
    mutate(fastqFileNumber = as.integer(str_extract(sample, "\\d+$")),
           audit_flag=NA, status=as.numeric(NA), statusDecomp="") %>%
    dplyr::rename(id = fastqFileNumber) %>%
    dplyr::rename(Sample = sample)
  #
  # write_csv(sample_df, "data/sample_df.csv")

  status_df = read_tsv("data/status_df.tsv", name_repair = "minimal")

  qc_data_paths = tools::file_path_sans_ext(
    basename(Sys.glob("data/multiqc_report_data/*txt")))
  qc_data_paths = qc_data_paths[
    str_detect(qc_data_paths, "^multiqc|.+featurecounts_biotype_.+|^tin")]
  qc_data_paths = qc_data_paths[
    str_detect(qc_data_paths,
               "warning|fastqc|sources|idxstats|picard",
               negate = TRUE)]
  qc_data_paths = as.list(qc_data_paths)
  names(qc_data_paths) = qc_data_paths

  qc_data_list = lapply(qc_data_paths, function(x)
    read_tsv(paste0("data/multiqc_report_data/",x,".txt"), name_repair = "minimal"))
  names(qc_data_list) = names(qc_data_paths)

  add_id_col = function(qc_table){
    qc_colnames = c("id", colnames(qc_table))
    qc_colnames = qc_colnames[!qc_colnames == "Sample"]

    qc_table %>%
      left_join(sample_df, by="Sample") %>%
      dplyr::select(qc_colnames) %>%
      arrange(id)
  }

  qc_data_list_with_id = lapply(qc_data_list, add_id_col)
  sample_df = arrange(sample_df, id)

  library(RSQLite)
  con <- dbConnect(RSQLite::SQLite(), here("www/qc_database.sqlite"))

  dbWriteTable(con, "sample", sample_df)
  dbWriteTable(con, "qc_metrics", status_df)
  lapply(names(qc_data_list_with_id), function(x)
    dbWriteTable(con, x, qc_data_list_with_id[[x]]))
}

if(CREATE){
  createDatabase()
}
