library(tidyverse)
library(here)

# purpose: create a dataframe to store QC flagging information. Structure:
# source (must be name of table in qc database. most likely the nf-co/rnaseq
# pipeline qc tables are simply their filename)
# metric name of the field in the table
# alias a field to rename the metric field for display
# threshold the value on which to threshold a given metric
# comparative one of >, >=, <, <= describing the direction on which to flag samples
# status the power of 2 which should be added to the status column in the sample_df
#        eg if a certain metric status is 1, then 2**1 would be added to the sample_df
#        status column if that sample were to be flagged for failing this metric

CREATE = FALSE

createStatusDf = function(){
  # create the 'user'/'custom'/better name? status df ----------------------------
  metric_vector = c("perturbed_coverage",
                    "nat_expected_coverage",
                    "nat_unexpected_coverage",
                    "nat_expected_log2cpm",
                    "nat_unexpected_log2cpm",
                    "g418_expected_coverage",
                    "g418_unexpected_coverage",
                    "g418_expected_log2cpm",
                    "g418_unexpected_log2cpm", #note, decreased here
                    "overexpression_fow",
                    "no_marker_in_metadata",
                    "rle_iqr")

  threshold_vector = c(0.25,
                       0.50,
                       0.50,
                       5.00,
                       2.50,
                       NA,
                       NA,
                       5.69,
                       NA,
                       2.00,
                       NA,
                       NA)

  status_vector = seq(1, length(metric_vector))

  comparative_vector = c(">", "<", ">", "<", ">", "<", ">", "<", ">", "<", "", ">")

  stopifnot(length(metric_vector) == length(threshold_vector) &
              length(threshold_vector) == length(status_vector) &
              length(status_vector) == length(comparative_vector))

  # note: alias can be a simplified name of the metric vector. Blank here --------
  # just name metric something reasonable. Alias is intended for multiqc output
  custom_status_df = tibble(source = rep("user", length(metric_vector)),
                            metric = metric_vector,
                            alias = rep("", length(metric_vector)),
                            threshold = threshold_vector,
                            comparative = comparative_vector,
                            status = status_vector)

  # read in and format multiqc output --------------------------------------------
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
    suppressMessages(read_tsv(here("data/multiqc_report_data/",
                                   paste0(x, ".txt")), name_repair = "minimal")))
  names(qc_data_list) = names(qc_data_paths)

  qcToStatusReformater = function(df, source_name){
    metric_vector = colnames(df)[str_detect(colnames(df), "Sample", negate = TRUE)]
    tibble(source = rep(source_name, length(metric_vector)),
           metric = metric_vector,
           alias = "",
           comparative = "",
           threshold = NA,
           status = NA)
  }

  qc_status_df = bind_rows(lapply(names(qc_data_list), function(x)
    qcToStatusReformater(qc_data_list[[x]], x) ))

  # combine user, multiqc to form the one status_df to rule them all -------------
  last_user_status = 12 # note: do programmatically
  qc_status_df = qc_status_df %>%
    mutate(status = seq(1, nrow(qc_status_df))+last_user_status)

  status_df = bind_rows(custom_status_df, qc_status_df)

  write_tsv(status_df, here("data/status_df.tsv"))

}

if(CREATE){
  createStatusDf()
}
