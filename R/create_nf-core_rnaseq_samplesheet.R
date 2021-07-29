# library(brentlabRnaSeqTools)
# library(tidyverse)
#
# combined_df = getMetadata(database_info$s288cr64_host,
#                           database_info$s288cr64_db_name,
#                           Sys.getenv('DB_USERNAME'),
#                           Sys.getenv('DB_PASSWORD')) %>%
#   dplyr::select(-fastqFileName.y) %>%
#   dplyr::rename(fastqFileName = fastqFileName.x)
#
# no_na_combined_df = combined_df %>%
#   filter(!is.na(fastqFileName)) %>%
#   # NOTE: get these moved over to crypto database
#   filter(!str_detect(genotype1, "CKF44"))
#
#
# fastq_paths = file.path("/mnt/htcf_lts/lts_sequence", paste0("run_", no_na_combined_df$runNumber,"_samples"), no_na_combined_df$fastqFileName)
#
# sample_columns = c("genotype1", "treatment", "timePoint", "floodmedia", "inductionDelay", "fastqFileNumber")
#
# nf_co_sample_sheet = createNfCorePipelineSampleSheet(no_na_combined_df,
#                                                      sample_columns,
#                                                      "/scratch/mblab/chasem/rnaseq_pipeline/scratch_sequence")

# write_csv(nf_co_sample_sheet, "/mnt/htcf_scratch/chasem/rnaseq_pipeline/query/all_yeast_nf_co_sample_sheet.csv")
