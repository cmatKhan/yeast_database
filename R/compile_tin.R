## NOTE: this function was moved to brentlabRnaSeqTools

# library(tidyverse)
# library(ggExtra)
# library(here)
#
# # compile results of rseqc tin.py
# compileTinOutput = function(tin_output_dir, bam_suffix='.markdup.sorted.bam'){
#
#   message('reading in tin.py summaries...')
#   tin_summary_list = Sys.glob(file.path(tin_output_dir, "*summary*"))
#   tin_df_list = suppressMessages(lapply(tin_summary_list, read_tsv))
#
#   message('merging tin.py summaries...')
#   tin_df = bind_rows(tin_df_list)
#   # todo: do rename in mutate, then select
#   tin_df %>%
#     mutate(Sample = str_remove(Bam_file, bam_suffix)) %>%
#     select(-Bam_file)
# }

# library(brentlabRnaSeqTools)
#
# tin_df = compileTinOutput(here("data/tin_output"))
