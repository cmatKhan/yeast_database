## SERVER VARIABLES ------------------------------------------------------------
DEBUG = FALSE
QC_DATABASE_PATH = here("www/qc_database.sqlite")
SAMPLE_DF = "sample"
QC_METRICS_DF = "qc_metrics"
NON_QC_TABLES = c(SAMPLE_DF, QC_METRICS_DF)
ID_FIELD = "id"
FIELDS_TO_REMOVE = c(ID_FIELD)

server <- function(input, output) {

  ## setup ---------------------------------------------------------------------
  qc_database_connection = dbConnect(RSQLite::SQLite(), QC_DATABASE_PATH)

  qc_table_list = dbListTables(qc_database_connection)
  qc_table_list = qc_table_list[!qc_table_list %in% NON_QC_TABLES]

  status_df = dbReadTable(qc_database_connection,
                          QC_METRICS_DF,
                          check.names = FALSE)

  sample_df = dbReadTable(qc_database_connection,
                          SAMPLE_DF,
                          check.names = FALSE) %>%
    mutate(statusDecomp = ifelse(is.na(statusDecomp) | statusDecomp == "",
                                 "passing",
                                 statusDecomp))

  reactive_values = reactiveValues(
    status_df = status_df,
    sample_df = sample_df,
    current_threshold = NA,
    current_comparative = "")

  updateSelectInput(inputId  = "qc_table",
                    choices  = qc_table_list,
                    selected = qc_table_list[[1]])

  updateSelectInput(inputId  = "qc_table_x",
                    choices  = qc_table_list,
                    selected = qc_table_list[[1]])

  updateSelectInput(inputId  = "qc_table_y",
                    choices  = qc_table_list,
                    selected = qc_table_list[[1]])

  updateCheckboxGroupInput(inputId = "histogram_table_column_selector",
                           choices = c(colnames(sample_df)[colnames(sample_df) != ID_FIELD],
                                       unique(status_df$metric)),
                           inline = TRUE,
                           selected = colnames(sample_df)[colnames(sample_df) != ID_FIELD])

  updateCheckboxGroupInput(inputId = "correlation_table_column_selector",
                           choices = c(colnames(sample_df)[colnames(sample_df) != ID_FIELD],
                                       unique(status_df$metric)),
                           inline = TRUE,
                           selected = colnames(sample_df)[colnames(sample_df) != ID_FIELD])

  updateCheckboxGroupInput(inputId = "scatterplot_table_column_selector",
                           choices = c(colnames(sample_df)[colnames(sample_df) != ID_FIELD],
                                       unique(status_df$metric)),
                           inline = TRUE,
                           selected = colnames(sample_df)[colnames(sample_df) != ID_FIELD])

  msg = "If you haven't already, it is a good idea to hit 'reset sample audit'
         before doing anything else to ensure that the audit is accurate to
         your current status_df"
  showNotification(ui = msg,
                   duration = 3,
                   closeButton = TRUE,
                   id = "initial_msg")

  dbDisconnect(qc_database_connection)
  rm(status_df)
  rm(sample_df)
  rm(qc_table_list)
  rm(msg)

  ## eventReactives ------------------------------------------------------------

  ### qc_tables ----------------------------------------------------------------
  qc_df = eventReactive(input$qc_table, {

    req(input$qc_table)

    qc_database_connection = dbConnect(RSQLite::SQLite(), QC_DATABASE_PATH)
    df = dbReadTable(qc_database_connection, input$qc_table, check.names = FALSE)
    dbDisconnect(qc_database_connection)

    df
  })

  qc_df_x = eventReactive(input$qc_table_x, {
    req(input$qc_table_x)
    qc_database_connection = dbConnect(RSQLite::SQLite(), QC_DATABASE_PATH)
    df = dbReadTable(qc_database_connection, input$qc_table_x, check.names = FALSE)
    dbDisconnect(qc_database_connection)

    df
  })

  qc_df_y = eventReactive(input$qc_table_y, {
    req(input$qc_table_y)
    qc_database_connection = dbConnect(RSQLite::SQLite(), QC_DATABASE_PATH)
    df = dbReadTable(qc_database_connection, input$qc_table_y, check.names = FALSE)
    dbDisconnect(qc_database_connection)

    df
  })

  distributions_table = eventReactive(input$histogram_table_column_selector, {

    req(reactive_values$sample_df, reactive_values$status_df)

    createMergedTable(input$histogram_table_column_selector,
                      ID_FIELD,
                      SAMPLE_DF,
                      reactive_values$status_df,
                      QC_DATABASE_PATH)
  })

  correlations_table = eventReactive(input$correlation_table_column_selector, {

    req(reactive_values$sample_df, reactive_values$status_df)

    createMergedTable(input$correlation_table_column_selector,
                      ID_FIELD,
                      SAMPLE_DF,
                      reactive_values$status_df,
                      QC_DATABASE_PATH)
  })

  scatterplot_table = eventReactive(input$scatterplot_table_column_selector, {

    req(reactive_values$sample_df, reactive_values$status_df)

    createMergedTable(input$scatterplot_table_column_selector,
                      ID_FIELD,
                      SAMPLE_DF,
                      reactive_values$status_df,
                      QC_DATABASE_PATH)

  })

  ### threshold valueboxes ------------------------------------------------------
  current_threshold_valuebox = eventReactive(reactive_values$current_threshold, {

    req(reactive_values$current_threshold)

    if(input$add_remove_threshold == "current_threshold_view"){
      valueBox(as.character(reactive_values$current_threshold),
               subtitle = "Threshold")
    }
  })

  current_comparative_valuebox = eventReactive(reactive_values$current_comparative, {

    req(reactive_values$current_comparative)

    if(input$add_remove_threshold == "current_threshold_view"){
      valueBox(as.character(reactive_values$current_comparative),
               subtitle = "Comparative")
    }
  })

  ### histogram plot ------------------------------------------------------------
  qc_metric_histogram = eventReactive(c(input$qc_metric,
                                        input$threshold_numeric,
                                        input$num_bins,
                                        input$histogram_n_breaks),{
    req(input$qc_table, input$qc_metric)

    threshold = ifelse(input$add_remove_threshold == "current_threshold_view",
                       reactive_values$current_threshold,
                       input$threshold_numeric)

    df = qc_df()
    metric_hist = ggplot(df) +
      geom_histogram(aes(!!rlang::sym(input$qc_metric)),
                     bins=input$num_bins) +
      scale_x_continuous(breaks =
                           scales::pretty_breaks(
                             n = input$histogram_n_breaks),
                         labels = function(x) format(x,scientific = TRUE)) +
      geom_vline(xintercept = threshold, color='purple',
                 linetype = "dashed") +
      ggtitle(paste0(input$qc_metric, " with threshold: ",
                     round(threshold, 4)))

    ggplotly(metric_hist, source = "histogram")
  })

  ### boxplot -------------------------------------------------------------------

  qc_metric_boxplot = eventReactive(c(input$distribution_plot_tab,
                                      input$threshold_numeric), {

    req(input$qc_table, input$qc_metric)

    if(input$distribution_plot_tab == 'boxplot'){

      # make jitter reproducible
      set.seed(1)

      threshold = ifelse(input$add_remove_threshold == "current_threshold_view",
                         reactive_values$current_threshold,
                         input$threshold_numeric)

      df = qc_df() %>%
        left_join(reactive_values$sample_df, by="id")

      metric_boxplot = ggplot(df, aes(!!rlang::sym(input$batch_variable),
                                      !!rlang::sym(input$qc_metric),
                                      fill = !!rlang::sym(input$batch_variable))) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(width = 0.05, alpha = .5) +
        scale_y_continuous(breaks =
                             scales::pretty_breaks(
                               n = input$histogram_n_breaks),
                           labels = function(x) format(x,scientific = TRUE)) +
        geom_hline(yintercept = threshold, color='purple',
                   linetype = "dashed") +
        ggtitle(paste0(input$qc_metric, " with threshold: ",
                       round(threshold, 4)))

      p = ggplotly(metric_boxplot, source = "boxplot")

      # remove ggplotly outliers
      # see https://github.com/ropensci/plotly/issues/1114
      p$x$data[1] <- lapply(p$x$data[1], FUN = function(x){
        x$marker = list(opacity = 0)
        return(x)
      })

      p

    }
  }, ignoreNULL = TRUE)

  ### scatterplot ---------------------------------------------------------------

  # note: removed the reactive on reactive_values$status_df. replace if
  #       add thresholding in the same view
  scatter_plot = eventReactive(c(input$scatter_x,
                                 input$scatter_y,
                                 reactive_values$sample_df),{
   req(input$qc_table_x, input$qc_table_y, input$scatter_x, input$scatter_y)

   x_var = ifelse(input$qc_table_x == input$qc_table_y,
                  paste0(input$scatter_x, ".x"), input$scatter_x)
   y_var = ifelse(input$qc_table_x == input$qc_table_y,
                  paste0(input$scatter_y, ".y"), input$scatter_y)

   x_metric_index =
     which(reactive_values$status_df$source == input$qc_table_x &
             reactive_values$status_df$metric == input$scatter_x)
   y_metric_index =
     which(reactive_values$status_df$source == input$qc_table_y &
             reactive_values$status_df$metric == input$scatter_y)

   x_var_threshold = reactive_values$status_df[[x_metric_index, "threshold"]]
   y_var_threshold = reactive_values$status_df[[y_metric_index, "threshold"]]

   scatter_plot = qc_df_x() %>%
     left_join(qc_df_y(), by = "id") %>%
     left_join(reactive_values$sample_df, by = "id") %>%
     ggplot(aes(!!rlang::sym(x_var),
                !!rlang::sym(y_var),
                label = Sample,
                label1 = id)) +
     geom_point(aes(color = statusDecomp)) +
     geom_vline(xintercept = x_var_threshold,
                linetype="dashed", color="purple") +
     geom_hline(yintercept = y_var_threshold,
                linetype="dashed", color="orange")

   ggplotly(scatter_plot, source = "scatter_plot")

 })

  ## observeEvents -------------------------------------------------------------

  ### reset sample_df audit, status, statusDecomp -------------------------------
  observeEvent(input$reset_sample_audit, {

    req(reactive_values$sample_df, reactive_values$status_df)

    thresholds_df = reactive_values$status_df %>%
      filter(!is.na(threshold) | (!is.na(comparative) & comparative != ""))

    malformed_status_df_msg = "There are thresholds without comparatives,
  or vice versa. This should not be. Close the session, check the status_df
  and then restart."

    if(
      length(thresholds_df$threshold[!is.na(thresholds_df$threshold) &
                                     thresholds_df$threshold != ""]) !=
      length(thresholds_df$comparative[!is.na(thresholds_df$comparative) &
                                       thresholds_df$comparative != ""])
    ){
      message(malformed_status_df_msg)
      showNotification(ui = malformed_status_df_msg,
                       duration = 5,
                       closeButton = FALSE,
                       id = "malformed_status_df",
                       type = "error")
    } else{
      reactive_values$sample_df = reactive_values$sample_df %>%
        mutate(status = NA,
               audit_flag = FALSE,
               statusDecomp = "") %>%
        mutate(status = as.numeric(status))

      for (i in seq(1, nrow(thresholds_df))){
        qc_df_name = thresholds_df[[i, "source"]]
        metric = thresholds_df[[i, "metric"]]
        threshold = thresholds_df[[i, "threshold"]]
        comparative = thresholds_df[[i, "comparative"]]
        metric_status = thresholds_df[[i, "status"]]

        reactive_values$sample_df = reactive_values$sample_df %>%
          mutate(statusDecomp = ifelse(statusDecomp == "" | is.na(statusDecomp),
                                       "passing",
                                       statusDecomp))

        # DEBUG only
        if(qc_df_name != "user"){

          qc_df = qc_df()

          # TODO repeated code from observeEvent set_threshold
          update_indicies = unlist(
            lapply(qc_df[[metric]],
                   function(x) parseComparatives(as.numeric(x),
                                                 comparative,
                                                 as.numeric(threshold))))
          # update sample_df
          reactive_values$sample_df[update_indicies, "audit_flag"] = TRUE

          update_status_vector = unlist(
            lapply(pull(reactive_values$sample_df[update_indicies,],
                        status),
                   function(x)
                     ifelse(is.na(x), 2**metric_status,
                            x + 2**metric_status)))

          update_statusDecomp_vector = unlist(lapply(update_status_vector,
                                                     decomposeStatus2Bit))

          reactive_values$sample_df[update_indicies, "status"] =
            update_status_vector

          reactive_values$sample_df[update_indicies, "statusDecomp"] =
            update_statusDecomp_vector

          if(DEBUG){
            View(reactive_values$sample_df)
          }

        } # end != user DEBUG if statement (remove this when user table is created)
      } # end for statement (todo replace this for statement with apply over rows)
    } # close of else statement in if/else error handling
  })

  ### set metric fields in metric dropdown --------------------------------------
  observeEvent(qc_df(), {

    req(input$qc_table)

    qc_df = qc_df()
    fields = colnames(qc_df)
    fields = fields[fields != FIELDS_TO_REMOVE]
    updateSelectInput(inputId = "qc_metric",
                      choices = fields,
                      selected = fields[[1]])
  })

  ### update threshold info based on qc_metric and table ------------------------
  observeEvent(input$qc_metric, {

    req(input$qc_table, input$qc_metric)

    # read in correct qc table and get metric vector
    qc_df = qc_df()
    metric_vector = pull(qc_df, input$qc_metric)

    # update threshold UI element based on whether there is a threshold
    # set in the reactive_values$status_df
    metric_index = which(reactive_values$status_df$source == input$qc_table &
                           reactive_values$status_df$metric == input$qc_metric)
    threshold = reactive_values$status_df[[metric_index, "threshold"]]
    comparative = reactive_values$status_df[[metric_index, "comparative"]]
    threshold_exists_flag = ifelse(is.na(threshold), FALSE, TRUE)

    if(threshold_exists_flag){
      updateTabsetPanel(inputId = 'add_remove_threshold',
                        selected = 'current_threshold_view')
      reactive_values$current_threshold = threshold
      reactive_values$current_comparative = comparative

    } else{
      updateTabsetPanel(inputId = 'add_remove_threshold',
                        selected = 'set_threshold_view')
    }
  })

  ### update set_threshold_view slider, numeric input ---------------------------
  observeEvent(c(input$add_remove_threshold, input$qc_metric), {

    if(input$add_remove_threshold == "set_threshold_view"){

      req(input$qc_table, input$qc_metric)

      # read in correct qc table and get metric vector
      qc_df = qc_df()
      metric_vector = pull(qc_df, input$qc_metric)

      # set min/max values for input (how to do this)
      input_min = min(metric_vector)-min(metric_vector)/10
      input_max = max(metric_vector)+min(metric_vector)/10

      updateNumericInput(inputId = "threshold_slider",
                         min = input_min,
                         value = median(metric_vector)+1000,
                         max = input_max)
      updateNumericInput(inputId = "threshold_numeric",
                         min = input_min,
                         value = median(metric_vector),
                         max = input_max)

      fence_output = outlierFence(metric_vector)

      showNotification(ui = fence_output$message,
                       duration = 3,
                       closeButton = TRUE,
                       id = "fence_na_count_msg",
                       type = "warning")

      updateRadioButtons(inputId = "threshold_options",
                         choiceNames = THRESHOLD_CHOICE_NAMES,
                         choiceValues = c(median(metric_vector),
                                          fence_output$lower_inner,
                                          fence_output$upper_inner))
    }
  })

  observeEvent(input$threshold_options, {
    updateNumericInput(inputId = 'threshold_numeric',
                       value = input$threshold_options)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # TODO connection between slider and numeric is not smooth. tried
  # https://stackoverflow.com/a/49668515/9708266
  # and
  # https://stackoverflow.com/questions/47822736/in-sync-sliderinput-and-textinput
  # to no avail

  ### tie threshold slider and numeric input together ---------------------------
  observeEvent(input$threshold_slider, {

    updateRadioButtons(inputId = "threshold_options", selected = 'Custom')

    updateNumericInput(inputId = "threshold_numeric",
                       value = input$threshold_slider)

  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$threshold_numeric, {

    updateRadioButtons(inputId = "threshold_options", selected = 'Custom')

    updateSliderInput(inputId = "threshold_slider",
                      value = input$threshold_numeric)

  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  ### handle set threshold button -----------------------------------------------
  observeEvent(input$set_threshold, {

    req(input$qc_table, input$qc_metric, reactive_values)

    # get metric details, store as local variables
    threshold     = as.numeric(input$threshold_numeric)
    comparative   = input$threshold_comparative
    metric_index  = which(reactive_values$status_df$source == input$qc_table &
                            reactive_values$status_df$metric == input$qc_metric)
    metric_status = reactive_values$status_df[[metric_index, "status"]]

    # update status_df with new threshold info
    reactive_values$status_df[metric_index, "threshold"] = threshold
    reactive_values$status_df[metric_index, "comparative"] = comparative

    # read in correct qc table and get update indicies
    qc_df = qc_df()
    update_indicies = unlist(
      lapply(qc_df[[input$qc_metric]],
             function(x) parseComparatives(as.numeric(x),
                                           comparative,
                                           as.numeric(threshold))))
    # update sample_df
    reactive_values$sample_df[update_indicies, "audit_flag"] = TRUE

    update_status_vector = unlist(
      lapply(pull(reactive_values$sample_df[update_indicies,],
                  status),
             function(x)
               ifelse(is.na(x), 2**metric_status,
                      x + 2**metric_status)))

    update_statusDecomp_vector = unlist(lapply(update_status_vector,
                                               decomposeStatus2Bit))

    reactive_values$sample_df[update_indicies, "status"] =
      update_status_vector

    reactive_values$sample_df[update_indicies, "statusDecomp"] =
      as.character(update_statusDecomp_vector)

    updateTabsetPanel(inputId = 'add_remove_threshold',
                      selected = 'current_threshold_view')

    reactive_values$current_threshold = threshold
    reactive_values$current_comparative = comparative

    if(DEBUG){
      View(reactive_values$status_df)
      View(reactive_values$sample_df)
    }
  })

  ### remove threshold from sample_df/update ui --------------------------------
  observeEvent(input$remove_threshold, {

    req(input$qc_table, input$qc_metric)

    # utils: this function has some finicky error handling. Hence some
    #        utilities
    reset_threshold_error_msg =
      "ERROR: could not remove threshold. Typically, this is due to
        an inconsistency in the sample_df and the status_df, where some
        thresholds are set, but the flagging in the sample_df is not consistent.
        Hit the Reset Sample Audit button and try again"
    current_sample_df = reactive_values$sample_df
    current_status_df = reactive_values$status_df
    resetSampleDf = function(){
      reactive_values$sample_df = current_sample_df
    }
    resetStatusDf = function(){
      reactive_values$status_df = current_status_df
    }

    metric_index  = which(reactive_values$status_df$source == input$qc_table &
                            reactive_values$status_df$metric == input$qc_metric)
    metric_status = reactive_values$status_df[[metric_index, "status"]]

    # update status_df with new threshold info
    reactive_values$status_df[metric_index, "threshold"] = NA
    reactive_values$status_df[metric_index, "comparative"] = NA

    # get indicies to update when removing a threshold
    list_statusDecomp = lapply(reactive_values$sample_df$statusDecomp,
                               str_split, ",")
    unlist_items_statusDecomp = lapply(list_statusDecomp, unlist)
    update_indicies = unlist(lapply(unlist_items_statusDecomp,
                                    function(x) metric_status %in% x))

    # remove the comparative for the metric
    reactive_values$status_df[metric_index, "comparative"] = ""

    reactive_values$sample_df[update_indicies, 'status'] =
      reactive_values$sample_df[update_indicies, 'status'] -
      2**metric_status

    # TODO: make status = 0 mean no status
    reactive_values$sample_df = reactive_values$sample_df %>%
      mutate(audit_flag = ifelse(is.na(status) | status == 0,
                                 FALSE,
                                 audit_flag)) %>%
      mutate(status = as.numeric(ifelse(status == 0, NA, status)))

    update_statusDecomp_vector = unlist(
      lapply(
        pull(reactive_values$sample_df[update_indicies,], status),
        function(x)
          ifelse(is.na(x) | x == 0, "", decomposeStatus2Bit(x))))

    tryCatch(
      expr = {
        reactive_values$sample_df[update_indicies, "statusDecomp"] =
          update_statusDecomp_vector

        reactive_values$sample_df = reactive_values$sample_df %>%
          mutate(statusDecomp = ifelse(statusDecomp == "" | is.na(statusDecomp),
                                       "passing",
                                       statusDecomp))

        updateTabsetPanel(inputId = 'add_remove_threshold',
                          selected = 'set_threshold_view')

      }, error = function(e) {
        showNotification(ui = reset_threshold_error_msg,
                         duration = 4,
                         closeButton = TRUE,
                         id = "reset_threshold_error",
                         type = "error")
        resetSampleDf()
        resetStatusDf()
      }, finally = {

        if(DEBUG){
          View(reactive_values$status_df)
          View(reactive_values$sample_df)
        }

      }) # end tryCatch()
  })

  ### scatterplot observerEvents -----------------------------------------------
  observeEvent(input$qc_table_x, {

    req(input$qc_table_x)

    qc_database_connection = dbConnect(RSQLite::SQLite(), QC_DATABASE_PATH)

    fields = dbListFields(qc_database_connection, input$qc_table_x)
    fields = fields[!fields %in% FIELDS_TO_REMOVE]
    updateSelectInput(inputId = "scatter_x", choices = fields)
    dbDisconnect(qc_database_connection)
  })

  observeEvent(input$qc_table_y, {

    req(input$qc_table_y)

    qc_database_connection = dbConnect(RSQLite::SQLite(), QC_DATABASE_PATH)
    fields = dbListFields(qc_database_connection, input$qc_table_y)
    fields = fields[!fields %in% FIELDS_TO_REMOVE]
    updateSelectInput(inputId = "scatter_y", choices = fields)
    dbDisconnect(qc_database_connection)
  })

  ## OUTPUT --------------------------------------------------------------------

  output$qc_metric_histogram = renderPlotly(qc_metric_histogram())
  output$qc_metric_boxplot = renderPlotly(qc_metric_boxplot())
  output$scatter_plot = renderPlotly(scatter_plot())

  output$current_threshold = renderValueBox(current_threshold_valuebox())
  output$current_comparative = renderValueBox(current_comparative_valuebox())

  output$histogram_sample_table = renderDataTable({
    DT::datatable(distributions_table(),
                  filter = list(
                    position = 'top'
                  ),
                  options = list(
                    pageLength = 10,
                    lengthMenu = c(10, 25, 50, 100, nrow(reactive_values$sample_df)),
                    scrollX = TRUE
                  )
    )
  })

  output$correlation_sample_table = renderDataTable({
    DT::datatable(correlations_table(),
                  filter = list(
                    position = 'top'
                  ),
                  options = list(
                    pageLength = 10,
                    lengthMenu = c(10, 25, 50, 100, nrow(reactive_values$sample_df)),
                    scrollX = TRUE
                  )
    )
  })

  output$scatterplot_sample_table = renderDataTable({
    DT::datatable(scatterplot_table(),
                  filter = list(
                    position = 'top'
                  ),
                  options = list(
                    pageLength = 10,
                    lengthMenu = c(10, 25, 50, 100, nrow(reactive_values$sample_df)),
                    scrollX = TRUE
                  )
    )
  })

  ### Downloadable csv of reactive_values$status_df ----------------------------

  output$download_audited_samplesheet <- downloadHandler(
    filename = function() {
      timenow = str_replace(str_remove_all(lubridate::now(), "-|:"), " ", "_")

      paste0("audited_samplesheet_", timenow, ".csv")
    },
    content = function(file) {
      write_csv(reactive_values$status_df, file)
    }
  )

  output$download_distribution_table <- downloadHandler(
    filename = function() {
      timenow = str_replace(str_remove_all(lubridate::now(), "-|:"), " ", "_")

      paste0("distributions_qc_subset_", timenow, ".csv")
    },
    content = function(file) {
      write_csv(distributions_table(), file)
    }
  )

  output$download_correlation_table <- downloadHandler(
    filename = function() {
      timenow = str_replace(str_remove_all(lubridate::now(), "-|:"), " ", "_")

      paste0("correlations_qc_subset_", timenow, ".csv")
    },
    content = function(file) {
      write_csv(correlations_table(), file)
    }
  )

  output$download_scatterplot_table <- downloadHandler(
    filename = function() {
      timenow = str_replace(str_remove_all(lubridate::now(), "-|:"), " ", "_")

      paste0("scatterplot_qc_subset_", timenow, ".csv")
    },
    content = function(file) {
      write_csv(scatterplot_table(), file)
    }
  )

}
