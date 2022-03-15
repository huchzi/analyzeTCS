library(analyzeTCS)

server <- function(input, output) {

  values <- reactiveValues(sensitivity_table = NULL,
                           file = reactive(input$raw_csv),
                           patid = NULL,
                           eye = NULL,
                           date_of_exam = NULL,
                           time_of_exam = NULL,
                           frequency = NULL
  )

  observeEvent(input$add_to_table,
               {
                 file <- input$raw_csv
                 req(file)
                 patient_info <- parseFilenameTCS(file$name)
                 tcs <- parseTCS(file$datapath)
                 header <- tcs$header

                 waveforms = header$waveform[header$field == input$field]
                 waveform = ifelse(duplicated(waveforms) == length(waveforms) - 1, waveforms[1], "mixed")


                 thresholds <- getThreshold(input$use_threshold, input$field, tcs)

                 if (input$field == "outer") cf <- ConeFundamentals10 else cf <- ConeFundamentals2
                 contrasts <- thresholds / 100
                 contrasts[header[header$field == input$field, "phase"] == 180] <- -1 * contrasts[header[header$field == input$field, "phase"] == 180]
                 lmean <- header[header$field == input$field, "luminance"]
                 pcs <- as.vector(findPhotoreceptorContrasts(contrasts, lmean, cf))
                 photoreceptorContrasts <- data.frame(
                   led = c("rod", "scone", "mcone", "lcone"),
                   contrast = round(pcs * 100,3)
                 )

                 photoreceptor_contrast <- NA
                 if (input$stimulus_type ==	"Rod") photoreceptor_contrast <- photoreceptorContrasts[photoreceptorContrasts$led == "rod", "contrast"]
                 else if (input$stimulus_type == "L-cone") photoreceptor_contrast <- photoreceptorContrasts[photoreceptorContrasts$led == "lcone", "contrast"]
                 else if (input$stimulus_type == "M-cone") photoreceptor_contrast <- photoreceptorContrasts[photoreceptorContrasts$led == "mcone", "contrast"]
                 else if (input$stimulus_type == "S-cone") photoreceptor_contrast <- photoreceptorContrasts[photoreceptorContrasts$led == "scone", "contrast"]

                 parameters <- data.frame(patient_info,
                                          test_field = as.character(input$field),
                                          illuminance_inner = sum(header$luminance[header$field == "inner"]) * 1.5 * 1.5 * pi * 10^(-input$filter),
                                          illuminance_outer = sum(header$luminance[header$field == "outer"]) * 1.5 * 1.5 * pi * 10^(-input$filter),
                                          stimulus_type = ifelse(length(input$stimulus_type) > 0,
                                                                 as.character(input$stimulus_type), NA),
                                          waveform = waveform[1],
                                          envelope = header$envelope[1],
                                          pause = header$pause[1],
                                          frequency = values$frequency,
                                          mean_luminance_red = ifelse(as.character(input$field) == "inner",
                                                                      header$luminance[header$field == "inner" & header$led == "red"],
                                                                      header$luminance[header$field == "outer" & header$led == "red"]
                                          ),
                                          mean_luminance_green = ifelse(as.character(input$field) == "inner",
                                                                        header$luminance[header$field == "inner" & header$led == "green"],
                                                                        header$luminance[header$field == "outer" & header$led == "green"]
                                          ),
                                          mean_luminance_blue = ifelse(as.character(input$field) == "inner",
                                                                       header$luminance[header$field == "inner" & header$led == "blue"],
                                                                       header$luminance[header$field == "outer" & header$led == "blue"]
                                          ),
                                          mean_luminance_cyan = ifelse(as.character(input$field) == "inner",
                                                                       header$luminance[header$field == "inner" & header$led == "cyan"],
                                                                       header$luminance[header$field == "outer" & header$led == "cyan"]
                                          ),
                                          used_threshold = input$use_threshold,
                                          threshold_red = thresholds[names(thresholds) == "red"],
                                          threshold_green = thresholds[names(thresholds) == "green"],
                                          threshold_blue = thresholds[names(thresholds) == "blue"],
                                          threshold_cyan = thresholds[names(thresholds) == "cyan"],
                                          photoreceptor_contrast_lcone = photoreceptorContrasts[photoreceptorContrasts$led == "lcone", "contrast"],
                                          photoreceptor_contrast_mcone = photoreceptorContrasts[photoreceptorContrasts$led == "mcone", "contrast"],
                                          photoreceptor_contrast_scone = photoreceptorContrasts[photoreceptorContrasts$led == "scone", "contrast"],
                                          photoreceptor_contrast_rod = photoreceptorContrasts[photoreceptorContrasts$led == "rod", "contrast"],
                                          photoreceptor_contrast_at_threshold = photoreceptor_contrast,
                                          sensitivity = 100 / photoreceptor_contrast,
                                          log_sensitivity = log10(100 / photoreceptor_contrast)
                 )

                 new_table <- rbind(values$sensitivity_table,
                                    parameters
                 )

                 if (!anyDuplicated(new_table[, 1:4])) values$sensitivity_table <- new_table

               })

  output$sensitivity_table <- renderDataTable({
    values$sensitivity_table
  },
  options = list(width = "80%",
                 scrollX = TRUE))

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("resultsTCSF", ".csv", sep = "")
    },
    content = function(file) {
      tab <- values$sensitivity_table
      write.csv(values$sensitivity_table, file, row.names = FALSE)
    }
  )

  output$rawData <- renderPrint({
    file <- input$raw_csv
    req(file)
    readLines(file$datapath)
  },
  width = 80)

  output$patid <- renderUI({
    file <- input$raw_csv
    req(file)
    patient_info <- parseFilenameTCS(file$name)

    values$patid <- patient_info$patid
    values$eye <- patient_info$eye
    values$date_of_exam <- patient_info$date_of_exam
    values$time_of_exam <- patient_info$date_of_exam

    pat_info <- paste0("<b>Patient ID:</b>  ", patient_info$patid,
                       "<br/><b>Eye:</b>  ", patient_info$eye,
                       "<br/><b>Date of exam:</b>  ", patient_info$date_of_exam,
                       "<br/><b>Time of exam:</b>  ", patient_info$time_of_exam)

    HTML(pat_info)
  })

  output$parsedData <- renderDataTable({
    file <- input$raw_csv
    req(file)
    parseTCS(file$datapath)$header
  })

  output$luminance <- renderPlot({
    file <- input$raw_csv
    req(file)
    header <- parseTCS(file$datapath)$header

    ggplot(header, aes(x = field, y = 10^(-input$filter) * pi * 1.5 * 1.5 * luminance, fill = led)) +
      geom_bar(stat="identity") +
      scale_y_continuous("Retinal illuminance [phot Td]", limits = c(0, 700)) +
      scale_fill_manual(values = c("red" = "red", "green" = "green", "blue" = "blue", "cyan" = "cyan")) +
      guides(fill = "none") +
      ggtitle("Retinal illuminances")
  })

  output$waveform <- renderPlot({
    file <- input$raw_csv
    req(file)
    header <- parseTCS(file$datapath)$header

    frequencies <- header[header$field == input$field, "frequency"]

    values$frequency <- max(frequencies)

    x <- seq(0, 8, .0001)

    if (header$envelope[1] == 0) envelope <- 1 else envelope <- sinpi(x * header$envelope[1])
    if (header$pause[1] == TRUE) envelope[envelope < 0] <- 0 else envelope <- abs(envelope)

    table_for_plot <- data.frame(
      x = x,
      y = sinpi(x * max(frequencies)) * envelope
    )

    ggplot(table_for_plot, aes(x = x, y = y)) +
      geom_line() +
      scale_x_continuous("Time [sec]", breaks = 0:8, labels = seq(0, 4, .5), limits = c(0, 8)) +
      scale_y_continuous("", breaks = NULL) +
      ggtitle("Waveform")

  })

  output$photoreceptors <- renderPlot({
    file <- input$raw_csv
    req(file)
    header <- parseTCS(file$datapath)$header

    if (input$field == "outer") cf <- ConeFundamentals10 else cf <- ConeFundamentals2

    contrasts <- apply(header[header$field == input$field, c("initial_contrast_1", "initial_contrast_2")],
                       1,
                       max) / 100

    contrasts[header[header$field == input$field, "phase"] == 180] <- -1 * contrasts[header[header$field == input$field, "phase"] == 180]

    lmean <- header[header$field == input$field, "luminance"]

    pcs <- as.vector(findPhotoreceptorContrasts(contrasts, lmean, cf))

    photoreceptorContrasts <- data.frame(
      led = c("rod", "scone", "mcone", "lcone"),
      contrast = round(pcs * 100,3)
    )

    ggplot(photoreceptorContrasts, aes(x = led, y = contrast, fill = led)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label=contrast), vjust=1.6, color="white")+
      scale_y_continuous("Contrast [%]", limits = c(0, 100), breaks = seq(0, 100, 10)) +
      scale_fill_manual(values = c("red","green", "blue", "black"),
                        breaks = c("lcone", "mcone", "scone", "rod")) +
      guides(fill = "none") +
      ggtitle("Photoreceptor contrasts")
  })

  output$staircases <- renderPlot({
    file <- input$raw_csv
    req(file)
    results <- parseTCS(file$datapath)[-1]

    thresholds1 <- apply(sapply(results$responses_1[, 2:9], as.numeric), 1, max)
    thresholds2 <- apply(sapply(results$responses_2[, 2:9], as.numeric), 1, max)


    # create responsetable
    response_table <-
      rbind(
        data.frame(staircase = "thresholds_1",
                   n = seq.int(nrow(results$responses_1)),
                   response = results$responses_1$response,
                   threshold = thresholds1),
        data.frame(staircase = "thresholds_2",
                   n = seq.int(nrow(results$responses_2)),
                   response = results$responses_2$response,
                   threshold = thresholds2)
      )

    thresholds <-
      data.frame(staircase = c("thresholds_1", "thresholds_2"),
                 threshold = apply(results$thresholds, 1, max))

    # plot table
    ggplot(response_table, aes(x = n, y = as.numeric(threshold))) +
      geom_bar(stat = "identity", aes(fill = response)) +
      geom_hline(data = thresholds, aes(yintercept = threshold)) +
      facet_wrap(~ staircase) +
      scale_y_continuous("Threshold [%]", limits = c(0, 100), breaks = seq(0, 100, 20)) +
      scale_x_discrete("Trial", limits = factor(1:max(response_table$n))) +
      scale_fill_manual("Stimulus\nseen", values = c("TRUE" = "darkgreen", "FALSE" = "darkred"))
  })

  output$gamut <- renderPlot({
    file <- input$raw_csv
    req(file)
    results <- parseTCS(file$datapath)$gamut

    if (is.null(results)) results <- data.frame(n = 1, staircase = c("staircase_1", "staircase_2"), contrast = -5)

    # plot table
    ggplot(results, aes(x = n, y = contrast)) +
      geom_point(color = "black") +
      facet_wrap(~ staircase) +
      scale_y_continuous("Threshold [%]", limits = c(-1, 100), breaks = seq(0, 100, 20)) +
      scale_x_discrete("Trial", limits = factor(1:3))
  })

  output$false_positives <- renderPlot({
    file <- input$raw_csv
    req(file)
    results <- parseTCS(file$datapath)$false_positives

    if (is.null(results)) results <- data.frame(n = 1, staircase = c("staircase_1", "staircase_2"), contrast = -5)

    # plot table
    ggplot(results, aes(x = n, y = contrast)) +
      geom_point(color = "black") +
      facet_wrap(~ staircase) +
      scale_y_continuous("Threshold [%]", limits = c(-1, 100), breaks = seq(0, 100, 20)) +
      scale_x_discrete("Trial", limits = factor(1:3))
  })

  output$parsedData2 <- renderPrint({
    file <- input$raw_csv
    req(file)
    parseTCS(file$datapath)[-1]
  })

  output$results <- renderPlot({
    req(input$raw_csv)

    if (is.null(values$sensitivity_table)) return(NULL)

    ggplot(values$sensitivity_table, aes(x = frequency, y = sensitivity)) +
      geom_point(aes(color = stimulus_type)) +
      facet_wrap(~ patid) +
      scale_color_manual("Photoreceptors",
                         values = c("M-cone" = "green",
                                    "S-cone" = "blue",
                                    "L-cone" = "red",
                                    "Rod" = "black",
                                    "Mixed" = "orange")) +
      scale_x_log10("Frequency") +
      scale_y_log10("Sensitivity")
  })

}
