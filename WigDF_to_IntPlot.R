###########################################################################################################################
Combine dt selection and plot
###############################################################################################################################
###R might needs to unlock settings to access bigger files
#options(shiny.maxRequestSize = 8000*1024^2)

###isert location to gene annotation file 
features_data <- read.csv(".csv", sep = "\t")
runApp(shinyApp(
  
  
  ui=(fluidPage(
    titlePanel("read data"),
    mainPanel(
      fileInput("file", "Upload file"),
      
      selectizeInput(
        inputId = "feature_name", 
        label = "Enter Gene Name", 
        choices = unique(features_data[c("genename","genelocus")]), 
        ###There need to be a pre selected option to render the y axis correctly at plot start
        selected = features_data$genename[2],
        multiple = FALSE
      ),
      
      selectizeInput(
        inputId = "strains", 
        label = "Select strains", 
        choices = unique(meta_data$file), 
        ###There need to be a pre selected option to render the y axis correctly at plot start
        selected = meta_data$file[1],
        multiple = TRUE
      ),
      
      numericInput("startPos", "start row:", 1, step = 1),
      numericInput("stopPos", "stop row:", 2, step = 1),
      actionButton("Go", "Go!"),

      plotlyOutput(outputId = "df_data_out")

    )
  )),
  
  server = (function(input, output, session) {
    values <- reactiveValues(df_data = NULL)
    
    observeEvent(input$file, {
      values$df_data <- read.csv(input$file$datapath, sep = "\t")
    })
    
    
    
    ### Find matching gene name and grab row nummber         ###Alt. Search only one line = geneA_row <- which(spider_data_genes[,1] == input$gene_name)
    observeEvent(input$feature_name, {
      values$feature_cord <- if(length(which(features_data[,2] == input$feature_name)) != 0) {
        features_data[which(features_data[,2] == input$feature_name),9:10]
      } else if(length(which(features_data[,3] == input$feature_name)) != 0) {
        features_data[which(features_data[,3] == input$feature_name),9:10]
      } 
      else {
        error_message <- "No match"
      }
    ###Use match to update coordinate boxes 
      updateNumericInput(session, "startPos", 
                         label = paste("Start ", input$feature_name),
                         value = as.integer(values$feature_cord[1])
                         )
      updateNumericInput(session, "stopPos", 
                         label = paste("Stop ", input$feature_name),
                         value = as.integer(values$feature_cord[2])
      )
    })

    
    
    create_df <- eventReactive(input$Go,{
      data.frame(values$df_data[input$startPos:input$stopPos, ])
    })
    ##Our Table, dont work, not needed #output$df_table_out <- renderTable(create_df()[grep(c(paste("WT.P4_1_reverse","Nico.2_forward",collapse="|")), create_df()$strain),])
    output$df_data_out <- renderPlotly({
      plot_ly(create_df(), x = ~ID, y = ~reads, color = ~sample, name = ~substr(strain, 1, nchar(strain)-8)) %>%
        # filter(strain %in% "NaCl.2.T_forward") %>%
        filter(strain %in% input$strains) %>%
        #      filter(ID %in% input$strains) %>%
        group_by(strain) %>%
        add_lines()
    })

    
  })))




