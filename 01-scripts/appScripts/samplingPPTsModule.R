### Cycles App Module

#creates a plot for the control and LBN offspring cycle plots
#Each animal is plotted individually
#Ability to filter by paraType, dam strain, and DOB

# https://shiny.rstudio.com/articles/modules.html

samplingPPTsUI <- function(id){
  ns <- NS(id)
  tagList(
    #header
    h3("Generate Sampling PPTs"),
    
    #filtering controls
    filteringDFUI(ns("sampling_filter")),
    
    tabsetPanel(
      tabPanel(
        "Remaining",
        
        h4("Remaining Today"),
        
        fluidRow(
          column(
            12,
            downloadButton(ns("download_remaining"), "Download remaining today")
          )
        )
      ),
      tabPanel(
        "For Date",
        h4("Presentation for Sampling Date"),
        
        fluidRow(
          column(
            6,
            dateInput(
              ns("presDate"),
              label = "Select Date for Presentation:",
              value = dateToday
            )
          ),
          column(
            6,
            downloadButton(ns("download_date"), "Download date's PPT")
          )
        )
      ),
      tabPanel(
        "All",
        
        h4("All Mice"),
        
        fluidRow(
          column(
            4,
            numericInput(
              ns("proUterineCutoff"),
              "Uterine mass (mg) min for proestrus:",
              value = 135
            ),
            numericInput(
              ns("diUterineCutoff"),
              "Uterine mass (mg) max for diestrus:",
              value = 100
            )
          ),
          column(
            4,
            radioButtons(
              ns("cycleStage"),
              "Which stages?",
              choices = c("Proestrus", "Diestrus", "All"),
              selected = "Proestrus"
            ),
            checkboxInput(
              ns("useMass"),
              "Filter by mass",
              value = TRUE
            ),
          ),
          column(
            4,
            downloadButton(ns("download_all"), "Download PowerPoint")
          )
        )
      )
    )
  )
}


samplingPPTsServer <- function(
  id,
  dateToday,
  AcuteStress_off,
  LBN_data,
  Cycles_off_all
  
){
  moduleServer(
    id,
    function(input, output, session) {
      AcuteStress_react <- filteringDFServer("sampling_filter", AcuteStress_off)
      
      filterByCycleStage <- function(df){
        if(input$cycleStage == "Proestrus"){
          df <- df %>%
            filter(
              if(input$useMass == TRUE) {ReproTract_mass > input$proUterineCutoff & Sac_cycle == "proestrus"}
              else {Sac_cycle == "proestrus"}
            )
        } else if(input$cycleStage == "Diestrus"){
          df <- df %>%
            filter(
              if(input$useMass == TRUE) {ReproTract_mass < input$diUterineCutoff & Sac_cycle == "diestrus"}
              else {Sac_cycle == "diestrus"}
            )
        }
        return(df)
      }
      
      samplingDF_selDate <- reactive({
        df <- AcuteStress_react() %>%
          filter(
            Sac_date == input$presDate
          )%>%
          addRegExForSamplingDF() %>%
          addSamplingImgFilePaths()
        return(df)
      })
      
      output$download_date <- downloadHandler(
        filename = function() {  
          paste0("sampling_", input$presDate, ".pptx")
        },
        content = function(file) {
          samplingPPT <- read_pptx("./samplingSlideTemplate.pptx")
          samplingPPT <- add_slide(samplingPPT, layout = "Title Slide")
          samplingPPT <- ph_with(
            samplingPPT,
            value = paste(input$presDate),
            location = ph_location_label("Title 1")
          )
          addSamplingSlidesFromDF(
            samplingDF_selDate(), 
            samplingPPT = samplingPPT, 
            cyclingDF = Cycles_off_all %>%
              filter(
                mouseID %in% samplingDF_selDate()$mouseID
              )
          )
          
          print(samplingPPT, target = file)
        }
      )
      
      samplingDF_ALPS <- reactive({
        df <- AcuteStress_react() %>%
          filter(
            sex == "F",
            !is.na(cyclingFolderPath),
            damStrain == "CBA"
          )%>%
          filterByCycleStage() %>%
          addRegExForSamplingDF() %>%
          addSamplingImgFilePaths()
        return(df)
      })
      
      output$download_all <- downloadHandler(
        filename = function() {  
          "samplingPPT.pptx"
        },
        content = function(file) {
          print("Starting to download")
         samplingPPT <- read_pptx("./samplingSlideTemplate.pptx")
          samplingPPT <- add_slide(samplingPPT, layout = "Title Slide")
          samplingPPT <- ph_with(
            samplingPPT,
            value = "LBN Cycling Images, Uterine Mass, and LH Values",
            location = ph_location_label("Title 1")
          )
          addSamplingSlidesFromDF(
            samplingDF_ALPS(), 
            samplingPPT = samplingPPT, 
            cyclingDF = Cycles_off_all %>%
              filter(
                mouseID %in% samplingDF_ALPS()$mouseID
              )
          )
          print(samplingPPT, target = file)
        }
      )
      
      remainingDF_today <- reactive({
        df <- LBN_data %>%
        filter(
          sex == "F",
          litterNum == 2,
          is.na(Sac_date),
          is.na(Sac_stop_off)
        ) %>%
        mutate(
          Sac_date = dateToday
        ) %>%
        calcAgeInDays() %>%
        combineStress() %>%
        addRegExForSamplingDF() %>%
        addSamplingImgFilePaths()
        return(df)
      })
      
      output$download_remaining <- downloadHandler(
        filename = function() {  
          paste0("remaining_", dateToday, ".pptx")
        },
        content = function(file) {
          samplingPPT <- read_pptx("./samplingSlideTemplate.pptx")
          samplingPPT <- add_slide(samplingPPT, layout = "Title Slide")
          samplingPPT <- ph_with(
            samplingPPT,
            value = paste("Mice remaining on", dateToday),
            location = ph_location_label("Title 1")
          )
          addSamplingSlidesFromDF(
            remainingDF_today(), 
            samplingPPT = samplingPPT, 
            cyclingDF = Cycles_off_all %>%
              filter(
                mouseID %in% remainingDF_today()$mouseID
              )
          )
          
          print(samplingPPT, target = file)
        }
      )
      
      
    }
  )
}

