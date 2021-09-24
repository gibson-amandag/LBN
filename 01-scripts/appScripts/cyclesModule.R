### Cycles App Module

# https://shiny.rstudio.com/articles/modules.html


# https://gist.github.com/wch/5436415/#gistcomment-1646351 

# https://stackoverflow.com/questions/38822863/shiny-dynamic-colour-fill-input-for-ggplot 
gg_fill_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cyclesUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      div(
        class = "col-xs-4",
        radioButtons(
          ns("xVar"),
          "Plot by:",
          c("Cycle Day" = "day", "PND" = "PND")
        )
      ),
      div(
        class = "col-xs-4",
        varSelectInput(
          ns("groupingVar"),
          "Grouping Variable",
          NULL
        ),
        selectInput(
          ns("includeLevels"),
          "Include levels",
          multiple = TRUE,
          choices = c()
        )
      ),
      div(
        class = "col-xs-4",
        checkboxInput(
          ns("useLineColor"),
          "Use line color",
          value = FALSE
        )
      )
    ),
    fluidRow(
      uiOutput(ns("colorUI"))
    ),
    
    actionButton(ns("resetColors"), "Default Colors", icon = icon("undo")),
    
    tabsetPanel(
      tabPanel(
        "Cycle Plots",
        fluidRow(
          div(
            class = "col-xs-4",
            sliderInput(
              ns("cycleDays"),
              "Select Days",
              min = 1,
              max = 100,
              step = 1,
              value = c(1, 100)
            )
          ),
          div(
            class = "col-xs-4",
            downloadButton(
              ns("downloadCyclePlot"),
              "Download current plot"
            )
          ),
          div(
            class = "col-xs-4",
            radioButtons(
              ns("imgType"),
              "Select File Type",
              choices = c("png", "pdf")
            )
          )
        ),
        plotOutput(ns("cyclesPlot"))
      ),
      
      # by mouse -------------
      tabPanel(
        "by mouse",
        fluidRow(
          div(
            class = "col-xs-4",
            selectInput(
              ns("selectedMouse"),
              label = "Select Mouse:",
              choices = character()
            ),
            actionButton(
              ns("makeImgDiv"),
              label = "Show Images",
              icon = icon("redo")
            )
          ),
          div(
            class = "col-xs-4",
            radioButtons(
              ns("imgsPerSlide"),
              label = "# imgs per slide",
              choices = c(4, 9, 12),
              selected = 12
            )
          ),
          div(
            class = "col-xs-4",
            downloadButton(
              ns("downloadMousePPT"),
              label = "Download Mouse's PPT"
            )
          )
        ),
        plotOutput(
          ns("selCyclePlot"),
          height = "200px"
        ),
        tags$div(id = "placeholder"),
        # uiOutput(
        #   ns("selImages")
        # )
      )
    )
  )
}


cyclesServer <- function(
  id,
  cycleDir,
  damInfo,
  offspringInfo,
  Cycles_off,
  compType
){
  moduleServer(
    id,
    function(input, output, session) {
      # print(cycleDir)
      
      cycles_react <- reactive({
        req(Cycles_off, input$groupingVar, input$includeLevels)
        if(!is.null(input$groupingVar)){
          df <- Cycles_off %>%
            filter(
              !! input$groupingVar %in% as.character(input$includeLevels)
            )
        # }
        } else (df <- Cycles_off)
        return(df)
      })
      
      # observeEvent(cycles_react(), {
      #   print("cycles react changed")
      # })
      # observeEvent(Cycles_off, {
      #   print("cycles off changed")
      # })
      
      observe({
        req(Cycles_off, input$groupingVar)
        df <- Cycles_off %>%
          makeCyclesLong(afterVar = cycleStartDate) %>%
          addPNDForCyles()
        minDay <- min(df$day, na.rm = TRUE)
        maxDay <- max(df$day, na.rm = TRUE)
        updateSliderInput(
          session = session,
          "cycleDays",
          min = minDay,
          max = maxDay,
          value = c(minDay, maxDay)
        )
      })
      
      cycles_long <- reactive({
        req(Cycles_off, input$groupingVar)
        # print(input$cycleDays[1])
        df <- cycles_react() %>%
          makeCyclesLong(afterVar = cycleStartDate) %>%
          addPNDForCyles()%>%
          filter(
            day >= input$cycleDays[1] & day <= input$cycleDays[2]
          )
        return(df)
      })
      
      # Cycle Plots Panel -------------------------------------------------------
      
      ## Color Variable and Levels ------------------
      observeEvent(Cycles_off, {
        updateVarSelectInput(
          session,
          "groupingVar",
          data = Cycles_off %>% 
            select(where(~ is.character(.x) | is.factor(.x)))
        )
      })
      
      groupingVarLevels <- reactive({
        req(Cycles_off, input$groupingVar)
        unique(Cycles_off[[input$groupingVar]])
      })
      
      observeEvent(input$groupingVar,{
       updateSelectInput(
          session = session,
          inputId = "includeLevels",
          choices = groupingVarLevels(),
          selected = groupingVarLevels()
        )
      })
      
      output$colorUI <- renderUI({
        req(Cycles_off, input$groupingVar)
        
        if(input$useLineColor){
          lev <- groupingVarLevels()
          cols <- gg_fill_hue(length(lev))
          
          # New IDs "col+level"
          lapply(seq_along(lev), function(i) {
            column(
              4,
              colourInput(inputId = session$ns(paste0("col", i)),
                          label = paste0("Choose color for ", lev[i]), 
                          value = cols[i]
              )
            )
          })
        }
      })
      
      observeEvent(input$resetColors, {
        # Problem: dynamic number of widgets
        # - lapply, do.call
        
        lev <- groupingVarLevels()
        cols <- gg_fill_hue(length(lev))
        
        lapply(seq_along(lev), function(i) {
          do.call(what = "updateColourInput",
                  args = list(
                    session = session,
                    inputId = paste0("col", lev[i]),
                    value = cols[i]
                  )
          )
        })
      })
      
      ## Plot --------------------------
      nrowsPlot <- reactive({
        numMice <- nrow(cycles_react())
        nrowsPlot <- ceiling(numMice/4)
        nrowsPlot <- ifelse(nrowsPlot > 0, nrowsPlot, 1)
        return(nrowsPlot)
      })
      
      groupCyclesPlot <- reactive({
        req(nrow(cycles_long()) > 0, input$groupingVar)
        
        
        if(input$useLineColor){
          lineColor = expr(.data[[input$groupingVar]])
          noLegends = FALSE
          lev <- groupingVarLevels()
          cols <- paste0("c(", paste0("input$col", 1:length(lev), collapse = ", "), ")")
          # print(cols)
          cols <- eval(parse(text = cols))
          # print(cols)
          
          req(length(lev) == length(cols))
        } else{
          lineColor = NULL
          noLegends = TRUE
          lev = NULL
          cols = NULL
        }
        
        viz <- cycles_long() %>%
          plotCycleTraces(
            day = .data[[input$xVar]],
            lineColorVar = !! lineColor,
            colorLimits = lev,
            colorValues = cols,
            removeFacets = FALSE,
            removeLegend = noLegends,
            scales = "free_x",
            ncol = 4
          )
        return(viz)
      })
      
      output$cyclesPlot <- renderPlot(
        groupCyclesPlot(),
        height = function() {nrowsPlot() * 150} #function prevents it from resetting each time
      )
      
      output$downloadCyclePlot <- downloadHandler(
        filename = function() {
          paste0("cyclePlots-", Sys.Date(), ".", input$imgType)
        },
        content = function(file) {
          flexSave(
            baseName = file,
            thisFilePrefix = NULL,
            plot = last_plot(),
            fileType = input$imgType,
            filePath = NULL,
            width = 8,
            height = ifelse(nrowsPlot() * 2 < 10, nrowsPlot() * 2, 10),
            units = "in",
            compType = currentCompType,
            shinySettings = TRUE,
          )
        }
      )
      
      # By Mouse ----------------------------------------------------------------
      observeEvent(cycles_react(), {
        updateSelectInput(
          session,
          "selectedMouse",
          choices = cycles_react()$mouseID
        )
      })
      
      selectedMouseFiles <- eventReactive(
        # c(input$selectedMouse, Cycles_off), 
        input$selectedMouse,
        {
        req(Cycles_off, input$selectedMouse)
        print("Updating selected files")
        
        
        cycleID <- cycles_react()$cycleID[which(cycles_react()$mouseID == input$selectedMouse)]
        
        if(! length(cycleID) == 0){
          idText <- sprintf("%04d", as.integer(cycleID))
          
          fileEnding <- paste0("*", idText, ".jpg")
          
          files <- dir_ls(
            normalizePath(cycleDir),
            all = TRUE,
            recurse = TRUE,
            type = "any",
            glob = fileEnding
          ) 
          sortOrder <- files %>% path_file() %>% order()
          files <- files[sortOrder]
          # print(files)
          return(
            files
          )
        } else{
          return(
            NULL
          )
        }
      })
      
      ## Individual Mouse Plot -------------------
      output$selCyclePlot <- renderPlot({
        req(Cycles_off, input$groupingVar)
        
        if(input$useLineColor){
          lineColor = expr(.data[[input$groupingVar]])
          noLegends = FALSE
          lev <- groupingVarLevels()
          cols <- paste0("c(", paste0("input$col", lev, collapse = ", "), ")")
          # print(cols)
          cols <- eval(parse(text = cols))
          # print(cols)
          
          req(length(lev) == length(cols))
        } else{
          lineColor = NULL
          noLegends = TRUE
          lev = NULL
          cols = NULL
        }
        
        cycles_long() %>%
          filter(
            mouseID == input$selectedMouse
          ) %>%
          plotCycleTraces_single(
            day = .data[[input$xVar]],
            lineColorVar = !! lineColor,
            colorLimits = lev,
            colorValues = cols,
            removeFacets = FALSE,
            removeLegend = noLegends
          )
      })
      
      ## Images UI -------------------------------
      
      ### Build UI -------------------------------
      observeEvent(
        c(
          # Cycles_off, 
          input$selectedMouse
        ),
      {
        # Remove the old one
        # print("removing old UI")
        removeUI("#cyclesDiv")
      })
        
      observeEvent(
        input$makeImgDiv,
      {
        req(selectedMouseFiles())
        image_output_list <-
          lapply(1:length(selectedMouseFiles()),
          function(i)
          {
          fileName <- paste0("name", i)
          stageName <- paste0("stage", i)
          imagename <- paste0("image", i)
          tags$div(
            class = "col-xs-6 col-sm-4 col-md-3", # "col-sm-3",
            textOutput(session$ns(fileName), container = h4),
            textOutput(session$ns(stageName), container = p),
            imageOutput(session$ns(imagename), height = "auto") # auto fixes the overlap
          )
          })

        # print(image_output_list)

        thisUI <- div(
          class = "container-fluid",
          id = "cyclesDiv",
          div(
            class = "row",
            p(class = "col", "The file names and stage may take a few seconds to update. Be patient")
          ),
          div(
            class = "row",
            do.call(tagList, image_output_list)
          )
        )
        
        # print("making new UI")
        insertUI(
          selector = '#placeholder',
          ui = thisUI
        )
        
        # Number of images within directory
        numImgs <- length(selectedMouseFiles())
        
        withProgress(message = 'Updating images', value = 0, {
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
          
          for (i in 1:numImgs)
          {
            # Increment the progress bar, and update the detail text.
            incProgress(1/numImgs, detail = paste("Doing image", i))
            
            local({
              my_i <- i
              imagename <- paste0("image", my_i)
              stageName <- paste0("stage", my_i)
              fileName <- paste0("name", my_i)
              outputWidth <- paste0("output_", imagename, "_width")
              outputHeight <- paste0("output_", imagename, "_height")
              # print(imagename)
              
              thisFileName <- selectedMouseFiles()[my_i] %>%
                path_file() %>%
                path_ext_remove()
              
              fileDate <- str_extract(thisFileName, "^20[0-2][0-9]-((0[1-9])|(1[0-2]))-(0[1-9]|[1-2][0-9]|3[0-1])")
              
              df <- cycles_long() %>%
                filter(
                  mouseID == input$selectedMouse,
                  cycleDate == as_date(fileDate)
                )
              
              thisStage <- df$stage[1]
              
              thisStageName <- case_when(
                thisStage == 1 ~ "estrus",
                thisStage == 2 ~ "diestrus",
                thisStage == 3 ~ "proestrus",
                TRUE ~ "no stage scored"
              )
              
              output[[fileName]] <- renderText({
                thisFileName
              })
              
              output[[stageName]] <- renderText({
                return(thisStageName)
              })
              
              output[[imagename]] <-
                renderImage({
                  list(src = selectedMouseFiles()[my_i],
                       width = "100%",
                       height = "auto",
                       alt = "Image failed to render")
                }, deleteFile = FALSE)
            })
          }
        })
      })
      
      # output$selImages <- renderUI({
      #   req(Cycles_off, input$selectedMouse, selectedMouseFiles())
      #   
      #   print("Updating cycle images space. Please be patient")
      #   
      #   image_output_list <-
      #     # lapply(1:nrow(selectedMouseFiles()),
      #     lapply(1:length(selectedMouseFiles()),
      #            function(i)
      #            {
      #              fileName <- paste0("name", i)
      #              stageName <- paste0("stage", i)
      #              imagename <- paste0("image", i)
      #              tags$div(
      #                class = "col-sm-3",
      #                textOutput(session$ns(fileName), container = h4),
      #                textOutput(session$ns(stageName), container = p),
      #                imageOutput(session$ns(imagename), height = "auto") # auto fixes the overlap
      #              )
      #            })
      #   
      #   thisUI <- div(
      #     class = "container-fluid",
      #     div(
      #       class = "row",
      #       do.call(tagList, image_output_list)
      #     )
      #   )
      #   print("Finished UI space")
      #   return(thisUI)
      # })
      
      
      ### Fill UI -------------
      observeEvent(
        input$selectedMouse,
        {
        req(Cycles_off, input$selectedMouse, selectedMouseFiles())
        
        # # Number of images within directory
        # numImgs <- length(selectedMouseFiles())
        # 
        # for (i in 1:numImgs)
        # {
        #   
        #   local({
        #     my_i <- i
        #     imagename <- paste0("image", my_i)
        #     stageName <- paste0("stage", my_i)
        #     fileName <- paste0("name", my_i)
        #     outputWidth <- paste0("output_", imagename, "_width")
        #     outputHeight <- paste0("output_", imagename, "_height")
        #     # print(imagename)
        #     
        #     thisFileName <- selectedMouseFiles()[my_i] %>%
        #       path_file() %>%
        #       path_ext_remove()
        #     
        #     fileDate <- str_extract(thisFileName, "^20[0-2][0-9]-((0[1-9])|(1[0-2]))-(0[1-9]|[1-2][0-9]|3[0-1])")
        #     
        #     df <- cycles_long() %>%
        #       filter(
        #         mouseID == input$selectedMouse,
        #         cycleDate == as_date(fileDate)
        #       )
        #     
        #     thisStage <- df$stage[1]
        #     
        #     thisStageName <- case_when(
        #       thisStage == 1 ~ "estrus",
        #       thisStage == 2 ~ "diestrus",
        #       thisStage == 3 ~ "proestrus",
        #       TRUE ~ "no stage scored"
        #     )
        #     
        #     output[[fileName]] <- renderText({
        #       thisFileName
        #     })
        #     
        #     output[[stageName]] <- renderText({
        #       return(thisStageName)
        #     })
        #     
        #     output[[imagename]] <-
        #       renderImage({
        #         list(src = selectedMouseFiles()[my_i],
        #              width = "100%",
        #              height = "auto",
        #              alt = "Image failed to render")
        #       }, deleteFile = FALSE)
        #   })
        # }
        # print("Finished filling cycle imaging space")
      })
      
      ## Powerpoint Output --------------------
      individualPPT <- reactive({
        req(input$imgsPerSlide) # To get to reset when this changes
        ppt <- read_pptx("estrousCycleTemplate.pptx")
        ppt <- add_slide(ppt, layout = "Title Slide")
        ppt <- ph_with(x = ppt, value = input$selectedMouse, location = ph_location_label("Title 1"))
      })
      
      output$downloadMousePPT <- downloadHandler(
        filename = function() {  
          paste0("cycleImgs_", input$selectedMouse, "-", Sys.Date(), ".pptx")
        },
        content = function(file) {
          # Number of images within directory
          numImgs <- length(selectedMouseFiles())
          
          # First index
          iImg <- 1
          # print("reset iImg")
          
          cyclePPT <- individualPPT()
          
          for (i in 1:numImgs)
          {
            thisFileName <- selectedMouseFiles()[i] %>%
              path_file() %>%
              path_ext_remove()
            
            fileDate <- str_extract(thisFileName, "^20[0-2][0-9]-((0[1-9])|(1[0-2]))-(0[1-9]|[1-2][0-9]|3[0-1])")
            
            df <- cycles_long() %>%
              filter(
                mouseID == input$selectedMouse,
                cycleDate == as_date(fileDate)
              )
            
            thisStage <- df$stage[1]
            
            thisStageName <- case_when(
              thisStage == 1 ~ "estrus",
              thisStage == 2 ~ "diestrus",
              thisStage == 3 ~ "proestrus",
              TRUE ~ "no stage scored"
            )
            
            img <- external_img(selectedMouseFiles()[i], width = 2.68, heigh = 2.14) # get the image
            textID <- paste0("text", iImg)
            stageID <- paste0("stage", iImg)
            imgID <- paste0("img", iImg)
            
            # If img index is 1, add a new slide
            if(iImg == 1){
              slideLayout <- paste0("estrousCycle", input$imgsPerSlide)
              cyclePPT <- add_slide(x = cyclePPT, layout = slideLayout)
            }
            
            # add the title
            cyclePPT <- ph_with(
              x = cyclePPT, value = thisFileName,
              location = ph_location_label(
                textID
              ),
              use_loc_size = TRUE)
            
            # add the title
            cyclePPT <- ph_with(
              x = cyclePPT, value = thisStageName,
              location = ph_location_label(
                stageID
              ),
              use_loc_size = TRUE)
            
            # add the image
            cyclePPT <- ph_with(
              x = cyclePPT, value = img,
              location = ph_location_label(
                imgID
              ),
              use_loc_size = TRUE)
            
            
            # if index is less than the number per slide, add one, otherwise, restart at 1
            if(iImg < as.numeric(input$imgsPerSlide)) {
              iImg <- iImg + 1
            } else {
              iImg <- 1
            }
          }
          
          print(individualPPT(), target = file)
        }
      )
      
    }
  )
}

