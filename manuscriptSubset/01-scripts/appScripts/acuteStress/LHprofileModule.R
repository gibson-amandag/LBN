### Acute Stress App Module

# https://shiny.rstudio.com/articles/modules.html

LHprofileUI <- function(id){
  ns <- NS(id)
  tagList(
    zoomAxisUI(ns("LHprofile_zoom_y"), "y"),
    filterLHUI(ns("filterLH")),
    plotUI(
      ns("LHprofile")
    ),
    # plotOutput(
    #   ns("LHprofile"),
    #   click = ns("LHprofile_click"),
    #   height = "600px"
    # ),
    
    verbatimTextOutput(
      ns("LHprofile_info")
    ),
    plotUI(
      ns("propSurged")
    )
  )
}


LHprofileServer <- function(
  id,
  LH_long,
  AcuteStressDF,
  proUterineCutoff,
  diUterineCutoff,
  dotSize,
  thisCompType
){
  moduleServer(
    id,
    function(input, output, session) {
      LHfilter <- filterLHServer("filterLH", minVal = 3, display = TRUE)
      
      observeEvent(LHfilter, print(LHfilter$surgeMin()))
      
      LH_long_react <- reactive({
        df <- LH_long 
        
        # 2024-02-06. Fast way to exclude ephys mice
        nonEphysMice <- surgedDF %>%
          filter(
            !is.na(LH_hr7.5) & !is.na(LH_hr5.5)
          )
        
        filterLH <- LHfilter$useFilter()
        minLH <- LHfilter$surgeMin()
        df <- df %>%
          filter(
            mouseID %in% nonEphysMice$mouseID
          )

        if(!is.null(filterLH) & !is.null(minLH)){
          if(filterLH & !is.na(minLH)){
            df <- df %>%
              filter(
                maxLH >= minLH
              )
          }
        }
        return(df)
      })
      ## LH Profile -----------------------------------------------------------
      LHprofile_zoom_y <- zoomAxisServer("LHprofile_zoom_y", "y", minVal = 0, maxVal = 45)
      
      # The warnings about removing rows seem to be related to error bars when there is only one in a group at a time point
      plot <- reactive({
        plot <- LH_long_react()%>% 
          LHPlot(
            fontSize = 16,
            dotSize = dotSize,
            zoom_y = LHprofile_zoom_y$zoom(),
            ymin = LHprofile_zoom_y$min(),
            ymax = LHprofile_zoom_y$max(),
            dodgeAmnt = 0.25
          )+
          facet_wrap(
            # ~adultTrt
            ~comboTrt
          ) +
          geom_hline(yintercept = LHfilter$surgeMin(), color = "blue")
        return(plot)
      })
      
      LHprofileInfo <- plotServer("LHprofile", plot, "LH_plot", thisCompType)
      
      # output$LHprofile <- renderPlot({
      #   plot()
      # })
      
      output$LHprofile_info <- renderPrint({
        if(is.null(LHprofileInfo$click())){
          "Click on a point to display values - click in the middle of the horizontal scatter"
        }else(
          # print(input$LHprofile_click)
          nearPoints(
            LH_long_react() %>%
              select(
                mouseID,
                num_ID,
                earlyLifeTrt,
                adultTrt,
                comboTrt, # has to be included for nearPoints to work
                time,
                LH,
                ReproTract_mass
              ),
            # input$LHprofile_click,
            LHprofileInfo$click(),
            xvar = "time",
            yvar = "LH"
          )
        )
      })
      
      plotSurged <- reactive({
        req(LHfilter$surgeMin())
        
        # 2024-02-06. Fast way to exclude ephys mice
        nonEphysMice <- surgedDF %>%
          filter(
            !is.na(LH_hr7.5) & !is.na(LH_hr5.5)
          )
        
        AcuteStressDF %>%
          mutate(
            surged = maxLH > LHfilter$surgeMin()
          ) %>%
          filter(
            Sac_cycle == "proestrus",
            ReproTract_mass > 125 # hard-coded
            # ,litterNum == 2
            , mouseID %in% nonEphysMice$mouseID
          ) %>%
          propSurgedPlot(
            fontSize = 16
          )
      })
      
      propSurgedInfo <- plotServer(
        "propSurged",
        plotSurged,
        "propSurged",
        thisCompType
      )
    }
  )
}

