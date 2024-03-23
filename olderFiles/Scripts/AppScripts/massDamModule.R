### Dam Mass App Module

# https://shiny.rstudio.com/articles/modules.html

massDamUI <- function(id,
                      Demo_dam){
  ns <- NS(id)
  tagList(
    h3("Dam Mass"),
    
    filteringDFUI(
      ns("Mass_dams_filter"), Demo_dam
    ),
    
    fluidRow(
      column(
        4,
        #plot by litter number?
        checkboxInput(
          ns("Mass_dams_by_litterNum"),
          "Plot by litter number?",
          value = TRUE
        )
      ),
      column(
        4,
        #plot individual dams?
        checkboxInput(
          ns("Mass_dams_individual_lines"),
          "Plot individual lines?",
          value = TRUE
        ),
        #plot means?
        checkboxInput(
          ns("Mass_dams_mean_lines"),
          "Plot mean lines?",
          value = TRUE
        ),
        
      ),
      column(
        4,
        #Add a title
        textInput(
          ns("Mass_dams_title"),
          "Graph Title:"
        )
      )
    ),
    
    zoomAxisUI(ns("zoom_x"), "x"),
    
    zoomAxisUI(ns("zoom_y"), "y"),
    
    #plot dam mass
    plotOutput(ns("Mass_dams_plot")),
    
    h3("Summary Table"),
    
    summaryTableUI(
      id = ns("massDamSum"), 
      df_sum = Demo_dam %>%
        select(Dam_Mass_P2:Dam_Mass_P21), #data frame with possible columns
      selected_sum =  c(
        "Dam_Mass_P4",
        "Dam_Mass_P11",
        "Dam_Mass_P21"
      ), # c(" ", " ") vector with selected variables
      df_group = Demo_dam %>%
        select(
          Litter_num,
          Treatment:Dam_Strain,
          Sac_or_stop
        ),
      selected_group = c(
        "Treatment"
      )
    )
    
  )
}


massDamServer <- function(
  id,
  Demo_dam
){
  moduleServer(
    id,
    function(input, output, session) {
      
      zoom_x <- zoomAxisServer("zoom_x", "x", minVal = 0, maxVal = 21)
      
      zoom_y <- zoomAxisServer("zoom_y", "y", minVal = 0, maxVal = 30)
      
      #Plot for dam mass - line plot
      output$Mass_dams_plot <- renderPlot({
        Mass_dams <- Demo_dam %>%
          select(
            Dam_ID, 
            Treatment:Strain,
            DOB:Litter_size_endPara, 
            pupLoss,
            Litter_num,
            Cohort
          ) 
        
        Mass_dams_long <- reshapeForMassPlot_dams(Mass_dams)
        
        Mass_dams_long_react <- filteringDFServer("Mass_dams_filter", Mass_dams_long)
        
        #Mass plot
        mass_plot_lines_litterNum(
          Mass_dams_long_react(),
          line_group = expr(Dam_ID),
          by_litterNum = input$Mass_dams_by_litterNum,
          individualLines = input$Mass_dams_individual_lines,
          mean_lines = input$Mass_dams_mean_lines,
          title = input$Mass_dams_title,
          zoom_x = zoom_x$zoom(),
          xmin = zoom_x$min(),
          xmax = zoom_x$max(),
          zoom_y = zoom_y$zoom(),
          ymin = zoom_y$min(),
          ymax = zoom_y$max(),
          width = 0.2
        )
      })
      
      #Mass summary table
      massDamSum <- summaryTableServer("massDamSum", reactive(Demo_dam))
      
      
    }
  )
}

