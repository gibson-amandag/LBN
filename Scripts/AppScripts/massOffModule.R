### Offspring Mass App Module

# https://shiny.rstudio.com/articles/modules.html

massOffUI <- function(id,
                      Mass_off){
  ns <- NS(id)
  tagList(
    
    h3("Offspring Mass"),
    
    filteringDFUI(ns("MassOff_filter")),
    
    fluidRow(
      column(4,
             radioButtons(ns("WhichSex"),
                          "Which sex?",
                          c("Both", "Male" = "M", "Female" = "F"),
                          selected = "Both")
      ),
      column(4,
             #plot individual dams?
             checkboxInput(ns("Individual_lines"),
                           "Plot individual lines?",
                           value = TRUE),
             #plot means?
             checkboxInput(ns("Mean_lines"),
                           "Plot mean lines?",
                           value = TRUE),
             #plot by dam strain?
             checkboxInput(ns("By_strain"),
                           "Plot by strain?",
                           value = TRUE)
             
      ),
      column(4,
             #Add a title
             textInput(ns("Title"),
                       "Graph Title:"),
             #plot by dam?
             checkboxInput(ns("By_dam"),
                           "Plot by litter?",
                           value = FALSE)
             )
      ),
      
    zoomAxisUI(ns("zoom_x"), "x"),
    
    zoomAxisUI(ns("zoom_y"), "y"),

    #plot dam mass
    plotOutput(ns("Plot"), height = "600px"),
    
    h3("Summary Table"),
    
    filteringDFUI(ns("sum_filter")),
    
    summaryTableUI(
      id = ns("massOffSum"), 
      df_sum = Mass_off %>%
        select(Avg_litter_mass_startPara:Mass_P72), #data frame with possible columns
      selected_sum = c("Avg_litter_mass_startPara",
                       "Mass_P9",
                       "Mass_P11"), # c(" ", " ") vector with selected variables
      df_group = Mass_off %>%
        select(Sex:Treatment,
               Dam_ID,
               Dam_Strain:ParaType),
      selected_group = c("Treatment",
                         "Dam_Strain")
    )
    
  )
}


massOffServer <- function(id,
                          Mass_off,
                          Demo_dam
                          ){
  moduleServer(
    id,
    function(input, output, session) {
      
      zoom_x <- zoomAxisServer("zoom_x", "x", minVal = 0, maxVal = 21)
      
      zoom_y <- zoomAxisServer("zoom_y", "y", minVal = 0, maxVal = 15)
      
      output$Plot <- renderPlot({
        #needs to be before the averaging by litter
        if(input$WhichSex == "M"){
          Mass_off <- Mass_off %>%
            filter(Sex == "M")
        }else if(input$WhichSex == "F"){
          Mass_off <- Mass_off %>%
            filter(Sex == "F")
        }
        
        if(input$By_dam == FALSE){
          Mass_off_long <- reshapeForMassPlot(Mass_off)
        }
        
        if(input$By_dam == TRUE){
          Mass_off_long <- Mass_off %>%
            getAvgByDam(Demo_dam) %>%
            reshapeForMassPlot()
        }
        
        Mass_off_react <- filteringDFServer("MassOff_filter", Mass_off_long)
        
        mass_plot_lines(Mass_off_react(),
                        line_group = ifelse(input$By_dam, expr(Dam_ID), expr(Mouse_ID)),
                        by_strain = input$By_strain,
                        individualLines = input$Individual_lines,
                        mean_lines = input$Mean_lines,
                        title = input$Title,
                        zoom_x = zoom_x$zoom(),
                        xmin = zoom_x$min(),
                        xmax = zoom_x$max(),
                        zoom_y = zoom_y$zoom(),
                        ymin = zoom_y$min(),
                        ymax = zoom_y$max())
      })
      
      Mass_off_sum_react <- filteringDFServer("sum_filter", Mass_off)
      
      massOffSum <- summaryTableServer("massOffSum", Mass_off_sum_react)

      
    }
  )
}

