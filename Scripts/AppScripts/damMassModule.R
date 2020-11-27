### Dam Mass App Module

# https://shiny.rstudio.com/articles/modules.html

damMassUI <- function(id,
                      Demo_dam){
  ns <- NS(id)
  tagList(
    h3("Dam Mass"),
    
    fluidRow(
      column(4,
             #Select paradigm type
             radioButtons(ns("Mass_dams_ParaTypes"),
                          "Which paradigm type?",
                          c("Both", "P2-P9" = 2, "P4-P11" = 4),
                          selected = "Both"),
             #select dam strains
             radioButtons(ns("Mass_dams_whichStrain"),
                          "Which dam strains?",
                          c("Both", "B6", "CBA"),
                          selected = "Both")
      ),
      column(4,
             #plot by dam strain?
             checkboxInput(ns("Mass_dams_by_strain"),
                           "Plot by strain?",
                           value = TRUE),
             #plot individual dams?
             checkboxInput(ns("Mass_dams_individual_lines"),
                           "Plot individual lines?",
                           value = TRUE),
             #plot means?
             checkboxInput(ns("Mass_dams_mean_lines"),
                           "Plot mean lines?",
                           value = TRUE),
             #Add a title
             textInput(ns("Mass_dams_title"),
                       "Graph Title:"),
             #what range of DOBs?
             dateRangeInput(ns("Mass_dams_DOBs"),
                            "Select range of birth dates",
                            start = "2019-12-15",
                            end = Sys.Date())
      ),
      column(2,
             #Zoom x?
             checkboxInput(ns("Mass_dams_zoom_x"),
                           "Zoom x axis?"),
             conditionalPanel(
               condition = "input.Mass_dams_zoom_x == true",
               numericInput(ns("Mass_dams_xmin"),
                            "Lower Limit x-axis:",
                            0),
               numericInput(ns("Mass_dams_xmax"),
                            "Upper Limit x-axis:",
                            21)
             )
      ),
      column(2,
             #Zoom y?
             checkboxInput(ns("Mass_dams_zoom_y"),
                           "Zoom y axis?"),
             conditionalPanel(
               condition = "input.Mass_dams_zoom_y == true",
               numericInput(ns("Mass_dams_ymin"),
                            "Lower Limit y-axis:",
                            0),
               numericInput(ns("Mass_dams_ymax"),
                            "Upper Limit y-axis:",
                            15))
      )
      
    ),
    #plot dam mass
    plotOutput(ns("Mass_dams_plot")),
    
    h3("Summary Table"),
    fluidRow(
      column(4,
             varSelectInput(ns("Mass_dams_vars_to_sum"),
                            "Select variables to summarize",
                            data = Demo_dam %>%
                              select(Dam_Mass_P2:Dam_Mass_P21),
                            selected = c("Dam_Mass_P2",
                                         "Dam_Mass_P4",
                                         "Dam_Mass_P9",
                                         "Dam_Mass_P11"),
                            multiple = TRUE)),
      column(4,
             varSelectInput(ns("Mass_dams_grouping_vars"),
                            "Select variables to group by",
                            data = Demo_dam %>%
                              select(Treatment:Dam_Strain,
                                     ParaType,
                                     Sac_or_stop),
                            selected = c("Treatment",
                                         "Dam_Strain"),
                            multiple = TRUE))
    ),
    dataTableOutput(ns("Mass_dam_summary"))
    
  )
}


damMassServer <- function(id,
                          Demo_dam
                          ){
  moduleServer(
    id,
    function(input, output, session) {
    
      #Plot for dam mass - line plot
      output$Mass_dams_plot <- renderPlot({
        Mass_dams <- Demo_dam %>%
          select(Dam_ID, Treatment:Strain,
                 DOB:Litter_size_endPara, pupLoss)
        
        Mass_dams_long <- reshapeForMassPlot_dams(Mass_dams)
        
        #Filter for paradigm type
        if(input$Mass_dams_ParaTypes == 2){
          Mass_dams_long <- Mass_dams_long %>%
            filter(ParaType == 2)
        }else if(input$Mass_dams_ParaTypes == 4){
          Mass_dams_long <- Mass_dams_long %>%
            filter(ParaType == 4)
        }
        
        #Filter for DOB
        Mass_dams_long <- Mass_dams_long %>%
          filter(DOB >= input$Mass_dams_DOBs[1] & DOB <= input$Mass_dams_DOBs[2])
        
        #Filter for Strain - By Dam Strain
        if(input$Mass_dams_whichStrain == "B6"){
          Mass_dams_long <- Mass_dams_long %>%
            filter(Dam_Strain == "B6")
        }else if(input$Mass_dams_whichStrain == "CBA"){
          Mass_dams_long <- Mass_dams_long %>%
            filter(Dam_Strain == "CBA")
        }
        
        #Mass plot
        mass_plot_lines(Mass_dams_long,
                        line_group = expr(Dam_ID),
                        by_strain = input$Mass_dams_by_strain,
                        individualLines = input$Mass_dams_individual_lines,
                        mean_lines = input$Mass_dams_mean_lines,
                        title = input$Mass_dams_title,
                        zoom_x = input$Mass_dams_zoom_x,
                        xmin = input$Mass_dams_xmin,
                        xmax = input$Mass_dams_xmax,
                        zoom_y = input$Mass_dams_zoom_y,
                        ymin = input$Mass_dams_ymin,
                        ymax = input$Mass_dams_ymax,
                        width = 0.2)
      })
      
      #Mass summary table
      output$Mass_dam_summary <- renderDataTable(
        map_dfr(input$Mass_dams_vars_to_sum, LBN_summary_byGroup, Demo_dam, input$Mass_dams_grouping_vars)
      )

      
    }
  )
}

