### Offspring Mass App Module

# https://shiny.rstudio.com/articles/modules.html

massOffUI <- function(
  id,
  Mass_off,
  LBN_data
){
  ns <- NS(id)
  tagList(
    
    h3("Offspring Mass"),
    
    tabsetPanel(
      tabPanel(
        "Graph",
        fluidRow(
          column(
            12,
            filteringDFUI(ns("MassOff_filter"), Mass_off),
          )
        ),
        fluidRow(
          column(
            4,
            radioButtons(
              ns("WhichSex"),
              "Which sex?",
              c("Both", "Male" = "M", "Female" = "F"),
              selected = "Both"
            )
          ),
          column(
            4,
            #plot individual dams?
            checkboxInput(
              ns("Individual_lines"),
              "Plot individual lines?",
              value = TRUE
            ),
            #plot means?
            checkboxInput(
              ns("Mean_lines"),
              "Plot mean lines?",
              value = TRUE
            ),
            #plot by litter number?
            checkboxInput(
              ns("By_litterNum"),
              "Plot by litter number?",
              value = TRUE
            )
            
          ),
          column(
            4,
            #Add a title
            textInput(ns("Title"), "Graph Title:"),
            #plot by dam?
            checkboxInput(
              ns("By_dam"),
              "Plot by litter?",
              value = FALSE
            )
          ),
          
        ),
        
        zoomAxisUI(ns("zoom_x"), "x"),
        
        zoomAxisUI(ns("zoom_y"), "y"),
        
        #plot dam mass
        plotOutput(ns("Plot"), height = "600px")
      ),
      tabPanel(
        "Summary Table",
        h3("Summary Table"),
        
        filteringDFUI(ns("sum_filter"), Mass_off),
        
        summaryTableUI(
          id = ns("massOffSum"), 
          df_sum = Mass_off %>%
            select(Avg_litter_mass_startPara:Mass_P72), #data frame with possible columns
          selected_sum = c(
            "Avg_litter_mass_startPara",
            "Mass_P9",
            "Mass_P11"
          ), # c(" ", " ") vector with selected variables
          df_group = Mass_off %>%
            select(
              Sex:Treatment,
              Dam_ID,
              Dam_Strain:ParaType,
              Litter_num
            ),
          selected_group = c(
            "Treatment",
            "Dam_Strain"
          )
        )
      ),
      tabPanel(
        "Litter Size",
        filteringDFUI(ns("litterSize_filter"), Mass_off),
        varSelectInput(
          ns("massDay"),
          label = "Select Mass Day",
          data = Mass_off %>% select(Mass_P11:Mass_P21)
        ),
        
        plotOutput(
          ns("litterSizeMass"),
          height = "600px"
        )
      ),
      tabPanel(
        "Regression",
        massRegUI(ns("massReg"), Mass_off)
      )
    )
  )
}


massOffServer <- function(
  id,
  Mass_off,
  Demo_dam
){
  moduleServer(
    id,
    function(input, output, session) {
      
      massRegServer("massReg", Mass_off)
      
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
        
        mass_plot_lines_litterNum(
          Mass_off_react() %>% filter(!is.na(Mass)),
          line_group = ifelse(input$By_dam, expr(Dam_ID), expr(Mouse_ID)),
          by_litterNum = input$By_litterNum,
          individualLines = input$Individual_lines,
          mean_lines = input$Mean_lines,
          title = input$Title,
          zoom_x = zoom_x$zoom(),
          xmin = zoom_x$min(),
          xmax = zoom_x$max(),
          zoom_y = zoom_y$zoom(),
          ymin = zoom_y$min(),
          ymax = zoom_y$max()
        )
      })
      
      Mass_off_sum_react <- filteringDFServer("sum_filter", Mass_off)
      Mass_off_litterSize_react <- filteringDFServer("litterSize_filter", Mass_off)
      
      massOffSum <- summaryTableServer("massOffSum", Mass_off_sum_react)
      
      output$litterSizeMass <- renderPlot({
        Mass_off_litterSize_react() %>%
          filter(!is.na(!! input$massDay)) %>%
          ggplot(aes(
            x = Litter_size_endPara, y = !! input$massDay, 
            color = Treatment,
            shape = Cohort
            # shape = Litter_num
          )
          ) +
          geom_jitter(
            alpha = 0.6,
            width = 0.2,
            size = 2
            # position = position_jitterdodge(jitter.width = .2)
          )+
          coord_cartesian(ylim = c(0, NA)) +
          # scale_colour_manual(
          #   values = c("gray 20", "gray 70"),
          #   breaks = c("Control", "LBN")
          # ) +
          # scale_shape_discrete(
          #   breaks = c("1", "2", "undisturbed"),
          #   labels = c("First Litter", "Second Litter", "Undisturbed")
          # ) +
          stat_summary(fun = mean, geom = "line", aes(linetype = interaction(Treatment, Cohort)), size = 1, alpha = .6)+
          # stat_summary(fun = mean, geom = "line", aes(linetype = interaction(Treatment, Litter_num)), size = 1, alpha = .6)+
          my_theme +
          # guides(color = FALSE) + 
          # scale_linetype_discrete(
          #   breaks = c("Control.1", "LBN.1", "Control.undisturbed", "Control.2"),
          #   labels = c("Control - Litter 1", "LBN - Litter 1", "Control - undisturbed", "Control - Litter 2")
          # )+
          theme(legend.position="bottom",
                legend.box="vertical", 
                legend.margin=margin()
          ) +
          labs(x = "Litter Size")
      })
    }
  )
}

