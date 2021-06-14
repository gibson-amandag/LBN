### Pup Loss App Module

#Creates a data frame that summarizes the number of pups lost during the paradigm for the different groups

# https://shiny.rstudio.com/articles/modules.html

pupLossUI <- function(
  id,
  Demo_dam
){
  ns <- NS(id)
  tagList(
    
    h3("Pup Loss During Paradigm"),
    fluidRow(
      varSelectInput(
        ns("Pup_loss_grouping_vars"),
        "Select variables to group by:",
        data = Demo_dam %>%
          select(
            Treatment:Dam_Strain,
            ParaType,
            Sac_or_stop
          ),
        selected = c(
          "Treatment",
          "Dam_Strain",
          "ParaType"
        ),
        multiple = TRUE)
    ),
    
    dataTableOutput(ns("Pup_loss_summary")),
    
    h4("Litter Size LBN_0002"),
    
    plotOutput(ns("LBN_0002")),
    
    h4("Litter Size LBN_0003"),
    
    plotOutput(ns("LBN_0003")),
    
    h4("Litter Size LBN_0004"),
    
    plotOutput(ns("LBN_0004")),
    
    h4("Litter Size LBN_0005"),
    
    plotOutput(ns("LBN_0005")),
    
    h4("Litter Size LBN_0006"),
    
    plotOutput(ns("LBN_0006"))
    
  )
}


pupLossServer <- function(
  id,
  Demo_dam
){
  moduleServer(
    id,
    function(input, output, session) {
      
      output$Pup_loss_summary <- renderDataTable(
        LBN_summary_byGroup(expr(pupLoss), Demo_dam, input$Pup_loss_grouping_vars) %>%
          select(-Variable, - VarName)
      )
      
      # Change to reference cohort
      output$LBN_0003 <- renderPlot({
        ggplot(
          Demo_dam %>%
            filter(!is.na(Litter_size_endPara)) %>%
            filter(Cohort == "3"),
          aes(x = Litter_size_endPara, fill = "#F8766D")
        )+
          geom_bar(
            position = position_dodge2(preserve = "single"),
            # binwidth = 1,
            width = 0.9,
            color = "white"
          ) +
          my_theme +
          scale_x_continuous(
            breaks = seq(1, 8, by = 1)
          ) +
          scale_y_continuous(
            breaks = seq(1, 4, by = 1)
          ) +
          guides(color = FALSE, fill = FALSE) +
          labs(x = "Litter Size", y = "# of Litters")
      })
      
      # Change to reference cohort
      output$LBN_0005 <- renderPlot({
        ggplot(
          Demo_dam %>%
            filter(!is.na(Litter_size_endPara)) %>%
            filter(Cohort == "5"),
          aes(x = Litter_size_endPara, fill = "#F8766D")
        )+
          geom_bar(
            position = position_dodge2(preserve = "single"),
            # binwidth = 1,
            width = 0.9,
            color = "white"
          ) +
          my_theme +
          scale_x_continuous(
            breaks = seq(1, 8, by = 1)
          ) +
          scale_y_continuous(
            breaks = seq(1, 4, by = 1)
          ) +
          guides(color = FALSE, fill = FALSE) +
          labs(x = "Litter Size", y = "# of Litters")
      })
      
      output$LBN_0002 <- renderPlot({
        ggplot(
          Demo_dam %>% 
            filter(Cohort == "2") %>% 
            filter(!is.na(Litter_size_startPara)), 
          aes(
            x = Litter_size_startPara,
            fill = Treatment
          )
        )+
          geom_bar(
            position = position_dodge2(preserve = "single"),
            # binwidth = 1, 
            width = 0.9,
            color = "white"
            ) +
          my_theme + 
          scale_x_continuous(
            breaks = seq(1, 8, by = 1)
          )+
          scale_y_continuous(
            breaks = seq(1, 4, by = 1)
          ) +
          guides(color = FALSE) +
          labs(x = "Litter Size", y = "# of Litters")
      })
      
      output$LBN_0004 <- renderPlot({
        ggplot(
          Demo_dam %>% 
            filter(Cohort == "4") %>%
            filter(!is.na(Litter_size_startPara)), 
          aes(
            x = Litter_size_startPara,
            fill = Treatment
          )
        )+
          geom_bar(
            position = position_dodge2(preserve = "single"),
            # binwidth = 1,
            width = 0.9,
            color = "white"
          ) +
          my_theme + 
          scale_x_continuous(
            breaks = seq(1, 8, by = 1)
          )+
          scale_y_continuous(
            breaks = seq(1, 4, by = 1)
          ) +
          guides(color = FALSE) +
          labs(x = "Litter Size", y = "# of Litters")
      })
      
      output$LBN_0006 <- renderPlot({
        ggplot(
          Demo_dam %>% 
            filter(Cohort == "6") %>%
            filter(!is.na(Litter_size_startPara)), 
          aes(
            x = Litter_size_startPara,
            fill = Treatment
          )
        )+
          geom_bar(
            position = position_dodge2(preserve = "single"),
            # binwidth = 1,
            width = 0.9,
            color = "white"
          ) +
          my_theme + 
          scale_x_continuous(
            breaks = seq(1, 8, by = 1)
          )+
          scale_y_continuous(
            breaks = seq(1, 4, by = 1)
          ) +
          guides(color = FALSE) +
          labs(x = "Litter Size", y = "# of Litters")
      })
    }
  )
}

