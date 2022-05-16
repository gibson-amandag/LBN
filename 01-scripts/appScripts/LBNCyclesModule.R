### Cycles App Module

# https://shiny.rstudio.com/articles/modules.html


# https://gist.github.com/wch/5436415/#gistcomment-1646351 - some initial inspiration for loading imgs


LBNCyclesUI <- function(id, off_data){
  ns <- NS(id)
  tagList(
    do.call(
      tabsetPanel,
      c(id = "tab",
        lapply(sort(unique(off_data$cohort)), function(thisCohort){
          tabPanel(
            title = paste("Cohort ", thisCohort),
            cyclesUI(ns(paste0("cohort", thisCohort)))
          )
        })
        )
    )
  )
}


LBNCyclesServer <- function(
  id,
  damInfo,
  offspringInfo,
  Cycles_off,
  CohortCyclingFolder,
  compType
){
  moduleServer(
    id,
    function(input, output, session) {
      cohorts <- sort(unique(Cycles_off$cohort))
      
      # observe({
      # for(thisCohort in cohorts){
      #   test <- thisCohort
      #   cycleDir <- CohortCyclingFolder$cyclingFolderPath[which(CohortCyclingFolder$cohort == thisCohort)]
      # 
      #   thisCyclesDF <- Cycles_off %>%
      #     filter(
      #       cohort == thisCohort
      #     ) %>%
      #     mutate(
      #       cycleID = num_ID
      #     )
      #   
      #   cyclesServer(
      #     paste0("cohort", thisCohort),
      #     cycleDir,
      #     damInfo,
      #     offspringInfo,
      #     thisCyclesDF,
      #     compType
      #   )
      # }
      # })
      
      # I can't get the for loop above to work consistently. It only pulls the last cohort values, for some occassions
      # It seems to depend on how things are referenced in the module. But it works if I hard code. So just add for each new cohort
      
      cycleDir1 <- CohortCyclingFolder$cyclingFolderPath[which(CohortCyclingFolder$cohort == 1)]

      thisCyclesDF1 <- Cycles_off %>%
        filter(
          cohort == 1
        ) %>%
        mutate(
          cycleID = num_ID
        )

      cyclesServer(
        paste0("cohort", 1),
        cycleDir1,
        damInfo,
        offspringInfo,
        thisCyclesDF1,
        compType
      )
      cycleDir2 <- CohortCyclingFolder$cyclingFolderPath[which(CohortCyclingFolder$cohort == 2)]

      thisCyclesDF2 <- Cycles_off %>%
        filter(
          cohort == 2
        ) %>%
        mutate(
          cycleID = num_ID
        )

      cyclesServer(
        paste0("cohort", 2),
        cycleDir2,
        damInfo,
        offspringInfo,
        thisCyclesDF2,
        compType
      )
      cycleDir4 <- CohortCyclingFolder$cyclingFolderPath[which(CohortCyclingFolder$cohort == 4)]

      thisCyclesDF4 <- Cycles_off %>%
        filter(
          cohort == 4
        ) %>%
        mutate(
          cycleID = num_ID
        )

      cyclesServer(
        paste0("cohort", 4),
        cycleDir4,
        damInfo,
        offspringInfo,
        thisCyclesDF4,
        compType
      )


      cycleDir5 <- CohortCyclingFolder$cyclingFolderPath[which(CohortCyclingFolder$cohort == 5)]
      thisCyclesDF5 <- Cycles_off %>%
        filter(
          cohort == 5
        ) %>%
        mutate(
          cycleID = num_ID
        )

      cyclesServer(
        paste0("cohort", 5),
        cycleDir5,
        damInfo,
        offspringInfo,
        thisCyclesDF5,
        compType
      )


      cycleDir6 <- CohortCyclingFolder$cyclingFolderPath[which(CohortCyclingFolder$cohort == 6)]
      thisCyclesDF6 <- Cycles_off %>%
        filter(
          cohort == 6
        ) %>%
        mutate(
          cycleID = num_ID
        )

      cyclesServer(
        paste0("cohort", 6),
        cycleDir6,
        damInfo,
        offspringInfo,
        thisCyclesDF6,
        compType
      )
      
      cycleDir7 <- CohortCyclingFolder$cyclingFolderPath[which(CohortCyclingFolder$cohort == 7)]
      thisCyclesDF7 <- Cycles_off %>%
        filter(
          cohort == 7
        ) %>%
        mutate(
          cycleID = num_ID
        )

      cyclesServer(
        paste0("cohort", 7),
        cycleDir7,
        damInfo,
        offspringInfo,
        thisCyclesDF7,
        compType
      )
      cycleDir8 <- CohortCyclingFolder$cyclingFolderPath[which(CohortCyclingFolder$cohort == 8)]
      thisCyclesDF8 <- Cycles_off %>%
        filter(
          cohort == 8
        ) %>%
        mutate(
          cycleID = num_ID
        )

      cyclesServer(
        paste0("cohort", 8),
        cycleDir8,
        damInfo,
        offspringInfo,
        thisCyclesDF8,
        compType
      )
    }
  )
}

