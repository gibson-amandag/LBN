#Loading Data Sets
myCSV_func = function(folderPath, fileName) {
  fullPath = paste0(folderPath, fileName)
  cat("\n", file = fullPath, append = TRUE) #in theory, this should add a blank line at the end and avoid the "incomplete final line" error
  read.csv(fullPath, na.strings="NA", fileEncoding="UTF-8-BOM")
}

myXLSX_func = function(folderPath, fileName, sheetName){
  read.xlsx(
    file.path(folderPath, fileName),
    sheet = sheetName,
    colNames = TRUE,
    rowNames = FALSE,
    detectDates = TRUE,
    skipEmptyRows = TRUE,
    skipEmptyCols = TRUE,
    na.strings = "NA"
  )
}
  
#Format dates function
#because detected above, I think this isn't needed anymore
format_dates = function(column){
  as.Date(column, format = "%Y-%m-%d")
}

### TASK FUNCTIONS
LBN_tasks_func <- function(
  df,
  id_var,
  val,
  task_text
){
  if(Count == 0){
    cat(paste0("*", task_text, ":* \n\n"))
    Count <<- 1
  }
  cat(paste0("+ ", df[[id_var]][val], "\n\n"))
}

Dam_tasks <- function(
  task_text,
  val,
  df = Dam_dates,
  id_var = "Dam_ID"
){
  if(Count == 0){
    cat(paste0("*", task_text, ":* \n\n"))
    Count <<- 1
  }
  cat(paste0("+ ", df[[id_var]][val], "\n\n"))
}

printLine_func <- function(Count){
  if(Count == 1) cat("\n")
  Count <<- 0
}

# printLine_func(Count)

Dam_seq <- function(){
  seq_along(Dam_dates$Dam_ID)
}

sac_stop <- function(val){
  is.na(Dam_dates$Sac_or_stop[val])
}

Dam_day_equals <- function(Day, var, val){
  Day == Dam_dates[[var]][val]
}

# Dam_day_equals("2020-11-30", "Breed_date", 1)

Dam_day_greater <- function(Day, var, val){
  Day >= Dam_dates[[var]][val]
}

# Dam_day_greater("2020-12-13", "mass_check", 1)
# Dam_day_greater("2020-11-11", "mass_check", 1)

Dam_not.na <- function(var, val){
  !is.na(Dam_dates[[var]][val])
}

# Dam_not.na("Breed_date", 1)
# Dam_not.na("Sac_or_stop", 1)

Dam_is.na <- function(var, val){
  is.na(Dam_dates[[var]][val])
}

# Dam_is.na("Sac_or_stop", 2)
# Dam_is.na("mass_G12", 1)

Off_tasks <- function(
  task_text,
  val,
  df = Off_dates,
  id_var = "Mouse_ID"
){
  if(Count == 0){
    cat(paste0("*", task_text, ":* \n\n"))
    Count <<- 1
  }
  cat(paste0("+ ", df[[id_var]][val], "\n\n"))
}

Off_seq <- function(){
  seq_along(Off_dates$Mouse_ID)
}

Off_day_equals <- function(Day, var, val){
  Day == Off_dates[[var]][val]
}


Off_day_greater <- function(Day, var, val){
  Day >= Off_dates[[var]][val]
}

Off_day_less <- function(Day, var, val){
  Day <= Off_dates[[var]][val]
}


Off_not.na <- function(var, val){
  !is.na(Off_dates[[var]][val])
}


Off_is.na <- function(var, val){
  is.na(Off_dates[[var]][val])
}

####Write to Excel Files 
#uses openxslx

writeToWorkbook = function(sheetName, df, wb){
  addWorksheet(wb, sheetName)
  writeDataTable(wb, sheetName, df, tableStyle = "TableStyleMedium2")
}



#Mass Plotting
#Make a long-form dataframe to be able to graph appropriately
# key = the new column to be created
# value = the value to be recorded for each iteration of the key; here this is mass
# What columns correspond to the key
# Make this new key column a factor
make_long_form = function(df){
  df %>%
    gather(key = "day", value = "Mass", c(Mass_P9:Mass_P72, Avg_litter_mass_P2), factor_key=TRUE)
}

#Create a new column "PND" for the corresponding post-natal day on which the mass was taken to be able to plot on a continuous x-scale
make_PND_col = function(df){
  df = df %>%
    mutate(PND = case_when(day == "Avg_litter_mass_P2" ~ 2,
                           day == "Mass_P9" ~ 9,
                           day == "Mass_P10" ~ 10,
                           day == "Mass_P11" ~ 11,
                           day == "Mass_P12" ~ 12,
                           day == "Mass_P13" ~ 13,
                           day == "Mass_P15" ~ 15,
                           day == "Mass_P17" ~ 17,
                           day == "Mass_P19" ~ 19,
                           day == "Mass_P21" ~ 21,
                           day == "Mass_P22" ~ 22,
                           day == "Mass_P23" ~ 23,
                           day == "Mass_P28" ~ 28,
                           day == "Mass_P35" ~ 35,
                           day == "Mass_P42" ~ 42,
                           day == "Mass_P49" ~ 49,
                           day == "Mass_P56" ~ 56,
                           day == "Mass_P63" ~ 63,
                           day == "Mass_P70" ~ 70,
                           day == "Mass_P71" ~ 71,
                           day == "Mass_P72" ~ 72)
    )
}

make_summary_table = function(df){
  df %>%
    group_by(Treatment, day) %>%
    summarize(mean = mean(Mass, na.rm = TRUE),
              sd = sd(Mass, na.rm = TRUE),
              n = n(),
              sem = sd/sqrt(n))
}



#depending on number trying to plot, can add "aes(shape = Mouse_ID)" to geom_point
mass_plot = function(df){
  ggplot(df, aes(PND, Mass, color = Treatment)) +
    geom_point(alpha = .5, aes(shape = Dam_Strain), size = 2)+
    stat_summary(fun.y = mean, geom = "line", linetype = "dotted", size = 1, aes(group = Treatment)) +
    stat_summary(fun.y = mean, geom = "point",  shape = 18, size = 3, aes(group = Treatment)) +
    stat_summary(geom = "errorbar", fun.data = mean_se, size = .7, aes(group = Treatment))+
    expand_limits(y = 0)+ #set y axis to 0
    labs(x = "Postnatal Day", y = "Mass (g)")+
    my_theme
}

#lower density line for each individual mouse
mass_plot_lines = function(df, line_group = Mouse_ID, by_strain = TRUE){
  line_group = enquo(line_group)
  ggplot(df, aes(PND, Mass, color = Treatment, group = if(by_strain == TRUE){interaction(Treatment, Dam_Strain)}else{Treatment})) + #if by_strain is TRUE, group by dam strain
    geom_line(alpha = .25, aes(linetype = Dam_Strain, group = !! line_group), size = .8)+
    stat_summary(fun.y = mean, geom = "line", if(by_strain == TRUE){aes(linetype = Dam_Strain)}, size = 1.4, alpha = 1) +
    #stat_summary(fun.y = mean, geom = "point",  shape = 18, size = 4) +
    stat_summary(geom = "errorbar", fun.data = mean_se, size = 1)+
    expand_limits(y = 0)+ #set y axis to 0
    labs(x = "Postnatal Day", y = "Mass (g)")+
    my_theme
}

# ggplot(df, aes(PND, Mass, color = Treatment)) +
#   geom_line(alpha = .25, aes(linetype = Dam_Strain, group = !! line_group), size = .8)+
#   stat_summary(fun.y = mean, geom = "line", size = 1.4, alpha = 1, aes(group = Treatment)) +
#   #stat_summary(fun.y = mean, geom = "point",  shape = 18, size = 4, aes(group = Treatment)) +
#   stat_summary(geom = "errorbar", fun.data = mean_se, size = 1, aes(group = Treatment))+
#   expand_limits(y = 0)+ #set y axis to 0
#   labs(x = "Postnatal Day", y = "Mass (g)")+
#   my_theme

funcs_to_plot_mass = function(df, line_group = Mouse_ID, by_strain = TRUE){
  line_group = enquo(line_group)
  df_nam = ensym(df)
  df_nam = as.character(df_nam)
  nam = paste0(df_nam, "_long")
  df_long = make_long_form(df)
  df_long = make_PND_col(df_long)
  assign(nam, df_long, envir = .GlobalEnv)
  
  nam = paste0(df_nam, "_summary")
  sum_df = make_summary_table(df_long)
  assign(nam, sum_df, envir = .GlobalEnv)
  
  nam = paste0(df_nam, "_plot")
  plot = mass_plot_lines(df_long, !! line_group, by_strain)
  assign(nam, plot, envir = .GlobalEnv)
}


#Functions for plotting dam mass
make_long_form_dams = function(df){
  df %>%
    gather(key = "day", value = "Mass", c(Dam_Mass_P2, Dam_Mass_P9, Dam_Mass_P21), factor_key=TRUE)
}

make_PND_col_dams = function(df){
  df = df %>%
    mutate(PND = case_when(day == "Dam_Mass_P2" ~ 2,
                           day == "Dam_Mass_P9" ~ 9,
                           day == "Dam_Mass_P21" ~ 21,
                           )
    )
}

mass_plot_dams = function(df){
  ggplot(df, aes(PND, Mass, color = Treatment)) +
    geom_point(alpha = .5, aes(shape = Dam_Strain), size = 3)+
    stat_summary(fun.y = mean, geom = "line", linetype = "dotted", size = 1, aes(group = Treatment)) +
    stat_summary(fun.y = mean, geom = "point",  shape = 18, size = 3, aes(group = Treatment)) +
    stat_summary(geom = "errorbar", fun.data = mean_se, size = .7, width = .2, aes(group = Treatment))+
    expand_limits(y = 0) + #set y axis to 0
    labs(x = "Postnatal Day", y = "Mass (g)")+
    my_theme
}

mass_plot_dams_lines = function(df, by_strain = TRUE){
  if(by_strain == TRUE) {
   viz = ggplot(df, aes(PND, Mass, color = Treatment, group = interaction(Treatment, Dam_Strain))) +
    geom_line(alpha = .25, aes(linetype = Dam_Strain, group = Dam_ID), size = .8)+
    stat_summary(fun.y = mean, geom = "line", aes(linetype = Dam_Strain), size = 1.4, alpha = 1) +
    #stat_summary(fun.y = mean, geom = "point",  shape = 18, size = 4, aes(group = Treatment)) +
    stat_summary(geom = "errorbar", fun.data = mean_se, size = 1, width = 1)+
    expand_limits(y = 0)+ #set y axis to 0
    labs(x = "Postnatal Day", y = "Mass (g)")+
    my_theme
  }else{
    viz = ggplot(df, aes(PND, Mass, color = Treatment, group = Treatment)) +
      geom_line(alpha = .25, aes(linetype = Dam_Strain, group = Dam_ID), size = .8)+
      stat_summary(fun.y = mean, geom = "line", aes(linetype = Dam_Strain), size = 1.4, alpha = 1) +
      #stat_summary(fun.y = mean, geom = "point",  shape = 18, size = 4, aes(group = Treatment)) +
      stat_summary(geom = "errorbar", fun.data = mean_se, size = 1, width = 1)+
      expand_limits(y = 0)+ #set y axis to 0
      labs(x = "Postnatal Day", y = "Mass (g)")+
      my_theme
  }
  
}

funcs_to_plot_dam_mass = function(df, by_strain = TRUE){
  df_nam = ensym(df)
  df_nam = as.character(df_nam)
  nam = paste0(df_nam, "_long")
  df_long = make_long_form_dams(df)
  df_long = make_PND_col_dams(df_long)
  assign(nam, df_long, envir = .GlobalEnv)
  
  nam = paste0(df_nam, "_plot")
  plot = mass_plot_dams_lines(df_long, by_strain)
  assign(nam, plot, envir = .GlobalEnv)
}

# LBN_plot_func = function(plot, title){
#   plot = plot + labs(title = title) + xlim(0, xlimit)
#   print(plot)
#   plot_name = paste0(title, day, ".png")
#   ggsave(plot_name, plot = plot, path = LBN_path, bg = "transparent", width = 9, height = 6)
# }
# 
# LBN_plot_func_2 = function(plot, title){
#   print(plot)
#   plot_name = paste0(title, day, ".png")
#   ggsave(plot_name, plot = plot, path = LBN_path, bg = "transparent", width = 9, height = 6)
# }

puberty_plot_func = function(data, var_to_plot, ytitle = NULL, title = NULL){
  var_to_plot = enquo(var_to_plot)
  viz = data %>%
    ggplot(aes(x = Treatment, y = !! var_to_plot))+
    geom_jitter(width = .2, size = 3, alpha = .6, aes(shape = Dam_Strain, colour = Dam_Strain))+
    stat_summary(fun.y = mean, geom = "point") +
    stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
    my_theme+
    expand_limits(y = 0)+
    scale_colour_manual(values = c("gray 20", "gray 70"))
  
  if(is.null(ytitle)){
    viz = viz
  }else{viz = viz + labs(y = ytitle)}
  
  if(is.null(title)){
    viz = viz
  }else{viz = viz + labs(title = title)}
  return(viz)
}



#depending on number trying to plot, can add "aes(shape = Mouse_ID)" to geom_point
interaction_plot = function(data, var_to_plot, ytitle = NULL, title = NULL){
  var_to_plot = enquo(var_to_plot)
  viz = data %>%
    ggplot(aes(Time_hr, !! var_to_plot, color = Treatment, group = interaction(Treatment, Stress_treatment))) + 
    geom_line(alpha = .4, aes(linetype = Stress_treatment, group = Mouse_ID)) +
    stat_summary(fun.y = mean, geom = "line", aes(linetype = Stress_treatment), size = 1) +
    stat_summary(fun.y = mean, geom = "point",  shape = 18, size = 3) +
    stat_summary(geom = "errorbar", fun.data = mean_se, size = .7, width = .3)+
    expand_limits(y = -0.5)+ #set y axis to just before 0
    labs(x = "Experimental Time (hr)")+
    my_theme
  
  if(is.null(ytitle)){
    viz = viz
  }else{viz = viz + labs(y = ytitle)}
  
  if(is.null(title)){
    viz = viz
  }else{viz = viz + labs(title = title)}
    
  return(viz)
}


#Error bars in color
# puberty_plot_func = function(data, var_to_plot, ytitle = NULL, title = NULL){
#   var_to_plot = enquo(var_to_plot)
#   viz = data %>%
#     ggplot(aes(x = Treatment, y = !! var_to_plot))+
#     geom_jitter(width = .1, size = 3, alpha = .6, aes(shape = Dam_Strain, colour = Dam_Strain))+
#     stat_summary(fun.y = mean, geom = "point", aes(colour = Treatment)) +
#     stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.2, aes(colour = Treatment))+
#     my_theme+
#     expand_limits(y = 0)+
#     scale_colour_manual(values = c("gray 20", "gray 70", "#F8766D", "#00BFC4"))+
#     guides(colour = FALSE)
#   
#   if(is.null(ytitle)){
#     viz = viz
#   }else{viz = viz + labs(y = ytitle)}
#   
#   if(is.null(title)){
#     viz = viz
#   }else{viz = viz + labs(title = title)}
#   return(viz)
# }

cum_freq_plot = function(data, var_to_plot, title){
  var_to_plot = enquo(var_to_plot)
  xtitle = paste0("Age at ", title, " (Days)")
  viz = ggplot(data, aes(!! var_to_plot, color = Treatment, linetype = Dam_Strain))+
    stat_ecdf(size=2)+
    my_theme +
    labs(x = xtitle, y = "Cumulative Frequency", title = title)+
    expand_limits(x = 21)
  return(viz)
}

#Save Functions
LBN_save_func = function(plot, title){
  plot = plot + xlim(0, xlimit)
  print(plot)
  plot_name = paste0(title, day, ".png")
  ggsave(plot_name, plot = plot, path = LBN_path, bg = "transparent", width = 9, height = 6)
}

LBN_save_func_2 = function(plot, title){
  print(plot)
  plot = plot + labs(title = NULL)
  plot_name = paste0(title, day, ".png")
  ggsave(plot_name, plot = plot, path = LBN_path, bg = "transparent", width = 9, height = 6)
}

LBN_puberty_dot_save_func = function(plot, title){
  print(plot)
  plot_name = paste0(title, day, ".png")
  ggsave(plot_name, plot = plot, path = LBN_path, bg = "transparent", width = 4, height = 6)
}

LBN_save_func_pdf = function(plot, title){
  print(plot)
  plot = plot + labs(title = NULL)
  plot_name = paste0(title, day, ".pdf")
  ggsave(plot_name, plot = plot, path = LBN_path, bg = "transparent", width = 9, height = 6)
}

