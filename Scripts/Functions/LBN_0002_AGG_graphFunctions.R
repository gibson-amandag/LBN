####################################### GRAPHING FUNCTIONS ###########################################################

#1/1/2021 - Coord_cartesian doesn't seem to work any more if given a NULL min/max value within c(min, max). 
# Need to provide numbers for both, apparently

my_theme = theme(
  text = element_text(size=18),
  legend.title = element_blank(),
  legend.position = "bottom",
  panel.background = element_rect(fill = "transparent"), # bg of the panel
  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
  panel.grid.major = element_blank(), # get rid of major grid
  panel.grid.minor = element_blank(), # get rid of minor grid
  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
  legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
  axis.line = element_line(colour = "black")
)


### Mass Plotting-----------------------------------------------------------------------------------------
#Make a long-form dataframe to be able to graph appropriately
# key = the new column to be created
# value = the value to be recorded for each iteration of the key; here this is mass
# What columns correspond to the key
# Make this new key column a factor
make_long_form = function(df){
  df %>%
    gather(key = "day", value = "Mass", c(Avg_litter_mass_startPara, Mass_P9:Mass_P72), factor_key=TRUE)
}

make_long_form_dams = function(df){
  df %>%
    gather(key = "day", value = "Mass", c(Dam_Mass_P2:Dam_Mass_P21), factor_key=TRUE)
}

#Create a new column "PND" for the corresponding post-natal day on which the mass was taken to be able to plot on a continuous x-scale
make_PND_col = function(df){
  df = df %>%
    mutate(
      PND = case_when(
        day == "Avg_litter_mass_startPara" & ParaType == 2 ~ 2,
        day == "Avg_litter_mass_startPara" & ParaType == 4 ~ 4,
        day == "Mass_P9" ~ 9,
        day == "Mass_P10" ~ 10,
        day == "Mass_P11" ~ 11,
        day == "Mass_P12" ~ 12,
        day == "Mass_P13" ~ 13,
        day == "Mass_P14" ~ 14,
        day == "Mass_P15" ~ 15,
        day == "Mass_P16" ~ 16,
        day == "Mass_P17" ~ 17,
        day == "Mass_P19" ~ 19,
        day == "Mass_P21" ~ 21,
        day == "Mass_P22" ~ 22,
        day == "Mass_P23" ~ 23,
        day == "Mass_P24" ~ 24,
        day == "Mass_P28" ~ 28,
        day == "Mass_P35" ~ 35,
        day == "Mass_P42" ~ 42,
        day == "Mass_P49" ~ 49,
        day == "Mass_P56" ~ 56,
        day == "Mass_P63" ~ 63,
        day == "Mass_P70" ~ 70,
        day == "Mass_P71" ~ 71,
        day == "Mass_P72" ~ 72
      )
    )
}

make_PND_col_dams = function(df){
  df = df %>%
    mutate(
      PND = case_when(
        day == "Dam_Mass_P2" ~ 2,
        day == "Dam_Mass_P4" ~ 4,
        day == "Dam_Mass_P9" ~ 9,
        day == "Dam_Mass_P11" ~ 11,
        day == "Dam_Mass_P21" ~ 21
      )
    )
}

reshapeForMassPlot <- function(df){
  df_long <- make_long_form(df)
  df_long_PND <- make_PND_col(df_long)
  df_long_noNA <- df_long_PND %>%
    filter(!is.na(Mass))
}

#select which variables in df first!
reshapeForMassPlot_dams <- function(df){
  df_long <- make_long_form_dams(df)
  df_long_PND <- make_PND_col_dams(df_long)
  df_long_noNA <- df_long_PND %>%
    filter(!is.na(Mass))
}

### CYCLES DATA PREPARATION -------------------------------------------------------------------------------
make_cycles_long <- function(df){
  df %>%
    # drop_na(Day1:Day21) %>%
    gather(
      key = "DayNum",
      value = "Stage",
      c(Day1:Day21),
      factor_key = TRUE
    )
}

add_Day_col <- function(df){
  df <- df %>%
    mutate(
      Day = 
        case_when(
          DayNum == "Day1" ~ 1,
          DayNum == "Day2" ~ 2,
          DayNum == "Day3" ~ 3,
          DayNum == "Day4" ~ 4,
          DayNum == "Day5" ~ 5,
          DayNum == "Day6" ~ 6,
          DayNum == "Day7" ~ 7,
          DayNum == "Day8" ~ 8,
          DayNum == "Day9" ~ 9,
          DayNum == "Day10" ~ 10,
          DayNum == "Day11" ~ 11,
          DayNum == "Day12" ~ 12,
          DayNum == "Day13" ~ 13,
          DayNum == "Day14" ~ 14,
          DayNum == "Day15" ~ 15,
          DayNum == "Day16" ~ 16,
          DayNum == "Day17" ~ 17,
          DayNum == "Day18" ~ 18,
          DayNum == "Day19" ~ 19,
          DayNum == "Day20" ~ 20,
          DayNum == "Day21" ~ 21
        )
    )
  return(df)
}

makeCortLong <- function (df) {
  df_long <- df %>%
    gather(key = "Time", value = "Cort", c(Cort_pre, Cort_post), factor_key = TRUE)
  
  df_long <- df_long %>%
    mutate(
      Time_hr = case_when(
        Time == "Cort_pre" ~ 0,
        Time == "Cort_post" ~ 5
        )
      )
  
  return(df_long)
}


### MY GEOMS -----------------------------------------------------------------------------------------------

##Line plot geoms----

#plot masses for individual animals (or by dam) with semi-transparent lines
my_geom_line <- function(
  linetype_var, #use expr
  lineGroup_var #use expr
){
  geom_line(
    alpha = .5, #make it semi-transparent
    aes(
      linetype = !! linetype_var,
      group = !! lineGroup_var
    ),
    size = 0.8
  )
}

#plot the average as a solid line. Include error bars
#Can use a different line type based on a categorical variable
my_line_mean_geom <- function(
  useLinetype, #TRUE/FALSE
  linetype_var, #use expr
  width = 1
){
  list(
    stat_summary(fun = mean, geom = "line", if(useLinetype){aes(linetype = !! linetype_var)}, size = 1.4, alpha = 1),
    stat_summary(geom = "errorbar", fun.data = mean_se, size = 1, width = width, color = "grey10", alpha = 0.6)
  )
}

#Geoms to make a line plot for mass
my_LBN_mass_geoms = function(
  useLinetype, #TRUE/FALSE
  linetype_var, #use expr
  lineGroup_var, #use expr
  xtitle, #for x axis nice title
  ytitle, #for y axis nice title
  title = NULL, #title of the graph
  individualLines = TRUE, #if want individual lines
  mean_lines = TRUE, #if want to include mean line with SEM
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL,
  width = 1 #for error bars
){
  list(
    labs(
      x = xtitle, 
      y = ytitle, 
      title = title
    ),
    #geom_rect(aes(xmin = 0, xmax = 21, ymin = 0, ymax = Inf), fill = 'grey90', color = "grey90", alpha = 0.1),
    #For background shading, but this seems to drastically increase time to plot
    if(mean_lines) #if mean_lines is true, add this layer
      my_line_mean_geom(
        useLinetype = useLinetype,
        linetype_var = linetype_var,
        width = width
      ),
    if(individualLines) #if individualLines is true, add this layer
      my_geom_line(
        #Right now, this always plots based on the linetype_var regardless of useLinetype value
        linetype_var = linetype_var,
        lineGroup_var = lineGroup_var
      ),
    expand_limits(y=0), #set y axis to 0
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}), #this just zooms in on the graph, versus scale_[]_continuous actually eliminates data not in the range
    my_theme
  )
}

## Cumulative Frequency Geoms ----
my_cumulative_freq_geoms <- function(
  phenotype_name,
  title = TRUE,
  change_xmax = FALSE,
  xmax = NA,
  xmin = 21
){
  list(
    stat_ecdf(size = 2),
    labs(
      x = paste0("Age at ", phenotype_name, " (Days)"),
      y = "Cumulative Frequency",
      title = if(title){phenotype_name}else{NULL}
    ),
    coord_cartesian(
      xlim = c(
        xmin,
        if(change_xmax){xmax}else{NA}
      )
    ),
    my_theme,
    theme(legend.box = "vertical")
  )
}

## Puberty Dot Plot Geoms ----
my_geom_jitter <- function(
  shape = expr(Dam_Strain),
  colour = expr(Dam_Strain)
){
  geom_jitter(width = .15, size = 3, alpha = .6, aes(shape = !! shape, colour = !! colour))
}

my_dot_geom_mean <- function(
  width = 0.3
){
  list(
    stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = width),
    stat_summary(fun = mean, geom = "point", color = "red", size = 2))
}

my_puberty_dot_geoms <- function(
  shape = expr(Dam_Strain),
  colour = expr(Dam_Strain),
  width = 0.3,
  change_ymax = FALSE,
  ymin = 0,
  ymax = NA,
  ytitle = NULL,
  title = NULL
){
  list(
    my_geom_jitter(
      shape = shape,
      colour = colour),
    my_dot_geom_mean(width = width),
    if(change_ymax)
      coord_cartesian(ylim = c(ymin, ymax)),
    if(change_ymax == FALSE)
      coord_cartesian(ylim = c(0, NA)),
    my_theme,
    if(colour == expr(Litter_num))
      scale_colour_manual(
        values = c("gray 5", "gray 75", "gray 40"), 
        breaks = c("1", "2", "undisturbed"),
        labels = c("First Litter", "Second Litter", "Undisturbed")
      ),
    if(shape == expr(Litter_num))
      scale_shape_discrete(
        breaks = c("1", "2", "undisturbed"),
        labels = c("First Litter", "Second Litter", "Undisturbed")
      ),
    if(colour == expr(Cohort))
      scale_colour_grey(),
    # scale_colour_manual(
    #   values = c("gray 20", "gray 70"), 
    #   if(colour == expr(Litter_num)){breaks = c("1", "2")}, 
    #   if(colour == expr(Litter_num)){labels = c("First Litter", "Second Litter")}
    #   ),
    # scale_shape_discrete(
    #   if(colour == expr(Litter_num)){breaks = c("1", "2")}, 
    #   if(colour == expr(Litter_num)){labels = c("First Litter", "Second Litter")}
    # ),
    if(!is.null(ytitle))
      labs(y = ytitle),
    if(!is.null(title))
      labs(title = title)
  )
}

### My plotting functions --------------------
#See "my_LBN_mass_geoms" for explanations of the geoms used
#Input should be a long-form dataframe, with na's for mass removed
mass_plot_lines = function(
  df, 
  line_group = expr(Mouse_ID), 
  by_strain = TRUE,
  individualLines = TRUE, #if want individual lines
  mean_lines = TRUE, #if want to include mean line with SEM
  title = NULL,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL,
  width = 1 #for error bars
)
{
  ggplot(
    df, 
    aes(
      PND, Mass, color = Treatment, 
      group = if(by_strain == TRUE){interaction(Treatment, Dam_Strain)}else{Treatment}  #if by_strain is TRUE, group by dam strain
    )
  ) +
    my_LBN_mass_geoms(
      useLinetype = by_strain,
      linetype_var = expr(Dam_Strain),
      lineGroup_var = line_group,
      xtitle = "Postnatal Day",
      ytitle = "Mass (g)",
      title = title,
      individualLines = individualLines,
      mean_lines = mean_lines,
      zoom_x = zoom_x,
      xmin = xmin,
      xmax = xmax,
      zoom_y = zoom_y,
      ymin = ymin,
      ymax = ymax, 
      width = width
    )
}

mass_plot_lines_litterNum = function(
  df, 
  line_group = expr(Mouse_ID), 
  by_litterNum = TRUE,
  individualLines = TRUE, #if want individual lines
  mean_lines = TRUE, #if want to include mean line with SEM
  title = NULL,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL,
  width = 1 #for error bars
)
{
  ggplot(
    df, 
    aes(
      PND, Mass, color = Treatment, 
      group = if(by_litterNum == TRUE){interaction(Treatment, Litter_num)}else{Treatment}  #if by_strain is TRUE, group by dam strain
    )
  ) +
    my_LBN_mass_geoms(
      useLinetype = by_litterNum,
      linetype_var = expr(Litter_num),
      lineGroup_var = line_group,
      xtitle = "Postnatal Day",
      ytitle = "Mass (g)",
      title = title,
      individualLines = individualLines,
      mean_lines = mean_lines,
      zoom_x = zoom_x,
      xmin = xmin,
      xmax = xmax,
      zoom_y = zoom_y,
      ymin = ymin,
      ymax = ymax, 
      width = width
    ) +
    # scale_linetype_manual(
    scale_linetype_discrete(
      breaks = c("1", "2"),
      # values = c("1" = "solid", "2" = "dashed"),
      labels = c("First Litter", "Second Litter")
    )
}

mat_plot_lines_litterNum = function(
  df,
  matVar = expr(VO_age)
)
{
  ggplot(
    df, 
    aes(
      x = Litter_size, y = !! matVar, color = Treatment, shape = Litter_num, 
      group = interaction(Treatment, Litter_num)
    )
  ) +
    geom_point(
      size = 3, 
      alpha = .6, 
      position = position_jitterdodge(
        jitter.width = NULL,
        jitter.height = 0,
        dodge.width = 0.75
        )
      )+
    stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.15) +
    stat_summary(geom = "point", color = "red", size = 2, fun = mean) +
    coord_cartesian(ylim = c(0, NA)) +
    scale_colour_manual(
      values = c("gray 5", "gray 75", "gray 40"), 
      breaks = c("1", "2", "undisturbed"),
      labels = c("First Litter", "Second Litter", "Undisturbed")
    ) +
    scale_shape_discrete(
      breaks = c("1", "2", "undisturbed"),
      labels = c("First Litter", "Second Litter", "Undisturbed")
    ) +
    my_theme
    
}



#See "my_cumuluative_freq_geoms" for explanation of the geoms used
my_cumulative_freq_plot <- function(
  df,
  color_var = expr(Treatment),
  linetype_var = expr(Dam_Strain),
  var_to_plot, #as expr()
  phenotype_name, #string
  title = TRUE,
  change_xmax = FALSE,
  xmax = NA,
  xmin = 21
){
  df %>%
    filter(!is.na(!! var_to_plot)) %>%
    ggplot(
      aes(
        !! var_to_plot,
        color = !! color_var,
        linetype = !! linetype_var
      )
    ) +
    my_cumulative_freq_geoms(
      phenotype_name,
      title = title,
      change_xmax = change_xmax,
      xmax = xmax,
      xmin = xmin
    ) 
}


#See "my_puberty_dot_geoms"
my_puberty_dot_plot <- function(
  df,
  var_to_plot, #expr()
  phenotype_name,
  shape = expr(Dam_Strain),
  colour = expr(Dam_Strain),
  width = 0.3,
  change_ymax = FALSE,
  ymin = 0,
  ymax = NA,
  DaysOrMass = "Days", #type "Days" or "Mass"
  alt_ytitle = FALSE,
  ytitle = NULL #alternative y title
){
  df %>%
    filter(!is.na(!! var_to_plot)) %>%
    ggplot(aes(x = Treatment, y = !! var_to_plot))+
    my_puberty_dot_geoms(
      shape = shape,
      colour = colour,
      width = width,
      change_ymax = change_ymax,
      ymin = ymin,
      ymax = ymax,
      ytitle = 
        if(alt_ytitle){
          ytitle
        }else if(DaysOrMass == "Days"){
          paste0("Age at ", phenotype_name, " (Days)")
        }else if(DaysOrMass == "Mass"){
          paste0("Mass at ", phenotype_name, " (g)")
        }
      ,
      title = phenotype_name
    )
}

#Cycles Plot
cyclesPlotFunc <- function(df){
  ggplot(df, aes(x = Day, y = Stage)) +
    geom_line() +
    my_theme +
    facet_wrap(Mouse_ID ~ .) + #each plot is a mouse
    scale_y_continuous(
      breaks = c(1, 2, 3), #axis ticks only at 1, 2, 3
      labels = c("E", "D", "P") #replace with E, D, and P
    ) +
    scale_x_continuous(
      breaks = seq(1, 21, 3) #labels every third integer
    )
}

#depending on number trying to plot, can add "aes(shape = Mouse_ID)" to geom_point
stress_interaction_plot = function(data, var_to_plot, ytitle = NULL, title = NULL, plotMean = TRUE){
  var_to_plot = enquo(var_to_plot)
  viz = data %>%
    ggplot(aes(Time_hr, !! var_to_plot, color = Treatment, group = interaction(Treatment, Stress_treatment))) +
    geom_line(alpha = .4, aes(linetype = Stress_treatment, group = Mouse_ID)) +
    expand_limits(y = -0.5)+ #set y axis to just before 0
    labs(x = "Experimental Time (hr)")+
    my_theme
  
  if(plotMean == TRUE){
    viz = viz + 
      stat_summary(fun = mean, geom = "line", aes(linetype = Stress_treatment), size = 1) +
      stat_summary(fun = mean, geom = "point",  shape = 18, size = 3) +
      stat_summary(geom = "errorbar", fun.data = mean_se, size = .7, width = .3)
  }

  if(is.null(ytitle)){
    viz = viz
  }else{viz = viz + labs(y = ytitle)}

  if(is.null(title)){
    viz = viz
  }else{viz = viz + labs(title = title)}

  return(viz)
}


#### NOT CLEANED ###########################################################

# make_summary_table = function(df){
#   df %>%
#     group_by(Treatment, day) %>%
#     summarize(mean = mean(Mass, na.rm = TRUE),
#               sd = sd(Mass, na.rm = TRUE),
#               n = n(),
#               sem = sd/sqrt(n))
# }

# #depending on number trying to plot, can add "aes(shape = Mouse_ID)" to geom_point
# mass_plot = function(df){
#   ggplot(df, aes(PND, Mass, color = Treatment)) +
#     geom_point(alpha = .5, aes(shape = Dam_Strain), size = 2)+
#     stat_summary(fun.y = mean, geom = "line", linetype = "dotted", size = 1, aes(group = Treatment)) +
#     stat_summary(fun.y = mean, geom = "point",  shape = 18, size = 3, aes(group = Treatment)) +
#     stat_summary(geom = "errorbar", fun.data = mean_se, size = .7, aes(group = Treatment))+
#     expand_limits(y = 0)+ #set y axis to 0
#     labs(x = "Postnatal Day", y = "Mass (g)")+
#     my_theme
# }


# funcs_to_plot_mass = function(df, line_group = Mouse_ID, by_strain = TRUE){
#   line_group = enquo(line_group)
#   df_nam = ensym(df)
#   df_nam = as.character(df_nam)
#   nam = paste0(df_nam, "_long")
#   df_long = make_long_form(df)
#   df_long = make_PND_col(df_long)
#   assign(nam, df_long, envir = .GlobalEnv)
#   
#   nam = paste0(df_nam, "_summary")
#   sum_df = make_summary_table(df_long)
#   assign(nam, sum_df, envir = .GlobalEnv)
#   
#   nam = paste0(df_nam, "_plot")
#   plot = mass_plot_lines(df_long, !! line_group, by_strain)
#   assign(nam, plot, envir = .GlobalEnv)
# }
# 
# 
# #Functions for plotting dam mass
# make_long_form_dams = function(df){
#   df %>%
#     gather(key = "day", value = "Mass", c(Dam_Mass_P2, Dam_Mass_P9, Dam_Mass_P21), factor_key=TRUE)
# }
# 
# make_PND_col_dams = function(df){
#   df = df %>%
#     mutate(PND = case_when(day == "Dam_Mass_P2" ~ 2,
#                            day == "Dam_Mass_P9" ~ 9,
#                            day == "Dam_Mass_P21" ~ 21,
#     )
#     )
# }
# 
# mass_plot_dams = function(df){
#   ggplot(df, aes(PND, Mass, color = Treatment)) +
#     geom_point(alpha = .5, aes(shape = Dam_Strain), size = 3)+
#     stat_summary(fun.y = mean, geom = "line", linetype = "dotted", size = 1, aes(group = Treatment)) +
#     stat_summary(fun.y = mean, geom = "point",  shape = 18, size = 3, aes(group = Treatment)) +
#     stat_summary(geom = "errorbar", fun.data = mean_se, size = .7, width = .2, aes(group = Treatment))+
#     expand_limits(y = 0) + #set y axis to 0
#     labs(x = "Postnatal Day", y = "Mass (g)")+
#     my_theme
# }
# 
# mass_plot_dams_lines = function(df, by_strain = TRUE){
#   if(by_strain == TRUE) {
#     viz = ggplot(df, aes(PND, Mass, color = Treatment, group = interaction(Treatment, Dam_Strain))) +
#       geom_line(alpha = .25, aes(linetype = Dam_Strain, group = Dam_ID), size = .8)+
#       stat_summary(fun.y = mean, geom = "line", aes(linetype = Dam_Strain), size = 1.4, alpha = 1) +
#       #stat_summary(fun.y = mean, geom = "point",  shape = 18, size = 4, aes(group = Treatment)) +
#       stat_summary(geom = "errorbar", fun.data = mean_se, size = 1, width = 1)+
#       expand_limits(y = 0)+ #set y axis to 0
#       labs(x = "Postnatal Day", y = "Mass (g)")+
#       my_theme
#   }else{
#     viz = ggplot(df, aes(PND, Mass, color = Treatment, group = Treatment)) +
#       geom_line(alpha = .25, aes(linetype = Dam_Strain, group = Dam_ID), size = .8)+
#       stat_summary(fun.y = mean, geom = "line", aes(linetype = Dam_Strain), size = 1.4, alpha = 1) +
#       #stat_summary(fun.y = mean, geom = "point",  shape = 18, size = 4, aes(group = Treatment)) +
#       stat_summary(geom = "errorbar", fun.data = mean_se, size = 1, width = 1)+
#       expand_limits(y = 0)+ #set y axis to 0
#       labs(x = "Postnatal Day", y = "Mass (g)")+
#       my_theme
#   }
#   
# }
# 
# funcs_to_plot_dam_mass = function(df, by_strain = TRUE){
#   df_nam = ensym(df)
#   df_nam = as.character(df_nam)
#   nam = paste0(df_nam, "_long")
#   df_long = make_long_form_dams(df)
#   df_long = make_PND_col_dams(df_long)
#   assign(nam, df_long, envir = .GlobalEnv)
#   
#   nam = paste0(df_nam, "_plot")
#   plot = mass_plot_dams_lines(df_long, by_strain)
#   assign(nam, plot, envir = .GlobalEnv)
# }
# 
# # LBN_plot_func = function(plot, title){
# #   plot = plot + labs(title = title) + xlim(0, xlimit)
# #   print(plot)
# #   plot_name = paste0(title, day, ".png")
# #   ggsave(plot_name, plot = plot, path = LBN_path, bg = "transparent", width = 9, height = 6)
# # }
# # 
# # LBN_plot_func_2 = function(plot, title){
# #   print(plot)
# #   plot_name = paste0(title, day, ".png")
# #   ggsave(plot_name, plot = plot, path = LBN_path, bg = "transparent", width = 9, height = 6)
# # }
# 
# puberty_plot_func = function(data, var_to_plot, ytitle = NULL, title = NULL){
#   var_to_plot = enquo(var_to_plot)
#   viz = data %>%
#     ggplot(aes(x = Treatment, y = !! var_to_plot))+
#     geom_jitter(width = .2, size = 3, alpha = .6, aes(shape = Dam_Strain, colour = Dam_Strain))+
#     stat_summary(fun.y = mean, geom = "point") +
#     stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
#     my_theme+
#     expand_limits(y = 0)+
#     scale_colour_manual(values = c("gray 20", "gray 70"))
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
# 
# 
# 
# #depending on number trying to plot, can add "aes(shape = Mouse_ID)" to geom_point
# interaction_plot = function(data, var_to_plot, ytitle = NULL, title = NULL){
#   var_to_plot = enquo(var_to_plot)
#   viz = data %>%
#     ggplot(aes(Time_hr, !! var_to_plot, color = Treatment, group = interaction(Treatment, Stress_treatment))) + 
#     geom_line(alpha = .4, aes(linetype = Stress_treatment, group = Mouse_ID)) +
#     stat_summary(fun.y = mean, geom = "line", aes(linetype = Stress_treatment), size = 1) +
#     stat_summary(fun.y = mean, geom = "point",  shape = 18, size = 3) +
#     stat_summary(geom = "errorbar", fun.data = mean_se, size = .7, width = .3)+
#     expand_limits(y = -0.5)+ #set y axis to just before 0
#     labs(x = "Experimental Time (hr)")+
#     my_theme
#   
#   if(is.null(ytitle)){
#     viz = viz
#   }else{viz = viz + labs(y = ytitle)}
#   
#   if(is.null(title)){
#     viz = viz
#   }else{viz = viz + labs(title = title)}
#   
#   return(viz)
# }
# 
# 
# #Error bars in color
# # puberty_plot_func = function(data, var_to_plot, ytitle = NULL, title = NULL){
# #   var_to_plot = enquo(var_to_plot)
# #   viz = data %>%
# #     ggplot(aes(x = Treatment, y = !! var_to_plot))+
# #     geom_jitter(width = .1, size = 3, alpha = .6, aes(shape = Dam_Strain, colour = Dam_Strain))+
# #     stat_summary(fun.y = mean, geom = "point", aes(colour = Treatment)) +
# #     stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.2, aes(colour = Treatment))+
# #     my_theme+
# #     expand_limits(y = 0)+
# #     scale_colour_manual(values = c("gray 20", "gray 70", "#F8766D", "#00BFC4"))+
# #     guides(colour = FALSE)
# #   
# #   if(is.null(ytitle)){
# #     viz = viz
# #   }else{viz = viz + labs(y = ytitle)}
# #   
# #   if(is.null(title)){
# #     viz = viz
# #   }else{viz = viz + labs(title = title)}
# #   return(viz)
# # }
# 
# cum_freq_plot = function(data, var_to_plot, title){
#   var_to_plot = enquo(var_to_plot)
#   xtitle = paste0("Age at ", title, " (Days)")
#   viz = ggplot(data, aes(!! var_to_plot, color = Treatment, linetype = Dam_Strain))+
#     stat_ecdf(size=2)+
#     my_theme +
#     labs(x = xtitle, y = "Cumulative Frequency", title = title)+
#     expand_limits(x = 21)
#   return(viz)
# }
# 
# #Save Functions
# LBN_save_func = function(plot, title){
#   plot = plot + xlim(0, xlimit)
#   print(plot)
#   plot_name = paste0(title, day, ".png")
#   ggsave(plot_name, plot = plot, path = LBN_path, bg = "transparent", width = 9, height = 6)
# }
# 
# LBN_save_func_2 = function(plot, title){
#   print(plot)
#   plot = plot + labs(title = NULL)
#   plot_name = paste0(title, day, ".png")
#   ggsave(plot_name, plot = plot, path = LBN_path, bg = "transparent", width = 9, height = 6)
# }
# 
# LBN_puberty_dot_save_func = function(plot, title){
#   print(plot)
#   plot_name = paste0(title, day, ".png")
#   ggsave(plot_name, plot = plot, path = LBN_path, bg = "transparent", width = 4, height = 6)
# }
# 
# LBN_save_func_pdf = function(plot, title){
#   print(plot)
#   plot = plot + labs(title = NULL)
#   plot_name = paste0(title, day, ".pdf")
#   ggsave(plot_name, plot = plot, path = LBN_path, bg = "transparent", width = 9, height = 6)
# }

