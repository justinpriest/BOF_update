


create_BOFfig <- function(datadf = SEAK_escape, river = "Berners River", 
                          River_column = "River", Year_column = "Year",
                          Escapement_column = "Escapement_Count",
                          species_column = "Species", species = "Coho Salmon", 
                          setybreaks = seq(from=0, to=35000, by=5000), 
                          xbreakspace = 3, maxy = 30000, minyear = 1982, 
                          setthousands = FALSE, ...) {

  # Clean up the species name for plotting the title
  #   (remove capitalizations except for Chinook)
  if(species=="Chinook Salmon"){
    speciesclean <- "Chinook salmon"
  } else{
    speciesclean <- tolower(species)
  }
  
  # Name the y axis depending on if thousands were set
  if(setthousands==TRUE){
    yaxislabel <- "Escapement (thousands)"
  } else{
    yaxislabel <- "Escapement"
  }
  
  # import data
  datadf %>% 
    # match which columns to look for, filter as set, remove NAs
    rename(Year = Year_column, River = River_column, 
           Species = species_column, Escapement_Count = Escapement_column) %>%
    filter(River == river, Year >= minyear, 
           Species == species, !is.na(Escapement_Count)) %>%
    # add column if esc not met so that we 
    mutate(abovebelow = if_else(Escapement_Count < EscapementGoal_Lower, 
                                "Escapement less than goal", "Escapement goal met or exceeded"),
           #logic to convert to thousands
           Escapement_Count = case_when(setthousands == TRUE ~ Escapement_Count / 1000,
                                        .default = Escapement_Count),
           EscapementGoal_Lower = case_when(setthousands == TRUE ~ EscapementGoal_Lower / 1000,
                                        .default = EscapementGoal_Lower),
           EscapementGoal_Upper = case_when(setthousands == TRUE ~ EscapementGoal_Upper / 1000,
                                        .default = EscapementGoal_Upper)) %>%
    # Plotting
    ggplot(data = ., aes(x = Year, y = Escapement_Count, fill = abovebelow)) + 
    geom_col(aes(x = Year, y = Escapement_Count, fill = abovebelow),
             color = "gray30", # bar outline color
             linewidth = 0.4,  # bar outline width
             width = 0.7) +    # spacing between bars
    geom_line(data = . %>%  
                # in order to have EG lines on the legend, we need to vary them in aes
                # to do that, we have to turn the columns into "long" format
                pivot_longer(cols = c(EscapementGoal_Lower, EscapementGoal_Upper),
                             names_to = "GoalEnd", values_to = "Goal") %>%
                # rename how we want it to appear in the legend
                mutate(GoalEnd = case_when(GoalEnd == "EscapementGoal_Lower" ~ "Escapement goal lower bound", 
                                           GoalEnd == "EscapementGoal_Upper" ~ "Escapement goal upper bound",
                                           .default = NA)),
              aes(x = Year, y = Goal, linetype = GoalEnd)) +
    scale_x_continuous(breaks = seq(from = minyear, to = 2023, by = xbreakspace),
                       minor_breaks = seq(from = minyear, to = 2023, by = 1),
                       guide = guide_axis(minor.ticks = TRUE)) +
    scale_y_continuous(limits = c(0, maxy), labels = scales::comma,
                       breaks = setybreaks) +
    scale_fill_manual(values = c("gray80", "black")) + # above / below color
    scale_linetype_manual(values = c("33", "solid"),   # 33 = 3 units dash, 3 units space
                          guide = guide_legend(override.aes=aes(fill=NA),
                                               reverse = TRUE, # put upper on top
                                               ncol = 1)) +
    guides(fill=guide_legend(ncol = 1)) + # for the fill legend, make 1 column
    labs(x="", y = yaxislabel,
         title = paste0(river, " ", speciesclean)) +
    theme_light(base_size = 12, base_family = "Times New Roman") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key.size = unit(0.9, "lines"),
          strip.background = element_rect(fill = NA, color = NA),
          strip.text.x = element_text(color = "gray30"),
          strip.text.y = element_text(color = "gray30"),
          plot.subtitle = element_text(color = "gray30", size = rel(.85)),
          # much of this was based on JTP's theme_crisp()
          legend.background = element_rect(color = NA),
          legend.position = "bottom",
          legend.justification.bottom = "left",
          legend.margin = margin(-5, 0, 0, 0),
          legend.location = "plot",
          legend.title = element_blank(),
          legend.text = element_text(size = 10, 
                                     margin = margin(l = 3, r = 10)),
          legend.box = "horizontal",
          legend.key.width = unit(0.75, "cm"),
          legend.key.height = unit(0.35, "cm"),
          legend.key.spacing.y = unit(0.05, "cm"),
          panel.border = element_blank(),
          axis.ticks = element_line(color = "gray40"),
          axis.ticks.length = unit(5, "pt"),
          axis.line = element_line(color = "gray40"),
          axis.text = element_text(color = "black", size = 11),
          axis.title.y = element_text(size = 14, face = "bold", color = "black"),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          plot.title = element_text(vjust = -5, hjust = 0.5, size = 14,
                                    face = "bold", color = "black"))
  # to use:
  # testfig <- create_BOFfig(datadf = SEAK_escape, river = "Berners River") 
  # testfig
  # ggsave(testfig, filename = "output/testfig.png", dpi = 500, height = 3.46, width = 4.76,  units = "in")
  
  # use + to add/overwrite existing scales/theme
}
  


