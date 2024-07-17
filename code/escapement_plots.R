library(tidyverse)
library(adfgcolors)
library(JTPfunc)


SEAK_escgoals <- read_csv("../coho_data/data/SEAK_Coho_EscGoals.csv") %>% 
  dplyr::select(-CollectionType, -GoalType, - Comment)
SEAK_escape <- read_csv("../coho_data/data/SEAK_Coho_Escapement_1972-2023.csv") %>%
  left_join(SEAK_escgoals, by = c("River" = "System")) %>% 
  mutate(abovebelow = if_else(Escapement_Count < EscapementGoal_Lower, 
                              "Escapement less than goal", "Escapement goal met or exceeded"))


berners_esc <- create_BOFfig(datadf = SEAK_escape, river = "Berners River")
berners_esc
ggsave(berners_esc, filename = "output/esc_berners.png", dpi = 500, height = 3.5, width = 5.25,  units = "in")


hsl_esc <- create_BOFfig(datadf = SEAK_escape, river = "Hugh Smith Lake",
                         maxy = 4200,
                         setybreaks = c(1000, 2000, 3000, 4000))
hsl_esc
ggsave(hsl_esc, filename = "output/esc_hsl.png", dpi = 500, height = 3.5, width = 5.25,  units = "in")


chilkat_esc <- create_BOFfig(datadf = SEAK_escape, river = "Chilkat River",
                             setthousands = TRUE, maxy = 210,
                         setybreaks = c(50, 100, 150, 200, 250, 300))
chilkat_esc
ggsave(chilkat_esc, filename = "output/esc_chilkat.png", dpi = 500, height = 3.5, width = 5.25,  units = "in")


taku_esc <- create_BOFfig(datadf = SEAK_escape, river = "Taku River",
                             setthousands = TRUE, maxy = 225,
                             setybreaks = c(50, 100, 150, 200, 250, 300))
taku_esc
ggsave(taku_esc, filename = "output/esc_taku.png", dpi = 500, height = 3.5, width = 5.25,  units = "in")


auke_esc <- create_BOFfig(datadf = SEAK_escape, river = "Auke Creek",
                          setthousands = FALSE, maxy = 1550,
                          setybreaks = seq(from=0, to=2000, by = 250))
auke_esc
ggsave(auke_esc, filename = "output/esc_auke.png", dpi = 500, height = 3.5, width = 5.25,  units = "in")


sitka_esc <- create_BOFfig(datadf = SEAK_escape, river = "Sitka Survey Index",
                          setthousands = FALSE, maxy = 3100,
                          setybreaks = seq(from=0, to=3500, by = 500))
sitka_esc
ggsave(sitka_esc, filename = "output/esc_sitka.png", dpi = 500, height = 3.5, width = 5.25,  units = "in")


ktn_esc <- create_BOFfig(datadf = SEAK_escape, river = "Ketchikan Survey Index",
                           setthousands = FALSE, maxy = 23000,
                           setybreaks = seq(from=0, to=25000, by = 5000))
ktn_esc
ggsave(ktn_esc, filename = "output/esc_ktn.png", dpi = 500, height = 3.5, width = 5.25,  units = "in")









SEAK_escape %>%  
  pivot_longer(cols = c(EscapementGoal_Lower, EscapementGoal_Upper),
               names_to = "GoalEnd", values_to = "Goal") %>%
  select(Year, River, GoalEnd, Goal) %>%
  mutate(GoalEnd = case_when(GoalEnd == "EscapementGoal_Lower" ~ "Escapement goal lower bound", 
                             GoalEnd == "EscapementGoal_Upper" ~ "Escapement goal upper bound",
            .default = NA))



esc_berners <- SEAK_escape %>%
  filter(Year >= 1982, River == "Berners River") %>%
  ggplot() +
  geom_col(aes(x = Year, y = Escapement_Count, fill = abovebelow),
           color = "gray30",
           linewidth = 0.4,
           width = 0.7) +
  geom_line(data = . %>%  
              pivot_longer(cols = c(EscapementGoal_Lower, EscapementGoal_Upper),
                                        names_to = "GoalEnd", values_to = "Goal") %>%
              mutate(GoalEnd = case_when(GoalEnd == "EscapementGoal_Lower" ~ "Escapement goal lower bound", 
                                         GoalEnd == "EscapementGoal_Upper" ~ "Escapement goal upper bound",
                                         .default = NA)),
            aes(x = Year, y = Goal, linetype = GoalEnd)) +
  scale_x_continuous(breaks = seq(from = 1982, to = 2023, by = 3),
                     minor_breaks = seq(from=1982, to=2023, by=1),
                     guide = guide_axis(minor.ticks = TRUE)) +
  scale_y_continuous(limits = c(0, 30000), labels = scales::comma,
                     breaks = seq(from=0, to=30000, by=5000)) +
  scale_fill_manual(values = c("gray80", "black")) +
  scale_linetype_manual(values = c("43", "solid"),
                        guide = guide_legend(override.aes=aes(fill=NA),
                                             reverse = TRUE,
                                             ncol = 1)) +
  guides(fill=guide_legend(ncol = 1)) +
  labs(x="", y = "Escapement", 
       title = "Berners River coho salmon") +
  theme_crisp(rotate_x = TRUE, base_family = "Times New Roman") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(legend.background = element_rect(color = NA),
        legend.position = c(0.4, -0.32),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.box = "horizontal",
        legend.key.width = unit(0.75, "cm"),
        legend.key.height = unit(0.35, "cm"),
        legend.key.spacing.y = unit(0.05, "cm"),
        legend.box.margin = margin(-0.3, 0.1, -0.1, 0.1, "cm"),
        panel.border = element_blank(),
        plot.margin = margin(0.1, 0.1, 2, 0.1, "cm"),
        axis.ticks = element_line(color = "gray40"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.line = element_line(color = "gray40"),
        axis.text = element_text(color = "black", size = 11),
        axis.title.y = element_text(size = 14, face = "bold", color = "black"),
        axis.title.x = element_blank(),
        plot.title = element_text(vjust = -5, hjust = 0.5, size = 14,
                                  face = "bold", color = "black"))
esc_berners








ggsave(esc_berners, filename = "output/esc_berners3.png", dpi = 500, height = 3.46, width = 4.76,  units = "in")










