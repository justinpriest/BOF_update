library(tidyverse)
library(adfgcolors)
library(JTPfunc)
library(patchwork)


SEAK_escgoals <- read_csv("../coho_data/data/SEAK_Coho_EscGoals.csv") %>% 
  dplyr::select(-CollectionType, -GoalType, - Comment)
SEAK_escape <- read_csv("../coho_data/data/SEAK_Coho_Escapement_1972-2023.csv") %>%
  left_join(SEAK_escgoals, by = c("River" = "System")) #%>% 
  # mutate(abovebelow = if_else(Escapement_Count < EscapementGoal_Lower, 
  #                             "Escapement less than goal", "Escapement goal met or exceeded"))

#### BERNERS RIVER COHO ####
berners_esc <- create_BOFfig(datadf = SEAK_escape, river = "Berners River")
berners_esc
ggsave(berners_esc, filename = "output/esc_berners.png", dpi = 500, height = 3.5, width = 5.25,  units = "in")

#### HUGH SMITH LAKE COHO ####
hsl_esc <- create_BOFfig(datadf = SEAK_escape, river = "Hugh Smith Lake",
                         maxy = 4200,
                         setybreaks = c(1000, 2000, 3000, 4000))
hsl_esc
ggsave(hsl_esc, filename = "output/esc_hsl.png", dpi = 500, height = 3.5, width = 5.25,  units = "in")

#### CHILKAT RIVER COHO ####
chilkat_esc <- create_BOFfig(datadf = SEAK_escape, river = "Chilkat River",
                             setthousands = TRUE, maxy = 210,
                         setybreaks = c(50, 100, 150, 200, 250, 300))
chilkat_esc
ggsave(chilkat_esc, filename = "output/esc_chilkat.png", dpi = 500, height = 3.5, width = 5.25,  units = "in")

#### TAKU RIVER COHO ####
taku_esc <- create_BOFfig(datadf = SEAK_escape, river = "Taku River",
                             setthousands = TRUE, maxy = 225, minyear = 1985,
                             setybreaks = c(50, 100, 150, 200, 250, 300))
taku_esc
ggsave(taku_esc, filename = "output/esc_taku.png", dpi = 500, height = 3.5, width = 5.25,  units = "in")

#### AUKE CREEK COHO ####
auke_esc <- create_BOFfig(datadf = SEAK_escape, river = "Auke Creek",
                          setthousands = FALSE, maxy = 1550,
                          setybreaks = seq(from=0, to=2000, by = 250))
auke_esc
ggsave(auke_esc, filename = "output/esc_auke.png", dpi = 500, height = 3.5, width = 5.25,  units = "in")

#### SITKA AREA COHO ####
sitka_esc <- create_BOFfig(datadf = SEAK_escape, river = "Sitka Survey Index",
                          setthousands = FALSE, maxy = 3100,
                          setybreaks = seq(from=0, to=3500, by = 500))
sitka_esc
ggsave(sitka_esc, filename = "output/esc_sitka.png", dpi = 500, height = 3.5, width = 5.25,  units = "in")

#### KETCHIKAN AREA COHO ####
ktn_esc <- create_BOFfig(datadf = SEAK_escape, river = "Ketchikan Survey Index",
                           setthousands = FALSE, maxy = 23000,
                           setybreaks = seq(from=0, to=25000, by = 5000))
ktn_esc
ggsave(ktn_esc, filename = "output/esc_ktn.png", dpi = 500, height = 3.5, width = 5.25,  units = "in")

#### KLAWOCK RIVER COHO ####
klawock_esc <- create_BOFfig(datadf = SEAK_escape %>%
                               add_row(Year = 1996, River = "Klawock River", 
                                       Species = "Coho Salmon", Escapement_Count = 0,
                                       EscapementGoal_Lower = 1), 
                             # had to "cheat" and add a dummy row to get it to show in legend
                             river = "Klawock River",
                             setthousands = FALSE, maxy = 25000,
                             setybreaks = seq(from=0, to=25000, by=5000)) 
klawock_esc
ggsave(klawock_esc, filename = "output/esc_klwk.png", dpi = 500, height = 3.5, width = 5.25,  units = "in")


#### YAKUTAT AREA COHO ####
tawah <- create_BOFfig(datadf = SEAK_escape, river = "Tawah Creek",
              setthousands = FALSE, maxy = 10000,
              setybreaks = seq(from=0, to=10000, by = 2500)) + 
  theme(legend.position="none")
situk <- create_BOFfig(datadf = SEAK_escape, river = "Situk River",
              setthousands = FALSE, maxy = 45000,
              setybreaks = seq(from=0, to=40000, by = 10000)) + 
  theme(legend.position="none")
tsiu <- create_BOFfig(datadf = SEAK_escape, river = "Tsiu River",
              setthousands = FALSE, maxy = 60000,
              setybreaks = seq(from=0, to=60000, by = 20000)) 

yak_esc <- tawah / situk / tsiu
ggsave(yak_esc, filename = "output/esc_yak.png", dpi = 500, height = 6, width = 5.25,  units = "in")










