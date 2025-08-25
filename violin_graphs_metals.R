library(openxlsx)
library(tidyverse)
library(reshape2)

df <- read.xlsx("data/S1 File_metals.xlsx", sheet="S1 File")
colnames(df) <- gsub("_µg/g.ww", "", colnames(df))
colnames(df)[colnames(df) == "Sampling_year"] <- "Year"
colnames(df)[colnames(df) == "Sampling_region_1"] <- "Area"

# Palladium, Platinum, Rhodium are dropped because only zero values.
dfl <- df[c("Year", "Area", "Mercury", "Methylmercury", "Cadmium", "Chromium",
            "Copper", "Lead", "Selenium", "Zinc")]
colnames(dfl) <- c("Year", "Area", "THg", "MeHg", "Cd", "Cr", "Cu", "Pb", "Se", "Zn")

dfl <- melt(dfl, value.name = "value", id.vars = c("Year", "Area"))

mins <- dfl[!(dfl$value == 0 | is.na(dfl$value) ) , ]
mins <- aggregate(mins$value, by = mins["variable"], FUN = min)
mins$x <- mins$x * 0.5

dfl <- merge(dfl, mins)
dfl$value <- ifelse(dfl$value == 0, dfl$x, dfl$value)

path <- "figs/"

ggplot(dfl, aes(x = variable, y = value, fill = Area))+geom_violin()+
  scale_y_log10()+
  labs(
    y="Concentration (µg/g ww)",
    x=""
  )+theme(legend.position = c(0.2, 0.8))
ggsave(paste0(path, "Oprey_metals_Fig2.png"), width=6, height=6)
ggsave(paste0(path, "Osprey_metal_Fig2.tiff"), width=5, height=5)

ggplot(dfl, aes(x = variable, y = value))+geom_violin()+
  scale_y_log10()+
  labs(
    y="Concentration (µg/g ww)",
    x=""
  )
ggsave(paste0(path, "violin_metals.png"), width=8, height=8)

ggplot(dfl, aes(x = variable, y = value))+geom_point()+
  geom_jitter(width = 0.3)+
  scale_y_log10()+
  labs(
    y="Concentration (µg/g ww)",
    x=""
  )

ggplot(dfl, aes(x = variable, y = value, color = Area))+geom_point()+
  geom_jitter(width = 0.3)+
  scale_y_log10()+
  labs(
    y="Concentration (µg/g ww)",
    x=""
  )
ggsave(paste0(path, "jitter_metals.png"), width=8, height=8)

ggplot(dfl, aes(x = variable, y = value, color=Area))+geom_boxplot()+
  scale_y_log10()+
  labs(
    y="Concentration (µg/g ww)",
    x=""
  )
ggsave(paste0(path, "boxplot_with_area_metals.png"), width=8, height=8)

ggplot(dfl, aes(x = variable, y = value))+geom_boxplot()+
  scale_y_log10()+
  labs(
    y="Concentration (µg/g ww)",
    x=""
  )
ggsave(paste0(path, "boxplot_metals.png"), width=8, height=8)

compounds = unique(dfl$variable)
for(compound in compounds) {
  g <- ggplot(dfl[dfl$variable == compound , ],
              aes(x = Year, y = value, group = Area, color = Area)
            )+geom_smooth()+
    scale_y_log10()+
    labs(
      title = paste(compound, "concentration"),
      y = "Concentration (µg/g ww)"
    )
  print(g)
  ggsave(paste0(path, compound, "_timetrend.png"))
}
