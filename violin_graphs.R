library(reshape2)

# Run the first two blocks from the ospreyanalysis.Rmd

dfl <- df[c("Area", "Year", "PCDD_F_Sum", "PCB_Sum_ng_g", "BDE_Sum", "PBB_Sum", "PCN_Sum", "Total_DDT_Sum")]
dfl <- melt(dfl, value.name = "value", id.vars = c("Year", "Area"))
dfl$variable <- factor(
  dfl$variable,
  c("PCDD_F_Sum", "PBB_Sum", "PCN_Sum", "BDE_Sum", "Total_DDT_Sum", "PCB_Sum_ng_g"),
  c("ΣPCDD/F", "ΣPBB", "ΣPCN", "ΣBDE", "ΣDDT", "ΣPCB"))
tst <- dfl$variable=="ΣPCDD/F"
dfl$value[tst] <- dfl$value[tst] * 0.001


ggplot(dfl, aes(x = variable, y = value, fill = Area))+geom_violin()+
  scale_y_log10()+
  labs(
    y="Concentration (ng/g fw)",
    x=""
  )
ggsave("violin_with_areas.png", width=8, height=8)

ggplot(dfl, aes(x = variable, y = value))+geom_violin()+
  scale_y_log10()+
  labs(
    y="Concentration (ng/g fw)",
    x=""
  )
ggsave("violin.png", width=8, height=8)

ggplot(dfl, aes(x = variable, y = value))+geom_point()+
  geom_jitter(width = 0.3)+
  scale_y_log10()+
  labs(
    y="Concentration (ng/g fw)",
    x=""
  )
ggsave("jitter.png", width=8, height=8)

ggplot(dfl, aes(x = variable, y = value, color=Area))+geom_boxplot()+
  scale_y_log10()+
  labs(
    y="Concentration (ng/g fw)",
    x=""
  )
ggsave("boxplot_with_area.png", width=8, height=8)

ggplot(dfl, aes(x = variable, y = value))+geom_boxplot()+
  scale_y_log10()+
  labs(
    y="Concentration (ng/g fw)",
    x=""
  )
ggsave("boxplot.png", width=8, height=8)
