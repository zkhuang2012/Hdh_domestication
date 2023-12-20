rm(list = ls())
gc()


# Fig1a SST-------------------------------------------------------------------

library(stars)
library(raster)
library(ncdf4)
library(ggspatial)
library(ggplot2)
library(tidyverse)
library(patchwork)
# install.packages("ggalt")
library(ggalt)


world_sf <-
  sf::st_read("./00SST/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp")

nc_data <-
  nc_open("./00SST/SST_NC/TERRA_MODIS.20210101_20211231.L3m.YR.SST4.sst4.4km.nc")

ndvi.array <- ncvar_get(nc_data, "sst4")


lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")

fillvalue <- ncatt_get(nc_data, "sst4", "_FillValue")
fillvalue

ndvi.array[ndvi.array == fillvalue$value] <- NA

nc_close(nc_data)

r <-
  raster(
    t(ndvi.array),
    xmn = min(lon),
    xmx = max(lon),
    ymn = min(lat),
    ymx = max(lat),
    crs = CRS(
      "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
    )
  )

# plot(r)

region <- extent(80, 160, 10, 70)

r.crop.stars <- crop(r, region) %>% st_as_stars()

# region_sf<- world_sf %>%
#   filter(COUNTRY %in% c("China","Japan","South Korea","North Korea","Russia"))

fig1a <- ggplot() +
  geom_stars(data = r.crop.stars) +
  scale_fill_gradient(
    name = expression("SST (" *  ~ degree * C * ")"),
    low = "yellow",
    high = "red",
    na.value = NA
  ) +
  labs(x = NULL, y = NULL) +
  # guides(fill=guide_legend(title="SST"))+
  geom_sf(
    data = world_sf,
    fill = "white",
    size = 0.7,
    color = "black"
  ) +
  coord_sf(xlim = c(100, 145), ylim = c(20, 55)) +
  annotation_north_arrow(
    location = "tl",
    which_north = "false",
    style = north_arrow_fancy_orienteering,
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.05, "cm"),
    pad_y = unit(0.05, "cm")
  ) +
  
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 7,
                             # face="bold",
                             color = "black"),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.5, 'cm'),
    #change legend key size
    legend.key.height = unit(0.5, 'cm'),
    #change legend key height
    legend.key.width = unit(0.5, 'cm'),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
    
  )

fig1a


# Fig1b Product -----------------------------------------------------------


product_table <-
  read.csv(
    "./01Phenotype/2003-2021product.csv",
    header = T,
    stringsAsFactors = F
  )

fig1b <-
  product_table %>% gather(key = "Group", value = "Product",-Year) %>%
  ggplot(data = ., aes(
    x = as.factor(Year),
    y = Product,
    group = Group,
    color = Group,
    linetype=Group
  )) +
  geom_point(size = 2) + geom_line(size = 1.5) +
  xlab("Year") + ylab("Product (ton)") +
  guides(fill = guide_legend(title = "Groups")) +
  scale_color_manual(values = c("#ec5c44", "#faaf3c")) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 8,
                             color = "black"),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.title = element_text(size = 10,
                              face = "bold",
                              color = "black"),
    legend.text = element_text(size = 8,
                               color = "black"),
    legend.title = element_text(size = 10,
                                face = "bold",
                                color = "black"),
    legend.position = "top",
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    # legend.spacing = unit(5,"cm")
  )

fig1b


# Fig1c ABT ---------------------------------------------------------------


abt_stat <- read.delim("./01Phenotype/ABT_stat.txt")
# head(abt_stat)
library(ggsignif)

# abt_stat %>%
  # sample_n(20, replace = FALSE) %>%
  # gather(Population,ABT,CNS:CNN) %>%
  # summary()

# sd(abt_stat$CNS)
# sd(abt_stat$CNN)

fig1c <-
  abt_stat %>%
  # sample_n(20, replace = FALSE) %>%
  gather(Population, ABT, CNS:CNN) %>%
  ggplot(aes(Population, ABT, fill = Population)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  scale_fill_manual(values = c("#00bfc4",
                               "#f8766d")) +
  geom_signif(
    comparisons = list(c("CNS", "CNN")),
    # y_position =12,
    step_increase = 0.06,
    test = "t.test",
    # test= function(a, b) {
    #   list(p.value = summary(aov(a ~ b))[[1]][["Pr(>F)"]][[1]])
    # },
    map_signif_level = T
  ) +
  ylab(expression("ABT (" *  ~ degree * C * ")")) +
  xlab("Population") +
  scale_x_discrete(labels = c("", "")) +
  # scale_x_discrete(labels = c("Nothern Population", "Southern Population")) +
  # ylim(27,35) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 8,
                             color = "black"),
    
    axis.title = element_text(size = 10,
                              face = "bold",
                              color = "black"),
    legend.position = "none",
    plot.margin = unit(c(0, 0, 0, 0.5), "cm")
  )

fig1c

# Fig1d Survival ----------------------------------------------------------

require(survival)
require(survminer)

com_gard_sur <-
  read.delim("./01Phenotype/com_gard_sur.txt",
             header = T,
             stringsAsFactors = F)

fig1d <-
  survfit(Surv(time, status) ~ pop, data = com_gard_sur) %>%
  ggsurvplot(
    surv.scale = "percent" ,
    pval = TRUE,
    pval.size = 3,
    pval.coord = c(4, 0.1),
    pval.method = TRUE,
    pval.method.coord = c(.1, 0.1),
    conf.int = TRUE,
    
    # font.y = c(14, "darkred"),
    # font.x = c(14, "darkred"),
    xlab = "Time (d)",
    # conf.int = TRUE,
    break.time.by = 2,
    xlim = c(0, 14),
    # legend.title = "",
    # legend.labs = c("Nothern Population", "Southern Population"),
    legend = "none",
    # Add risk table
    # risk.table = TRUE,
    # risk.table = "percentage",
    # risk.table.col = "strata",
    # risk.table.y.text = FALSE,
    # risk.table.title = "Survival probability",
    palette = c("#00bfc4", "#f8766d"),
    
    # Change ggplot2 theme
    ggtheme = theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 8,
                                 color = "black"),
        axis.title = element_text(size = 10,
                                  face = "bold",
                                  color = "black"),
        # legend.text = element_text(size = 6,
        #                            color = "black"),
        # legend.title = element_text(size = 8,
        #                             face = "bold",
        # color = "black"),
        legend.position = "none",
        plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")
      )
  )

fig1d

# Fig1e Growth ------------------------------------------------------------
growth_data <- read.csv("./01Phenotype/Growth_JJ_DL.csv", header = T)



grow_me <- growth_data %>% na.omit() %>%
  mutate(pop = ifelse(Group == "1", "CNS", "CNN")) %>%
  group_by(Temp, pop) %>%
  summarise(me = median(as.numeric(SGR_WW)))

fig1e <-
  growth_data %>% na.omit() %>%
  mutate(pop = ifelse(Group == "1", "CNS", "CNN")) %>%
  ggplot(aes(
    x = factor(Temp),
    y = as.numeric(SGR_WW),
    fill = as.factor(pop)
  )) +
  geom_boxplot( outlier.alpha = 0.1) +
  scale_fill_manual(
    values = c("#00bfc4",
               "#f8766d"),
    labels = c("Nothern Population", "Southern Population")
  ) +
  
  geom_smooth(
    data = grow_me,
    aes(
      x = factor(Temp),
      y = as.numeric(me),
      group = as.factor(pop)
    ),
    method = "lm",
    formula = y ~ x + I(x ^ 2)
  ) +
  # geom_xspline(
  #   data = grow_me,
  #   spline_shape = 1,
  #   size = 0.5,
  #   aes(
  #     x = factor(Temp),
  #     y = as.numeric(me),
  #     group = as.factor(pop),
  #     color = as.factor(pop)
  #   )
  # ) +
  
  scale_color_manual(values = c("#00bfc4",
                                "#f8766d")) +
  ylab("Specific Growth Rate of Weight") +
  xlab(expression("Temperature (" *  ~ degree * C * ")")) +
  guides(color = "none") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 8,
                             color = "black"),
    # axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.title = element_text(size = 10,
                              face = "bold",
                              color = "black"),
    legend.text = element_text(size = 8,
                               color = "black"),
    legend.title = element_blank(),
    
    legend.position = "none",
    plot.margin = unit(c(0, 0.5, 0, 0), "cm")
  )

# CNN tem=16.5, CNS=20.5
fig1e



lm2_N <-
  lm(as.numeric(me) ~ Temp + I(Temp ^ 2), data = grow_me[grow_me$pop == "CNN", ])
lm2_S <-
  lm(as.numeric(me) ~ Temp + I(Temp ^ 2), data = grow_me[grow_me$pop == "CNS", ])

e_lm2 <-
  growth_data %>% na.omit() %>%
  mutate(pop = ifelse(Group == "1", "CNS", "CNN")) %>%
  ggplot(aes(
    x = factor(Temp),
    y = as.numeric(SGR_WW),
    fill = as.factor(pop)
  )) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("#00bfc4",
               "#f8766d"),
    labels = c("Nothern Population", "Southern Population")
  ) +
  
  geom_smooth(
    data = grow_me,
    aes(
      x = factor(Temp),
      y = as.numeric(me),
      group = as.factor(pop)
    ),
    method = "lm",
    formula = y ~ x + I(x ^ 2)
  ) +
  labs(
    title = paste0(
      "Reg of medium  using smooth, method=lm, formula=y ~ x + I(x^2), R_CNN=",
      round(summary(lm2_N)$adj.r.squared, 2),
      ", R_CNS=",
      round(summary(lm2_S)$adj.r.squared, 2)
    )
  ) +
  
  scale_color_manual(values = c("#00bfc4",
                                "#f8766d")) +
  ylab("Specific Growth Rate of Weight") +
  xlab(expression("Temperature (" *  ~ degree * C * ")")) +
  guides(color = "none") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 8,
                             color = "black"),
    # axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.title = element_text(size = 10,
                              face = "bold",
                              color = "black"),
    legend.text = element_text(size = 8,
                               color = "black"),
    legend.title = element_blank(),
    
    legend.position = "none",
    plot.margin = unit(c(0, 0.5, 0, 0), "cm")
  )

e_lm2


# Fig1 Merge --------------------------------------------------------------
library(patchwork)

pdf(
  "./Fig1.v2.pdf",
  width = 8,
  height = 6,
  bg = "white"
)

(fig1a + fig1b + plot_layout(widths = c(1, 2))) / (fig1c + fig1d$plot + fig1e) +
  plot_annotation(tag_level = "a")

dev.off()
