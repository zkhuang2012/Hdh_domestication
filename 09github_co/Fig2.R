rm(list = ls())
gc()

library(ggplot2)
library(tidyverse)
library(patchwork)
library(ggtree)
library(ggrepel)
library(treeio)


# Fi2a Genome--------------------------------------------------------------------


# Fig2b PSMC-------------------------------------------------------------------


# Fig2c PhyTree-------------------------------------------------------------------


phytree <- read.tree("./02PSMC_Tree/Hdh_divtree.newick")

tree_group <-
  read.delim("./02PSMC_Tree/tree_group.txt",
             header = F,
             stringsAsFactors = F)

groupInfo <- split(tree_group$V1, tree_group$V2)

tree <- groupOTU(phytree, groupInfo)


ggtree(tree,
       # layout = "fan",
       ladderize = FALSE,
       branch.length = "none",
       aes(color = group)) +
  geom_tiplab2() +
  # geom_label(aes(x=branch, label=S), fill='lightgreen') +
  # geom_label(aes(label=D), fill='steelblue') +
  # geom_text(aes(label=B), hjust=-.5)+
  theme(legend.position = "right")


# Fig2d TE-------------------------------------------------------------------
hdh_te <-
  read.delim("./02TE/Hdh.repeat.div.draw",
             header = T,
             stringsAsFactors = F)
hruf_te <-
  read.delim("./02TE/Hruf.repeat.div.draw",
             header = T,
             stringsAsFactors = F)
hrub_te <-
  read.delim("./02TE/Hrub.repeat.div.draw",
             header = T,
             stringsAsFactors = F)
hla_te <-
  read.delim("./02TE/Hla.repeat.div.draw",
             header = T,
             stringsAsFactors = F)

pdf("./02TE/te_div.pdf",width = 12,height = 4)

hrub_te %>% rbind(hla_te) %>% rbind(hdh_te) %>% rbind(hruf_te) %>%
  # mutate(Species=str_replace_all(Species,c("hrub" = "Hrub", "hla" = "Hla", "hdh" = "Hdh","hruf=Hruf"))) %>%
  # head()
  rename(Div.adj = Div) %>%
  rename(Length = Lenght) %>%
  rename(Percentage = Value) %>%
  ggplot() +
  geom_bar(
    aes(x = Div.adj, y = Percentage, fill = Type),
    position = "stack",
    # color = "black",
    width = 0.9,
    stat = "identity",
    alpha = 0.8
  ) +
  facet_wrap(. ~ Species, nrow = 1, labeller = as_labeller(c(
    Hrub = 'Hrub',
    Hla = 'Hla',
    Hdh = 'Hdh',
    Hruf = 'Hruf'
  ))) +
  scale_fill_manual(values = c("#db3801", "#f9932f", "#f1ca1d", "#85c9db", "#2d79b7")) +
  xlab("Divergence")+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 7,
                             color = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10, face = "bold"),
    # legend.key.size = unit(0.5, 'cm'),
    # #change legend key size
    # legend.key.height = unit(0.5, 'cm'),
    # #change legend key height
    # legend.key.width = unit(0.5, 'cm'),
    strip.text.x = element_text(size = 10, face = "bold.italic"),
    
    strip.background = element_rect(
      color = "black",
      fill = "#FC4E07",
      # size=1.5,
      linetype = "solid"
    ),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )


dev.off()

# Fig2 Merge --------------------------------------------------------------

pdf(
  "./Fig2.v2.pdf",
  width = 8,
  height = 6,
  bg = "white"
)

(plot_spacer() + plot_spacer()) + (fig1a + fig1b + plot_layout(widths = c(1, 2)))
+ plot_annotation(tag_level = "a")

dev.off()