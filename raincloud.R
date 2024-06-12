library(tidyverse)
library(gghalves)


#读入数据
df <- readxl::read_xlsx("data.xlsx")

#设置颜色
my_color = c('#000000','#E21F26','#24B99A','#CCBE93','#A7CEE2')

#排好序
variable <-c('CentralWesternAsia','NorthernEurope','SouthernEurope','CentralEasternEurope','WesternEurope')
my_sort <-factor(variable,levels = variable)


ggplot(df,aes(factor(Region,levels = my_sort),`Age average`/1000))+
  coord_flip()+  
  geom_half_violin(aes(fill=factor(Region,levels = my_sort),
                       color=factor(Region,levels = my_sort)),
                   side ='r',
                   position = position_nudge(x = .25, y = 0))+
  geom_boxplot(aes(fill=factor(Region,levels = my_sort)),
               width=0.12,cex=1.2,outliers = FALSE, alpha = 0.5,
               position = position_nudge(x = .1, y = 0))+
  geom_dotplot(binaxis = "y",binwidth = 0.05,stackdir = "down",dotsize = 1,
               fill = '#A7A6A6', color = "transparent") +
  scale_fill_manual(values = my_color, guide = 'none')+
  scale_color_manual(values = my_color, guide = 'none') +
  scale_y_reverse(expand=c(0,0), limits =c(15,0), breaks = seq(0, 15, 1))+
  labs(x=NULL,y="kyr BP")+
  theme_classic(base_size = 20) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(color = "black", size = 22,
                                   vjust = -2, hjust = 0,
                                   margin = margin(0,-7,0,0,'cm')),
        axis.text.x = element_text(color = "black", size = 22))

ggsave("raincloud.png", width = 10, height = 7, dpi = 600)
ggsave("raincloud.pdf", width = 10, height = 7)

