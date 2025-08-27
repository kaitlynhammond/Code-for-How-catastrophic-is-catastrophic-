library("ggtern")
library("tidyverse")



buffer1000 <- read.csv("Data/buffer1000.csv")



buffer1000 <- buffer1000 %>%
  select(mtnash, full2007, partial2007, full2016,
         partial2016, none2016, dom_ten) %>%
  mutate(full2007 = full2007/mtnash,
         partial2007 = partial2007/mtnash,
         full2016 = full2016/mtnash,
         partial2016 = partial2016/mtnash,
         none2016 = none2016/mtnash,
         none2007 = 0,
         dom_ten = ifelse(dom_ten == "parks", "Parks and reserves", "State forest"))



ggtern(buffer1000, aes(x = partial2016, y = full2016, z= none2016))+
  geom_point()+
  xlab("") +
  ylab("") +
  zlab("") +
  Tarrowlab("Full canopy")+
  Larrowlab("Partial canopy")+
  Rarrowlab("No canopy")+
  facet_wrap(~dom_ten) +
  theme(strip.background =element_rect(color = "black", fill = "white"),
        strip.text.x = element_text(
          size = 18))+
  theme_showarrows()
  

percent_partial <- buffer1000 %>%
  filter(partial2016 > 0.2) %>%
  group_by(dom_ten) %>%
  summarise(count = n())
percent_partial

total <- buffer1000 %>%
  group_by(dom_ten) %>%
  summarise(count = n())
total




