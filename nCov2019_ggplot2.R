remotes::install_github("GuangchuangYu/nCov2019")
remotes::install_github("GuangchuangYu/chinamap")
require(tidyverse)
require(nCov2019)
require(maps)
require(chinamap)
require(sp)
require(mapproj)
require(magick)

x = get_nCov2019()
plot(x)
cn = get_map_china() 
plot(x, chinamap=cn)
plot(x, region='china', chinamap=cn)
plot(x, region='china', chinamap=cn, continuous_scale=FALSE, palette='Blues')


#绘制全球病例数动图
y <- load_nCov2019()
d <- c(paste0("2020-03-", 10:25))
img <- image_graph(600, 450, res = 96)
out <- lapply(d, function(date){
  p <- plot(y, date=date, label=FALSE, continuous_scale=FALSE)
  print(p)
})
dev.off()
animation <- image_animate(img, fps = 2)
image_write(animation, "ncov2019_global.gif")

#绘制病例数前10的国家的病例数折线图
y <- load_nCov2019(lang="en")
d = y['global',]
dd <- filter(d, time == time(y)) %>% arrange(desc(cum_confirm)) 
dd = dd[1:10, ]

p1 <- ggplot(dd, aes(reorder(country,-cum_confirm), cum_confirm, group=1)) +
  geom_point(color="orange") + 
  geom_line(color="orange") + 
  scale_x_discrete(name = NULL) + 
  scale_y_continuous(name = NULL) + 
  ggtitle("Cumulative confirmed cases")
p1

d <- load_nCov2019(lang="en")
dd <- d['global'] %>% 
  as_tibble %>%
  rename(confirm=cum_confirm) %>%
  filter(time > "2020-02-01" & country %in% c("China",
                                              "Italy",
                                              "United States",
                                              "Spain",
                                              "Germany",
                                              "Iran",
                                              "France",
                                              "Switzerland",
                                              "South Korea",
                                              "United Kingdom")) %>%
  group_by(country) %>%
  ungroup 

Sys.setlocale("LC_TIME", "English")

p2 <- ggplot(dd, aes(time, confirm, color = country)) +
  geom_line(size = 0.8) +
  geom_point(pch = 21, size = 1) +
  labs(x = NULL, y = NULL, 
       title = "Confirmed COVID-19 cases",
       subtitle = time(d))
p2

#绘制病例数前10的国家的病例数变化率折线图
d <- load_nCov2019(lang="en")
dd <- d['global'] %>% 
  as_tibble %>%
  rename(confirm=cum_confirm) %>%
  arrange(country) %>%
  filter(time > "2020-02-01" & country %in% c("China",
                                              "Italy",
                                              "United States",
                                              "Spain",
                                              "Germany",
                                              "Iran",
                                              "France",
                                              "Switzerland",
                                              "South Korea",
                                              "United Kingdom")) %>%
  group_by(country) %>%
  ungroup 

rate = c()
for(i in 2:469) {
  if(dd$country[i] == dd$country[i-1]) 
    rate[i] = (dd$confirm[i]-dd$confirm[i-1])/dd$confirm[i-1]
  else rate[i] = NA
}
dd$rat <- rate

Sys.setlocale("LC_TIME", "English")

p3 <- ggplot(dd, aes(time, rat, color = country)) +
  geom_line(size = 0.8) +
  geom_point(pch = 21, size = 1) +
  labs(x = NULL, y = NULL, 
       title = "Change rate of confirmed COVID-19 cases",
       subtitle = time(d))
p3
