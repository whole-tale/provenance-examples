library(dplyr)
library(sf)

rsf <- function(x, y){
  us <- filter(x, SumAllYr > 0)
  av <- filter(j2, is.na(lizard))
  z <- us[!duplicated(us$uniID),]
  all <- rbind(z, av)
  #print(sum(all$CountAllYr))
  counts <- us %>% group_by(lizard, year) %>% count()
  counts <- as.data.frame(counts)
  empty <- all[FALSE,]
  for (i in 1:nrow(counts)) {
    nu <- counts[i, "n"]
    liz <- counts[i, "lizard"]
    yr <- counts[i, "year"]
    size <- y * nu
    rows <- all[sample(1:nrow(all), size, replace = TRUE),]
    rows$lizard <- liz
    rows$year <- yr
    empty <- rbind(empty, rows)
  }
  empty$pres <- 0
  us$pres <- 1
  s_mcp95 <- rbind(empty, us)
  print(sum(s_mcp95$pres))
  s_mcp95
}

#import relocations
reloc <- st_read("data/telemetryexportPR.shp")
reloc$id <- "id"

#spatially join relocation dataset
grid20 <- st_read("data/Fishnet20m.shp")
j <- st_join(grid20, reloc)
j <- dplyr::select(j, uniID, relocation, lizard, year, date, time, 14:17,behavior, 31)

#import covariate data from 95% mcp
data20 <- read.csv("data/mcp20m.csv")

j2 <- left_join(j, data20, by = "uniID")

#need to filter out observations outside the mcp
j2 <- filter(j2, !is.na(x_utm))
ck <- j2 %>% group_by(lizard, year) %>% count()


j2 <- mutate(j2, aspect.cat = ifelse(Aspect >= 0 & Aspect < 22.5, "north", ifelse(Aspect >= 22.5 & Aspect < 67.5, "northeast", ifelse(Aspect >= 67.5 & Aspect < 112.5, "east", ifelse(Aspect >= 112.5 & Aspect < 157.5, "southeast", ifelse(Aspect >= 157.5 & Aspect < 202.5, "south", ifelse(Aspect >= 202.5 & Aspect < 247.5, "southwest", ifelse(Aspect >= 247.5 & Aspect < 292.5, "west", ifelse(Aspect >= 292.5 & Aspect < 337.5, "northwest", ifelse(Aspect >= 337.5 & Aspect < 360, "north", "flat"))))))))))

j2 <- select(j2, 1:8, 11,12,  14, 22:29)

with <- rsf(j2, 1)

without <- rsf(j2, 2)
pres <- filter(without, pres == 1)
av <- filter(without, pres == 0)
#remove doubles
av <- distinct(av)
av <- anti_join(as.data.frame(av), as.data.frame(pres), by = "uniID")
av <- av[sample(nrow(av), 3354, replace = FALSE),]
without <- bind_rows(av, pres)
without <- select(without, -geometry)

write.csv(without, "data/data_no_replace.csv")
write.csv(with, "data/data_with_replace.csv")
