library(tidyverse)
library(lubridate)
library(ggthemr)
library(extrafont)
library(gridExtra)
library(grid)

# Setup --------------------------------------------------------------------------------------
loadfonts()
ggthemr("flat")

frank_med <- "Franklin Gothic Medium Cond"
frank_demi <- "Franklin Gothic Demi Cond"

my_palette <- c('#3498db', '#2ecc71', '#f1c40f')
my_palette_muted <- scales::muted(my_palette, l = 50, c = 70)

update_geom_defaults("text", list(family = frank_med, size = 2.8, color = "#2c3e50",
                                  lineheight = 0.8))
update_geom_defaults("label", list(family = frank_med, size = 3, color = "white",
                                   fill = "#2c3e50"))

newtheme <- theme(text = element_text(family = frank_med),
                  legend.background = element_blank(),
                  legend.text = element_text(size = 9),
                  plot.title = element_text(face = "plain", family = frank_demi, size = 12,
                                            margin = margin(0, 5, 5, 0)),
                  panel.grid.major = element_blank(),
                  axis.title = element_blank(),
                  axis.line.y.left = element_blank(),
                  plot.background = element_rect(fill = NA, color = NA))

# Read Data ----------------------------------------------------------------------------------

snowfall_raw <- read.csv("2452965.csv")

snowfall <- snowfall_raw %>%
  select(DATE, SNOW)%>%
  mutate(DATE = as.Date(DATE),
         SNOW = ifelse(is.na(SNOW), 0, SNOW))%>%
  rbind(data.frame(DATE = as.Date("2021-04-30"), SNOW = 0)) %>%
  filter(DATE >= as.Date("2018-08-01"))%>%
  mutate(YEAR = year(DATE),
         MONTH = month(DATE),
         DAY = day(DATE),
         CommonDate = as.Date(paste(ifelse(MONTH <= 7, "2001", "2000"), MONTH, DAY, sep = "-"))) %>%
  mutate(Winter = ifelse(MONTH <= 7, paste(YEAR - 1, "-", YEAR),
                         paste(YEAR, "-", YEAR + 1)))

snow_day_range <- range((snowfall %>% filter(SNOW > 0))$CommonDate)

plot_caption <- paste0("Source: NOAA (Boston Logan Station); Data as of ", month.abb[tail(snowfall, 2)$MONTH[1]], 
                       " ", tail(snowfall, 2)$DAY[1], ", ", tail(snowfall, 2)$YEAR[1])


# Data Wrangling -------------------------------------------------------------------------------

## Monthly Snowfall 
snowfall_monthly <- snowfall %>%
  filter(between(CommonDate, snow_day_range[1], as.Date("2001-04-30")))%>%
  group_by(Winter, MONTH)%>%
  summarise(SNOW_sum = sum(SNOW, na.rm = TRUE),
            n_snow_days = sum(SNOW >= 0.1))%>%
  mutate(SNOW_sum = ifelse(SNOW_sum == 0, NA, SNOW_sum),
         n_snow_days = ifelse(n_snow_days == 0, NA, n_snow_days))

## Annual snowfall
snowfall_annual <- snowfall %>%
  filter(SNOW != 0)%>%
  group_by(Winter)%>%
  mutate(Total_SNOW = sum(SNOW, na.rm = TRUE),
         Total_n_snow_days = sum(SNOW >= 0.1, na.rm = TRUE),
         first_day = min(CommonDate),
         last_day = max(CommonDate),
         largest_SNOW = max(SNOW, na.rm = TRUE))%>%
  arrange(desc(SNOW))%>%
  slice(1)


data_blank <- data.frame(Winter = c("2018 - 2019", "2019 - 2020", "2020 - 2021"),
                         ymax = c(11.2, c(4, 12) + 2.2),
                         CommonDate = rep(as.Date("2000-12-30"), 3))

# Data Visualization -----------------------------------------------------------------------

## Daily Snowfall barplot
p_daily_inches <- snowfall %>%
  filter(between(CommonDate, snow_day_range[1], as.Date("2001-04-30")))%>%
  mutate(SNOW_cumsum = cumsum(SNOW)) %>%
  ggplot(aes(x = CommonDate, y = SNOW, fill = Winter))+
  geom_hline(data = data.frame(Winter = c(rep("2018 - 2019", 2),
                                          rep("2019 - 2020", 1),
                                          rep("2020 - 2021", 3)),
                               y = c(4, 8, 4, 4, 8, 12)),
             aes(yintercept = y), size = 0.5, color = "#bdc3c7", lty = 2)+
  geom_hline(yintercept = -0.2, size = 0.4, color = "#2c3e50")+
  geom_blank(data = data_blank, aes(y = ymax))+
  geom_col(color = NA, width = 1)+
  geom_label(data = snowfall_annual,
            aes(x = snow_day_range[1]-4, y = c(8, 4, 12) + 2.2, label = Winter),
            inherit.aes = FALSE, hjust = 0, vjust = 1)+
  geom_text(data = snowfall_annual,
            aes(x = CommonDate, y = SNOW + 0.5, label = SNOW), inherit.aes = FALSE, vjust = 0,
            size = 2.9)+
  facet_grid(Winter ~ ., scales = "free_y", space = "free_y") +
  scale_x_date(date_labels = "%b", expand = expansion(add = c(0, 2)),
               date_breaks = "1 month")+
  scale_y_continuous(expand = expansion(add = c(0, 0)),
                     breaks = seq(0, 12, 4))+
  labs(x = NULL, title = "Daily Snowfall in inches") +
  newtheme +
  theme(strip.text = element_blank(),
        strip.background = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0, margin = margin(b = 3)))

# Monthly Snowfall in inches
p_monthly_inches <- snowfall_monthly %>%
  ggplot(aes(x = factor(MONTH, levels = c(10:12, 1:4)), y = SNOW_sum,
             fill = Winter))+
  geom_hline(yintercept = c(6, 12), size = 0.5, color = "#bdc3c7",
              lty = 2)+
  geom_hline(yintercept = -0.2, size = 0.4, color = "#2c3e50")+
  geom_col(color = NA, width = 0.6)+
  geom_text(aes(y = SNOW_sum + 0.4, label = SNOW_sum), 
            size = 3.2, vjust = 0)+
  geom_label(data = snowfall_annual,
             aes(x = 0.5, y = 15, label = Winter),
             inherit.aes = FALSE, hjust = 0, vjust = 1)+
  facet_grid(Winter~ ., scales = "free_y", space = "free_y") +
  scale_x_discrete(labels = month.abb[c(10:12, 1:4)], expand = expansion(add = c(0.2, 0.2)))+
  scale_y_continuous(expand = expansion(add = c(0, 0.3)),
                     breaks = seq(0, 12, 6))+
  labs(x = NULL, title = "Monthly Snowfall in inches") +
  newtheme +
  theme(strip.text = element_blank(),
        strip.background = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0, margin = margin(b = 3)))

# No. of Days with Snowfall >= 0.1 inch
p_monthly_days <- snowfall_monthly %>%
  ggplot(aes(x = factor(MONTH, levels = c(10:12, 1:4)), 
             y = n_snow_days, fill = Winter))+
  geom_hline(yintercept = seq(3, 6, 3), size = 0.5, color = "#bdc3c7", lty = 2)+
  geom_col(color = NA, width = 0.6)+
  geom_hline(yintercept = -0.1, size = 0.4, color = "#2c3e50")+
  geom_text(aes(y = n_snow_days + 0.3, label = n_snow_days), 
            size = 3.2, vjust = 0)+
  geom_label(data = data.frame(x = 0.5, y = 8, Winter = paste(2018:2020, "-", 2019:2021)),
             aes(x = x, y = y, label = Winter),
             inherit.aes = FALSE, hjust = 0, vjust = 1,
             family = frank_med, size = 3.2, color = "white",
             fill = "#2c3e50")+
  facet_wrap(~ Winter, ncol = 1, strip.position = "top") +
  scale_x_discrete(labels = month.abb[c(10:12, 1:4)], expand = expansion(add = c(0.2, 0.2)))+
  scale_y_continuous(expand = expansion(add = c(0, 0.4)),
                     breaks = seq(0, 6, 3))+
  labs(x = NULL, title = "No. of Days with Snowfall \u2265 0.1 inch") +
  newtheme +
  theme(strip.text = element_blank(),
        strip.background = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(hjust = 0, margin = margin(b = 3)),
        legend.position = "none")

## Cumulative Snowfall
data_mark <- snowfall %>% 
  filter(between(CommonDate, snow_day_range[1], as.Date("2001-04-30"))) %>%
  filter(SNOW >= 0.1)%>%
  group_by(Winter)%>%
  mutate(SNOW_cumsum = cumsum(SNOW),
         color = ifelse(Winter == "2018 - 2019", my_palette_muted[1],
                        ifelse(Winter == "2019 - 2020", my_palette_muted[2], 
                               my_palette_muted[3])))

p_snowfall_cumsum <- snowfall %>%
  filter(between(CommonDate, snow_day_range[1] - 5, as.Date("2001-04-30") + 4))%>%
  merge(snowfall_annual %>% select(Winter, last_day))%>%
  mutate(SNOW = ifelse(DATE > tail(snowfall, 2)$DATE[1], NA, SNOW))%>%
  group_by(Winter)%>%
  mutate(SNOW_cumsum = cumsum(SNOW)) %>%
  ggplot(aes(x = CommonDate, y = SNOW_cumsum, 
             group = Winter, color = Winter))+
  geom_hline(yintercept = seq(10, 30, 10), size = 0.5, color = "#bdc3c7", lty = 2)+
  geom_line(size = 0.6) +
  geom_text(data = data_mark,
            aes(label = "*"), vjust = 0.6, color = data_mark$color,
            size = 5, family = "Lucida Sans Unicode",
            show.legend = FALSE)+
  geom_text(data = snowfall_annual, 
            aes(x = last_day + c(0, 0, -4), y = Total_SNOW + c(1.5, 1.5, -1), 
                label = paste0(Total_SNOW, " (", Total_n_snow_days, " days)")),
            hjust = c(0, 0.7, 1), vjust = 0, 
            color = my_palette_muted, size = 3.2)+
  scale_x_date(date_labels = "%b", expand = c(0, 0),
               date_breaks = "1 month")+
  scale_y_continuous(expand = expansion(add = c(1.3, 2)))+
  scale_color_discrete(name = NULL)+
  labs(title = "Cumulative Snowfall in inches", x = NULL, y = NULL)+
  guides(color = guide_legend(keyheight = 0.9, keywidth = 0.9, 
                              override.aes = list(label = c("", "", ""))))+
  newtheme + 
  theme(legend.position = "top",
        legend.margin = margin(b = -8, l = -5, t = 4),
        legend.justification = c(0, 0))

## Arrange multiple plots on a single page 
mat <- matrix(
  c(1, 1, 1, 1, 2, 2, 2,
    1, 1, 1, 1, 2, 2, 2,
    1, 1, 1, 1, 2, 2, 2,
    4, 4, 4, 4, 3, 3, 3,
    4, 4, 4, 4, 3, 3, 3,
    4, 4, 4, 4, 3, 3, 3),
  ncol = 7, byrow = TRUE
)

p <- arrangeGrob(p_daily_inches + theme(plot.margin = margin(5, 10, -5, 5),
                                        plot.title = element_text(margin = margin(0, 5, -2, 0))), 
                 p_monthly_inches + theme(plot.margin = margin(5, 5, -5, 10)),
                 p_monthly_days + theme(plot.margin = margin(20, 5, 10, 10)), 
                 p_snowfall_cumsum + theme(plot.margin = margin(20, 10, 10, 5)),
                 top = textGrob(label = c("\u2744 Boston Snowfall 2018 - 2021 \u2744", "@akela"), x = c(0.003, 0.99), hjust = c(0, 1),
                                gp = gpar(fontfamily = c(frank_demi, frank_med),
                                          fontsize = c(14, 11), col = "#2c3e50")),
                 bottom = textGrob(label = plot_caption, x = 1, hjust = 1, y = 0.7,
                                   gp = gpar(fontfamily = frank_med, 
                                             fontsize = 11, col = "#2c3e50")),
                 layout_matrix = mat)%>%
  cowplot::ggdraw()+
  theme(plot.background = element_rect(fill= "#ecf0f1", color = NA),
        plot.margin = margin(5, 5, 0, 5))

ggsave("boston-snowfall.png", p, width = 7.5, height = 8)

