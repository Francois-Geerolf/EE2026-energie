library(dplyr)
library(eurostat)
library(ggplot2)
load_data("oecd/EO110_INTERNET.RData")
load_data("oecd/EO_117.RData")

EO_117_extract <- EO_117 %>%
  filter(FREQ == "Q",
         REF_AREA == "DEU",
         MEASURE == "GDPV") %>%
  select(obsTime, obsValue)

# Data -------

figure1 <- EO110_INTERNET %>%
  mutate(vintage = "Perspectives économiques No 110 - Décembre 2021") %>%
  filter(VARIABLE == "GDPV", 
         LOCATION == "DEU",
         FREQUENCY == "Q") %>%
  bind_rows(EO_117_extract %>%
              mutate(vintage = "Perspectives économiques No 117 - Juin 2025")) %>%
  quarter_to_date() %>%
  filter(date >= as.Date("2021-01-01")) %>%
  bind_rows(EO_117_extract %>%
              mutate(vintage = "PIB réalisé") %>%
              quarter_to_date() %>%
              filter(date <= as.Date("2024-10-01"),
                     date >= as.Date("2021-01-01"))) %>%
  group_by(vintage) %>%
  mutate(obsValue = 100*obsValue/obsValue[date == as.Date("2021-01-01")]) %>%
  select(vintage, date, obsValue)

write_csv2(figure1, file = "figure1.csv")

# Graph -------

figure1 %>%
  ggplot + geom_line(aes(x = date, y = obsValue, linetype = vintage)) +
  xlab("") + ylab("PIB en volume, Allemagne") + theme_minimal() +
  scale_x_date(breaks = seq(1920, 2100, 1) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  theme(legend.position = c(0.6, 0.2),
        legend.title = element_blank()) +
  scale_y_log10(breaks = seq(20, 200, 1))



ggsave("figure1.pdf", width = 5, height = 3, device = cairo_pdf)
ggsave("figure1.png", width = 5, height = 3, bg = "white")




