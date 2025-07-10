library(tidyverse)
library(scales)

# Data ---------

figure2 <- fread("destatis/produktionsentwicklung-energieintensiven-industriezweige.csv") %>%
  setNames(c("date", "Indice de la production industrielle (secteurs manufacturier et extractif)",
             "Indice de production des branches industrielles à forte intensité énergétique")) %>%
  mutate(date = as.Date(date)) %>%
  gather(variable, value, -date) %>%
  select(variable, date, value)

write_csv2(figure2, file = "figure2.csv")

# Graph -------

figure2 %>%
  ggplot+ geom_line(aes(x = date, y = value, linetype = variable)) +
  theme_minimal() + xlab("") + ylab("") +
  scale_x_date(breaks = c(seq(1992, 2100, 1)) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  scale_y_log10(breaks = seq(-10, 300, 5),
                labels = dollar_format(accuracy = 1, prefix = "")) +
  theme(legend.title = element_blank(),
        legend.position = c(0.5, 0.1),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))


ggsave("figure2.pdf", width = 5, height = 3, device = cairo_pdf)
ggsave("figure2.png", width = 5, height = 3, bg = "white")



