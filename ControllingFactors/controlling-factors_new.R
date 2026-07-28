### WY 26 updated by Lillian McCormick
# lmccormick@usbr.gov

library(tidyverse)
library(readxl)

# start/end dates (edit each year)
wy <- year(Sys.Date())
py <- wy-1
start <- paste0(py,"-10-01")
#end <- paste0(wy,"-06-30")
# or, if season ends prior to June 30:
end <- "2026-06-24"

data_import <- read_excel('ControllingFactors/CVP Delta OPS_WY26.xlsx', skip = 1) %>%
  select(date = 1, status = 2, JPP = 4, CCF = 6, DCC = 5,
         omr_usgs_1 = 7, omr_usgs_5 = 8, omr_usgs_14 = 9,
         omr_1 = 10, omr_5 = 11, omr_7 = 12, omr_14 = 13) %>%
  mutate(date = ymd(date)) %>%
  filter(!is.na(date)) %>% 
  filter(date > as.Date('2025-09-30'))

dcc_temp <- select(data_import, 1,5) %>%
  filter(DCC == 'O',
         date < as.Date('2026-07-19') &
           date > as.Date('2025-09-30')) %>%
  mutate(Factor = 'DCC Gate Open') %>%
  select(3,Date = 1)

controlling <- read_csv('ControllingFactors/controlling-factors_WY2026.csv') %>%
  select(1,2) %>% 
  mutate(Date = mdy(Date)) %>%
  #bind_rows(dcc_temp) %>%
  na.omit() %>% 
  mutate(Factor = factor(Factor, levels = c('D-1641 Collinsville Standard', 'D-1641 E/I Ratio','D-1641 Minimum Delta Outflow',
                                            'D-1641 Port Chicago Standard','D-1641 Vernalis 1:1','D-1641 Delta Water Quality', 
                                            'DCC Gate Open', 'OMRI -3500 cfs due to First Flush Action','OMRI -5000 cfs due to First Flush Action', 
                                            'OMRI -5000 cfs'))) %>% 
  filter(Date >= as.Date('2025-09-15'), Date < as.Date('2026-07-01'))

delta_condition <- data_import %>%
  mutate(condition = case_when(status == 'B' ~ 'Balanced', 
                               status == 'E' ~  'Excess',
                               status == 'E/R' ~ 'Excess w/ Restrictions')) %>%
  filter(!is.na(condition),
         date <= as.Date('2026-06-30'))


condition_graph <- ggplot(delta_condition, aes(x = date, y = condition)) +
  geom_tile(fill = 'black') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b %Y') +
  theme_bw() +
  labs(x = 'Date') +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# View
condition_graph


controlling_graph <-ggplot(controlling, aes(x = Date, y = Factor)) +
  geom_tile(fill = 'black') +
  geom_tile(data= dcc_temp, aes(x= Date, y= Factor), fill= "coral2")+
  scale_x_date(date_breaks = '1 month', date_labels = '%b %Y') +
  theme_bw() +
  labs(x = 'Date') +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# View
controlling_graph

# Save
ggsave(condition_graph, file = 'ControllingFactors/excess_balance_fig_2026.png', height = 2, width = 6)
ggsave(controlling_graph, file = 'ControllingFactors/control_factor_fig_2026.png', height = 2.5, width = 7)

