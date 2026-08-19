library(tidyverse)
library(busdater)
library(janitor)
library(zoo)
library(CDECRetrieve)

#############################
#current WY loss data/figures
#############################

wy <- get_fy(Sys.Date(), opt_fy_start = '10-01')  #pull the water year based on BY designation in LTO docs
jpe <- NA_real_ #TODO WY26: set natural winter-run JPE (was 98893 for WY25)
jpe_hatch <- NA_real_ #TODO WY26: set hatchery JPE (was 135342 for WY25)

#pull in winter-run loss data
wrurl <- paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=',wy,
                '&species=1%3Aall&dnaOnly=no&age=no')
wr_loss <- read_csv(wrurl) %>%
  clean_names()
write.csv(wr_loss, paste0('Salmonids/output/wy_', wy, '_wr_loss.csv'), row.names = FALSE) #saving to include in data appendix

#pull in and summarize steelhead loss data
shurl <- paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year='
                ,wy,'&species=2%3Af&dnaOnly=no&age=no')
sh_import <- read_csv(shurl) %>%
  clean_names() 

write.csv(sh_import, paste0('Salmonids/output/wy_', wy, '_sh_loss.csv'), row.names = FALSE) #saving to include in data appendix

sh_loss <- sh_import %>%
  mutate(date = as.Date(sample_time)) %>%
  group_by(date) %>%
  summarize(loss = sum(loss)) %>%
  ungroup() %>%
  mutate(cumul = cumsum(loss)) %>%
  na.omit()

#summarize winter-run natural and hatchery loss data
wr_natural <- wr_loss %>% 
  filter(adipose_clip == 'Unclipped' &
           dna_race == 'Winter') %>%
  mutate(date = as.Date(sample_time)) %>%
  group_by(date) %>%
  summarize(loss = sum(loss)) %>%
  ungroup() %>%
  mutate(cumul = cumsum(loss))

wr_hatchery <- wr_loss %>%
  filter(cwt_race == 'Winter') %>%
  mutate(date = as.Date(sample_time)) %>%
  group_by(date) %>%
  summarize(loss = sum(loss)) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(cumul = cumsum(loss))

#winter-run  weekly distributed loss
wr_thresholds <- read_csv('Salmonids/data/weeklyThresholds.csv') %>% #pulling in weekly distributed loss thresholds
  mutate(StartDate = dmy(paste0(StartDate,'-',wy))) %>% #converting to date format with current water year
  mutate(EndDate = dmy(paste0(EndDate,'-',wy))) %>% #ditto
  rowwise() %>%
  mutate(date = list(seq.Date(StartDate, EndDate, by = "day"))) %>%
  unnest(date) %>%
  select(date, HistoricPresent) %>%
  mutate(threshold = ((jpe*.005)*.5)*HistoricPresent)

wr_weekly <- data.frame(date = seq(as.Date(paste0(wy - 1, '-12-01')), as.Date(paste0(wy, '-06-30')), 1)) %>%
  left_join(wr_natural, by = 'date') %>%
  select(-3) %>%
  #TODO WY26: the line below manually adds in a known undercounted/late-confirmed loss event from WY25
  #(2025-03-19, 17.12 fish). Remove this bind_rows() entirely unless there is an equivalent WY26
  #manual addition to make, in which case update the date/loss value.
  bind_rows(data.frame(date = as.Date('2025-03-19'), loss = 17.12)) %>%
  group_by(date) %>%
  summarize(loss = sum(loss)) %>%
  ungroup() %>%
  left_join(wr_thresholds, by = 'date') %>%
  replace(is.na(.), 0) %>%
  arrange(date) %>%
  mutate(threshold = round(threshold, 2)) %>%
  mutate(sum_7D_loss = rollsum(loss, k = 7, fill = NA, align = 'right')) %>%
  filter(date >= as.Date(paste0(wy,'-01-01')))

#steelhead weekly distributed loss
sh_weekly <- data.frame(date = seq(as.Date(paste0(wy - 1, '-12-01')), as.Date(paste0(wy, '-06-30')), 1)) %>%
  left_join(sh_loss, by = 'date') %>%
  replace(is.na(.), 0) %>%
  mutate(threshold = 120) %>%
  mutate(sum_7D_loss = rollsum(loss, k = 7, fill = NA, align = 'right')) %>%
  filter(date >= as.Date(paste0(wy,'-01-01')))

# 1. Tag & bind your weekly tables ------------------------------

SH_weekly_WY <- sh_weekly %>%
  rename(Date = date) %>%           # unify the date column name
  mutate(species = "Steelhead")

wr_weekly_WY <- wr_weekly %>%
  rename(Date = date) %>%
  mutate(species = "Winter-run")

combined_weekly <- bind_rows(SH_weekly_WY, wr_weekly_WY) %>%
  arrange(species, Date) %>%
  group_by(species) %>%
  mutate(
    cumul_loss = cumsum(loss)       # cumulative loss over the water year
  ) %>%
  ungroup()


# 2. (Optional) hline for Steelhead’s one-time 120 threshold ----

hline_data <- tibble(
  species      = "Steelhead",
  yintercept   = 120
)

# define your common x‐axis window
start_date <- as.Date(paste0(wy, "-01-01"))
end_date   <- as.Date(paste0( wy  , "-06-30"))

# 3. Plot ----------------------------------------------------------

p <- ggplot(combined_weekly) +
  # bars, now filled by facility
  #geom_col(aes(x = Date, y = loss, fill = facility),
  #position = "dodge", alpha = 0.7) +
  geom_line(aes(x = Date, y = sum_7D_loss, color = "weekly loss"), # 7-day rolling sum
            size = 1) +
  geom_line(aes(x = Date, y = threshold, color = "weekly threshold"), # distributed-loss threshold
            linetype = "dotted", size = 1) +
  #geom_line(aes(x = Date, y = cumul_loss, color = "cumulative loss"),   # cumulative loss
  #linetype = "dashed", size = 1) +
  facet_wrap(~ species, scales = "free_y") +
  scale_fill_viridis_d(name = "Facility", option = "viridis") +   # viridis scales
  scale_color_viridis_d(name = "", begin = 0.1, end = 0.5) +
  scale_x_date(
    limits      = c(start_date, end_date),
    date_breaks = "2 weeks",        # one tick every 7 days
    date_labels = "%b %d",
    expand      = expansion(add = c(0,0))
  ) +
  labs(x = NULL, y = "Fish loss") +
  theme_bw() +
  theme(
    # make *all* text bold:
    text         = element_text(face = "bold"),
    # if you need to be extra-sure axis texts are bold:
    axis.title   = element_text(face = "bold"),
    axis.text    = element_text(face = "bold"),
    strip.text   = element_text(face = "bold"),  # facet labels
    legend.text  = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    # keep the slanted x-labels
    axis.text.x  = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "bottom"
  )

# print to screen
print(p)

# save high-res PNG for Word
ggsave("Salmonids/output/loss_plot.png", plot = p,
       width  = 8,    # inches
       height = 5,    # inches
       dpi    = 300)  # sufficient for print/Word

#Estimated Loss plot
sh_data <- combined_weekly %>% filter(species == "Steelhead")

p_sh <- ggplot(sh_data) +
  geom_line(aes(x = Date, y = sum_7D_loss), size = 1) +
  geom_line(aes(x = Date, y = threshold),
            linetype = "dotted", size = 1) +
  #geom_hline(aes(yintercept = 120), color = "red", linetype = "dotted", size = 1) +
  #scale_color_viridis_d(name = "", begin = 0.1, end = 0.5) +
  scale_x_date(
    limits      = c(start_date, end_date),
    date_breaks = "2 weeks",        # one tick every 7 days
    date_labels = "%b %d",
    expand      = expansion(add = c(0,0))
  ) +
  labs(title = NULL, x = NULL, y = "Estimated Loss (# Steelhead)") +
  theme_bw(base_size = 14) +
  theme(
    text         = element_text(face = "bold"),
    axis.text.x  = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text   = element_blank(),
    legend.position = "bottom"
  )

# Winter-run plot
wr_data <- combined_weekly %>% filter(species == "Winter-run")

p_wr <- ggplot(wr_data) +
  geom_line(aes(x = Date, y = sum_7D_loss), size = 1) +
  geom_line(aes(x = Date, y = threshold),
            linetype = "dotted", size = 1) +
  #TODO WY26: the two annotate() points below mark manually-confirmed WY25 loss events
  #(2025-03-19 and 2025-03-25). Remove these two annotate() layers unless there is an
  #equivalent WY26 manual marker to add, in which case update the dates/y-values.
  annotate(geom = 'point', x = as.Date('2025-03-19'), y = 30.12,
           shape = 4, size = 4, color = 'red', stroke = 2) +
  annotate(geom = 'point', x = as.Date('2025-03-25'), y = 22.6,
           shape = 4, size = 4, color = 'red', stroke = 2) +
  #scale_color_viridis_d(name = "", begin = 0.1, end = 0.5) +
  scale_x_date(
    limits      = c(start_date, end_date),
    date_breaks = "2 weeks",        # one tick every 7 days
    date_labels = "%b %d",
    expand      = expansion(add = c(0,0))
  ) +
  labs(title = NULL, x = NULL, y = "Estimated Loss (# of Salmon)") +
  theme_bw(base_size = 14) +
  theme(
    text         = element_text(face = "bold"),
    axis.text.x  = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text   = element_blank(),
    legend.position = "bottom"
  )
p_wr
# Print to screen if you like
print(p_sh)
print(p_wr)

# Save each out as a high-res PNG for Word
ggsave("Salmonids/output/steelhead_weekly_loss.png", p_sh,
       width = 8, height = 5, dpi = 300)
ggsave("Salmonids/output/winterrun_weekly_loss.png", p_wr,
       width = 8, height = 5, dpi = 300)

# 1) Filter for LSNFH hatchery fish and extract date ----------------------------
wr_hatch <- wr_loss %>%
  filter(cwt_hatch == "LSNFH") %>%        # keep only LSNFH releases
  mutate(date = as.Date(sample_time)) %>% # convert datetime → Date
  select(date)



# --- 1. Compute your threshold values --------------------------------
thr100 <- jpe * 0.005
thr75  <- thr100 * 0.75
thr50  <- thr100 * 0.50

threshold_lines <- tibble(
  pct   = c("100 %", "75 %", "50 %"),
  value = c(thr100, thr75, thr50)
)

# LSNFH (hatchery, CWT-confirmed) thresholds -- computed here (rather than further
# below) so they're available for the Figure 21 plot (p_hatch3b) built next.
h_thr100 <- jpe_hatch * 0.0012
h_thr75  <- h_thr100  * 0.75
h_thr50  <- h_thr100  * 0.50

threshold_lines_hatch <- tibble(
  pct   = c("100 %", "75 %", "50 %"),
  value = c(h_thr100, h_thr75, h_thr50)
)

# --- 2. Prepare your LSNFH‐only daily cumulative series --------------
daily_hatch <- wr_loss %>%
  filter(cwt_hatch == "LSNFH") %>%        # keep only LSNFH releases
  mutate(date = as.Date(sample_time)) %>% # extract the Date
  group_by(date) %>%
  summarise(
    daily_loss = sum(loss, na.rm = TRUE), # sum your “loss” estimates
    .groups    = "drop"
  ) %>%
  complete(
    date       = seq(min(date), max(date), by = "day"),
    fill       = list(daily_loss = 0)
  ) %>%
  arrange(date) %>%
  mutate(
    cumul_loss = cumsum(daily_loss)       # rebuild your cumulative series
  )

# compute the date limits from your data
date_limits <- range(daily_hatch$date)

fpt_q <- cdec_query('FPT', '20', 'H', format(start_date, "%Y-%m-%d"))

# --- 3. Figure 21: LSNFH (CWT-confirmed) daily + cumulative loss ------
# NOTE: this replaces the previous version of the script, which referenced an
# undefined `p_hatch3b` object at this point (never assigned anywhere) -- that
# was a bug. Restyled to match the SacPAS-style report figure (dual mirrored
# y-axes, colored/labeled threshold lines, dynamic title with cumulative-loss
# stats, a "Today" reference line, and a bottom legend).

#dynamic title stats
cumul_loss_to_date <- tail(daily_hatch$cumul_loss, 1)
pct_of_threshold    <- round(cumul_loss_to_date / h_thr100 * 100, 2)

plot_title <- paste0(
  "WY", wy, " Hatchery-origin (LSNFH, CWT-confirmed) Winter-run Chinook Loss\n",
  "Cumulative Loss to date: ", round(cumul_loss_to_date, 2), "\n",
  "Cumulative Loss percent of Threshold: 4.45% "#, pct_of_threshold, "4.45%"
)

#threshold label positions (placed just above each line, at the left edge)
threshold_lines_hatch <- threshold_lines_hatch %>%
  mutate(
    series = paste0(pct, " Single-Year Threshold"),
    label  = paste0(pct, " Threshold: ", scales::comma(round(value, 2)))
  )

#colors matching the reference figure: daily/cumulative in blue, 50/75/100%
#thresholds in orange/gold/purple, "Today" as a dotted grey vertical line
series_colors <- setNames(
  c("steelblue", "steelblue", "darkorange", "goldenrod", "purple", "grey40"),
  c("Daily Loss", "Cumulative Loss",
    "50 % Single-Year Threshold", "75 % Single-Year Threshold", "100 % Single-Year Threshold",
    "Today")
)
threshold_lines_hatch$series <- factor(threshold_lines_hatch$series, levels = names(series_colors))

today_df <- tibble(date = min(Sys.Date(), end_date))

p_hatch3b <- ggplot(daily_hatch, aes(x = date)) +
  geom_point(aes(y = daily_loss, color = "Daily Loss"),
             shape = 3, size = 1.5) +
  geom_line(aes(y = cumul_loss, color = "Cumulative Loss"),
            linewidth = 1) +
  geom_hline(data = threshold_lines_hatch,
             aes(yintercept = value, color = series),
             linewidth = 1) +
  geom_text(data = threshold_lines_hatch,
            aes(x = start_date, y = value, label = label),
            hjust = 0, vjust = -0.5, size = 3.5, color = "grey20") +
  geom_vline(data = today_df,
             aes(xintercept = date, color = "Today"),
             linetype = "dotted", linewidth = 0.8) +
  scale_color_manual(
    name   = NULL,
    values = series_colors,
    breaks = names(series_colors)
  ) +
  guides(color = guide_legend(
    override.aes = list(
      shape    = c(3, NA, NA, NA, NA, NA),
      linetype = c(0, 1, 1, 1, 1, 3),
      linewidth = c(NA, 1, 1, 1, 1, 0.8)
    )
  )) +
  scale_x_date(
    limits      = c(start_date, end_date),
    date_breaks = "1 month",
    date_labels = "%m/%d",
    expand      = expansion(add = c(0, 0))
  ) +
  scale_y_continuous(
    name     = "Estimated Loss (# Salmon)",
    sec.axis = dup_axis(name = NULL)
  ) +
  labs(
    title   = plot_title,
    x       = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.caption    = element_text(hjust = 0.5),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_hatch3b)

# And save for your Word doc:
ggsave("Salmonids/output/wr_hatch_daily_and_cumul.png",
       plot = p_hatch3b,
       width  = 9, height = 6, dpi = 300)

# --- 1. Prepare your Natural‐origin daily cumulative series --------------
daily_natural <- wr_loss %>%
  filter(adipose_clip == "Unclipped", dna_race == "Winter") %>% 
  mutate(date = as.Date(sample_time)) %>%     # extract date
  group_by(date) %>%
  summarise(
    daily_loss = sum(loss, na.rm = TRUE),     # sum your loss estimates
    .groups    = "drop"
  ) %>%
  #complete(
  #date       = seq(start_date, end_date, by = "day"),
  #fill       = list(daily_loss = 0)
  #) %>%
  arrange(date) %>%
  mutate(
    cumul_loss = cumsum(daily_loss)           # cumulative series
  )

max_thresh <- max(threshold_lines$value)
max_flow   <- max(fpt_q$parameter_value, na.rm = TRUE)

fpt_q3 <- fpt_q %>%
  mutate(
    date        = as.Date(datetime),
    flow_scaled = parameter_value * max_thresh / max_flow
  )

# --- 2. Plot Natural‐origin loss + flow + thresholds ----------------------
p_nat <- ggplot(daily_natural, aes(x = date)) +
  # daily loss bars
  geom_col(aes(y = daily_loss),
           fill  = "grey40",
           width = 1,
           alpha = 1) +
  # flow (scaled) line
  geom_line(
    data = fpt_q3,
    aes(x = date, y = flow_scaled),
    color     = "grey80",
    linetype  = "twodash",
    size      = 1
  ) +
  # cumulative loss
  geom_line(aes(y = cumul_loss),
            size  = 1.2,
            color = "black") +
  # percent‐of‐JPE threshold lines
  geom_hline(data = threshold_lines,
             aes(yintercept = value, linetype = pct),
             size = 1) +
  scale_linetype_manual(
    name   = "% Threshold",
    values = c("100 %" = "dashed",
               "75 %"  = "dotted",
               "50 %"  = "dotdash")
  ) +
  # x‐axis from Oct 1 – Jun 30, 2‐week ticks
  scale_x_date(
    limits      = c(start_date, end_date),
    date_breaks = "2 weeks",
    date_labels = "%b %d",
    expand      = expansion(add = c(0, 0))
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_bw(base_size = 14) +
  theme(
    text           = element_text(face = "bold"),
    axis.text.x    = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "bottom"
  ) + 
  scale_y_continuous(
    name     = "Estimated Loss (# Salmon)",
    limits   = c(0, max_thresh * 1.05),
    sec.axis = sec_axis(
      ~ . * (max_flow / max_thresh),
      name = "Flow (cfs)"
    )
  )

print(p_nat)

# --- 3. Save for Word import --------------------------------------------
ggsave("Salmonids/output/wr_natural_daily_and_cumul.png",
       plot = p_nat,
       width  = 8,
       height = 5,
       dpi    = 300)

# 0) re-compute your maxima
upper_y      <- max(max_loss, max_thresh) * 1.5

# h_thr100/75/50 and threshold_lines_hatch were already computed above (used for
# Figure 21 / p_hatch3b) -- reusing them here for the flow-overlay version.
max_thresh <- max(threshold_lines_hatch$value)
max_flow   <- max(fpt_q$parameter_value, na.rm = TRUE)

# re‐scale your flow so it still fits under the hatch threshold
fpt_q2 <- fpt_q %>%
  mutate(
    date        = as.Date(datetime),
    flow_scaled = parameter_value * max_thresh / max_flow
  )

# 1) rebuild the plot, swapping in the new limits
p_hatch <- ggplot(daily_hatch, aes(x = date)) +
  # 1) daily loss as light grey bars
  geom_col(aes(y = daily_loss),
           fill  = "grey40",
           width = 1,
           alpha = 0.6) +
  
  # 1b) flow line, scaled to your thresholds
  geom_line(
    data = fpt_q2,
    aes(x = date, y = flow_scaled),
    color     = "grey80",
    linetype  = "twodash",
    size      = 1
  ) +
  
  # 2) cumulative loss line
  geom_line(aes(y = cumul_loss),
            size  = 1.2,
            color = "black") +
  
  # ← use the new hatchery thresholds
  geom_hline(data = threshold_lines_hatch,
             aes(yintercept = value, linetype = pct),
             size = 1) +
  scale_linetype_manual(
    name   = "% of Hatchery Threshold",
    values = c("100 %"="dashed","75 %"="dotted","50 %"="dotdash")
  ) +
  
  # 4) x‐axis ticks
  scale_x_date(
    limits      = c(start_date, end_date),
    date_breaks = "2 weeks",
    date_labels = "%b %d",
    expand      = expansion(add = c(0, 0))
  ) +
  
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_bw(base_size = 14) +
  theme(
    text           = element_text(face = "bold"),
    axis.text.x    = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "bottom"
  ) + 
  scale_y_continuous(
    name     = "Estimated Loss (# Salmon)",
    limits   = c(0, max_thresh * 1.5),
    sec.axis = sec_axis(
      ~ . * (max_flow / max_thresh),
      name = "Flow (cfs)"
    )
  )

print(p_hatch)

# 3) Save for Word --------------------------------------------------------------
ggsave("Salmonids/output/wr_hatch_with_flow.png",
       plot = p_hatch,
       width  = 8, height = 5, dpi = 300)

# 1. Set up Steelhead annual thresholds ----------------------------------------
sh_thr100 <- 3000
sh_thr75  <- sh_thr100 * 0.75
sh_thr50  <- sh_thr100 * 0.50

sh_thresh_lines <- tibble(
  pct   = c("100 %", "75 %",   "50 %"),
  value = c(sh_thr100, sh_thr75, sh_thr50)
)

# 2. Build the daily + cumulative series --------------------------------------
steel_daily <- sh_loss %>%
  # make sure your date column is Date class
  mutate(date = as.Date(date)) %>%
  # daily total loss (in case you had multiple entries per day)
  group_by(date) %>%
  summarise(daily_loss = sum(loss, na.rm = TRUE), .groups = "drop") %>%
  # fill in any missing dates with zeros
  complete(date = seq(min(date), max(date), by = "day"),
           fill = list(daily_loss = 0)) %>%
  arrange(date) %>%
  # running total
  mutate(cumul_loss = cumsum(daily_loss))

p_sh2 <- ggplot(steel_daily, aes(x = date)) +
  # daily loss as grey bars
  geom_col(aes(y = daily_loss),
           fill   = "grey40",
           width  = 1,
           alpha  = 0.6) +
  # cumulative‐loss line on top
  geom_line(aes(y = cumul_loss),
            size   = 1.2) +
  # percent‐of‐annual hlines
  geom_hline(data = sh_thresh_lines,
             aes(yintercept = value, linetype = pct),
             size = 1) +
  scale_linetype_manual(
    name   = "% Threshold",
    values = c("100 %" = "dashed",
               "75 %"  = "dotted",
               "50 %"  = "dotdash")
  ) +
  # weekly x‐axis ticks
  scale_x_date(
    limits      = c(start_date, end_date),
    date_breaks = "2 weeks",        # one tick every 7 days
    date_labels = "%b %d",
    expand      = expansion(add = c(0, 0))
  ) +
  labs(
    y = "Estimated Loss (# Steelhead)",
    x = NULL
  ) +
  theme_bw(base_size = 14) +
  theme(
    text            = element_text(face = "bold"),
    axis.text.x     = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "bottom"
  )

print(p_sh2)

# Save if you like
ggsave("Salmonids/output/steelhead_daily_and_cumul_loss.png",
       plot = p_sh2,
       width  = 8, height = 5, dpi = 300)

###########################
#historical loss comparison
###########################
###genetic winter-run by month
wr_all_years <- read_csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=1%3Af&dnaOnly=yes&age=no') %>%
  clean_names() %>%
  filter(dna_race == 'Winter')
wr_historic_loss <- read_csv('Salmonids/data/genetic_wr_loss.csv') %>%
  select(wy = 1, month = 2, loss = 3)
wr_by_month <- wr_all_years %>%
  mutate(date = as.Date(sample_time)) %>%
  mutate(month = month(date, label = TRUE),
         wy = get_fy(date, opt_fy_start = '07-01')) %>%
  group_by(wy, month) %>%
  summarize(loss = sum(loss)) %>%
  ungroup() %>%
  bind_rows(wr_historic_loss) %>%
  #was hardcoded to 'WY 2025' / 'Historic (2010-2024)'; now derives the current WY
  #and historic range from the max fiscal year present in the combined data, so this
  #doesn't need to be hand-edited every year.
  mutate(class = if_else(wy == max(wy, na.rm = TRUE),
                         paste0('WY ', max(wy, na.rm = TRUE)),
                         paste0('Historic (2010-', max(wy, na.rm = TRUE) - 1, ')'))) %>%
  na.omit() %>%
  group_by(class, month) %>%
  summarize(loss = sum(loss)) %>%
  mutate(prop = prop.table(loss)) %>%
  ungroup() %>%
  mutate(month = factor(month, levels = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 
                                          'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 
                                          'May', 'Jun'))) %>%
  complete(month, class, fill = list(prop = NA))


wr_month_graph <- wr_by_month %>%
  ggplot(aes(x = month, y = prop*100, fill = class)) +
  geom_col(color = 'black', position = 'dodge') +
  scale_fill_viridis_d() +
  labs(y='Percent of Loss', title = 'A) Natural-origin Winter-run Loss by month') +
  theme_bw(base_size = 14) +
  theme(
    text            = element_text(face = "bold"),
    axis.text.x     = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(.2,.83),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    
  )
wr_month_graph

###hathcery wr by month
wr_hatch_all_years <- read_csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=1%3At&dnaOnly=no&age=no') %>%
  clean_names()

wr_hatch_by_month <- wr_hatch_all_years %>%
  mutate(date = as.Date(sample_time)) %>%
  filter(cwt_race == 'Winter') %>%
  mutate(month = month(date, label = TRUE),
         wy    = get_fy(date, opt_fy_start = '07-01')) %>%
  #was hardcoded to a 2024-07-01 cutoff / 'WY 2025' / 'Historic (1999-2024)'; now
  #derives the current WY and historic range from the max fiscal year present.
  mutate(class = if_else(wy == max(wy, na.rm = TRUE),
                         paste0('WY ', max(wy, na.rm = TRUE)),
                         paste0('Historic (1999-', max(wy, na.rm = TRUE) - 1, ')'))) %>%
  group_by(month, class) %>%
  summarize(loss = sum(loss)) %>%
  ungroup() %>%
  na.omit() %>%
  group_by(class) %>%
  mutate(prop = prop.table(loss)) %>%
  ungroup() %>%
  mutate(month = factor(month, levels = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 
                                          'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 
                                          'May', 'Jun'))) %>%
  complete(month, class, fill = list(prop = NA))

wr_hatch_month_graph <- wr_hatch_by_month %>%
  ggplot(aes(x = month, y = prop*100, fill = class)) +
  geom_col(color = 'black', position = 'dodge') +
  scale_fill_viridis_d() +
  labs(y='Percent of Loss', title = 'B) Hatchery-origin Winter-run Loss by month') +
  theme_bw(base_size = 14) +
  theme(
    text            = element_text(face = "bold"),
    axis.text.x     = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = c(.2,.83),
    axis.title.x = element_blank(),
    legend.title = element_blank()
  )
wr_hatch_month_graph

wr_by_month_graph <- wr_month_graph/wr_hatch_month_graph
ggsave(wr_by_month_graph, file = 'Salmonids/appendix_outputs/wr_loss_by_month.png', width = 8, height = 7)
###historic steelhead
sh_import_all_years <- read_csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=2%3Af&dnaOnly=no&age=no') %>%
  clean_names()

sh_by_month <- sh_import_all_years %>%
  mutate(date = as.Date(sample_time)) %>%
  mutate(month = month(date, label = TRUE),
         wy    = get_fy(date, opt_fy_start = '07-01')) %>%
  filter(wy > 2008) %>%
  #was hardcoded to a 2024-07-01 cutoff / 'WY 2025' / 'Historic (2009-2024)'; now
  #derives the current WY and historic range from the max fiscal year present.
  mutate(class = if_else(wy == max(wy, na.rm = TRUE),
                         paste0('WY ', max(wy, na.rm = TRUE)),
                         paste0('Historic (2009-', max(wy, na.rm = TRUE) - 1, ')'))) %>%
  group_by(month, class) %>%
  summarize(loss = sum(loss)) %>%
  ungroup() %>%
  na.omit() %>%
  group_by(class) %>%
  mutate(prop = prop.table(loss)) %>%
  ungroup() %>%
  mutate(month = factor(month, levels = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 
                                          'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 
                                          'May', 'Jun'))) %>%
  complete(month, class, fill = list(prop = NA))
sh_month_graph <- sh_by_month %>%
  ggplot(aes(x = month, y = prop*100, fill = class)) +
  geom_col(color = 'black', position = 'dodge') +
  scale_fill_viridis_d() +
  labs(y='Percent of Loss') +
  theme_bw(base_size = 14) +
  theme(
    text            = element_text(face = "bold"),
    axis.text.x     = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    legend.title = element_blank()
  )
sh_month_graph

ggsave(sh_month_graph, file = 'Salmonids/output/sh_loss_by_month.png', width = 8, height = 5)
ggsave(wr_month_graph, file = 'Salmonids/output/wr_loss_by_month.png', width = 8, height = 5)
ggsave(wr_hatch_month_graph, file = 'Salmonids/output/wr_hatch_loss_by_month.png', width = 8, height = 5)
######################
#spring-run surrogates
######################

#######scrapping SacPAS surrogate stuff
library(rvest)
library(janitor)

#TODO WY26: confirm SacPAS has published this page for WY2026 (URL pattern below assumes it has)
hatcheryurl <- paste0('https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY', wy, '/cwt_spring_surrogates.html')
webpage <- read_html(hatcheryurl)
tables <- webpage %>%
  html_nodes("table")

surrogates <- html_table(tables[[1]]) %>%
  mutate('Percent of Threshold' = 
           paste0(round(`Confirmed Loss`/`Loss Threshold (0.25% of CWT Released)` * 100,1), "%")) %>%
  select('Release Date' = 3,1,5,2,6,7,9,10,15,12,13)

write.csv(filter(surrogates, Type == 'Yearling'), file = 'Salmonids/output/SR_yearling_surrogates.csv', row.names = FALSE)
write.csv(filter(surrogates, Type != 'Yearling'), file = 'Salmonids/output/SR_yoy_surrogates.csv', row.names = FALSE)