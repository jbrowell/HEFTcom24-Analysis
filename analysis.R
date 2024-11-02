require(dplyr)
require(data.table)
require(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
require(ggplot2)
require(ggridges)

## Fig format
fig_format <- "pdf"
fig_size_in <- c(8,4)
custom_theme <- theme_bw() + theme(
  text=element_text(size=20,family="serif"),
  strip.background =element_rect(fill="white"))
color_pal_top10 <- RColorBrewer::brewer.pal(10,"Paired")

## load data

### Trades, prices and revenue, with missed submissions filled
trade_data <- fread("data/trades.csv")

### Pinball score by timestamp with missed submissions filled
forecast_score <- fread("data/pinball.csv")

### Raw forecast submissions, outturn and pinball, missed submissions not filled
forecast_data <- fread("data/forecasts.csv")

### Energy data
energy_data <- rbind(fread("data/Energy_Data_20200920_20240118.csv"),
                     fread("data/Energy_Data_20240119_20240519.csv"))

## Plots

### Competition data

energy_plot_data <- melt(energy_data[dtm>=as.POSIXct("2023-12-01",tz = "UTC") &
                                       dtm<as.POSIXct("2024-05-19 23:00:00",tz = "UTC"),
                                     .(dtm,Wind = Wind_MW/2+boa_MWh,Solar=Solar_MW/2)],
                         id.vars = "dtm",value = "Generation [MWh]")

p_energy <- ggplot(data = energy_plot_data,
                   aes(x=dtm,y=`Generation [MWh]`)) +
  geom_line() +
  facet_grid(rows = "variable",scales = "free_y") +
  xlab("Date/Time [settlement period]") +
  geom_vline(xintercept=as.POSIXct("2024-02-20",tz = "UTC"),linetype="dashed") +
  custom_theme

p_energy

ggsave(filename = paste0("figs/wind_solar.",fig_format), p_energy,
       width = fig_size_in[1],height = fig_size_in[2],units = "in")


price_plot_data <- melt(energy_data[dtm>=as.POSIXct("2024-02-20",tz = "UTC") &
                                      dtm<as.POSIXct("2024-02-24",tz = "UTC"),
                                    .(dtm,`Day-ahead`=DA_Price,`Imbalance`=SS_Price)],
                        id.vars = "dtm",value = "Price [£/MWh]",variable.name = "Market")

p_price <- ggplot(data = price_plot_data,
                  aes(x=dtm,y=`Price [£/MWh]`,linetype=Market)) +
  geom_line() +
  # facet_grid(rows = "variable",scales = "free_y") +
  xlab("Date/Time [settlement period]") +
  # geom_vline(xintercept=as.POSIXct("2024-02-20",tz = "UTC"),linetype="dashed") +
  custom_theme

p_price
ggsave(filename = paste0("figs/prices.",fig_format), p_price,
       width = fig_size_in[1],height = fig_size_in[2],units = "in")


### Revenue evolution:
top_teams <- trade_data[,sum(revenue),by=team][order(V1,decreasing = T)][1:10,team]


mean10 <- trade_data[team%in%top_teams,.(mean10_revenue=mean(revenue)),by=dtm]
mean10[order(dtm),mean10_cumrevenue := cumsum(mean10_revenue)]

trade_data <- merge(trade_data,mean10,by="dtm",all.x=T)

setkey(trade_data,dtm)
p <- ggplot(trade_data[team %in% top_teams,.(dtm,rel_revenue=(cumsum(revenue)-mean10_cumrevenue)/1e6),by=team],
            aes(x=dtm,y=rel_revenue,color=team)) +
  geom_line() +
  xlab("Date/Time") + ylab("Relative Revenue of Top 10 [£m]") +
  guides(color=guide_legend(title="Team (Top 10)")) +
  scale_color_discrete(breaks=top_teams) +
  scale_color_manual(values = color_pal_top10) +
  custom_theme

p

ggsave(filename = paste0("figs/revenue_top10.",fig_format),p,
       width = fig_size_in[1],height = fig_size_in[2],units = "in")



## Pinball evolution
top_teams_fc <- forecast_score[,mean(pinball),by=team][order(V1,decreasing = F)][1:10,team]
setkey(forecast_score,dtm)

forecast_score[,n:=as.numeric((dtm-min(dtm))/(60*30)+1)]

p2 <- ggplot(forecast_score[team %in% top_teams_fc,.(dtm,pinball=cumsum(pinball)/n),by=team],
             aes(x=dtm,y=pinball,color=team)) +
  geom_line() +
  xlab("Date/Time") + ylab("Pinball [MWh]") +
  guides(color=guide_legend(title="Team (Top 10)")) +
  ylim(c(15,32)) +
  scale_color_discrete(breaks=top_teams_fc) +
  scale_color_manual(values = color_pal_top10) +
  custom_theme

p2

ggsave(filename = paste0("figs/pinball_top10.",fig_format), p2,
       width = fig_size_in[1],height = fig_size_in[2],units = "in")




### Trades vs Forecasts

forecast_trade <- merge(forecast_data,
                        trade_data[,.(dtm,team,market_bid,imbalance_price,price)],by=c("dtm","team"),
                        all.y = T)

forecast_trade[,unique_forecasts:=length(unique(forecast)),
               by=c("dtm","team")]

forecast_trade[!is.na(quantile) & !is.na(forecast) & unique_forecasts>1,bid_quantile:=approxfun(x=forecast,y=quantile,rule = 2)(market_bid),
               by=c("dtm","team")]

ggplot(data = forecast_trade[quantile==50],
       aes(x=bid_quantile)) +
  geom_histogram() +
  facet_wrap(~team,ncol=10,scales = "free_y") +
  # scale_fill_manual(values = color_pal_top10) +
  xlim(c(10,90)) +
  custom_theme +
  theme(strip.text = element_blank()) +
  labs(y = "Counts",x="Bid Quantile [%]") + 
  guides(y = "none")

plot_data <- forecast_trade[quantile==50 & team%in%top_teams]
plot_data$team <- factor(plot_data$team,levels = top_teams)
p_bidq <-  ggplot(data = plot_data,
                  aes(x=bid_quantile)) +
  geom_histogram() +
  facet_wrap(~team,ncol=5,scales = "free_y") +
  # scale_fill_manual(values = color_pal_top10) +
  xlim(c(10,90)) +
  custom_theme +
  labs(y = "Counts",x="Bid Quantile [%]") + 
  guides(y = "none") +
  theme(strip.text.x = element_text(size = 10))


ggsave(filename = paste0("figs/bid_quantile.",fig_format), p_bidq,
       width = fig_size_in[1],height = fig_size_in[2],units = "in")

### Revenue vs pinball loss

top_teams <- trade_data[,sum(revenue),by=team][order(V1,decreasing = T)][1:10,team]

p_revvpinball <- merge(forecast_score[,.(dtm,team,pinball)],
             trade_data[,.(dtm,team,revenue)],by=c("dtm","team"),
             all.y = T) %>%
  mutate(team = factor(team, levels=top_teams)) %>%
  filter(team %in% top_teams[1:5]) %>% 
  group_by(team) %>% 
  mutate(binned_pinball = cut(x=pinball, breaks=c(0, 60, 120, 180, 240, 300))) %>% 
  ungroup() %>%
  tidyr::drop_na() %>%
  ggplot(aes(x=revenue, y=binned_pinball, height = after_stat(density))) +
  facet_wrap(~team, nrow = 3) +
  geom_density_ridges(jittered_points = TRUE, scale=1, alpha=0.4, 
                      point_shape = "|", point_size = 2,
                      position = position_points_jitter(height = 0)) +
  scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  labs(
    x = "Revenue (GBP)",
    y = "Binned pinball loss (MWh)"
  ) +
  custom_theme

p_revvpinball

### Revenue vs time of day

tmp <- merge(forecast_score[,.(dtm,team,pinball)],
             trade_data[,.(dtm,team,revenue)],by=c("dtm","team"),
             all.y = T) %>%
  filter(team %in% top_teams[1:5]) %>%
  mutate(hod = strftime(tmp$dtm, format = "%H:%M"))

tmp %>% ggplot(., aes(x=hod)) + 
  facet_wrap(~team, nrow=5, scales = "fixed") +
  geom_boxplot(aes(y=revenue)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### Price spreads
tmp <- merge(forecast_data[,.(dtm, team, quantile, forecast, actual_mwh)],
             trade_data[,.(dtm, team, revenue, imbalance_price, price, market_bid)],by=c("dtm","team"),
             all.y = T) %>%
  filter(team %in% top_teams[1:5]) %>%
  filter(quantile == 50) %>%
  mutate(spread = imbalance_price - price) %>%
  slice_max(spread) %>%
  mutate(error = market_bid - actual_mwh)
tmp

### Market bids - actual_mwh vs revenue

merge(forecast_data[,.(dtm, team, quantile, forecast, actual_mwh)],
      trade_data[,.(dtm, team, revenue, imbalance_price, price, market_bid)],by=c("dtm","team"),
      all.y = T) %>%
  filter(team %in% top_teams[1:5]) %>%
  filter(quantile == 50) %>%
  # mutate(spread = imbalance_price - price) %>%
  mutate(error = market_bid - actual_mwh) %>%
  ggplot(., aes(x=error, y=revenue)) +
  facet_wrap(~team, nrow=3, scales = "fixed") +
  geom_point() +
  geom_smooth(method='lm') +
  xlab("Market bid minus actual (MWh)")

trade_data %>%
  filter(team %in% top_teams[1]) %>%
  mutate(spread = imbalance_price - price) %>%
  summarise(spread = quantile(spread, seq(5, 95, by=5)/100), percentile = seq(5, 95, by=5))
  # ggplot(., aes(x=spread)) +
  # # facet_wrap(~team, nrow=3, scales = "fixed") +
  # # geom_histogram() +
  # stat_ecdf(aes(y=after_stat(y)*100)) +
  # # scale_y_continuous(sec.axis=sec_axis(trans = ~./100 , name="percentage")) +
  # theme_bw()  

### Electricity price vs production
trade_data %>%
  filter(team %in% top_teams[1]) %>%
  mutate(spread = imbalance_price - price) %>%
  ggplot(., aes(x=spread, y=actual_mwh)) +
  geom_density_2d_filled()
