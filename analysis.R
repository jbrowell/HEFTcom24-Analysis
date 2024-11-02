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

### leaderboard
leaderboard <- fread("data/overall_leaderboard.csv")
leaderboard[15,Team:="Faces"]

### Repot data
reports <- fread("data/HEFTcom Reports_May 29, 2024_11.54.csv",
                 skip = 0,header = T)[-(1:2),]
reports[9,RecipientFirstName:="Faces"]
setnames(reports,"RecipientFirstName","team")

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

### Forecast evaluation

top_teams_fc

team_include <- forecast_data[,.N,by=team][N>(39000/2),team]

reliability_data <- rbind(
  forecast_data[,.(empirical = 100*mean(actual_mwh<=forecast),
                   TOD = "All"),
                by=c("team","quantile")],
  forecast_data[hour(dtm)<=7.5 | hour(dtm)>=16.5,
                .(empirical = 100*mean(actual_mwh<=forecast),
                  TOD = "Overnight"),
                by=c("team","quantile")],
  forecast_data[hour(dtm)>7.5 & hour(dtm)<16.5,
                .(empirical = 100*mean(actual_mwh<=forecast),
                  TOD = "Daytime"),
                by=c("team","quantile")])

reliability_data <- reliability_data[team %in% team_include]

plot_data <- reliability_data[team%in%top_teams_fc[1:5]]
plot_data$team <- factor(plot_data$team,levels = top_teams_fc)

rel_plot <- ggplot(plot_data,aes(x=quantile,y=empirical,color=team)) +
  geom_line(data=reliability_data[!team%in%top_teams_fc[1:5]],
            mapping=aes(x=quantile,y=empirical,group=team),
            color=gray(0.1,0.1)) +
  geom_point(aes(shape=team)) + geom_line() +
  geom_abline(slope = 1,intercept = 0, linetype="dashed") +
  facet_wrap(~TOD,ncol=3) +
  guides(color=guide_legend(title="Team (Top 5)"),
         shape=guide_legend(title="Team (Top 5)")) +
  scale_color_discrete(breaks=top_teams_fc) +
  scale_color_manual(values = color_pal_top10) +
  xlab("Nominal [%]") + ylab("Empirical [%]") +
  custom_theme +
  theme(aspect.ratio = 1)

rel_plot

ggsave(filename = paste0("figs/reliability.",fig_format), rel_plot,
       width = 1.5*fig_size_in[1],height = fig_size_in[2],units = "in")


#### Forecast methods
plot_data <- merge(rbind(reports[,.(type="regression",
                                    method=transpose(strsplit(Q3.7,","))),by=team],
                         reports[,.(type="feature engineering",
                                    method=transpose(strsplit(Q3.5,","))),by=team]),
                   leaderboard[,.(team=Team,Rank=rank(Pinball))],
                   by = "team",all.y = T)

plot_data[,method := paste0(method)]
plot_data <- plot_data[!method %in% c("None","Others (please specify)","Other supervised learning/regression","NULL")]


top_methods <- plot_data[,.(score=min(Rank,na.rm = T)),by=method]
top_methods[order(score),score2 := cumsum(score)]



plot_data$method <- factor(plot_data$method,
                           levels = top_methods[order(score2,decreasing = T),method])

ggplot(plot_data[order(Rank)],aes(x=method,y=Rank)) +
  ylim(c(1,26)) +
  geom_point() +
  coord_flip() +
  custom_theme +
  theme(axis.title.y=element_blank())


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
  filter(team %in% top_teams) %>%
  mutate(team = factor(team, levels=top_teams)) %>%
  group_by(team) %>% 
  mutate(binned_pinball = cut(x=pinball, breaks=c(0, 60, 120, 180, 240, 300))) %>% 
  ungroup() %>%
  tidyr::drop_na() %>%
  ggplot(aes(x=revenue, y=binned_pinball, height = after_stat(density))) +
  facet_wrap(~team, nrow = 5) +
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

ggsave(filename = paste0("figs/revenue_vs_pinball.",fig_format), p_revvpinball,
       width = 8, height = 10, units = "in")

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

p_revv_marketbids <- merge(forecast_data[,.(dtm, team, quantile, forecast, actual_mwh)],
      trade_data[,.(dtm, team, revenue, imbalance_price, price, market_bid)],by=c("dtm","team"),
      all.y = T) %>%
  filter(team %in% top_teams) %>%
  mutate(team = factor(team, levels=top_teams)) %>%
  filter(quantile == 50) %>%
  tidyr::drop_na() %>%
  mutate(difference = market_bid - actual_mwh) %>%
  ggplot(., aes(x=difference, y=revenue)) +
  facet_wrap(~team, nrow=5, scales = "fixed") +
  geom_point(alpha=0.3) +
  geom_smooth(method='lm') +
  xlab("Market bid minus actual (MWh)") +
  labs(
    y = "Revenue (GBP)",
    x = "Market bid minus actual (MWh)"
  ) +
  custom_theme

ggsave(filename = paste0("figs/revenue_vs_marketbids.",fig_format), p_revv_marketbids,
       width = 8, height = 10, units = "in")

### Revenue from bidding p50 revenue vs strategic bidding (i.e., participant's actual bids)

forecast_trade <- merge(forecast_data[,.(dtm, team, quantile, forecast, actual_mwh)],
                        trade_data[,.(dtm, team, market_bid, imbalance_price, price, revenue)],by=c("dtm", "team"),
                        all.y = T)

forecast_trade[,unique_forecasts:=length(unique(forecast)),
               by=c("dtm","team")]

forecast_trade[!is.na(quantile) & !is.na(forecast) & unique_forecasts>1,bid_quantile:=approxfun(x=forecast,y=quantile,rule = 2)(market_bid),
               by=c("dtm","team")]

p_strategic_vs_medianfc. <- forecast_trade %>%
  filter(team %in% top_teams) %>%
  mutate(team = factor(team, levels=top_teams)) %>%
  filter(quantile == 50) %>%
  mutate(bid_as_forecast = forecast*price + (actual_mwh - forecast) * (imbalance_price - 0.07*(actual_mwh - forecast))) %>%
  ggplot(., aes(x=bid_as_forecast, y=revenue, color=bid_quantile)) +
  facet_wrap(~team, nrow = 5, scales = "fixed") +
  geom_point(alpha=0.5) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="black") +
  scale_color_viridis_c(name = "Bid quantile (%)") +
  labs(
    y = "Revenue from bidding strategically (GBP)",
    x = "Revenue from bidding median forecast (GBP)"
  ) +
  custom_theme +
  theme(legend.key.height = unit(0.75,"lines"),
        legend.position = "bottom",
        legend.justification = "center")

ggsave(filename = paste0("figs/strategic_vs_medianfc.",fig_format), p_strategic_vs_medianfc.,
       width = 8, height = 10, units = "in")
