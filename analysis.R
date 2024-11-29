require(dplyr)
require(data.table)
require(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
require(ggplot2)
require(ggridges)
require(xtable)
library(latex2exp)
require(patchwork)

## Fig format
fig_format <- "pdf"
fig_size_in <- c(8,4)
custom_theme <- theme_bw() + theme(
  text=element_text(size=20,family="serif"),
  strip.background =element_rect(fill="white"))
color_pal_top10 <- RColorBrewer::brewer.pal(10,"Paired")

## Load data

### Trades, prices and revenue, with missed submissions filled
trade_data <- fread("data/trades.csv")
trade_data[team=="à¼¼ ã\u0081¤ â—•_â—• à¼½ã\u0081¤",team:="Please hug"]
trade_data[team=="Ihubex",verified_student:=TRUE]

### Pinball score by timestamp with missed submissions filled
forecast_score <- fread("data/pinball.csv")
forecast_score[team=="à¼¼ ã\u0081¤ â—•_â—• à¼½ã\u0081¤",team:="Please hug"]
forecast_score[team=="Ihubex",verified_student:=TRUE]

### Raw forecast submissions, outturn and pinball, missed submissions not filled
forecast_data <- fread("data/forecasts.csv")
forecast_data[team=="à¼¼ ã\u0081¤ â—•_â—• à¼½ã\u0081¤",team:="Please hug"]
forecast_data[team=="Ihubex",verified_student:=TRUE]

### Energy data
energy_data <- rbind(fread("data/Energy_Data_20200920_20240118.csv"),
                     fread("data/Energy_Data_20240119_20240519.csv"))

### leaderboard
leaderboard <- fread("data/overall_leaderboard.csv")
leaderboard[Team=="à¼¼ ã\u0081¤ â—•_â—• à¼½ã\u0081¤",Team:="Please hug"]

### Repot data
reports <- fread("data/HEFTcom Reports.csv",
                 skip = 0,header = T)[-(1:2),]
reports[9,RecipientFirstName:="Please hug"]
setnames(reports,"RecipientFirstName","team")

## Leaderboard
full_leaderboard <- merge(
  forecast_score[,.(Pinball=round(mean(pinball),2),
                    Report=report[1],
                    Student=verified_student[1]),
                 by="team"],
  trade_data[,.(Revenue=round(sum(revenue)/1e6,2),
                `Missed submissions`=round(sum(filled)/48)),
             by="team"],
  by="team",all=T)

full_leaderboard[Report==T & `Missed submissions`<=5 & !team %in% c("Benchmark","quantopia"),
                 `Forecasting rank`:=rank(Pinball)]
full_leaderboard[Report==T & `Missed submissions`<=5 & !team %in% c("Benchmark","quantopia")
                 ,`Trading rank`:=rank(-Revenue)]
full_leaderboard[,`Combined score`:=`Trading rank`+`Forecasting rank`+`Forecasting rank`/100]
full_leaderboard[!is.na(`Combined score`),`Combined rank`:=rank(`Combined score`)]
full_leaderboard[,`Combined score`:=NULL]

full_leaderboard <- full_leaderboard[order(Pinball),.(Team=team,Pinball,Revenue,`Forecasting rank`,`Trading rank`,`Combined rank`,Report,`Missed submissions`,Student)]

print(xtable(full_leaderboard), include.rownames=FALSE)


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
top_teams_rev <- trade_data[,sum(revenue),by=team][order(V1,decreasing = T)][1:10,team]

mean10 <- trade_data[team%in%top_teams_rev,.(mean10_revenue=mean(revenue)),by=dtm]
mean10[order(dtm),mean10_cumrevenue := cumsum(mean10_revenue)]

trade_data_plot <- merge(trade_data,mean10,by="dtm",all.x=T)


trade_data_plot <- trade_data_plot[team %in% top_teams_rev]#[dtm > "2024-03-01"]
trade_data_plot$team <- factor(trade_data_plot$team,levels = top_teams_rev)

setkey(trade_data_plot,dtm)
p <- ggplot(trade_data_plot[,
                            .(dtm,rel_revenue=(cumsum(revenue)-mean10_cumrevenue)/1e6),
                            by=team],
            aes(x=dtm,y=rel_revenue,color=team)) +
  geom_line() +
  xlab("Date/Time") + ylab("Relative Revenue [£m]") +
  guides(color=guide_legend(title="Team (Top 10)")) +
  scale_color_discrete(breaks=top_teams_rev) +
  scale_color_manual(values = color_pal_top10) +
  custom_theme

p

ggsave(filename = paste0("figs/revenue_top10.",fig_format),p,
       width = fig_size_in[1],height = fig_size_in[2],units = "in")



## Pinball evolution

forecast_score_plot <- forecast_score#[dtm < "2024-03-21 00:00:00"]

top_teams_fc <- forecast_score_plot[,mean(pinball),by=team][order(V1,decreasing = F)][1:10,team]
forecast_score_plot <- forecast_score_plot[team %in% top_teams_fc]
setkey(forecast_score_plot,dtm)
forecast_score_plot$team <- factor(forecast_score_plot$team,levels = top_teams_fc)

forecast_score_plot[,n:=as.numeric((dtm-min(dtm))/(60*30)+1)]


p2 <- ggplot(forecast_score_plot[,.(dtm,pinball=cumsum(pinball)/n),by=team],
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

team_include <- forecast_data[,.N,by=team][N>(39000/2),team]

# include_dtm <- energy_data[dtm>"2024-03-20 00:00:00",dtm] 
# include_dtm <- energy_data[DA_Price>=0,dtm]
include_dtm <- energy_data[,dtm]

reliability_data <- rbind(
  forecast_data[dtm %in% include_dtm,.(empirical = 100*mean(actual_mwh<=forecast),
                                       TOD = "All"),
                by=c("team","quantile")],
  forecast_data[(hour(dtm)<=7.5 | hour(dtm)>=16.5) & dtm %in% include_dtm,
                .(empirical = 100*mean(actual_mwh<=forecast),
                  TOD = "Overnight"),
                by=c("team","quantile")],
  forecast_data[(hour(dtm)>7.5 & hour(dtm)<16.5) & dtm %in% include_dtm,
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
                                    method=transpose(strsplit(Q3.5,","))),by=team],
                         reports[,.(type="model selection",
                                    method=transpose(strsplit(Q3.9,","))),by=team]),
                   leaderboard[,.(team=Team,Rank=rank(Pinball))],
                   by = "team",all.y = T)

plot_data[,method := paste0(method)]
plot_data <- plot_data[!method %in% c("None","Others ","Others (please specify)","Other supervised learning/regression","NULL")]

plot_data[,method := gsub("\\(please provide details\\)","",method)]
plot_data[,method := gsub(" based on",":",method)]

top_methods <- plot_data[,.(score=min(Rank,na.rm = T)),by=method]
top_methods <- merge(top_methods,
                     plot_data[Rank>1,.(score1=min(Rank,na.rm = T)),by=method],
                     by = "method")
top_methods[order(score+score1/20),score2 := cumsum(score+score1/20)]



plot_data$method <- factor(plot_data$method,
                           levels = top_methods[order(score2,decreasing = T),method])

fc_methods <- ggplot(plot_data[order(Rank)],aes(x=method,y=Rank)) +
  ylim(c(1,26)) +
  ylab("Team [ordered by pinball]") +
  scale_y_continuous(breaks = 1:plot_data[,max(Rank)],
                     labels = leaderboard[order(Pinball)][1:26,Team]) +
  geom_point() +
  coord_flip() +
  custom_theme +
  theme(axis.title.y=element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(angle=90,vjust = 0.5,
                                   hjust = 1,size = 10))

fc_methods

ggsave(filename = paste0("figs/forecast_methods.",fig_format), fc_methods,
       width = fig_size_in[1],height = fig_size_in[2]+0.2,units = "in")

#### Trade methods

leaderboard_trading <- trade_data %>% 
  group_by(team) %>% 
  summarise(sum_revenue = sum(revenue)) %>% 
  arrange(desc(sum_revenue))

setDT(leaderboard_trading)
plot_data <- merge(rbind(reports[,.(type="approach",
                                    method=transpose(strsplit(Q4.5,","))),by=team],
                         reports[,.(type="trade strategy",
                                    method=Q4.6),by=team]),
                   leaderboard_trading[,.(team=team,Rank=rank(-1*sum_revenue))],
                   by = "team",all.y = T)

plot_data[,method := paste0(method)]
plot_data <- plot_data[!method %in% c("None","Others (please specify)","Other supervised learning/regression","NULL","NA")]

plot_data[,method := gsub("\\(please provide details\\)","",method)]
plot_data[,method := gsub("Others","Other",method)]

plot_data <- plot_data[!(team == "quantopia" & type == "trade strategy")]
plot_data <- plot_data[!(method == "Risk-seeking strategy" | method == "Decision model based on deterministic power forecast only")]

top_methods <- plot_data[,.(score=min(Rank,na.rm = T)),by=method]
top_methods <- merge(top_methods,
                     plot_data[Rank>1,.(score1=min(Rank,na.rm = T)),by=method],
                     by = "method")
top_methods[order(score+score1/20),score2 := cumsum(score+score1/20)]


plot_data$method <- factor(plot_data$method,
                           levels = top_methods[order(score2,decreasing = T),method])

trade_methods <- ggplot(plot_data[order(Rank)],aes(x=method,y=Rank)) +
  ylab("Team [ordered by total revenue]") +
  scale_y_continuous(breaks=1:26, labels = leaderboard_trading[1:26, team],
                     limits=c(1,26)) +
  geom_point() +
  coord_flip() +
  custom_theme +
  theme(axis.title.y=element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(angle=90,vjust = 0.5,
                                   hjust = 1,size = 10))

trade_methods
ggsave(filename = paste0("figs/trade_methods.",fig_format), trade_methods,
       width = fig_size_in[1],height = fig_size_in[2],units = "in")

### Trades vs Forecasts

forecast_trade <- merge(forecast_data,
                        trade_data[,.(dtm,team,market_bid=floor(market_bid),imbalance_price,price)],
                        by=c("dtm","team"),
                        all.y = T)


forecast_trade[,unique_forecasts:=length(unique(forecast)),
               by=c("dtm","team")]

forecast_trade[!is.na(quantile) & !is.na(forecast) & unique_forecasts>1,
               bid_quantile:=approxfun(x=forecast,y=quantile,rule = 2)(market_bid),
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

plot_data <- forecast_trade[quantile==50 & team%in%top_teams_rev]
plot_data$team <- factor(plot_data$team,levels = top_teams_rev)

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

p_bidq

ggsave(filename = paste0("figs/bid_quantile.",fig_format), p_bidq,
       width = fig_size_in[1],height = fig_size_in[2],units = "in")


plot_data[,bid_q50vol := market_bid-forecast]
p_bid_q50vol <-  ggplot(data = plot_data,#[dtm>"2024-03-20 00:00:00"],
                        aes(x=bid_q50vol)) +
  geom_histogram() +
  facet_wrap(~team,ncol=5,scales = "free_y") +
  # # scale_fill_manual(values = color_pal_top10) +
  xlim(c(-150,150)) +
  ylab("Counts") +
  xlab(TeX("Strategic bid $x - \\hat{q}_{50\\%}$ [MWh]",)) +
  guides(y = "none") +
  custom_theme +
  theme(strip.text.x = element_text(size = 10))

p_bid_q50vol

ggsave(filename = paste0("figs/bid_q50vol.",fig_format), p_bid_q50vol,
       width = fig_size_in[1],height = fig_size_in[2],units = "in")


merge(
  plot_data[,
            .(actual_hit_rate = round(
              100*mean(
                ((market_bid>actual_mwh) & (price>imbalance_price)) |
                  ((market_bid<actual_mwh) & (price<imbalance_price))),
              digits = 1),
              q50_hit_rate = round(
                100*mean(
                  ((forecast>actual_mwh) & (price>imbalance_price)) |
                    ((forecast<actual_mwh) & (price<imbalance_price))),
                digits = 1)
            ),by="team"],
  plot_data[bid_q50vol!=0,
            .(strategic_bid_hit_rate = round(
              100*mean(
                ((bid_q50vol>0) & (price>imbalance_price)) |
                  ((bid_q50vol<0) & (price<imbalance_price))),
              digits = 1),
              number_of_strategic_bids=sum(bid_q50vol!=0)
            ),by="team"],all = T,
)[order(team)]


hist(plot_data[team=="SVK",(imbalance_price-price)/0.14],xlim = c(-500,500),breaks = 100)
hist(plot_data[team=="SVK",market_bid-forecast],xlim = c(-500,500),breaks = 100)

plot_data[team=="SVK",plot((imbalance_price-price)/0.14,market_bid-forecast)]
plot_data[team=="SVK",plot((imbalance_price-price)/0.14,market_bid-actual_mwh)]


### Revenue vs pinball loss

### Plot Pinball vs Revenue

## Strategic bidders:

inset <- ggplot(full_leaderboard[Pinball<65 & Revenue>77,],
                aes(x=Pinball,y=Revenue)) +
  geom_point() +
  scale_x_continuous(limits=c(20,65),breaks=c(20,65)) +
  scale_y_continuous(limits=c(77,90),breaks=c(80,90)) +
  custom_theme +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),#element_rect(fill = gray(0.9)),
        panel.background = element_rect(fill = gray(0.9,alpha = 0.5))) +
  guides(shape="none")

pinball_vs_rev <- ggplot(full_leaderboard[Pinball<40 & Revenue,],
                         aes(x=Pinball,y=Revenue)) +
  geom_point() +
  xlab("Pinball [MWh]") +
  ylab("Revenue [£m]") +
  custom_theme +
  inset_element(inset, 0.57, 0.55, 0.96, 0.96)

pinball_vs_rev

ggsave(filename = paste0("figs/pinball_vs_rev.",fig_format), pinball_vs_rev,
       width = 0.7*fig_size_in[1],height = fig_size_in[2],units = "in")

### Densities

top_teams <- trade_data[,sum(revenue),by=team][order(V1,decreasing = T)][1:10,team]
p_revvpinball <- merge(forecast_score[,.(dtm,team,pinball)],
                       trade_data[,.(dtm,team,revenue,actual_mwh)],by=c("dtm","team"),
                       all.y = T) %>%
  filter(team %in% top_teams) %>%
  mutate(team = factor(team, levels=top_teams)) %>%
  mutate(revenue_per_mwh = if_else(actual_mwh > 20, revenue / actual_mwh, NA_real_)) %>%
  group_by(team) %>% 
  mutate(binned_pinball = cut(x=pinball, breaks=c(0, 20, 40, 60, 80, 120, 300))) %>%
  ungroup() %>%
  tidyr::drop_na() %>%
  ggplot(aes(x=revenue_per_mwh, y=binned_pinball)) +
  facet_wrap(~team, nrow = 5) +
  geom_density_ridges(jittered_points = F, scale=1, alpha=0.4, 
                      point_shape = "|", point_size = 2,
                      position = position_points_jitter(height = 0)) +
  scale_y_discrete(expand = c(0, 0)) +     
  scale_x_continuous(expand = c(0, 0), limits = c(-100, 100)) +   
  coord_cartesian(clip = "off") +
  labs(
    x = "Revenue [£/MWh]",
    y = "Binned pinball loss [MWh]"
  ) +
  custom_theme
p_revvpinball
ggsave(filename = paste0("figs/revenue_vs_pinball.",fig_format), p_revvpinball,
       width = 8, height = 10, units = "in")

#### As above, but relative to day-ahead price
p_excess_revvpinball <- merge(forecast_score[,.(dtm,team,pinball)],
                              trade_data[,.(dtm,team,revenue,actual_mwh,price,imbalance_price)],by=c("dtm","team"),
                              all.y = T) %>%
  filter(team %in% top_teams_rev) %>%
  mutate(team = factor(team, levels=top_teams)) %>%
  mutate(spread = imbalance_price - price,
         trade_for_max_revenue = actual_mwh - spread/0.14,
         max_revenue = trade_for_max_revenue*price + (actual_mwh - trade_for_max_revenue) * (imbalance_price - 0.07*(actual_mwh - trade_for_max_revenue)),
         excess_revenue = revenue - max_revenue,
         excess_revenue_per_mwh = if_else(actual_mwh > 0, (excess_revenue / actual_mwh) , NA_real_)) %>%
  group_by(team) %>% 
  mutate(binned_pinball = cut(x=pinball, breaks=c(0, 20, 40, 60, 80, 120, 300))) %>%
  ungroup() %>%
  tidyr::drop_na() %>%
  ggplot(aes(x=excess_revenue_per_mwh, y=binned_pinball,height = after_stat(density))) +
  facet_wrap(~team, nrow = 2) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    quantile_lines = T,
    quantiles = 0.5) +
  scale_y_discrete(expand = c(0, 0)) +     
  scale_x_continuous(expand = c(0, 0), limits = c(-25, 0), breaks = c(0,-10,-20)) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Opportunity cost [£/MWh]",
    y = "Binned pinball loss [MWh]"
  ) +
  custom_theme +
  theme(strip.text.x = element_text(size = 10))

p_excess_revvpinball
ggsave(filename = paste0("figs/opportunitiy_cost_from_optimal_trade_vs_pinball_v2.",fig_format),
       p_excess_revvpinball,width = fig_size_in[1],height = fig_size_in[2], units = "in")
# ggsave(filename = paste0("figs/opportunitiy_cost_from_optimal_trade_vs_pinball.",fig_format),
#        p_excess_revvpinball,
#        width = 8, height = 10, units = "in")



### Relative change in pinball loss compared to change in revenue

n_teams <- 21
top_teams_fc <- forecast_score[,mean(pinball),by=team][order(V1,decreasing = F)][1:n_teams,team]
top_teams_fc <- factor(top_teams_fc, levels=top_teams_fc)

forecast_trade <- merge(forecast_data[,.(dtm, team, quantile, pinball)],
                        trade_data[,.(dtm, team, revenue)],by=c("dtm", "team"),
                        all.y = T) %>%
  filter(team %in% top_teams_fc) %>%
  group_by(team) %>%
  summarise(avg_pinball = mean(pinball, na.rm = T),
            revenue = sum(revenue, na.rm = T)) %>%
  ungroup()

worst_pinball <- forecast_trade %>%
  select(avg_pinball) %>%
  max(.)
worst_revenue <- forecast_trade %>%
  select(revenue) %>%
  min(.)

p_percent_change <- forecast_trade %>% 
  arrange(desc(avg_pinball)) %>%
  mutate(Pinball = (worst_pinball-avg_pinball) / worst_pinball * 100) %>%
  mutate(Revenue = (revenue - worst_revenue) / worst_revenue * 100) %>%
  tidyr::drop_na() %>%
  select(team, Pinball, Revenue) %>%
  tidyr::pivot_longer(!team, names_to = "Percentage change", values_to = "value") %>%
  ggplot(., aes(x=team, y=value, color=`Percentage change`, group=`Percentage change`)) +
  geom_line() +
  scale_x_discrete(limits = rev(levels(top_teams_fc[1:(n_teams-1)]))) +
  custom_theme +
  scale_color_brewer(palette = "Set1", name="Performance metric") +
  labs(x="Team [-]", y="Improvement [%]") +
  theme(axis.text.y = element_text(size=10),
        axis.text.x = element_text(angle=90,vjust = 0.5,
                                   hjust = 1, size=10),
        legend.title = element_blank())
p_percent_change
ggsave(filename = paste0("figs/percent_change.",fig_format), p_percent_change,
       width = fig_size_in[1],height = fig_size_in[2],units = "in")

### Capture ratio as a function of time of day

p_capture_ratio <- trade_data %>%
  select(dtm, actual_mwh, market_bid, revenue, team, price, imbalance_price) %>%
  filter(team %in% top_teams_rev) %>%
  mutate(team = factor(team, levels=top_teams)) %>%
  mutate(spread = imbalance_price - price,
         trade_for_max_revenue = actual_mwh - spread/0.14,
         max_revenue = trade_for_max_revenue*price + (actual_mwh - trade_for_max_revenue) * (imbalance_price - 0.07*(actual_mwh - trade_for_max_revenue)),
         capture_ratio = if_else(revenue / max_revenue > -2, revenue / max_revenue, -2),
         hod = strftime(dtm, format="%H:%M")) %>%
  tidyr::drop_na() %>%
  group_by(team, hod) %>%
  summarise(median_capture_ratio = median(capture_ratio)) %>%
  ungroup() %>%
  ggplot(., aes(x=hod, y=median_capture_ratio, color=factor(team), group=team, shape = factor(team))) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = color_pal_top10, name="Team") +
  scale_shape_discrete(name="Team") +
  scale_x_discrete(breaks=~ .x[seq(1, length(.x), 8)]) +
  custom_theme +
  labs(x="Time of day [30 min]", y="Median capture ratio [-]")

p_capture_ratio

ggsave(filename = paste0("figs/capture_ratio.",fig_format), p_capture_ratio,
       width = fig_size_in[1],height = fig_size_in[2],units = "in")

### Price spreads

trade_data[,dtm_GB:=copy(dtm)]
attr(trade_data$dtm_GB, "tzone") <- "Europe/London"
trade_data[,sp:=1+2*difftime(dtm_GB,as.POSIXct(as.Date(dtm_GB)),units = "hours")]

p_spread <- trade_data %>%
  filter(team == "SVK") %>%
  mutate(Spread = imbalance_price - price,
         # tod = strftime(dtm_GB, format="%H:%M")) %>%
         tod = as.factor(sp)) %>%
  # select(tod, price, imbalance_price,Spread) %>%
  # rename(`Day-ahead price`=price, `Imbalance price`=imbalance_price) %>%
  select(tod, price,Spread) %>%
  rename(`Day-ahead price`=price) %>%
  tidyr::pivot_longer(!tod, names_to = "price", values_to = "value") %>%
  ggplot(., aes(x=tod, y=value)) +
  geom_boxplot() +
  facet_wrap(~price, nrow = 1, scales = "free_y") +
  scale_x_discrete(breaks=~ .x[seq(0, length(.x), 12)]) +
  labs(y="Price [£/MWh]", x="Time of day [settlement period]") +
  custom_theme

p_spread

ggsave(filename = paste0("figs/price_spread_boxplot.",fig_format),
       p_spread,
       device = cairo_pdf,
       width = fig_size_in[1], height = fig_size_in[2], units = "in")

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

p_revv_marketbids

# ggsave(filename = paste0("figs/revenue_vs_marketbids.",fig_format), p_revv_marketbids,
# width = 8, height = 10, units = "in")

### Revenue from bidding p50 revenue vs strategic bidding (i.e., participant's actual bids)

forecast_trade <- merge(forecast_data[,.(dtm, team, quantile, forecast, actual_mwh, pinball)],
                        trade_data[,.(dtm, team, market_bid, imbalance_price, price, revenue)],
                        by=c("dtm", "team"),
                        all.y = T)

forecast_trade[,Pinball:=mean(pinball,na.rm=T),by="team"]

forecast_trade[,unique_forecasts:=length(unique(forecast)),
               by=c("dtm","team")]

forecast_trade[,bid_as_forecast := forecast*price + (actual_mwh - forecast) * (imbalance_price - 0.07*(actual_mwh - forecast))]

plot_data <- forecast_trade[quantile==50,.(Revenue=sum(revenue)/1e6,
                                           `Revenue (q50)`=sum(bid_as_forecast)/1e6,
                                           Pinball=Pinball[1]),by="team"]

plot_data[,Gain:=Revenue - `Revenue (q50)`]
plot_data[order(Gain,decreasing = T)][Revenue>81]

ggplot(plot_data[Pinball<40 & Revenue>75],
       aes(x=Pinball,ymin=`Revenue (q50)`,ymax=Revenue)) +
  geom_errorbar() +
  geom_point(aes(y=Revenue),shape=15,color="green") +
  geom_point(aes(y=`Revenue (q50)`),shape=3,color="red") +
  custom_theme


### Table with trade statistics


trade_stats <- trade_data %>% 
  filter(team %in% top_teams_rev) %>%
  mutate(team = factor(team, levels=top_teams_rev)) %>%
  group_by(team) %>%
  summarise(#`Total revenue` = sum(revenue),
    `Win rate [-]` = mean(as.integer(revenue > 0)),
    `Relative bid volume [-]` = sum(market_bid) / sum(actual_mwh),
    `Trade VWAP [GBP/MWh]` = sum(revenue) / sum(market_bid),
    `Production VWAP [GBP/MWh]` = sum(revenue) / sum(actual_mwh),
    `Sharpe ratio [-]` = mean(revenue) / sd(revenue),
    `Sortino ratio [-]` = mean(revenue) / sd(if_else(revenue < 0, revenue, NA_real_), na.rm=TRUE),
    `Profit factor [-]` = sum(if_else(revenue > 0, revenue, NA_real_), na.rm = T) / sum(if_else(revenue < 0, revenue, NA_real_), na.rm = T),
    `5\\% VaR [GBP]` = quantile(revenue, probs = 0.05),
    `5\\% ES [GBP]` = mean(if_else(revenue <= `5\\% VaR [GBP]`, revenue, NA_real_), na.rm = T)) %>%
  data.table(.)

latex_table <- trade_stats %>% 
  filter(team %in% top_teams_rev) %>%
  mutate(team = factor(team, levels=top_teams_rev)) %>%
  xtable(.)
print(latex_table, type = "latex", include.rownames = FALSE,
      sanitize.text.function = identity,
      add.to.row = list(pos = list(0), command = '\\resizebox{\\textwidth}{!}{')
)


## last 2 months

trade_stats_2 <- trade_data %>% 
  filter(dtm>="2024-02-27 00:00:00") %>%
  # filter(team %in% top_teams_rev) %>%
  # mutate(team = factor(team, levels=top_teams_rev)) %>%
  group_by(team) %>%
  summarise(#`Total revenue` = sum(revenue),
    `Win rate [-]` = mean(as.integer(revenue > 0)),
    `Relative bid volume [-]` = sum(market_bid) / sum(actual_mwh),
    `Trade VWAP [GBP/MWh]` = sum(revenue) / sum(market_bid),
    `Production VWAP [GBP/MWh]` = sum(revenue) / sum(actual_mwh),
    `Sharpe ratio [-]` = mean(revenue) / sd(revenue),
    `Sortino ratio [-]` = mean(revenue) / sd(if_else(revenue < 0, revenue, NA_real_), na.rm=TRUE),
    `Profit factor [-]` = sum(if_else(revenue > 0, revenue, NA_real_), na.rm = T) / sum(if_else(revenue < 0, revenue, NA_real_), na.rm = T),
    `5\\% VaR [GBP]` = quantile(revenue, probs = 0.05),
    `5\\% ES [GBP]` = mean(if_else(revenue <= `5\\% VaR [GBP]`, revenue, NA_real_), na.rm = T)) %>%
  data.table(.)

trade_stats_2[`5\\% VaR [GBP]`>-1500,# & `Production VWAP [GBP/MWh]`>53,
              plot(`5\\% VaR [GBP]`,`Production VWAP [GBP/MWh]`)]



inset <- ggplot(trade_stats_2[`Production VWAP [GBP/MWh]`>45],
                aes(x=`5\\% VaR [GBP]`,y=`Production VWAP [GBP/MWh]`)) +
  geom_point() +
  scale_x_continuous(limits=c(-5000,0),breaks=c(-5000,0)) +
  scale_y_continuous(limits=c(48,55),breaks=c(48,55)) +
  custom_theme +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),#element_rect(fill = gray(0.9)),
        panel.background = element_rect(fill = gray(0.9,alpha = 0.5))) +
  guides(shape="none")

inset

Rev_vs_Risk <- ggplot(trade_stats_2[`5\\% VaR [GBP]`>-1500],#`Production VWAP [GBP/MWh]`>53],
                      aes(x=`5\\% VaR [GBP]`,y=`Production VWAP [GBP/MWh]`)) +
  geom_point()+
  geom_text(hjust=1.1, vjust=1.05,
            aes(label=team),
            data = trade_stats_2[team %in% c("SVK","Ihubex")])+
  geom_text(hjust=0.5, vjust=-0.5,
            aes(label=team),
            data = trade_stats_2[team %in% c("RE-Cast")])+
  ylim(c(50,55))+
  xlim(c(-1500,0)) +
  xlab("5% VaR [£]") +
  ylab("Production VWAP [£/MWh]") +
  custom_theme +
  inset_element(inset, 0.02, 0.54, 0.40, 0.96)
  

Rev_vs_Risk

ggsave(filename = paste0("figs/Rev_vs_Risk.",fig_format), Rev_vs_Risk,
       width = 0.7*fig_size_in[1],height = fig_size_in[2],units = "in")



