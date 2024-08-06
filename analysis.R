require(data.table)
require(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
require(ggplot2)

## Fig format
fig_format <- "pdf"
fig_size_in <- c(8,4.5)
custom_theme <- theme_bw() + theme(text=element_text(size=20,family="serif"))

## load data

### Trades, prices and revenue, with missed submissions filled
trade_data <- fread("data/trades.csv")

### Pinball score by timestamp with missed submissions filled
forecast_score <- fread("data/pinball.csv")

### Raw forecast submissions, outturn and pinball, missed submissions not filled
forecast_data <- fread("data/forecasts.csv")



## Plots


### Revenu evolution:
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
  custom_theme

p2

ggsave(filename = paste0("figs/pinball_top10.",fig_format), p2,
       width = fig_size_in[1],height = fig_size_in[2],units = "in")
