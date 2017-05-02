library(ggplot2); library(reshape2); library(xts); library(scales)
# WALLACE
# source("/users/nick/documents/daytrader/funs/fin.R")
# source("/users/nick/documents/daytrader/funs/multiplot.R")
# filename <- "/SierraChart/TradeActivityLogs/SIM_TradesList.txt"

# HUXLEY
source("/users/ncoutrakon/daytrader/funs/fin.R")
source("/users/ncoutrakon/daytrader/funs/multiplot.R")
setwd("/users/ncoutrakon/daytrader/reporter")
filename <- "/users/ncoutrakon/.wine/drive_c/SierraChart/TradeActivityLogs/SIM_TradesList.txt"

options(stringsAsFactors = F)



to_daily_cum <- function(x){
        t_date <- as.Date(index(x[1]))
        tmp <- x[1, ]
        tmp[1, ] <- 0
        index(tmp) <- index(tmp) - 1
        x <- rbind.xts(tmp, x)
        x <- xts(cumsum(x[, 'Profit.Loss']), index(x) + (Sys.Date() - t_date))
        x <- fill_in(x)

        names(x) <- as.character(t_date)
        return(x)
}

what_if <- function(trade_df, runup){
        trade_df$Profit.Loss <- trade_df$Profit.Loss*(trade_df$Runup < runup) + trade_df$Runup*(trade_df$Runup > runup)
        return(trade_df)
}

##################################### MUNGE #############################################
# Reads and formats trades file
trades <- read.table(filename, sep = "\t", header = T)
trades <- trades[, c(1:10, 13:15)]
names(trades)[c(9, 10, 12, 13)] <- c("Runup", "Drawdown", "Profit.Loss", "Cum.P.L")


# Converts Entry.DateTime and Exit.DateTime to POSIXct
trades[, 3] <- as.POSIXct(trades[, 3])
trades[, 8] <- as.POSIXct(trades[, 8])
trades$Time.Length <- as.numeric(trades$Exit.DateTime - trades$Entry.DateTime)
trades <- trades[order(trades$Exit.DateTime),]


# Convert Total.Efficiency columns to decimals
trades[, 11] <- as.numeric(substr(trades[, 13], 1, nchar(trades[, 13])-1))/100
trades$Profitable <- (trades$Profit.Loss > 0)
trades$Index <- factor(row.names(trades))
last_date <- as.Date(trades[dim(trades)[1], 'Exit.DateTime'])
last_week_date <- last_date - as.numeric(format(last_date, "%w"))
trades <- trades[as.Date(trades$Exit.DateTime) > last_week_date, ]



######################## PLOTS ###############################
# Overlapped Daily CumPnL
daily_stat <- get_stats(trades)[1, 2:8]
pl_title <- paste(names(daily_stat), daily_stat, sep = ': ',collapse = "   ")

trades_cum <- as.xts(trades[, c(4:7, 9:13)], order.by = trades$Exit.DateTime)
by_day_cum <- suppressWarnings(split(trades_cum, as.Date(index(trades_cum))))

trades_cum <- do.call(cbind, lapply(by_day_cum, to_daily_cum))
trades_cum <- data.frame(Timestamp = index(trades_cum), trades_cum)
names(trades_cum) <- c("Timestamp", substr(names(trades_cum)[-1], 2, nchar(names(trades_cum)[-1])))
trades_cum <- melt(trades_cum, id.vars = "Timestamp")
names(trades_cum) <- c("Timestamp", "Date", "Ticks")
cum_pl <- ggplot(trades_cum) + geom_line(aes(x = Timestamp, y = Ticks, color = Date)) +
        scale_x_datetime(breaks = date_breaks("1 hour"), labels = date_format("%Hh", tz = "America/Chicago")) +
        xlab("") + scale_y_continuous(breaks = seq(-100, 100, 10)) + geom_hline(yintercept = 0, color = "red") +
        ggtitle(pl_title) + theme(legend.position = c(.1, .9), plot.title = element_text(size = 10)) +
        expand_limits(y=-10) + expand_limits(y=10)



# total runups
day_delim <- (diff(as.Date(trades$Entry.DateTime)) != 0)
day_delim <- day_delim*index(day_delim) +.5
day_delim <- sort(unique(day_delim))[-1]


stats_pl <- ggplot(trades) +
        geom_bar(aes(x = Index, y =Profit.Loss, fill = Profitable), stat = "identity") +
        geom_bar(aes(x = Index, y = Drawdown, fill = Profitable), width = .1, stat = "identity") +
        geom_bar(aes(x = Index, y = Runup, fill = Profitable), width = .1, stat = "identity") +
        theme(legend.position="none", axis.title.x = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
        scale_y_continuous(minor_breaks = seq(-100 , 100, 2.5), breaks = seq(-20, 50, 5)) +
         geom_hline(yintercept = 0) + geom_vline(xintercept = day_delim) + ylab("")






############################ OUTPUT ##########################################
out_name <- paste0("reports/", last_week_date, "_W.pdf")
pdf(out_name, width = 8.5, height = 11)
multiplot(cum_pl, stats_pl, cols = 1)
dev.off()

# Opens the PDF for viewing
system2('open', args = out_name, wait = F)
