library(ggplot2); library(reshape2); library(xts); library(scales)
# WALLACE
# source("/users/nick/documents/daytrader/funs/fin.R")
# source("/users/nick/documents/daytrader/funs/multiplot.R")
# setwd("/users/nick/documents/daytrader/reporter")
# filename <- "/SierraChart/TradeActivityLogs/SIM_TradesList.txt"

# HUXLEY
source("/users/ncoutrakon/daytrader/funs/fin.R")
source("/users/ncoutrakon/daytrader/funs/multiplot.R")
setwd("/users/ncoutrakon/daytrader/reporter")
filename <- "/users/ncoutrakon/.wine/drive_c/SierraChart/TradeActivityLogs/SIM_TradesList.txt"

options(stringsAsFactors = F)



##################################### MUNGE #############################################
# Reads and formats trades file
trades <- read.table(filename, sep = "\t", header = T)

trades <- trades[, c(1:10, 13:15)]
names(trades)[c(9, 10, 12, 13)] <- c("Runup", "Drawdown", "Profit.Loss", "Cum.P.L")
trades$Profitable <- (trades$Profit.Loss >= 0)
trades$Index <- row.names(trades)


# Convert Total.Efficiency columns to decimals
trades[, 11] <- as.numeric(substr(trades[, 11], 1, nchar(trades[, 11])-1))/100

# Converts Entry.DateTime and Exit.DateTime to POSIXct
trades[, 3] <- as.POSIXct(trades[, 3])
trades[, 8] <- as.POSIXct(trades[, 8])
trades$Time.Length <- as.numeric(trades$Exit.DateTime - trades$Entry.DateTime)
trades <- trades[order(trades$Exit.DateTime),]



# splits trade file into list by days, collects daily stats for each day
per_day <- split(trades, as.Date(trades$Entry.DateTime))
day_stats <- do.call(rbind, lapply(per_day, get_stats))
day_stats$Date <- as.Date(day_stats$Date)

# Pick and setup the dataframe to display Cumulative PnL
which_day <- length(per_day)
cumpnl_df <- per_day[[which_day]]
trade_date <- unique(as.Date(cumpnl_df$Entry.DateTime))
tmp_row <-cumpnl_df[1,]
tmp_row$Profit.Loss <- 0
cumpnl_df <- rbind(tmp_row, cumpnl_df)
cumpnl_df$Cum.P.L <- cumsum(cumpnl_df$Profit.Loss)
cumpnl_df <- xts(cumpnl_df$Cum.P.L, cumpnl_df$Exit.DateTime)
cumpnl_df <- fill_in(cumpnl_df)
cumpnl_df[is.na(cumpnl_df), ] <- 0
cumpnl_df <- data.frame(index(cumpnl_df),  cumpnl_df[,1])
names(cumpnl_df) <- c("Timestamp", "Cum.Profit.Loss")
cumpnl_df$Exit <- c(0, (diff(cumpnl_df$Cum.Profit.Loss) != 0)*1)



################################ PLOTS #########################################
# Plots daily PnL histogram and title with  which_day EOD stats
daily_stat <- day_stats[dim(day_stats)[1], 2:7]
daily_pl_title <- paste(names(daily_stat), daily_stat, sep = ': ',collapse = "   ")
day_pnl_pl <- ggplot(day_stats) + geom_bar(aes(x = Date, y = Total.PnL), stat = "identity")  +
        scale_y_continuous(minor_breaks = seq(-500, 500, 5), breaks = seq(-500, 500, 10)) +
        theme(axis.title.x = element_blank(), plot.title = element_text(size = 10)) +  ggtitle(daily_pl_title) +
        geom_hline(yintercept = 0, color = "red") +
        scale_x_date(date_breaks = "1 day", date_minor_breaks = "1 day", date_labels = "%m-%d")

# Plots average daily Runup, Drawdown, and PnL per trade for each day
day_stats$Index <- factor(index(day_stats), levels = index(day_stats))
day_stats_pl <- ggplot(day_stats) +
        geom_bar(aes(x = Date, y =Profit.Loss, fill = Profitable), stat = "identity") +
        geom_bar(aes(x = Date, y = Drawdown, fill = Profitable), width = .1, stat = "identity") +
        geom_bar(aes(x = Date, y = Runup, fill = Profitable), width = .1, stat = "identity") +
        theme(legend.position="none", axis.title.x = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
        scale_y_continuous(minor_breaks = seq(-20 , 50, 2.5), breaks = seq(-20, 50, 5)) +
        geom_hline(yintercept = 0, color = "red")



# Plots which_day cumulative PnL vs Time

cumpnl_pl <- ggplot(cumpnl_df, aes(x = Timestamp, y = Cum.Profit.Loss)) +
        geom_line() + xlab(names(per_day)[which_day]) +
        scale_y_continuous(minor_breaks = seq(-200, 500, 5), breaks = seq(-200, 500, 10)) +
        geom_hline(yintercept = 0, color = "red") +
        geom_label(data = cumpnl_df[cumpnl_df$Exit !=0, ],
                   aes(x = Timestamp, y = Cum.Profit.Loss, label = format(Timestamp, "%H:%M:%S")),
                   fill = "sky blue", color = "white", size = 2) + expand_limits(y=-10) + expand_limits(y=10) +
        scale_x_datetime(breaks = date_breaks("1 hour"), labels = date_format("%Hh", tz = "America/Chicago"))




# Plots Runup, Drawdown, and PnL the which_day trades
trade_stats_df <- trades[as.Date(trades$Entry.DateTime) == trade_date,]
trade_delim <- (diff(trade_stats_df$Entry.DateTime) > 1)
trade_delim <- trade_delim*index(trade_delim) +.5
trade_delim <- sort(unique(trade_delim))[-1]
trade_stats_df$Index <- factor(trade_stats_df$Index, levels = trade_stats_df$Index)
trade_stats_pl <- ggplot(trade_stats_df) +
        geom_bar(aes(x = Index, y =Profit.Loss, fill = Profitable), stat = "identity") +
        geom_bar(aes(x = Index, y = Drawdown, fill = Profitable), width = .1, stat = "identity") +
        geom_bar(aes(x = Index, y = Runup, fill = Profitable), width = .1, stat = "identity") +
        theme(legend.position="none", axis.title.x = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
        scale_y_continuous(minor_breaks = seq(-20 , 50, 2.5), breaks = seq(-20, 50, 5)) +
        geom_vline(xintercept = trade_delim) + geom_hline(yintercept = 0, color = "red")



############################ OUTPUT ##########################################
pdf(paste0("reports/", trade_date, ".pdf"), width = 8.5, height = 11)
multiplot(day_pnl_pl, day_stats_pl, cumpnl_pl, trade_stats_pl, cols = 1)
dev.off()

# Opens the PDF for viewing
system2('open', args = paste0("reports/", trade_date, ".pdf"), wait = F)

