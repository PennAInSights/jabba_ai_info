library(ggplot2)
library(tidyr)

load_jabba_server_logs <- function(filenames) {
    rdat=NULL
    for (f in filenames) {
        rdat = rbind(rdat, read.csv(f, sep='-', header=F))
    }

    splits = do.call(rbind, strsplit(rdat[,3], ' '))

    day = as.integer(splits[,1])
    time = as.character(splits[,2])

    date = paste0(rdat[,1], '-', sprintf('%02d', rdat[,2]), '-', sprintf('%02d',day))
    stamp = paste(date, time)
    dat = data.frame(timestamp=strptime(stamp, "%Y-%m-%d %H:%M:%OS"))
    dat$timestamp_string = stamp
    dat$caller=rdat[,4]
    dat$type=rdat[,5]
    dat$message=paste0(rdat[,6], '-', rdat[,7])

    return(dat)

}

get_cstores <- function(dat) {
    cstores = which(grepl('C-STORE', dat$message))
    store_req = dat$message[cstores]
    store_time = dat$timestamp[cstores]
    store_string = dat$timestamp_string[cstores]

    inf = do.call(rbind, strsplit(store_req, ':'))[,2]
    inf = do.call(rbind, strsplit(trimws(inf), ","))
    odat = data.frame(timestamp=store_time, accession=inf[,1], study_uid=inf[,2], series_uid=inf[,3], instance_uid=inf[,4])
    return(odat)
}

get_accession_windows <- function(dat) {
    stores = get_cstores(dat)
    mint = str |> group_by(accession) |> slice(which.min(timestamp)) |> select(timestamp, accession) |> rename(start=timestamp)
    maxt = str |> group_by(accession) |> slice(which.max(timestamp)) |> select(timestamp, accession) |> rename(end=timestamp)
    win = mint |> left_join(maxt)   
    win$transfer_time = win$end - win$start
    win$dow = as.POSIXlt( win$start )$wday
    win$dow = factor(win$dow, levels=0:6, labels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
    return(win)
}

plot_server_log <- function(filename, date=NA) {
    dat <- read.csv(filename)
    if (!is.na(date)) {
        dat <- dat[dat$Date == date, ]
    }



}

accession_transfer_windows <- function(dat) {

    dat$timestamp <- strptime(paste(dat$date, dat$time), "%Y-%m-%d %H:%M:%OS")

    dat$accession <- as.character(dat$accession)
    acc_list <- unique(dat$accession)
    odat <- data.frame()


    for (i in seq_len(length(acc_list))) {
        print(acc_list[i])
        idx <- which(dat$accession == acc_list[i])
        if (length(idx) > 0) {
            first <- min(dat$timestamp[idx])
            last <- max(dat$timestamp[idx])
            acc = acc_list[i]
            cpt = dat$cpt[idx[1]]
            study_uid = dat$study_uid[idx[1]]
            institution = dat$institution[idx[1]]
            size = dat$size[idx[1]]
            ip = dat$ip[idx[1]]
            odat <- rbind(odat, data.frame(accession = acc,
                cpt = cpt, uid = study_uid,
                start = first, end = last, duration = last - first,
                institution = institution, ip = ip,
                size = size))


            #odat <- rbind(odat, data.frame(accession = acc_list[i],
            #    cpt = dat$cpt[idx[1]], uid = dat$study_uid[idx[1]],
            #    start = first, end = last, duration = last - first,
            #    institution = dat$institution[idx[1]],
            #    size = dat$size[idx[1]]))
        } else {
            print("No accession found")
            #odat <- rbind(odat, data.frame(accession = acc_list[i],
            #    cpt = NA, uid = dat$study_uid[idx[1]],
            #    start = NA, end = NA, duration = NA, 
            #    size = NA, institution = NA))
        }
    }


    odat <- gather(odat, condition, measurement, start:end)
    odat$accession <- factor(odat$accession)
    odat$timestamp <- strptime(odat$measurement, "%Y-%m-%d %H:%M:%OS")
    odat$day <- factor(strftime(odat$timestamp, "%Y-%m-%d"))
    odat$time <- strftime(odat$timestamp, "%H:%M:%S")
    hour = as.numeric(strftime(odat$timestamp, "%H"))
    minute = as.numeric(strftime(odat$timestamp, "%M"))
    second = as.numeric(strftime(odat$timestamp, "%S"))
    odat$seconds = hour * 3600 + minute * 60 + second
    return(odat)

}

get_transfer_data <- function(filenames) {
    dat = load_jabba_server_logs(filenames)
    win = get_accession_windows(dat)
    return(win)
}

get_transfer_plots <- function(win, dir=NA) {

    win$transfer_time_min = as.numeric(win$transfer_time / 60)
    days = win |> group_by(dow) |> count(dow)
    
    startlt = as.POSIXlt( win$start )
    #endlt = as.POSIXlt( win$end )

    # Get start and stop times w/o date
    win$start_hour = rep(0, nrow(win))
    win$end_hour = rep(0, nrow(win))
    for (i in 1:nrow(win)) {
        win$start_hour[i] = startlt[i]$hour + startlt[i]$min/60 + startlt[i]$sec/60/60
        win$end_hour[i] = endlt[i]$hour + endlt[i]$min/60 + endlt[i]$sec/60/60
        #win$end_hour_full[i] = win$start_hour[i] + win$transfer_time_min/60
    }
    win = win[win$start_hour < win$end_hour,]

    toolong = length(which(win$transfer_time_min > 10))
    toolongstr = paste(toolong, 'studies exceeded 10 min')

    # All transfers by Day of Week
    p1 = ggplot(win[win$transfer_time_min < 10,] , aes(x=dow, group=dow, y=transfer_time_min)) + geom_violin() + geom_boxplot(width=0.2) #+ geom_jitter(width=0.2, alpha=0.2)
    p1 = p1 + ylab("Transfer time (min)") + xlab("Day of Week")
    p1 = p1 + labs(title="Time to receive studies", caption=toolongstr) 

    # studies transfered by day of week
    p2 = ggplot(win, aes(x=dow)) + geom_bar() + xlab("Day of Week") + ylab("Number of Studies")
    p2 = p2 + labs(title="Number of studies received")

    # Transfer times on weekdays
    #win_weekday = win |> filter(dow %in% c("Mon","Tue","Wed","Thu","Fri"))
    #win_weekday = win_weekday[win_weekday$cross_day==0,]
    p3 = ggplot(win[win$transfer_time_min < 10,], aes(xmin=start_hour, xmax=end_hour, ymin=0, ymax=transfer_time_min, group=accession, fill=accession, alpha=0)) + geom_rect() + theme(legend.position="none")
    p3 = p3 + xlab("Time of Day") + ylab("Transfer time (min)")
    p3 = p3 + labs(title="Studies received by time of day", caption=toolongstr)
    p3 = p3 + scale_x_continuous( breaks=seq(0,24,by=1))

    return(data.frame(ntransfers=p1, dow=p2, timeofday=p3))

}

get_study_plots <- function(accessions, dir)
 {

    for (a in accessions) {
        accdir=paste(dir,'/', a)
        if (dir.exists(accdir)) {

        }
    }
    
 }
# ggplot(wins, aes(x=seconds, y=size, group=uid, colour=cpt)) + geom_line(size=2) + facet_grid(rows=vars(day)) + scale_x_continuous(n.breaks=12, limits=c(0,86400))
# ggplot( wins, aes(x=as.numeric(duration) )) + geom_histogram(binwidth=30) + scale_x_continuous(breaks=seq(0,max(as.numeric(wins$duration)),30))