summarise_ts <- function(dataframe, by_reg, by_dis, timescale, IBCM_break,
                         sep_col="Suspect", sep_labels=c("Suspect", "Healthy")){

  # Set breaks for summarising numbers - run from start date to end date by month
  breaks <- seq(start_date, ceiling_date(end_date,'month'),"month")-1

  # Collect vector of years for months of data - ready for data from 2019 etc.
  yrs <- substr(breaks[2:length(breaks)], 1,4)

  # Collect vector of months
  mnths <- as.numeric(substr(breaks[2:length(breaks)], 6,7))

  if(by_dis==TRUE){

    # Set vector of regions
    regs <- sort(unique(dataframe$LOCATION_OF_EVENT_REGION))

    n=1

    for(i in regs){

      # Subset data for the i'th region
      dataframe_sub <- dataframe[which(dataframe$LOCATION_OF_EVENT_REGION == i),]

      # Collect vector of districts from region
      dis <- sort(unique(dataframe_sub$LOCATION_OF_EVENT_DISTRICT))

      # Take labels
      sus <- sep_labels[1]
      healt <- sep_labels[2]

      # Subset data for suspect and healthy
      suspect_df <- dataframe[which(dataframe[[sep_col]]==sus),]
      healthy_df <- dataframe[which(dataframe[[sep_col]]==healt),]

      for(j in dis){

        # Use hist function to summarise data using breaks
        ts_month_suspect <- hist(suspect_df$Date_proxy[which(suspect_df$LOCATION_OF_EVENT_DISTRICT==j)], plot=T, breaks=breaks, freq=T)
        ts_month_healthy <- hist(healthy_df$Date_proxy[which(healthy_df$LOCATION_OF_EVENT_DISTRICT==j)], plot=T, breaks=breaks, freq=T)
        #ts_month_nodata <- hist(nodata_df$Date_proxy[which(nodata_df$LOCATION_OF_EVENT_DISTRICT==j)], plot=F, breaks=breaks, freq=T)

        # Combine summaries from suspect and healthy animals in a new dataframe
        month_combined <- data.frame(year=rep(yrs, 2),
                                     month=rep(mnths, 2),
                                     month_n=rep(1:length(ts_month_suspect$counts), 2),
                                     count=c(ts_month_suspect$counts, ts_month_healthy$counts),
                                     status=c(rep("Suspect", length(ts_month_suspect$counts)), rep("Healthy", length(ts_month_healthy$counts))))

        month_combined$region <- i
        month_combined$district <- j

        if(n==1){
          summary_df <- month_combined
        } else if(n > 1){
          summary_df <- rbind(summary_df, month_combined)
        }

        # Add 1 to the count
        n = n+1

      }

    }
  }

  if(by_reg==TRUE){

    # Set vector of regions
    regs <- sort(unique(dataframe$LOCATION_OF_EVENT_REGION))

    n=1

    for(i in regs){

      # Subset data for the i'th region
      dataframe_sub <- dataframe[which(dataframe$LOCATION_OF_EVENT_REGION == i),]

      sus <- sep_labels[1]
      healt <- sep_labels[2]

      # Subset data for suspect and healthy
      suspect_df <- dataframe_sub[which(dataframe_sub[[sep_col]]==sus),]
      healthy_df <- dataframe_sub[which(dataframe_sub[[sep_col]]==healt),]
      #nodata_df <- dataframe_sub[which(dataframe_sub$Suspect=="No data"),]

      # Use hist function to summarise data using breaks
      ts_month_suspect <- hist(suspect_df$Date_proxy, plot=F, breaks=breaks, freq=T)
      ts_month_healthy <- hist(healthy_df$Date_proxy, plot=F, breaks=breaks, freq=T)
      #ts_month_nodata <- hist(nodata_df$Date_proxy[which(nodata_df$LOCATION_OF_EVENT_DISTRICT==j)], plot=F, breaks=breaks, freq=T)

      # Combine summaries from suspect and healthy animals in a new dataframe
      month_combined <- data.frame(year=rep(yrs, 2),
                                   month=rep(mnths, 2),
                                   month_n=rep(1:length(ts_month_suspect$counts), 2),
                                   count=c(ts_month_suspect$counts, ts_month_healthy$counts),
                                   status=c(rep(sus, length(ts_month_suspect$counts)), rep(healt, length(ts_month_healthy$counts))))

      month_combined$region <- i

      if(n==1){
        summary_df <- month_combined
      } else if(n > 1){
        summary_df <- rbind(summary_df, month_combined)
      }

      # Add 1 to the count
      n = n+1

    }
  }

  if(by_dis==FALSE & by_reg==FALSE){

    # Take labels
    sus <- sep_labels[1]
    healt <- sep_labels[2]

    # Subset data for suspect and healthy
    suspect_df <- dataframe[which(dataframe[[sep_col]]==sus),]
    healthy_df <- dataframe[which(dataframe[[sep_col]]==healt),]

    # Use hist function to summarise data using breaks
    ts_month_suspect <- hist(suspect_df$Date_proxy, plot=T, breaks=breaks, freq=T)
    ts_month_healthy <- hist(healthy_df$Date_proxy, plot=F, breaks=breaks, freq=T)
    #ts_month_nodata <- hist(nodata_df$Date_proxy[which(nodata_df$LOCATION_OF_EVENT_DISTRICT==j)], plot=F, breaks=breaks, freq=T)

    # Combine summaries from suspect and healthy animals in a new dataframe
    month_combined <- data.frame(year=rep(yrs, 2),
                                 month=rep(mnths, 2),
                                 month_n=rep(1:length(ts_month_suspect$counts), 2),
                                 count=c(ts_month_suspect$counts, ts_month_healthy$counts),
                                 status=c(rep(sus, length(ts_month_suspect$counts)), rep(healt, length(ts_month_healthy$counts))))

    month_combined$region <- sort(unique(dataframe$LOCATION_OF_EVENT_REGION))


    summary_df <- month_combined

  }

  # Add column giving month-year for plot x-axis labels
  summary_df$plot_label <- paste0(month.abb[summary_df$month], " ", summary_df$year)

  # Set factor levels for suspect column
  summary_df$status <- factor(summary_df$status, levels=c(healt, sus))

  # Add column for IBCM present
  summary_df$IBCM <- "No"
  summary_df$IBCM[which(summary_df$month_n>=IBCM_break)] <- "Yes"

  return(summary_df)
}