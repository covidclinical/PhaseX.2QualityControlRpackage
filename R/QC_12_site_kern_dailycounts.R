##########################
##########  DailyCounts
##########################
err_report_dailycounts_site=function(dat.DailyCounts,site.nm){
  err.label=
    c("cumulative_pts_all is not largest in last date",
      "cumulative_pts_severe is not largest in last date",
      "cumulative_pts_dead is not largest in last date",
      "cumulative_pts_all < cumulative_pts_severe",
      "cumulative_pts_icu < cumulative_pts_severe_icu",
      "cumulative_pts_dead < cumulative_pts_severe_dead",
      "pts_in_hosp_on_this_date < pts_severe_in_hosp_on_this_date",
      "pts_in_icu_on_this_date < pts_severe_in_icu_on_this_date",
      "The cumulative values are not increasing",
      "negative patient counts"
    )

  err.report.all=NULL
  err=NULL

  dat.site=dat.DailyCounts
  colnames(dat.site)=tolower(colnames(dat.site))

    dat.site=dat.site[order(as.Date(as.character(dat.site$calendar_date),format='%Y-%m-%d')),]

    # check that cumulative_pts_all is the largest at last date
    err=dim(dat.site)[1]%in%which(dat.site[,"cumulative_pts_all"]==max(dat.site[,"cumulative_pts_all"]))!=1

    # check that cumulative_pts_all is the largest at last date
    err=c(err, dim(dat.site)[1]%in%which(dat.site[,"cumulative_pts_severe"]==max(dat.site[,"cumulative_pts_severe"]))!=1)

    # check that cumulative_pts_dead is the largest at last date
    err=c(err, dim(dat.site)[1]%in%which(dat.site[,"cumulative_pts_dead"]==max(dat.site[,"cumulative_pts_dead"]))!=1)

    # check that cumulative_pts_all > cumulative_pts_severe
    id.nomiss=which(dat.site[,"cumulative_pts_all"]>0 & dat.site[,"cumulative_pts_severe"]>0)
    id3.1=which(dat.site[id.nomiss,"cumulative_pts_all"] < dat.site[id.nomiss,"cumulative_pts_severe"])
    err=c(err,length(id3.1)>1)

    # check that cumulative_pts_icu > cumulative_pts_severe_icu
    id.nomiss=which(dat.site[,"cumulative_pts_icu"]>0 & dat.site[,"cumulative_pts_severe_icu"]>0)
    id3.2=which(dat.site[id.nomiss,"cumulative_pts_icu"] < dat.site[id.nomiss,"cumulative_pts_severe_icu"])
    err=c(err,length(id3.2)>1)

    # check that cumulative_pts_dead > cumulative_pts_severe_dead
    id.nomiss=which(dat.site[,"cumulative_pts_dead"]>0 & dat.site[,"cumulative_pts_severe_dead"]>0)
    id3.3=which(dat.site[id.nomiss,"cumulative_pts_dead"] < dat.site[id.nomiss,"cumulative_pts_severe_dead"])
    err=c(err,length(id3.3)>1)

    # check that pts_in_hosp_on_this_date > pts_severe_in_hosp_on_this_date
    id.nomiss=which(dat.site[,"pts_in_hosp_on_this_date"]>0 & dat.site[,"pts_severe_in_hosp_on_date"]>0)
    id3.4=which(dat.site[id.nomiss,"pts_in_hosp_on_this_date"] < dat.site[id.nomiss,"pts_severe_in_hosp_on_date"])
    err=c(err,length(id3.4)>1)

    # check that pts_in_icu_on_this_date > pts_severe_in_icu_on_this_date
    id.nomiss=which(dat.site[,"pts_in_icu_on_this_date"]>0 & dat.site[,"pts_severe_in_icu_on_date"]>0)
    id3.5=which(dat.site[id.nomiss,"pts_in_icu_on_this_date"] < dat.site[id.nomiss,"pts_severe_in_icu_on_date"])
    err=c(err,length(id3.5)>1)

    # check that the cumulative values are increasing
    err1=NULL
    for (col.num in 4:9){
      x=dat.site[, col.num]
      err1=c(err1, all(x==cummax(x))==FALSE)
    }
    if(TRUE%in%err1){x.test=TRUE}else{x.test=FALSE}
    err=c(err,x.test)

    # check if there are negative patient counts
    dat.check=dat.site[,c(4:13)]
    id4=which(dat.check[dat.check<0]%in%c(-999,-99)!=1)
    err=c(err,length(id4)>1)

    report=data.frame(site.nm, label=err.label, err)
    err.report=report[report[,"err"]==T,c("site.nm", "label")]

  list(err.report=err.report, err.label=err.label)
}

##########################
## QC NUMBER 21: DATE SHOULD BE SORTED
##########################
err_report_date_sort=function(dat.DailyCounts, site.nm){

  file.nms=c("dat.DailyCounts")
  err=NULL
  err.label = NULL

  for(file.nm in file.nms){
    file.check=get(file.nm)

    file.date.ord=file.check[order(as.Date(file.check$calendar_date, format="%m/%d/%Y")),]
    bench.date.nm=file.check$calendar_date

    error_var <- sum(file.date.ord$calendar_date!=bench.date.nm)
    err=c(err,error_var)
    err.label=c(err.label,paste0("date is not ordered correctly for: ", file.nm))
  }
  #print(err)
  report=data.frame(site.nm, label=err.label, err)
  err.report=report[report[,"err"]==T,c("site.nm", "label")]
  list(err.report=err.report, err.label=err.label)
}

## Test QC 21
#err_report_date_sort(dat.DailyCounts, site.nm)
