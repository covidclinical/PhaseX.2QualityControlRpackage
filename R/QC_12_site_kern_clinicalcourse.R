##########################
##########  ClinicalCourse
##########################
err_report_clinicalcourse_site=function(dat.ClinicalCourse, site.nm){
  err.label=c(
    "days_since_admission should start at day 0",
    "N_all at day 0 is not the largest",
    "N_all_in_hosp < N_ever_severe_in_hosp",
    "N_all_in_icu < N_ever_severe_in_icu",
    "N_all_dead < N_ever_severe_dead",
    "negative N (not -999 or -99)")

  err.report.all=NULL
  err=NULL

  dat.site=dat.ClinicalCourse
  colnames(dat.site)=tolower(colnames(dat.site))

    # check that days0
    err=c(err,0%in%dat.site[,"days_since_admission"]!=1)

    # check that N_all at day 0 is the largest
    err=c(err,0%in%dat.site[which(dat.site[,"pts_all_in_hosp"]==max(dat.site[,"pts_all_in_hosp"])),
                            "days_since_admission"]!=1)

    # check that N_all>N_ever_severe
    id.nomiss=which(dat.site[,"pts_all_in_hosp"]>0 & dat.site[,"pts_ever_severe_in_hosp"]>0)
    id3.1=which(dat.site[id.nomiss,"pts_all_in_hosp"] < dat.site[id.nomiss,"pts_ever_severe_in_hosp"])
    err=c(err,length(id3.1)>1)

    id.nomiss=which(dat.site[,"pts_all_in_icu"]>0 & dat.site[,"pts_ever_severe_in_icu"]>0)
    id3.2=which(dat.site[id.nomiss,"pts_all_in_icu"] < dat.site[id.nomiss,"pts_ever_severe_in_icu"])
    err=c(err,length(id3.2)>1)

    id.nomiss=which(dat.site[,"pts_all_dead"]>0 & dat.site[,"pts_ever_severe_dead"]>0)
    id3.3=which(dat.site[id.nomiss,"pts_all_dead"] < dat.site[id.nomiss,"pts_ever_severe_dead"])
    err=c(err,length(id3.3)>1)

    # check if there are negative counts
    dat.check=dat.site[,c(4:10)]
    id4=which(dat.check[dat.check<0]%in%c(-999,-99)!=1)
    err=c(err,length(id4)>1)

    report=data.frame(site.nm, label=err.label, err)
    err.report=report[report[,"err"]==T,c("site.nm", "label")]
  list(err.report=err.report, err.label=err.label)
}
