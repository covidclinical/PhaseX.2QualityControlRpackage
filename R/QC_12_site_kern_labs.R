##########################
##########  Lab
##########################
#' @import dplyr
err_report_lab_site=function(dat.Labs, dat.AgeSex, site.nm){
  err.label=c(
    "N_all < N_ever_severe",
    "negative N (not -999 or -99)",
    "day 0+ not included",
    "Inf, -Inf, NA, Nan",
    "Less than 33% of all patients have the important labs")

  err = NULL
  dat.site=dat.Labs
  colnames(dat.site)=tolower(colnames(dat.site))


  # N_all < N_ever_severe
  id.nomiss=which(dat.site[,"pts_all"] > 0 & dat.site[,"pts_ever_severe"] > 0)
  err2=any(dat.site[id.nomiss,"pts_all"] < dat.site[id.nomiss,"pts_ever_severe"])

  # negative N (not -999 or -99)
  dat.check=dat.site[,setdiff(colnames(dat.site), c("siteid","cohort", "loinc", "units", "days_since_admission", colnames(dat.site)[grepl("log",colnames(dat.site))]))]
  err3=any(unique(dat.check[dat.check<0])%in%c(-99, -999)!=1)

  # day 0+ not included
  err4=sum(dat.site[,"days_since_admission"]>0)<1

  # Inf, -Inf, NA, Nan
  err5=any(dat.site%in%c(Inf, -Inf, NA, NaN))

  # Less than 33% of all patients have the main labs
  lab.want = data.frame(loinc=c('1988-5','2276-4','3255-7','33959-8','8066-5','8066-7','2160-0'))
  lab.have = data.frame(loinc=unique(dat.site[,'loinc']))

  pat.thresh = floor(0.33*as.numeric(dat.AgeSex %>% filter(age_group=='all', sex=='all') %>% select(pts_all)))
  total.pat = dat.site%>%group_by(loinc)%>%
    filter(loinc%in%lab.want$loinc)%>%
    slice(which.max(pts_all))%>%
    select(loinc,pts_all)
  err6=length(total.pat%>%filter(pts_all<pat.thresh)%>%select(loinc))>0

  err=c(err2, err3, err4, err5, err6)
  report=data.frame(site.nm, label=err.label, err)

  err.report=report[report[,"err"]==TRUE, c("site.nm", "label")]
  list(err.report=err.report, err.label=err.label)
}



##########################
##########  Missing Labs
##########################
err_report_lab_miss=function(dat.Labs, site.nm){

  dat.site=dat.Labs
  # Some important labs are missing
  # CRP(1988-5), ferritin(2276-4), fibrinogen(3255-7), procalcitonin(33959-8), D-dimer(48066-5,48066-7), creatinine(2160-0)
  lab.want = data.frame(loinc=c('1988-5','2276-4','3255-7','33959-8','8066-5','8066-7','2160-0'))
  lab.have = data.frame(loinc=unique(dat.site[,'loinc']))

  err.report=lab.want[which(!lab.want$loinc %in% lab.have$loinc),'loinc']

  if(is.null(err.report)==TRUE){err.report="No important labs are missing"}else{err.report=data.frame("labs"=err.report)}
  list(err.report=err.report)
}



##########################
##########  Lab units
##########################
err_report_lab_unit_site=function(dat.Labs, site.nm){
  dat=dat.Labs
  colnames(dat)=tolower(colnames(dat))

  dat$siteid=toupper(dat$siteid)
  nm.day="days_since_admission"
  comb.lab=dat
  comb.lab$loinc=trimws(comb.lab$loinc, which = c("both", "left", "right"))
  nm.lab.all = setdiff(unique(comb.lab$loinc),c("2019-8", "2703-7", "777-3", "34714-6"))

  comb.lab=comb.lab[which(comb.lab$days_since_admission%in%c(0:30)),]
  err.report=NULL
  for(nm.lab in nm.lab.all){
    tmp=comb.lab[comb.lab$loinc%in%nm.lab,c("days_since_admission", "mean_log_value_all")]
    tmp$mean_log_value_all[tmp$mean_log_value_all%in%c(-99,-999, -Inf, Inf)]=NA
    tmp=tmp[which(is.na(tmp$mean_log_value_all)!=1),]
    if(dim(tmp)[1]>=5){
      tmp.range=lab.range[,c(1,which(grepl(gsub("-",".",nm.lab),colnames(lab.range))==1))]
      colnames(tmp.range)[2:3]=c("LB", "UB")
      tmp=left_join(tmp, tmp.range, by="days_since_admission")
      err.tmp=1*(sum(tmp$mean_log_value_all<tmp$LB)>(0.9*dim(tmp)[1])|sum(tmp$mean_log_value_all>tmp$UB)>(0.9*dim(tmp)[1]))
      err.tmp=c(site.nm, err.tmp, paste0("lab unit issue for ", nm.lab))
      err.report=rbind(err.report, err.tmp)
    }
  }
  if(is.null(err.report)!=1){
    err.report=data.frame(err.report)}else{err.report=data.frame(matrix(NA,1,3))
    }
  colnames(err.report)=c(site.nm, "err","label")
  err.report=err.report[err.report[,"err"]==1,c(site.nm, "label")]
  list(err.report=err.report)
}

