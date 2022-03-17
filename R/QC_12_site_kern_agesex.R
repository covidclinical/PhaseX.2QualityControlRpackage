##########################
##########  AgeSex
##########################
#' @import dplyr
err_report_agesex_site=function(dat.AgeSex, site.nm){
  err.label=
    c(
      "missing (sex,age)=all",
      "N_all < N_ever_severe",
      "negative N (not -999 or -99)",
      "the mean age is not in the age range"
    )
  err.report.all=NULL
  err=NULL
  err1=NULL

  dat.site=dat.AgeSex

  colnames(dat.site)=tolower(colnames(dat.site))

    # check that sum in groups equals to all
  nm.check=c("sex", "age_group")
  dat.check=dat.site[,nm.check]
  err=c(err,0%in%apply(dat.check, 1, function(x) sum(x!="all"))!=1)

    # check that number of total patients inferior to number of severe patients
  id.nomiss=which(dat.site[,"pts_all"]>0 & dat.site[,"pts_ever_severe"]>0)
  err=c(err, any(dat.site[id.nomiss,"pts_all"] < dat.site[id.nomiss,"pts_ever_severe"]))

    # check that there is no negative patient count
  dat.check=dat.site[,c("pts_all", "pts_ever_severe")]
  err=c(err, any(dat.check[dat.check<0] %in% c(-999,-99)!=1))

    # check that mean_age is in age_group range
  for (age.group in (unique(dat.site$age_group))){
    dat.check=dat.site%>%filter(age_group %in% age.group)%>%select(mean_age)
    if (age.group=='00to02'){
      err1=c(err1,!(0<=dat.check & dat.check<=2))
    }
    else if (age.group=='03to05'){
      err1=c(err1,!(3<=dat.check & dat.check<=5))
    }
    else if (age.group=='06to11'){
      err1=c(err1,!(6<=dat.check & dat.check<=11))
    }
    else if (age.group=='12to17'){
      err1=c(err1,!(12<=dat.check & dat.check<=17))
    }
    else if (age.group=='18to20'){
      err1=c(err1,!(18<=dat.check & dat.check<=20))
    }
    else if (age.group=='21to25'){
      err1=c(err1,!(21<=dat.check & dat.check<=25))
    }
    else if (age.group=='26to49'){
      err1=c(err1,!(26<=dat.check & dat.check<=49))
    }
    else if (age.group=='50to69'){
      err1=c(err1,!(50<=dat.check & dat.check<=69))
    }
    else if (age.group=='70to79'){
      err1=c(err1,!(70<=dat.check & dat.check<=79))
    }
    else if (age.group=='80plus'){
      err1=c(err1,!(80<=dat.check))
    }
    else{}
  }
  if(TRUE%in%err1){x=TRUE}else{x=FALSE}
  err=c(err,x)

  report=data.frame(site.nm, label=err.label, err)
  err.report=report[report[,"err"]==T,c("site.nm", "label")]
  list(err.report=err.report, err.label=err.label)
}
