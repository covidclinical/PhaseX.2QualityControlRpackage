##########################
########## DiagProcMed
##########################
err_report_dpm_site=function(dat.DiagProcMed, site.nm){
  err.label1 = "Negative N (not -999 or -99)"
  err.label2 = "N_all < N_ever_severe"
  err.label8 = "Data unavailable"

  err.label=c(err.label1, err.label2, err.label8)
  if(is.null(dat.DiagProcMed)==1){
    err.report=data.frame(site.nm=site.nm, label="no input data file for Diagnosis, Procedures or Medicationa")
    }else{
      dat.site=dat.DiagProcMed
      colnames(dat.site)=tolower(colnames(dat.site))

      if(dim(dat.site)[1]!=0){
        #"Data unavailable"
        err8=FALSE

        #"Negative N (not -999 or -99)"
        dat.check=dat.site[,setdiff(colnames(dat.site), c("siteid", "cohort", "concept_type", "concept_code"))]
        err1=any(unique(dat.check[dat.check<0])%in%c(-99, -999)!=1)

        #"N_all < N_ever_severe"
        cat.list = setdiff(colnames(dat.site), c("siteid", "cohort", "concept_type", "concept_code"))
        day.list = unique(sub("(pts_all_|pts_ever_severe_)", "", cat.list))
        err.cat = NULL
        for (day in day.list){
            pts_all=paste0("pts_all_", day)
            pts_evs=paste0("pts_ever_severe_", day)
            id.nomiss=which(dat.site[,pts_all]>0 & dat.site[,pts_evs]>0)
            err.cat = c(err.cat,
                        paste0(day," : ",any(dat.site[id.nomiss,pts_all]<dat.site[id.nomiss,pts_evs])))
        }
        err=c(err1, err.cat, err8)}else{
          err1=err.cat=FALSE; err8=TRUE
          err=c(err1, err.cat, err8)
          }
      report=data.frame(site.nm, label=err.label, err)

      err.report=report[report[,"err"]==TRUE,c("site.nm", "label")]}
  list(err.report=err.report, err.label=err.label)
}

