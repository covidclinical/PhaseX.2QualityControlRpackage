



##########################
## QC NUMBER 1: MISSING DEMOGRAPHIC GROUP
##########################
err_report_missing_demographic_group=function(dat.DailyCounts, dat.ClinicalCourse, dat.AgeSex, dat.DiagProcMed,
                                              dat.Labs, dat.RaceByLocalCode, dat.RaceBy4CECode, site.nm, cohort.list.all){

  all_file = list(dat.DailyCounts, dat.ClinicalCourse,dat.AgeSex, dat.DiagProcMed,
                  dat.Labs, dat.RaceByLocalCode, dat.RaceBy4CECode)
  all_filename = c("DailyCounts", "ClinicalCourse", "AgeSex",
                   "DiagProcMed", "Labs", "RaceByLocalCode", "RaceBy4CECode")
  i = 1

  #report=data.frame(site.nm, label=err.label, err)
  err.report = NULL
  missing_cohort_all = NULL

  for (file in all_file){
    filename_now = all_filename[i]
    cohort_list_now <- unique(unlist(file[c("cohort")]))
    #print(filename_now)
    missing_cohort = setdiff(cohort.list.all,cohort_list_now)
    missing_cohort_merged = paste( unlist(missing_cohort), collapse=',')
    missing_cohort_all = c(missing_cohort_all, missing_cohort_merged)
    #print(missing_cohort_merged)
    combined_data = list(filename_now,missing_cohort_merged)
    #print(combined_data)
    i=i+1
  }
  report= data.frame(filename=all_filename, missing_cohort=missing_cohort_all)
  err.label = c("filename", "missing_cohort")
  err.report=report
  list(err.report=err.report, err.label =err.label)
}
## Test QC 1
# err_report_missing_demographic_group(dat.DailyCounts, dat.ClinicalCourse, dat.AgeSex, dat.DiagProcMed,
#                                      dat.Labs, dat.RaceByLocalCode, dat.RaceBy4CECode, site.nm)

##########################
## QC NUMBER : negative N (not -999 or -99)
## QC NUMBER : N_all < N_ever_severe (for all N)
## QC NUMBER : check that sum in groups equals to all
## QC NUMBER : check that cumulative count increases (N_all > N_all at day0)
## QC NUMBER : Inf, -Inf, NA, Nan
## QC NUMBER : day 0+ not included
## QC NUMBER : days_since_admission starts at 0
##########################




##########################
## QC NUMBER 16: CHECK THE COLUMN NAMES
##########################
err_report_colnames_site=function(dat.DailyCounts, dat.ClinicalCourse, dat.AgeSex, dat.DiagProcMed,
                                         dat.Labs, dat.RaceByLocalCode, dat.RaceBy4CECode, dat.LabCodes, site.nm){


  file.nms=
    c("dat.DailyCounts",
      "dat.ClinicalCourse",
      "dat.AgeSex",
      "dat.DiagProcMed",
      "dat.Labs",
      "dat.LabCodes",
      "dat.RaceByLocalCode",
      "dat.RaceBy4CECode"
    )
  col.nms=NULL
  col.nms[[file.nms[1]]]=c("siteid", "cohort", "calendar_date" ,"cumulative_pts_all","cumulative_pts_icu","cumulative_pts_dead","cumulative_pts_severe","cumulative_pts_severe_icu",
                           "cumulative_pts_severe_dead", "pts_in_hosp_on_this_date", "pts_in_icu_on_this_date", "pts_severe_in_hosp_on_date", "pts_severe_in_icu_on_date")
  col.nms[[file.nms[2]]]=c("siteid", "cohort", "days_since_admission","pts_all_in_hosp","pts_all_in_icu", "pts_all_dead", "pts_severe_by_this_day", "pts_ever_severe_in_hosp",
                           "pts_ever_severe_in_icu", "pts_ever_severe_dead")
  col.nms[[file.nms[3]]]=c("siteid", "cohort", "age_group", "mean_age", "sex", "pts_all", "pts_ever_severe")
  col.nms[[file.nms[4]]]=c("siteid", "cohort", "concept_type", "concept_code", "pts_all_before_adm","pts_all_since_adm","pts_all_dayN14toN1","pts_all_day0to29", "pts_all_day30to89",
                           "pts_all_day30plus", "pts_all_day90plus", "pts_all_1st_day0to29", "pts_all_1st_day30plus", "pts_all_1st_day90plus", "pts_ever_severe_before_adm",
                           "pts_ever_severe_since_adm", "pts_ever_severe_dayN14toN1", "pts_ever_severe_day0to29", "pts_ever_severe_day30to89", "pts_ever_severe_day30plus", "pts_ever_severe_day90plus",
                           "pts_ever_severe_1st_day0to29", "pts_ever_severe_1st_day30plus", "pts_ever_severe_1st_day90plus")
  col.nms[[file.nms[5]]]=c("siteid", "cohort", "loinc", "days_since_admission", "pts_all", "mean_value_all", "stdev_value_all", "mean_log_value_all", "stdev_log_value_all",
                           "pts_ever_severe", "mean_value_ever_severe", "stdev_value_ever_severe", "mean_log_value_ever_severe","stdev_log_value_ever_severe",
                           "pts_never_severe", "mean_value_never_severe", "stdev_value_never_severe", "mean_log_value_never_severe", "stdev_log_value_never_severe")

  # if(site.nm%in%c("MGB", "MGBPED")){
  #   col.nms[[file.nms[5]]]=c(
  #     "siteid","loinc","days_since_admission","units","num_patients_all","mean_value_all","stdev_value_all","mean_log_value_all","stdev_log_value_all",
  #     "num_patients_ever_severe","mean_value_ever_severe","stdev_value_ever_severe","mean_log_value_ever_severe","stdev_log_value_ever_severe","num_patients_never_severe","mean_value_never_severe","stdev_value_never_severe",
  #     "mean_log_value_never_severe","stdev_log_value_never_severe")}

  col.nms[[file.nms[6]]]=c("siteid","fource_loinc","fource_lab_units","fource_lab_name","scale_factor","local_lab_code","local_lab_units","local_lab_name","notes")
  col.nms[[file.nms[7]]]=c("siteid","cohort","race_local_code","race_4ce","pts_all","pts_ever_severe")
  col.nms[[file.nms[8]]]=c("siteid","cohort","race_4ce","pts_all","pts_ever_severe")
  err.label=paste0("wrong/missing column names for ", file.nms, "; column names should be: ", unlist(lapply(col.nms, function(xx) paste(xx,collapse=";"))))


  err=NULL
  for(file.nm in file.nms){
    file.check=get(file.nm)
    file.col.nm=tolower(colnames(file.check))
    bench.col.nm=col.nms[[file.nm]]
    if(length(file.col.nm)!=length(bench.col.nm)){err=c(err,1)}else{
      if(length(file.col.nm)==length(bench.col.nm)){
        err=c(err,sum(file.col.nm!=col.nms[[file.nm]]))
      }
    }
  }
  report=data.frame(site.nm, label=err.label, err)
  err.report=report[report[,"err"]==T,c("site.nm", "label")]
  list(err.report=err.report, err.label=err.label)
}

## Test QC 16

# err_report_capitalization(dat.DailyCounts, dat.ClinicalCourse, dat.AgeSex, dat.DiagProcMed,
#                           dat.Labs, dat.RaceByLocalCode, dat.RaceBy4CECode, dat.LabCodes, site.nm)


##########################
##########  Crossover
##########################
err_report_crossover_site=function(dat.ClinicalCourse, dat.AgeSex, dat.DailyCounts, dat.Labs, dat.DiagProcMed, dat.RaceBy4CECode, site.nm){

  err.label=c(
    "missing ClinicalCourse or Demographics or DailyCounts",
    "N_all in Demographics and DailyCounts do not match",
    "N_all in ClincalCourse and DailyCounts do not match",
    "N_ever_severe in Demographics and DailyCounts do not match",
    "N_ever_severe in ClinicalCourse and DailyCounts do not match",
    #"N_all in DiagProcMed is larger than the total number of all patients",
    "N_all in Labs is larger than the total number of all patients",
    "N_all in RaceBy4CEcode is larger than the total number of all patients",
    #"N_ever_severe in DiagProcMed is larger than the total number of ever severe patients",
    "N_ever_severe in Labs is larger than the total number of ever severe patients",
    "N_ever_severe in RaceBy4CEcode is larger than the total number of ever severe patients"
  )

  exist.cc=is.null(dat.ClinicalCourse)!=1
  exist.dm=is.null(dat.AgeSex)!=1
  exist.dc=is.null(dat.DailyCounts)!=1

  if(exist.cc*exist.dm*exist.dc==0){err1=err2=err3=err4=err5=TRUE}else{
    err1=FALSE
    dat.site.cc=dat.ClinicalCourse
    colnames(dat.site.cc)=tolower(colnames(dat.site.cc))
    dat.site.dm=dat.AgeSex
    colnames(dat.site.dm)=tolower(colnames(dat.site.dm))
    dat.site.dc=dat.DailyCounts
    colnames(dat.site.dc)=tolower(colnames(dat.site.dc))
    dat.site.dpm=dat.DiagProcMed
    colnames(dat.site.dpm)=tolower(colnames(dat.site.dpm))
    dat.site.lab=dat.Labs
    colnames(dat.site.lab)=tolower(colnames(dat.site.lab))
    dat.site.rc=dat.RaceBy4CECode
    colnames(dat.site.rc)=tolower(colnames(dat.site.rc))

    #"N_all in Demographics and DailyCounts do not match"
    #"N_all in ClincalCourse and DailyCounts do not match"

    # all
    n.dm=dat.site.dm[which(apply(dat.site.dm[,c("sex", "age_group")],1, function(x) all(x=="all"))),c("pts_all")]
    n.cc=dat.site.cc[dat.site.cc$days_since_admission==0,"pts_all_in_hosp"]
    n.dc=max(dat.site.dc[,"cumulative_pts_all"])

    n.all=max(n.cc, n.dm, n.dc, na.rm=T)
    # n.dpm=max(dat.site.dpm[,"num_patients_all_before_admission"])
    # err6=n.dpm>n.all

    # "N_all in Labs is larger than the total number of all patients",
    id.nomiss=which(dat.site.lab[,"pts_all"]>0 & dat.site.lab[,"pts_ever_severe"]>0)
    err6=any(dat.site.lab[id.nomiss,"pts_all"]>max(dat.site.dc[, "cumulative_pts_all"]))
    if(err6==TRUE){
      label6=paste(err.label[6],paste(as.character(dat.site.lab[id.nomiss[which(dat.site.lab[,"pts_all"]>max(dat.site.dc[, "cumulative_pts_all"]))],"loinc"]),collapse = ";"),sep=" ")
      err.label[6]=label6
    }

    # "N_all in RaceBy4CEcode is larger than the total number of all patients"
    id.nomiss=which(dat.site.rc[,"pts_all"]>0 & dat.site.rc[,"pts_ever_severe"]>0)
    err7=any(dat.site.rc[id.nomiss,"pts_all"]>max(dat.site.dc[, "cumulative_pts_all"]))
    if(err7==TRUE){
      label7=paste(err.label[7],paste(as.character(dat.site.rc[id.nomiss[which(dat.site.rc[id.nomiss,"pts_all"]>max(dat.site.dc[, "cumulative_pts_all"]))],"loinc"]),collapse = ";"),sep=" ")
      err.label[7]=label7
    }

    if(all(c(length(n.dm), length(n.dc))>0)){err2=n.dm!=n.dc}else{err2=FALSE}
    if(all(c(length(n.cc), length(n.dc))>0)){err3=n.cc!=n.dc}else{err3=FALSE}

    # ever severe
    n.dm=dat.site.dm[which(apply(dat.site.dm[,c("sex", "age_group")],1, function(x) all(x=="all"))),c("pts_ever_severe")]
    n.cc=max(dat.site.cc[dat.site.cc$days_since_admission==0,"pts_ever_severe_in_hosp"])
    n.dc=max(dat.site.dc[,"cumulative_pts_severe"])

    if(all(c(length(n.dm), length(n.dc))>0)){err4=n.dm!=n.dc}else{err4=FALSE}
    if(all(c(length(n.cc), length(n.dc))>0)){err5=n.cc!=n.dc}else{err5=FALSE}
  }

  # "N_ever_severe in Labs is larger than the total number of ever_severe patients",
  err8=any(dat.site.lab[id.nomiss,"num_patients_ever_severe"]>max(dat.site.dc[, "cumulative_pts_severe"]))
  if(err8==TRUE){
    label8=paste(err.label[8],paste(as.character(dat.site.lab[id.nomiss[which(dat.site.lab[id.nomiss,"num_patients_ever_severe"]>max(dat.site.dc[, "cumulative_pts_severe"]))],"loinc"]),collapse = ";"),sep=" ")
    err.label[8]=label8
  }

  # "N_ever_severe in RaceBy4CEcode is larger than the total number of ever_severe patients"
  err9=any(dat.site.rc[id.nomiss,"pts_all"]>max(dat.site.dc[, "cumulative_pts_severe"]))
  if(err9==TRUE){
    label9=paste(err.label[9],paste(as.character(dat.site.rc[id.nomiss[which(dat.site.rc[id.nomiss,"pts_ever_severe"]>max(dat.site.dc[, "cumulative_pts_severe"]))],"loinc"]),collapse = ";"),sep=" ")
    err.label[9]=label9
  }
  err=c(err1, err2, err3, err4, err5, err6, err7, err8, err9)
  report=data.frame(site.nm, label=err.label, err)

  err.report=report[report[,"err"]==TRUE,c("site.nm", "label")]
  list(err.report=err.report, err.label=err.label)
}









