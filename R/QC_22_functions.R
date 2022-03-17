sink.txt=function(x, file, method=print, append){sink(file, append=append); method(x); sink()}

err_report_colnames_site.phase2=function(phase2.ClinicalCourse, phase2.Observations, phase2.Summary, phase2.Race, site.nm){
  
  file.nms=
    c("phase2.ClinicalCourse",
      "phase2.Observations",
      "phase2.Summary",
      "phase2.Race"
    )
  col.nms=NULL
  col.nms[[file.nms[1]]]=c("siteid","cohort","patient_num","days_since_admission","calendar_date","in_hospital","severe","in_icu","dead")
  col.nms[[file.nms[2]]]=c("siteid","cohort","patient_num","days_since_admission","concept_type","concept_code","value")
  col.nms[[file.nms[3]]]=c("siteid","cohort","patient_num","admission_date","source_data_updated_date","days_since_admission","last_discharge_date","still_in_hospital","severe_date","severe",             
                           "icu_date","icu","death_date","dead","age_group","age","sex")
  col.nms[[file.nms[4]]]=c("siteid","cohort","patient_num","race_local_code","race_4ce")
  
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












