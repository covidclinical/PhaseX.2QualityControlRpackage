
sink.txt=function(x, file, method=print, append){sink(file, append=append); method(x); sink()}

#' @import dplyr
err_report_colnames_site.phase2=function(phase2.ClinicalCourse, phase2.Observations, phase2.Summary, phase2.Race, site.nm){
  phase2.Race = NA
  if (is.na(phase2.Race)==FALSE){
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

  }else{
    file.nms=
      c("phase2.ClinicalCourse",
        "phase2.Observations",
        "phase2.Summary"
      )
    col.nms=NULL
    col.nms[[file.nms[1]]]=c("siteid","cohort","patient_num","days_since_admission","calendar_date","in_hospital","severe","in_icu","dead")
    col.nms[[file.nms[2]]]=c("siteid","cohort","patient_num","days_since_admission","concept_type","concept_code","value")
    col.nms[[file.nms[3]]]=c("siteid","cohort","patient_num","admission_date","source_data_updated_date","days_since_admission","last_discharge_date","still_in_hospital","severe_date","severe",
                             "icu_date","icu","death_date","dead","age_group","age","sex")

    err.label=paste0("wrong/missing column names for ", file.nms, "; column names should be: ", unlist(lapply(col.nms, function(xx) paste(xx,collapse=";"))))

  }

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



############ frequency of the codes
#' @import dplyr
#' @import tidyr
run_qc_tab_frequency.phase2=function(file.nm2, phase2.Observations, phase1.AgeSex, select.all.cohorts=T, output.dir){
  dat.keep = phase2.Observations %>% filter(!concept_type %in% c('DIAG-ICD10', 'DIAG-ICD9'))
  dat.keep$cohort.cat = sub("20.*", "", dat.keep$cohort)
  dat.count.pat = dat.keep %>% group_by(concept_type, concept_code, cohort.cat) %>% count(patient_num)
  dat.count = dat.count.pat %>% group_by(concept_type) %>% count(concept_code)
  #Add per cohort category

  dat.count.cat = dat.count.pat %>% group_by(concept_type, cohort.cat) %>% count(concept_code)
  dat.count.cat.w = spread(dat.count.cat,
                           key = cohort.cat,
                           value = n,
                           fill = 0)
  n.tot = sum(phase1.AgeSex[which(phase1.AgeSex$age_group == 'all' &
                                    phase1.AgeSex$sex == 'all'), 'pts_all'])
  dat.count$n.proportion = round(dat.count$n / n.tot, 2)
  colnames(dat.count) = c("concept_type", "concept_code", "AllCohorts", "AllCohorts.p")
  dat.count.all = left_join(dat.count,
                            dat.count.cat.w,
                            by = c('concept_type', 'concept_code'), fill=0)
  tryCatch(
    sink.txt(paste0("\n\nCode frequencies (patient) across all cohorts\n\n"),file = file.nm2,cat,append = T),error = function(e)NA)
  # tryCatch(sink.txt(paste(apply(dat.count.all, 1, function(ll) paste(paste0(ll), collapse=" ")), collapse="\n"), file=file.nm2, cat, append=T), error=function(e) NA)
  max.print <- getOption('max.print')
  options(max.print = nrow(dat.count.all) * ncol(dat.count.all))
  sink(file = file.nm2, append = T)
  print(noquote(as.matrix(dat.count.all)))
  options(max.print = max.print)
  sink()

  sink.txt("\n\n", file=file.nm2, cat, append=T)
  tryCatch(sink.txt(paste0("Missing concept codes:\n\n"), file=file.nm2, cat, append=T), error=function(e) NA)
  dat.count.all = data.frame(dat.count.all)
  all.codes.dict$V2 =gsub(" ", "", all.codes.dict$V2, fixed = TRUE)
  codes.miss = all.codes.dict %>% filter(!V2 %in% dat.count.all$concept_code)
  colnames(codes.miss)= c("concept_type", "concept_code")
  id.issue = as.vector(codes.miss$concept_code)
    if(length(id.issue)!=0){
      max.print <- getOption('max.print')
      options(max.print=nrow(codes.miss) * ncol(codes.miss))
      sink(file=file.nm2, append=T)
      print(noquote(as.matrix(codes.miss)))
      options(max.print=max.print)
      sink()

    }

}

############ Inf, NA, NaN
run_qc_na.phase2=function(phase2.ClinicalCourse, phase2.Observations, phase2.Summary, output.dir){
  x.na = which(is.na(phase2.ClinicalCourse) | is.infinite(rowSums(phase2.ClinicalCourse[,c(3,5:8)])))
}

############ truncation date is consistent





