
qc_site=function(dat.DailyCounts, dat.ClinicalCourse, dat.AgeSex, dat.DiagProcMed,
                 dat.Labs, dat.RaceByLocalCode, dat.RaceBy4CECode, dat.LabCodes,
                 site.nm, cohort.list.all, race.list.all){

  # qc_site_kern
  qc.grp = err_report_missing_demographic_group(dat.DailyCounts, dat.ClinicalCourse,
                                                dat.AgeSex, dat.DiagProcMed,dat.Labs,
                                                dat.RaceByLocalCode, dat.RaceBy4CECode, site.nm, cohort.list.all)
  qc.col = err_report_colnames_site(dat.DailyCounts, dat.ClinicalCourse, dat.AgeSex,
                                    dat.DiagProcMed,dat.Labs, dat.RaceByLocalCode,
                                    dat.RaceBy4CECode, dat.LabCodes, site.nm)
  qc.cros = err_report_crossover_site(dat.ClinicalCourse, dat.AgeSex, dat.DailyCounts, dat.Labs,
                                      dat.DiagProcMed, dat.RaceBy4CECode, site.nm)
  # qc_site_kern_agesex
  qc.as = err_report_agesex_site(dat.AgeSex, site.nm)
  # qc_site_kern_clinicalcourse
  qc.cc = err_report_clinicalcourse_site(dat.ClinicalCourse, site.nm)
  # qc_site_kern_dailycounts
  qc.dc = err_report_dailycounts_site(dat.DailyCounts,site.nm)
  qc.dt = err_report_date_sort(dat.DailyCounts, site.nm)
  # qc_site_kern_diagprocmed
  qc.dpm = err_report_dpm_site(dat.DiagProcMed, site.nm)
  # qc_site_kern_labs
  qc.lab = err_report_lab_site(dat.Labs, dat.AgeSex, site.nm)
  qc.lab.mis = err_report_lab_miss(dat.Labs, site.nm)
  qc.lab.val = err_report_lab_unit_site(dat.Labs, site.nm)
  # qc_site_kern_race
  qc.rc = err_report_race_site(dat.RaceBy4CECode, dat.RaceByLocalCode, site.nm)
  #qc.rc.mis = err_report_missing_race_group(dat.RaceByLocalCode, dat.RaceBy4CECode, site.nm, race.list.all)

  list(qc.grp=qc.grp,
       qc.col=qc.col,
       qc.cros=qc.cros,
       qc.as=qc.as,
       qc.cc=qc.cc,
       qc.dc=qc.dc,
       qc.dt=qc.dt,
       qc.dpm=qc.dpm,
       qc.lab=qc.lab,
       qc.lab.mis=qc.lab.mis,
       qc.lab.val=qc.lab.val,
       qc.rc=qc.rc
       #qc.rc.mis=qc.rc.mis
       )
}

