#------------------------------------------------------------------------------#
#                         Report values for the manuscript                     #
#------------------------------------------------------------------------------#

rm(list=ls())
options(stringsAsFactors=FALSE)

# Load libraries
library(dplyr)
library(rgdal)

#----- Set variables -----------------------------------------------------------

# Set project start and IBCM start dates
start_date <- as.Date("2018-01-01")
IBCM_start <- as.Date("2018-06-01")
end_date <- as.Date("2019-07-31")

# Calculate number of months for each study period
n_months_total <- length(seq(from=start_date, to=end_date, by='month'))
n_months_pre <- length(seq(from=start_date, to=IBCM_start, by='month')) -1
n_months_post <- length(seq(from=IBCM_start, to=end_date, by='month'))

#----- Load data ---------------------------------------------------------------

# Load in datasets
HF <- read.csv("ms_data/processed_HF.csv")
VET <- read.csv("ms_data/processed_VET.csv")

# Load shapefiles
regions <- readOGR("ms_data/gis", "TZ_Region_2012_pop")
districts <- readOGR("ms_data/gis", "TZ_District_2012_pop")

#------ Process data -----------------------------------------------------------

# Format as dates
HF$Date_proxy <- as.Date(HF$Date_proxy)
VET$Date_proxy <- as.Date(VET$Date_proxy)
VET$SAMPLE_WAS_COLLECTED_SAMPLE_DATE <- as.Date(VET$SAMPLE_WAS_COLLECTED_SAMPLE_DATE)

# Subset HF data for first visit/positive clinical signs
table(HF$VISIT_STATUS, HF$Suspect)
HF <- HF[which(HF$Suspect!= "No data"),]; dim(HF)

# Subset data to ensure it is within bounds
HF <- HF[which(HF$Date_proxy>=start_date & HF$Date_proxy<=end_date),]
VET <- VET[which(VET$Date_proxy>=start_date & VET$Date_proxy<=end_date),]

# Create subsets based on IBCM start dates
pre_ibcm_HF <- HF[which(HF$Date_proxy<HF$IBCM_start),]; nrow(pre_ibcm_HF)
post_ibcm_HF <- HF[which(HF$Date_proxy>=HF$IBCM_start),]; nrow(post_ibcm_HF)
pre_ibcm_VET <- VET[which(VET$Date_proxy<VET$IBCM_start),]; nrow(pre_ibcm_VET)
post_ibcm_VET <- VET[which(VET$Date_proxy>=VET$IBCM_start),]; nrow(post_ibcm_VET)

#----- Abstract ----------------------------------------------------------------

message("Number of LFO Investigations carried out: ", nrow(post_ibcm_VET))
message("Number of probable rabies cases: ",
        round((length(which(post_ibcm_VET$ASSESSMENT_DECISION=="suspicious_for_of_rabies"))/nrow(post_ibcm_VET))*100, digits=1), "% (",
        length(which(post_ibcm_VET$ASSESSMENT_DECISION=="suspicious_for_of_rabies")), "/", nrow(post_ibcm_VET), ")")


#----- Patient Presentations ---------------------------------------------------

#----- Summarise by month
pre_ibcm_summary <- pre_ibcm_HF %>%
  group_by(Year_month) %>%
  summarise(n=length(Year_month))
message("Pre IBCM mean reports/month: ", round(mean(pre_ibcm_summary$n), digits=1), " (", min(pre_ibcm_summary$n), "-",
        max(pre_ibcm_summary$n), ")")

pre_ibcm_summary_suspect <- pre_ibcm_HF[which(pre_ibcm_HF$Suspect=="Suspect"),] %>%
  group_by(Year_month) %>%
  summarise(n=length(Year_month))
message("Percentage of suspect reports Pre IBCM: ", round((sum(pre_ibcm_summary_suspect$n)/sum(pre_ibcm_summary$n))*100, digits=1), "%")

post_ibcm_summary <- post_ibcm_HF %>%
  group_by(Year_month) %>%
  summarise(n=length(Year_month))
message("Post IBCM mean reports/month: ", round(mean(post_ibcm_summary$n), digits=1), " (", min(pre_ibcm_summary$n), "-",
        max(post_ibcm_summary$n), ")")

post_ibcm_summary_suspect <- post_ibcm_HF[which(post_ibcm_HF$Suspect=="Suspect"),] %>%
  group_by(Year_month) %>%
  summarise(n=length(Year_month))
message("Percentage of suspect reports Post IBCM: ", round((sum(post_ibcm_summary_suspect$n)/sum(post_ibcm_summary$n))*100, digits=1), "%")

#----- Summarise incidence/100,000/annum
summary_inc <- HF[which(HF$Suspect %in% c("Suspect", "Positive")),] %>%
  group_by(Year_month) %>%
  summarise(n=length(Year_month))

summary_pre_inc <- pre_ibcm_HF[which(pre_ibcm_HF$Suspect %in% c("Suspect", "Positive")),] %>%
  group_by(Year_month) %>%
  summarise(n=length(Year_month))

summary_post_inc <- post_ibcm_HF[which(post_ibcm_HF$Suspect %in% c("Suspect", "Positive")),] %>%
  group_by(Year_month) %>%
  summarise(n=length(Year_month))

dists = sort(unique(HF$LOCATION_OF_EVENT_DISTRICT))
for(i in dists){
  present_dis_total <-  HF[which(HF$Suspect=="Suspect" & HF$LOCATION_OF_EVENT_DISTRICT==i),] %>%
    group_by(LOCATION_OF_EVENT_DISTRICT, Year_month) %>%
    summarise(n=length(Year_month))

  present_dis_pre <-  pre_ibcm_HF[which(pre_ibcm_HF$Suspect=="Suspect" & pre_ibcm_HF$LOCATION_OF_EVENT_DISTRICT==i),] %>%
    group_by(LOCATION_OF_EVENT_DISTRICT, Year_month) %>%
    summarise(n=length(Year_month))

  present_dis_post <-  post_ibcm_HF[which(post_ibcm_HF$Suspect=="Suspect" & post_ibcm_HF$LOCATION_OF_EVENT_DISTRICT==i),] %>%
    group_by(LOCATION_OF_EVENT_DISTRICT, Year_month) %>%
    summarise(n=length(Year_month))

  present_dis_summary <- data.frame(District=i, n_overall=sum(present_dis_total$n),
                                    n_pre=sum(present_dis_pre$n),
                                    n_post=sum(present_dis_post$n),
                                    stringsAsFactors=FALSE)

  if(i==dists[1]){
    presentation_by_dis <- present_dis_summary
  } else { presentation_by_dis <- rbind(presentation_by_dis, present_dis_summary) }
}
presentation_by_dis <- merge(presentation_by_dis,
                             data.frame(District=districts$District_N, pop_2012=districts$pop_2012),
                     by="District", all.x=TRUE)

presentation_by_dis$inc_overall <- ((presentation_by_dis$n_overall/presentation_by_dis$pop_2012)*100000)
presentation_by_dis$inc_pre <- ((presentation_by_dis$n_pre/presentation_by_dis$pop_2012)*100000)
presentation_by_dis$inc_post <- ((presentation_by_dis$n_post/presentation_by_dis$pop_2012)*100000)
presentation_by_dis <- presentation_by_dis[-which(is.na(presentation_by_dis$pop_2012)),]
study_pop <- sum(regions$pop_2012[which(regions$Region_Nam %in% c("Mara", "Lindi", "Morogoro", "Mtwara"))])

message("Overall patient presentation: ",
        round((((nrow(HF)/study_pop)*100000)/n_months_total)*12, digits=1), " per 100,000 per annum (range: ",
        round(min(presentation_by_dis$inc_overall), digits=1), " to ", round(max(presentation_by_dis$inc_overall), digits=1), ")")
message("Post-IBCM incidence: ",
        round((((sum(summary_post_inc$n)/study_pop)*100000)/n_months_post)*12, digits=1), " per 100,000 per annum (range: ",
        round(min(presentation_by_dis$inc_post), digits=1), " to ", round(max(presentation_by_dis$inc_post), digits=1), ")")
message("Pre-IBCM incidence: ",
        round((((sum(summary_pre_inc$n)/study_pop)*100000)/n_months_pre)*12, digits=1), " per 100,000 per annum")

#----- Summarise incidence/100,000/annum by region
pre_ibcm_region <- pre_ibcm_HF %>%
  group_by(Year_month, LOCATION_OF_EVENT_REGION, Suspect) %>%
  summarise(n=length(Year_month))

post_ibcm_region <- post_ibcm_HF %>%
  group_by(Year_month, LOCATION_OF_EVENT_REGION, Suspect) %>%
  summarise(n=length(Year_month))

mara_pop <- regions$pop_2012[which(regions$Region_Nam=="Mara")]
lindi_pop <- regions$pop_2012[which(regions$Region_Nam=="Lindi")]
morogoro_pop <- regions$pop_2012[which(regions$Region_Nam=="Morogoro")]
mtwara_pop <- regions$pop_2012[which(regions$Region_Nam=="Mtwara")]

#----- Summary of presentations
patient_pres <- data.frame(Region = c("Lindi", "Mara", "Morogoro", "Mtwara"),
                           pop_size = c(lindi_pop, mara_pop, morogoro_pop, mtwara_pop),
                           n_pre = c(sum(pre_ibcm_region$n[which(pre_ibcm_region$LOCATION_OF_EVENT_REGION=="Lindi")]),
                                     sum(pre_ibcm_region$n[which(pre_ibcm_region$LOCATION_OF_EVENT_REGION=="Mara")]),
                                     sum(pre_ibcm_region$n[which(pre_ibcm_region$LOCATION_OF_EVENT_REGION=="Morogoro")]),
                                     sum(pre_ibcm_region$n[which(pre_ibcm_region$LOCATION_OF_EVENT_REGION=="Mtwara")])),
                           pre_pres = c(NA, NA, NA, NA),

                           n_pre_high_risk = c(sum(pre_ibcm_region$n[which(pre_ibcm_region$Suspect %in% c("Suspect", "Positive") & pre_ibcm_region$LOCATION_OF_EVENT_REGION=="Lindi")]),
                                             sum(pre_ibcm_region$n[which(pre_ibcm_region$Suspect %in% c("Suspect", "Positive") & pre_ibcm_region$LOCATION_OF_EVENT_REGION=="Mara")]),
                                             sum(pre_ibcm_region$n[which(pre_ibcm_region$Suspect %in% c("Suspect", "Positive") & pre_ibcm_region$LOCATION_OF_EVENT_REGION=="Morogoro")]),
                                             sum(pre_ibcm_region$n[which(pre_ibcm_region$Suspect %in% c("Suspect", "Positive") & pre_ibcm_region$LOCATION_OF_EVENT_REGION=="Mtwara")])),
                           pre_percent = c(NA, NA, NA, NA),

                           n_post = c(sum(post_ibcm_region$n[which(post_ibcm_region$LOCATION_OF_EVENT_REGION=="Lindi")]),
                                     sum(post_ibcm_region$n[which(post_ibcm_region$LOCATION_OF_EVENT_REGION=="Mara")]),
                                     sum(post_ibcm_region$n[which(post_ibcm_region$LOCATION_OF_EVENT_REGION=="Morogoro")]),
                                     sum(post_ibcm_region$n[which(post_ibcm_region$LOCATION_OF_EVENT_REGION=="Mtwara")])),
                           post_pres = c(NA, NA, NA, NA),

                           n_post_high_risk = c(sum(post_ibcm_region$n[which(post_ibcm_region$Suspect %in% c("Suspect", "Positive") & post_ibcm_region$LOCATION_OF_EVENT_REGION=="Lindi")]),
                                                sum(post_ibcm_region$n[which(post_ibcm_region$Suspect %in% c("Suspect", "Positive") & post_ibcm_region$LOCATION_OF_EVENT_REGION=="Mara")]),
                                                sum(post_ibcm_region$n[which(post_ibcm_region$Suspect %in% c("Suspect", "Positive") & post_ibcm_region$LOCATION_OF_EVENT_REGION=="Morogoro")]),
                                                sum(post_ibcm_region$n[which(post_ibcm_region$Suspect %in% c("Suspect", "Positive") & post_ibcm_region$LOCATION_OF_EVENT_REGION=="Mtwara")])),
                           post_percent = c(NA, NA, NA, NA))

patient_pres$pre_pres <- round((((patient_pres$n_pre/patient_pres$pop_size)*100000)/n_months_pre)*12, digits=1)
patient_pres$pre_percent <- round((patient_pres$n_pre_high_risk/patient_pres$n_pre)*100, digits=1)
patient_pres$post_pres <- round((((patient_pres$n_post/patient_pres$pop_size)*100000)/n_months_post)*12, digits=1)
patient_pres$post_percent <- round((patient_pres$n_post_high_risk/patient_pres$n_post)*100, digits=1)

dplyr::select(patient_pres, Region, pre_pres, pre_percent, post_pres, post_percent)

#----- Print p-values for difference in %
message("P-values for difference in % high-risk (pre-/post-IBCM)")
for(i in 1:4){
  regions = patient_pres$Region
  message(paste0(regions[i], ": "))
  print(chisq.test(rbind(c(patient_pres$n_pre_high_risk[i], patient_pres$n_post_high_risk[i]), # Chi-sq test
                   c(patient_pres$n_pre[i]-patient_pres$n_pre_high_risk[i], patient_pres$n_post[i]-patient_pres$n_post_high_risk[i])))$p.value)
}

message("--- fig 3 caption ---")
message("n human deaths due to rabies: ", length(which(HF$VISIT_STATUS=="positive_clinical_signs")))
message("n human deaths due to rabies post-ibcm: ", length(which(post_ibcm_HF$VISIT_STATUS=="positive_clinical_signs")))
message("n RDT confirmed cases: ", length(which(post_ibcm_VET$SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_OUTCOME=="positive")))

#----- Summary of biting animals
message("Number of presentations post-IBCM: ", nrow(post_ibcm_HF))
message("Percentage domestic dog bites overall: ",
        round((length(which(post_ibcm_HF$GROUP_FIRST_VISIT_BITING_ANIMAL=="dog"))/nrow(post_ibcm_HF)*100), digits=1), "% (",
        length(which(post_ibcm_HF$GROUP_FIRST_VISIT_BITING_ANIMAL=="dog")), "/", nrow(post_ibcm_HF), ")")
message("Wildlife cases in Lindi: ", length(which(post_ibcm_HF$GROUP_FIRST_VISIT_BITING_ANIMAL=="wildlife" & post_ibcm_HF$LOCATION_OF_EVENT_REGION=="Lindi")))
message("Wildlife cases in Morogoro: ", length(which(post_ibcm_HF$GROUP_FIRST_VISIT_BITING_ANIMAL=="wildlife" & post_ibcm_HF$LOCATION_OF_EVENT_REGION=="Morogoro")))
message("Wildlife cases in Mtwara: ", length(which(post_ibcm_HF$GROUP_FIRST_VISIT_BITING_ANIMAL=="wildlife" & post_ibcm_HF$LOCATION_OF_EVENT_REGION=="Mtwara")))
message("Wildlife cases in Mara: ", length(which(post_ibcm_HF$GROUP_FIRST_VISIT_BITING_ANIMAL=="wildlife" & post_ibcm_HF$LOCATION_OF_EVENT_REGION=="Mara")))

#----- Summary of bite severity
message("Percentage of scratch/minor wounds: ",
        round((length(which(post_ibcm_HF$GROUP_FIRST_VISIT_BITE_DETAILS%in% c("scratch", "minor_wounds")))/nrow(post_ibcm_HF))*100, digits=1), "% (",
        length(which(post_ibcm_HF$GROUP_FIRST_VISIT_BITE_DETAILS %in% c("scratch", "minor_wounds"))), "/", nrow(post_ibcm_HF), ")")
message("Percentage of major wounds: ",
        round((length(which(post_ibcm_HF$GROUP_FIRST_VISIT_BITE_DETAILS %in% c("large_wounds", "large_wounds_broken_bones")))/nrow(post_ibcm_HF))*100, digits=1), "% (",
        length(which(post_ibcm_HF$GROUP_FIRST_VISIT_BITE_DETAILS%in% c("large_wounds", "large_wounds_broken_bones"))), "/", nrow(post_ibcm_HF), ")")
message("Percentage of hospitalised wounds: ",
        round((length(which(post_ibcm_HF$GROUP_FIRST_VISIT_BITE_DETAILS == "severe_hospitalization"))/nrow(post_ibcm_HF))*100, digits=1), "% (",
        length(which(post_ibcm_HF$GROUP_FIRST_VISIT_BITE_DETAILS == "severe_hospitalization")), "/", nrow(post_ibcm_HF), ")")
message("Number of fatal wounds: ",
        length(which(post_ibcm_HF$GROUP_FIRST_VISIT_BITE_DETAILS=="fatal_bite")),
        "; ages ", paste0(post_ibcm_HF$AGE[which(post_ibcm_HF$GROUP_FIRST_VISIT_BITE_DETAILS=="fatal_bite")], sep=", "))

no_pep <- post_ibcm_HF[which(post_ibcm_HF$PEP_AVAILABLE=="no"),]
table(no_pep$Year_month, no_pep$LOCATION_OF_EVENT_REGION)
message("Number of patients with no PEP available on initial presentation: ",
        nrow(no_pep), " (", round((nrow(no_pep)/nrow(post_ibcm_HF))*100, digits=1), "%)")
message("Number of patients that were referred: ", length(which(no_pep$VICTIM_WAS_REFERED == "yes")))
message("Number of patients that were referred and high-risk: ",
        length(which(no_pep$Suspect == "Suspect" & no_pep$VICTIM_WAS_REFERED == "yes")))

deaths_summary <- post_ibcm_HF[which(post_ibcm_HF$VISIT_STATUS=="positive_clinical_signs"),] %>%
  group_by(LOCATION_OF_EVENT_REGION, LOCATION_OF_EVENT_DISTRICT) %>%
  summarise(n=length(LOCATION_OF_EVENT_DISTRICT))
sum(deaths_summary$n)
deaths_summary

#----- LFO investigations ------------------------------------------------------

message("Number of LFO investigations pre IBCM: ", nrow(pre_ibcm_VET))
message("Number of LFO investigations post IBCM total: ", nrow(post_ibcm_VET))
message("Number of LFO investigations post IBCM following exposure: ",
        length(which(grepl("human_exposure|victim_has_rabies", post_ibcm_VET$REASON_FOR_REPORT)==TRUE)))
message("Number of LFO investigations post IBCM other trigger: ",
        length(which(grepl("human_exposure|victim_has_rabies", post_ibcm_VET$REASON_FOR_REPORT)==FALSE)))

post_ibcm_VET$year_mon <- paste0(post_ibcm_VET$Year, "-", post_ibcm_VET$Month)
reg_summary <- post_ibcm_VET %>%
  group_by(LOCATION_OF_EVENT_REGION, year_mon) %>%
  summarise(n_cases=length(LOCATION_OF_EVENT_REGION))

message("Average LFO investigations /month in Mara: ", round(mean(reg_summary$n_cases[which(reg_summary$LOCATION_OF_EVENT_REGION=="Mara")]), digits=1))
message("Average LFO investigations /month in Lindi: ", round(mean(reg_summary$n_cases[which(reg_summary$LOCATION_OF_EVENT_REGION=="Lindi")]), digits=1))
message("Average LFO investigations /month in Morogoro: ", round(mean(reg_summary$n_cases[which(reg_summary$LOCATION_OF_EVENT_REGION=="Morogoro")]), digits=1))
message("Average LFO investigations /month in Mtwara: ", round(mean(reg_summary$n_cases[which(reg_summary$LOCATION_OF_EVENT_REGION=="Mtwara")]), digits=1))

message("Number of LFO investigations post IBCM in person: ", length(which(post_ibcm_VET$TYPE_OF_INVESTIGATION=="in_person")))
message("Number of LFO investigations post IBCM by phone: ", length(which(post_ibcm_VET$TYPE_OF_INVESTIGATION=="phone_consultation")))

in_person <- post_ibcm_VET[which(post_ibcm_VET$TYPE_OF_INVESTIGATION=="in_person"),]
sampled <- in_person[which(in_person$SAMPLE_COLLECTED=="yes"),]
message("Percentage of cases investigated where sample was taken: ", round((nrow(sampled)/nrow(in_person))*100, digits=1),
        "% (", nrow(sampled), "/", nrow(in_person), ")")
message("Samples were collected between: ", min(sampled$SAMPLE_WAS_COLLECTED_SAMPLE_DATE), " and ",
        max(sampled$SAMPLE_WAS_COLLECTED_SAMPLE_DATE))
message("Number tested with RDT: ", length(which(sampled$SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_DONE=="yes")))

message("LFO suspect animals: ",
        round((length(which(post_ibcm_VET$ASSESSMENT_DECISION=="suspicious_for_of_rabies"))/nrow(post_ibcm_VET))*100, digits=1), "% (",
        length(which(post_ibcm_VET$ASSESSMENT_DECISION=="suspicious_for_of_rabies")), "/", nrow(post_ibcm_VET), ")")
message("Number of positive samples: ", length(which(sampled$SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_OUTCOME=="positive")), "/", length(which(!is.na(sampled$SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_OUTCOME))))
message("Number of Negative samples: ", length(which(sampled$SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_OUTCOME=="negative")), "/", length(which(!is.na(sampled$SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_OUTCOME))))
message("Number of Inconclusive samples: ", length(which(sampled$SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_OUTCOME=="inconclusive")), "/", length(which(!is.na(sampled$SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_OUTCOME))))

message("LFO healthy animals: ",
        round((length(which(post_ibcm_VET$ASSESSMENT_DECISION=="healthy"))/nrow(post_ibcm_VET))*100, digits=1), "% (",
        length(which(post_ibcm_VET$ASSESSMENT_DECISION=="healthy")), "/", nrow(post_ibcm_VET), ")")
message("LFO sick animals: ",
        round((length(which(post_ibcm_VET$ASSESSMENT_DECISION=="sick_not_rabies"))/nrow(post_ibcm_VET))*100, digits=1), "% (",
        length(which(post_ibcm_VET$ASSESSMENT_DECISION=="sick_not_rabies")), "/", nrow(post_ibcm_VET), ")")
message("LFO unknown animals: ",
        round((length(which(post_ibcm_VET$ASSESSMENT_DECISION=="unknown"))/nrow(post_ibcm_VET))*100, digits=1), "% (",
        length(which(post_ibcm_VET$ASSESSMENT_DECISION=="unknown")), "/", nrow(post_ibcm_VET), ")")

post_ibcm_VET_probable <- post_ibcm_VET
message("% animals alive at time of investigation: ",
        round((length(which(post_ibcm_VET_probable$ANIMAL_OUTCOME == "alive"))/nrow(post_ibcm_VET_probable))*100, digits=1), "% (",
        length(which(post_ibcm_VET_probable$ANIMAL_OUTCOME == "alive")), "/", nrow(post_ibcm_VET_probable), ")")
message("% animals dead/disappeared at time of investigation: ",
        round((length(which(post_ibcm_VET_probable$ANIMAL_OUTCOME %in% c("dead", "disappeared", "lost")))/nrow(post_ibcm_VET_probable))*100, digits=1), "% (",
        length(which(post_ibcm_VET_probable$ANIMAL_OUTCOME %in% c("dead", "disappeared", "lost"))), "/", nrow(post_ibcm_VET_probable), ")")
message("% animals disappeared at time of investigation: ",
        round((length(which(post_ibcm_VET_probable$ANIMAL_OUTCOME %in% c("disappeared", "lost")))/nrow(post_ibcm_VET_probable))*100, digits=1), "% (",
        length(which(post_ibcm_VET_probable$ANIMAL_OUTCOME %in% c("disappeared", "lost"))), "/", nrow(post_ibcm_VET_probable), ")")
message("% animals dead at time of investigation: ",
        round((length(which(post_ibcm_VET_probable$ANIMAL_OUTCOME == "dead"))/nrow(post_ibcm_VET_probable))*100, digits=1), "% (",
        length(which(post_ibcm_VET_probable$ANIMAL_OUTCOME == "dead")), "/", nrow(post_ibcm_VET_probable), ")")
message("n animals killed by community: ", length(which(post_ibcm_VET_probable$DEATH_CAUSE == "killed_by_community")))
message("n animals killed by owner: ", length(which(post_ibcm_VET_probable$DEATH_CAUSE == "killed_by_owner")))

#----- Presentations -----------------------------------------------------------

#----- Create a summary table to be used in a flow chart:
# phone call/ inperson > animal alive/ dead/ disappeared > of dead sample collected > of collected n tested
df <- post_ibcm_VET %>%
  group_by(TYPE_OF_INVESTIGATION,
           ANIMAL_OUTCOME,
           Assessment,
           SAMPLE_COLLECTED,
           SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_DONE,
           SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_OUTCOME) %>%
  summarise(n=length(Date_proxy))

message("N investigated: ", sum(df$n))
message("In person: ")
message("n total: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="in_person")]))
message("n alive: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="in_person" & df$ANIMAL_OUTCOME=="alive")]))
message("n dead: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="in_person" & df$ANIMAL_OUTCOME=="dead")]))
message("n disappeared/lost: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="in_person" & df$ANIMAL_OUTCOME %in% c("disappeared", "lost"))]))
message("n alive sampled: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="in_person" & df$ANIMAL_OUTCOME=="alive" & df$SAMPLE_COLLECTED=="yes")]))
message("n dead sampled: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="in_person" & df$ANIMAL_OUTCOME=="dead" & df$SAMPLE_COLLECTED=="yes")]))
message("n dead not sampled: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="in_person" & df$ANIMAL_OUTCOME=="dead" & df$SAMPLE_COLLECTED=="no")]))
message("n dead sampled +ve: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="in_person" & df$ANIMAL_OUTCOME=="dead" & df$SAMPLE_COLLECTED=="yes" & df$SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_OUTCOME=="positive")]))
message("n dead sampled -ve: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="in_person" & df$ANIMAL_OUTCOME=="dead" & df$SAMPLE_COLLECTED=="yes" & df$SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_OUTCOME=="negative")]))
message("n dead sampled incon: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="in_person" & df$ANIMAL_OUTCOME=="dead" & df$SAMPLE_COLLECTED=="yes" & df$SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_OUTCOME=="inconclusive")]))
message("n not tested: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="in_person" & df$ANIMAL_OUTCOME == "dead" & df$SAMPLE_COLLECTED=="yes" & df$SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_DONE=="no")]))
message("n disappeared/lost sampled: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="in_person" & df$ANIMAL_OUTCOME %in% c("disappeared", "lost") & df$SAMPLE_COLLECTED=="yes")]))

message("By phone: ")
message("n total: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="phone_consultation")]))
message("n alive: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="phone_consultation" & df$ANIMAL_OUTCOME=="alive")]))
message("n dead: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="phone_consultation" & df$ANIMAL_OUTCOME=="dead")]))
message("n disappeared/lost: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="phone_consultation" & df$ANIMAL_OUTCOME %in% c("disappeared", "lost"))]))
message("n alive sampled: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="phone_consultation" & df$ANIMAL_OUTCOME=="alive" & df$SAMPLE_COLLECTED=="yes")]))
message("n dead sampled: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="phone_consultation" & df$ANIMAL_OUTCOME=="dead" & df$SAMPLE_COLLECTED=="yes")]))
message("n disappeared/lost sampled: ", sum(df$n[which(df$TYPE_OF_INVESTIGATION=="phone_consultation" & df$ANIMAL_OUTCOME %in% c("disappeared", "lost") & df$SAMPLE_COLLECTED=="yes")]))

#----- Description of different criteria used to assess whether biting animal is rabid
message("Percentage HF workers report unprovoked aggression: ", round((length(which(post_ibcm_HF$S_unprovoked_aggression==1))/nrow(post_ibcm_HF))*100, digits=1), "%")
message("Percentage HF workers report exccessive salivation: ", round((length(which(post_ibcm_HF$S_excessive_salivation==1))/nrow(post_ibcm_HF))*100, digits=1), "%")
message("Percentage HF workers report restlessness: ", round((length(which(post_ibcm_HF$S_restlessness==1))/nrow(post_ibcm_HF))*100, digits=1), "%")
message("Percentage HF workers report abnormal vocalisation: ", round((length(which(post_ibcm_HF$S_abnormal_vocalization==1))/nrow(post_ibcm_HF))*100, digits=1), "%")
message("Percentage HF workers report no symptoms: ", round((length(which(is.na(post_ibcm_HF$SYMPTOMS)))/nrow(post_ibcm_HF))*100, digits=1), "% (",
        length(which(is.na(post_ibcm_HF$SYMPTOMS))), "/", nrow(post_ibcm_HF), ")")
message("Number of HF suspect cases without clinical signs: ", length(which(is.na(post_ibcm_HF$SYMPTOMS) & post_ibcm_HF$Suspect=="Suspect")))

message("Percentage VET workers report unprovoked aggression: ", round((length(which(post_ibcm_VET$S_unprovoked_aggression==1))/nrow(post_ibcm_VET))*100, digits=1), "%")
message("Percentage VET workers report abnormal vocalisation: ", round((length(which(post_ibcm_VET$S_abnormal_vocalization==1))/nrow(post_ibcm_VET))*100, digits=1), "%")
message("Percentage VET workers report restlessness: ", round((length(which(post_ibcm_VET$S_restlessness==1))/nrow(post_ibcm_VET))*100, digits=1), "%")
message("Percentage VET workers report excessive salivation: ", round((length(which(post_ibcm_VET$S_excessive_salivation==1))/nrow(post_ibcm_VET))*100, digits=1), "%")
message("Percentage VET workers report no signs: ", round((length(which(is.na(post_ibcm_VET$SYMPTOMS)))/nrow(post_ibcm_VET))*100, digits=1), "% (",
        length(which(is.na(post_ibcm_VET$SYMPTOMS))), "/", nrow(post_ibcm_VET), ")")
message("Number of VET suspect cases without clinical signs: ", length(which(is.na(post_ibcm_VET$SYMPTOMS) & post_ibcm_VET$Assessment=="Suspect")))

#----- Discussion --------------------------------------------------------------

message("% of bite patients that should receive PEP: ",
        round((length(which(HF$Suspect =="Suspect"))/length(which(HF$Suspect %in% c("Suspect", "Healthy"))))*100, digits=1), "%")
