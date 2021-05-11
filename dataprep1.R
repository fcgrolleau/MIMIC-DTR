pivoted_rrt <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/pivoted_rrt.csv")
crrt_durations <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/crrt_durations.csv")
kdigo_stages <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/kdigo_stages.csv")
kdigo_creat <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/kdigo_creat.csv")
icustay_detail <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/icustay_detail.csv")
pivoted_lab <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/pivoted_lab.csv")
pivoted_bg <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/pivoted_bg.csv")
elixhauser_ahrq_v37 <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/elixhauser_ahrq_v37.csv")

#save.image("SQLtables.RData")

library(dplyr)

# Get the first KDIGO3 event per icustay
kdigo3p1 <- kdigo_stages %>% filter(aki_stage==3) %>% mutate(charttime=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS")) %>%
                  group_by(icustay_id) %>% arrange(charttime) %>%
                  filter(row_number()==1) %>% rename(charttime.kdigo3 = charttime)

# Merge ICU stays with KGIGO3 events with the icustay_detail table, then exclude the patients that were over 18 at hospital admission
kdigo3p2 <- merge(icustay_detail, kdigo3p1, by="icustay_id") %>% filter(admission_age>=18)

# A patient can't be included again after their first inclusion
kdigo3p3 <- kdigo3p2 %>% 
            mutate(admittime=as.POSIXct(admittime, format="%Y-%m-%d %H:%M:%OS")) %>%
        mutate(dischtime=as.POSIXct(dischtime, format="%Y-%m-%d %H:%M:%OS")) %>%
        mutate(intime=as.POSIXct(intime, format="%Y-%m-%d %H:%M:%OS")) %>%
        mutate(outtime=as.POSIXct(outtime, format="%Y-%m-%d %H:%M:%OS")) %>%
        group_by(subject_id) %>% arrange(subject_id, intime) %>% filter(row_number()==1)

# Rename column explicitly for disambiguation in pivoted_rrt table
pivoted_rrt <- pivoted_rrt %>% mutate(charttime=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS")) %>%
        rename(charttime.rrt = charttime)

# Get the patients who had RRT prior to KDIGO3
rrt_nokdigo3 <- merge(kdigo3p3, pivoted_rrt, by = "icustay_id", all = TRUE) %>% filter(charttime.rrt<charttime.kdigo3, dialysis_active==1) %>%
        group_by(subject_id) %>% arrange(charttime.rrt) %>% filter(row_number()==1)

# Exclude these patients
kdigo3p4 <- filter(kdigo3p3, !(icustay_id %in% rrt_nokdigo3$icustay_id) )

# Get first active RRT time and type for each icu stay
firstrrt <- pivoted_rrt %>% filter(dialysis_active==1) %>% group_by(icustay_id) %>% arrange(icustay_id, charttime.rrt) %>%
        filter(row_number()==1)

# Left join these first active RRT onto our included patients
kdigo3p5 <- merge(x=kdigo3p4, y=firstrrt, by="icustay_id", all.x = TRUE)

# Add time to first RRT
kdigo3p6 <- kdigo3p5 %>% mutate(kdigo3_to_rrt_time=difftime(charttime.rrt, charttime.kdigo3, units = "hours"))

# Lot of patients meet KDIGO3 based on uo
# Add lab and blood gas values
# Exclude patients who don't met third eligibility criteria from STARTAKI?

# Try to understand and then exclude the patients who had peritoneal RRT and CAVH

temp <- merge(x=kdigo3p6, y=pivoted_lab, by="subject_id", all.x = TRUE)

# Prepare to select last creatinine prior to KDIGO3
p6_lab <- temp %>% rename(charttime.lab = charttime) %>% mutate(charttime.lab=as.POSIXct(charttime.lab, format="%Y-%m-%d %H:%M:%OS")) %>%
        mutate(time_after_kdigo3=difftime(charttime.lab, charttime.kdigo3, unit="hours")) %>%
        arrange(subject_id, time_after_kdigo3)

# Select the patients whose last creatinine prior to KDIGO3 meets STARRT AKI 3rd inclusion criteria
creat_eligible <- p6_lab %>% group_by(subject_id) %>% filter(time_after_kdigo3<=0 & !is.na(CREATININE)) %>%
                      arrange(subject_id, desc(time_after_kdigo3)) %>% filter(row_number()==1) %>%
                      filter(gender=="F" & CREATININE>=1.13 | gender=="M" & CREATININE>=1.47)

# explore time from last creatinine measurement to kdigo3
plot(sort(as.numeric(as.data.frame(creat_eligible)[,"time_after_kdigo3"])), pch=16, col=rgb(0,0,1, alph=.1))
head(sort(as.numeric(as.data.frame(creat_eligible)[,"time_after_kdigo3"])),10)/24

# Get latest lab value with kaliemia prior to KDIGO3 form p6 patients
latest_lab_kaliemia1 <- p6_lab %>% group_by(subject_id) %>% filter(time_after_kdigo3<=0 & !is.na(POTASSIUM)) %>%
        arrange(desc(time_after_kdigo3)) %>% filter(row_number()==1) %>%
        arrange(subject_id) 

# Get blood gas from p6 patients 
temp <- merge(x=kdigo3p6, y=pivoted_bg, by="icustay_id", all.x = TRUE)
p6_bg <- temp %>% rename(charttime.bg = charttime) %>% mutate(charttime.bg=as.POSIXct(charttime.bg, format="%Y-%m-%d %H:%M:%OS")) %>%
        mutate(time_after_kdigo3=difftime(charttime.bg, charttime.kdigo3, unit="hours")) %>%
        arrange(subject_id, time_after_kdigo3)

# Get latest blood gas with kaliemia prior to KDIGO3 form p6 patients
latest_lab_kaliemia2 <- p6_bg %>% group_by(subject_id) %>% filter(time_after_kdigo3<=0 & !is.na(POTASSIUM)) %>%
        arrange(desc(time_after_kdigo3)) %>% filter(row_number()==1) %>%
        arrange(subject_id) 

# Select the patients whose last kaliemia (lab or gas) prior to KDIGO3 is less than 6mmol/L
kaliemia_eligible <- rbind(latest_lab_kaliemia1, latest_lab_kaliemia2) %>% group_by(subject_id) %>% 
        arrange(desc(time_after_kdigo3)) %>% filter(row_number()==1) %>%
        arrange(subject_id) %>% filter(POTASSIUM<=6) 

# Explore and argue for the exclusion of the patients with no kaliemia and no creat in the 24 hours prior to KDIGO3
plot(sort(as.numeric(as.data.frame(kaliemia_eligible)[,"time_after_kdigo3"])/24), pch=16, col=rgb(0,0,1, alph=.1), ylim=c(-5,0))
head(sort(as.numeric(as.data.frame(kaliemia_eligible)[,"time_after_kdigo3"])),10)/24
abline(h=-1, col="red")

plot(sort(as.numeric(as.data.frame(creat_eligible)[,"time_after_kdigo3"])/24), pch=16, col=rgb(0,0,1, alph=.1))#, ylim=c(-5,0))
head(sort(as.numeric(as.data.frame(creat_eligible)[,"time_after_kdigo3"])),10)/24
abline(h=-1, col="red")
# This choice seems reasonable from the plotting.Besides do we really ask ourselves whether or not to start RRT
# we we don't take the trouble to measure creatinine nor kailemia? Probably not.

# Select only the patients with eligible creatinine measured no more than 24hours prior to KDIGO3
creat_eligible2 <- creat_eligible %>% filter(time_after_kdigo3>-24)

# Select only the patients with eligible kaliemia measured no more than 24hours prior to KDIGO3
kaliemia_eligible2 <- kaliemia_eligible %>% filter(time_after_kdigo3>-24)

# Total eligible patients
sum(creat_eligible2$subject_id %in% kaliemia_eligible2$subject_id )

kdigo3p7 <- kdigo3p6 %>%
        filter(subject_id %in% creat_eligible$subject_id, subject_id %in% kaliemia_eligible$subject_id)

hist(as.numeric(kdigo3p7$kdigo3_to_rrt_time)/24, breaks = 0:63)
sum(kdigo3p7$kdigo3_to_rrt_time/24<3, na.rm=T)


## explore de patients who received cavh
cavh <- kdigo3p7 %>% filter(dialysis_type=="CAVH") %>% select(icustay_id)

# OK so CAVH seems like a temporary solution in the 14 patients who had it
# They seem to never have had it for more than a few hours
pivoted_rrt %>% filter(icustay_id %in% cavh$icustay_id[14]) %>% arrange(icustay_id, charttime.rrt)

# Get the last dialysis_type available prior to RRT initiation (dialysis_active==1)
rtt_type <- merge(x=kdigo3p7, y=pivoted_rrt, by="icustay_id", all.x = TRUE) %>%
        select(icustay_id, charttime.kdigo3, charttime.rrt.y, dialysis_present.y, dialysis_active.y, dialysis_type.y) %>%
        mutate(kdigo3_to_rrteval_time=difftime(charttime.rrt.y, charttime.kdigo3, unit="hours")) %>%
        group_by(icustay_id) %>% arrange(icustay_id, kdigo3_to_rrteval_time) %>%
        filter(!is.na(dialysis_type.y), dialysis_type.y!="") %>% filter(row_number()==1) %>%
        rename(first_known_type=dialysis_type.y)

# Premerge rtt_type
rtt_type2 <- rtt_type %>% select(icustay_id, first_known_type)
# Merge
kdigo3p7 <- merge(x=kdigo3p7, y=rtt_type2, by="icustay_id", all.x = TRUE)
kdigo3p7$likely_dialysis_type <- ifelse(!is.na(kdigo3p7$dialysis_type) & kdigo3p7$dialysis_type!="", kdigo3p7$dialysis_type, kdigo3p7$first_known_type)

# Exclude the patients with known chronic kidney disease
temp <- merge(x=kdigo3p7, y=elixhauser_ahrq_v37, by="hadm_id", all.x = TRUE)
kdigo3p8 <- temp %>% filter(renal_failure==0)

## explore de patients who received Peritoneal Dialysis
peritoneal <- kdigo3p8 %>% filter(likely_dialysis_type=="Peritoneal") %>% select(icustay_id)

# OK so these seem to be real patients with CKD...
pivoted_rrt %>% filter(icustay_id %in% peritoneal$icustay_id[11]) %>% arrange(icustay_id, charttime.rrt)

# Get their reports from the noteevent table to understand
