pivoted_rrt <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/pivoted_rrt.csv")
crrt_durations <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/crrt_durations.csv")
kdigo_stages <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/kdigo_stages.csv")
icustay_detail <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/icustay_detail.csv")

library(dplyr)

# Get the first KDIGO3 event per icustay
kdigo3p1 <- kdigo_stages %>% filter(aki_stage==3) %>%
                  group_by(icustay_id) %>% arrange(charttime) %>%
                  filter(row_number()==1) %>% rename(charttime.kdigo3 = charttime)

# Merge ICU stays with KGIGO3 events with the icustay_detail table, then exclude the patients that were over 18 at hospital admission
kdigo3p2 <- merge(icustay_detail, kdigo3p1, by="icustay_id") %>% filter(admission_age>=18)

# A patient can't be included again after their first inclusion
kdigo3p3 <- kdigo3p2 %>% group_by(subject_id) %>% arrange(subject_id, admittime) %>% filter(row_number()==1)

# Rename column explicitly for disambiguation
pivoted_rrt <- pivoted_rrt %>% rename(charttime.rrt = charttime)

# Get the patients who had RRT prior to KDIGO3
rrt_nokdigo3 <- merge(kdigo3p3, pivoted_rrt, by = "icustay_id", all = TRUE) %>% filter(charttime.rrt<charttime.kdigo3, dialysis_active==1) %>%
        group_by(subject_id) %>% arrange(charttime.rrt) %>% filter(row_number()==1)

# Exclude these 602 patients
kdigo3p4 <- filter(kdigo3p3, !(subject_id %in% rrt_nokdigo3$subject_id) )

# Get first active RRT time and type
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




