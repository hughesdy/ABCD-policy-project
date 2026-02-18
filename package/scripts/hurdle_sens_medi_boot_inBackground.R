# Dylan E. Hughes, hughesdy@ucla.edu, 02/07/2026
#
# ---- DESCRIPTION ----
# This is just a helper script so that we can the hurdle model bootstrapping to generate null distributions / p-values for the proportions mediated by hurdle models for sensitivity analyses
# 
# Usage:
#     From your terminal, you can run:
#         Rscript hurdle_sens_medi_boot_inBackground.r
#
# ---- SETUP ----
library(dplyr)
library(glmmTMB)

nboot = 1000

proj_dir = "/Users/dylanhughes/Documents/psychosis_gender_project/abcd/manuscript/package/"

output = paste0("outputs/hurdle_sens_medi_null_dists_nboot", nboot, ".csv")
output = file.path(proj_dir, output)

# ========================
# ---- READ DATA
# ========================

## Read in master
master <- read.csv(file.path(proj_dir,'data/masterSheet_20260206.csv'))

# relevel eventname
master$eventname = factor(master$eventname, levels = c("Y1","Y2","Y3","Y4"))

# relevel/recode race
master$race.6level[master$race.6level == "Mixed"] = "Multiracial"

master$race.6level = factor(
  master$race.6level, 
  levels = c("White", "Black", "Asian", "AIAN/NHPI", "Other", "Multiracial")
)

# relevel hispanic variable
master$hispanic = factor(
  master$hispanic,
  levels = c("Non-Hispanic", "Hispanic")
)

# code family id as a factor
master$rel_family_id = as.factor(master$rel_family_id)

# relevel gv step; 06/13/25: change names
master$GV.step = recode(master$GV.step,
                        `CONGRUENT` = "Least GD",
                        `MINORITY` = "Most GD")

master$GV.step = factor(master$GV.step, levels = c("Least GD","1-STEP","2-STEP","Most GD"))


# relevel gv step long
master$GV.step.long = factor(
  master$GV.step.long,
  levels = c("CONGRUENT","1-STEP","2-STEP","MINORITY")
)

# new variable gv.step.long min or not
master$GV.step.long.bin = ifelse(master$GV.step.long %in% c("MINORITY", "1-STEP","2-STEP"), "MINORITY", 
                                 ifelse(master$GV.step.long %in% "CONGRUENT", "CONGRUENT", NA))

## munge df so that we only have data from > timepoint 1
master <- filter(master, eventname != 'baseline_year_1_arm_1') %>%
  filter(!is.na(GV.step))

# set age on a year scale
master$age.year = master$age/12

# recode sex at birth as male and female
master$sex = recode(master$sex,
                    `1` = "M",
                    `2` = "F")

# =======================
# ---- MUNGE DATA
# =======================

year3 <- filter(master, eventname == 'Y3') 

year3_nona <- year3 %>%
  dplyr::select(
    subjectkey,
    eventname,
    STATE, site_id_l, rel_family_id, # groupings
    age, age.year, sex, gender.identity, 
    combined_income, highest_edu,
    ppdms.mean, # puberty
    peq.perp.sum, peq.vic.sum, z.peq.vic.sum, z.peq.perp.sum, # bullying
    GV.step, # gender diversity (main IV)
    gid_tally_prop, gid_tally_prop_bin, # state-level policy
    gini, # gini coefficient
    pqb.distress, z.pqb.distress #pqbc
  ) %>%
  na.omit() %>%
  
  # Rejoin with data that are needed for supplementary analyses
  left_join(., dplyr::select(
    year3,
    subjectkey, race.4level, race.6level, hispanic,
    bpm.total, z.bpm.total,
    gish1m, gish2m, gish3m, gish4m, gish1f, gish2f, gish3f, gish4f
  ), by = "subjectkey")

year3_nona$z.puberty = as.numeric(scale(year3_nona$ppdms.mean))
year3_nona$z.pqb.distress = as.numeric(scale(year3_nona$pqb.distress))
year3_nona$gid_tally_prop_bin = as.factor(year3_nona$gid_tally_prop_bin)

# ---- Resample data and generate nulls
# Bootstrapping to estimate bootstrapped confidence intervals/p-values
prop_conds = c()
prop_zis = c()

for (i in 1:nboot) {
  
  booty = sample(1:nrow(year3_nona), size = nrow(year3_nona), replace = T)
  bootdat = year3_nona[booty,]
  
  init = glmmTMB(
    formula = pqb.distress ~ GV.step + z.puberty + age + sex + highest_edu + (1|site_id_l/rel_family_id), 
    data = bootdat,
    family = ziGamma(link = "log"),
    ziformula = ~ GV.step + z.puberty + age + sex + highest_edu + (1|site_id_l/rel_family_id)
  )
  
  medi = glmmTMB(
    formula = pqb.distress ~ GV.step + z.peq.vic.sum + z.puberty + age + sex + highest_edu + (1|site_id_l/rel_family_id), 
    data = bootdat,
    family = ziGamma(link = "log"),
    ziformula = ~ GV.step + z.peq.vic.sum + z.puberty + age + sex + highest_edu + (1|site_id_l/rel_family_id)
  )
  
  c_cond = summary(init)$coefficients$cond[4,1]
  c_prime_cond = summary(medi)$coefficients$cond[4,1]
  prop_conds[i] = 1 - (c_prime_cond / c_cond)
  
  c_zi = summary(medi)$coefficients$zi[4,1]
  c_prime_zi = summary(medi)$coefficients$zi[4,1]
  prop_zis[i] = 1 - (c_prime_zi / c_zi)
  
  cat(sprintf("\r%d remaining       ", nboot - i))
  flush.console()
}

result = data.frame(
  prop_conds, prop_zis
)

write.csv(result, output, row.names=F)