
#====================================================================================================================================================#
# notes: generates input sets for protective factors analysis
#====================================================================================================================================================#

#==================================================#
# ==== p1: load packages and clear objects/log ====
#==================================================#

# load easimple and clear objects/log
library(easimple)
ea_start()

# load other packages
library(data.table)
library(xlsx)
library(psych)
library(ggplot2)
library(stargazer)
#========================#
# ==== p2: set parms ====
#========================#

# set export toggle
p_opt_exp <- FALSE

# set timestamp
p_timestamp <- ea_timestamp()

# set output directories
p_dir_in <- "/projects/core/ies_sel_grant/year_1_2324/study_5/30_create_input_sets/longitudinal_sel/most_recent/"
p_dir_out <- "/projects/core/ies_sel_grant/year_1_2324/study_5/researcher/shu_study5/protective_factor/00_input_sets/"


# set parameters
data_set <- "sbac_2122_1819"
year_set <- c("2122","1819")
grade_set <- c("04","05","08")

#========================#
# ==== p3: load data ====
#========================#

in_data <- ea_load(paste0(p_dir_in, paste0("protective_",data_set, ".rdata")))
main_data <- copy(in_data)

#===========================#
# ==== prepare the data ====
#===========================#
colnames(main_data) <- gsub(paste0("_", year_set[1]),"_post", colnames(main_data))
colnames(main_data) <- gsub(paste0("_", year_set[2]),"", colnames(main_data))

# unique observations
uniqueN(main_data, by = "student_id")
nrow(main_data)
# 50735

# rename
colnames(main_data) <- gsub("sbac_scale_score_", "", colnames(main_data))

colnames(main_data) <- gsub("grade_level_code","grade", colnames(main_data))
colnames(main_data) <- gsub("positive_growth_mindset", "gm", colnames(main_data))
colnames(main_data) <- gsub("self_efficacy", "se", colnames(main_data))
colnames(main_data) <- gsub("self_management", "sm", colnames(main_data))
colnames(main_data) <- gsub("social_awareness", "sa", colnames(main_data))
colnames(main_data) <- gsub("sel_irt_", "", colnames(main_data))
colnames(main_data) <- gsub("sel_raw_mean_", "raw_", colnames(main_data))
colnames(main_data) <- gsub("demo_", "", colnames(main_data))
colnames(main_data) <- gsub("_y", "", colnames(main_data))
colnames(main_data) <- gsub("race_", "", colnames(main_data))
old <- c("cc_belonging_raw_mean", "cc_climate_support_raw_mean", "cc_knowledge_fairness_raw_mean","cc_safety_raw_mean")
new <- c("belong", "support","fair", "safe")
setnames(main_data, old, new)


# recode the demographics
# learn the main demo variables
ea_table(main_data, "race")
ea_table(main_data, "gender")
ea_table(main_data, "frl")
ea_table(main_data, "ell")
ea_table(main_data, "swd")


# gender
main_data[, gender := as.character(gender)]
main_data[gender == "f", gender := 1]
main_data[gender == "m", gender := 0]
main_data[, gender := as.numeric(gender)]


# ELL
main_data[, ell := as.character(ell)]
main_data[ell == "el_pure", ell := 1]
main_data[ell == "rfep", ell := 1]
main_data[ell == "none", ell := 0]
main_data[, ell := as.numeric(ell)]


# FRL
main_data[, frl := as.character(frl)]
main_data[frl == "none", frl := 0]
main_data[frl == "reduced", frl := 1]
main_data[frl == "free", frl := 1]
main_data[, frl := as.numeric(frl)]


# SWD
main_data[, swd := as.numeric(swd)]

# Race
main_data$race <- factor(main_data$race, levels = c("white", "hispanic", "black",  "asian", "other"))

# make school_code a factor variable
main_data[, school_code := as.factor(school_code)]
main_data[, school_code_post := as.factor(school_code_post)]


# remove extra variables
meta_cols <- grep("meta", colnames(main_data), value = T) 
sem_cols <- grep("sem", colnames(main_data), value = T) 
rate_50_cols <- grep("50", colnames(main_data), value = T) 
main_data <- ea_subset(main_data, in_vars_remove = c(meta_cols, sem_cols, rate_50_cols), opt_print = 0)

#===================================================#
# ==== standardize test scores and SEL measures ====
#===================================================#

# standardize pre measure within pre-grade
var_list <- c("math", "ela", "gm", "sm","se", "sa")
zscore_list <- lapply(var_list, function(p_var){
  main_data[, var1 := scale(get(p_var)), by = "grade"]
  data <- main_data[, "var1"]
  setnames(data, "var1", paste0("z_", p_var))
  return(data)
})
zscore_data <- do.call(cbind, zscore_list)
main_data <- cbind(main_data, zscore_data)


# standardize post test scores within post-grade
var_list <- c("math_post", "ela_post")
zscore_list_post <- lapply(var_list, function(p_var){
  main_data[, var1 := scale(get(p_var)), by = "grade_post"]
  data <- main_data[, "var1"]
  setnames(data, "var1", paste0("z_", p_var))
  return(data)
})
zscore_data_post <- do.call(cbind, zscore_list_post)
main_data <- cbind(main_data, zscore_data_post)

names(main_data)
#===================================#
# ==== set up analytics samples ====
#===================================#

# sub1_data <-  main_data[!is.na(math_post)&!is.na(math)&!is.na(gm)&!is.na(sm)&!is.na(se)&!is.na(sa)]
# sub2_data <-  main_data[!is.na(ela_post)&!is.na(ela)&!is.na(gm)&!is.na(sm)&!is.na(se)&!is.na(sa)]
# sub3_data <-  main_data[!is.na(math_post)&!is.na(math)&!is.na(gm)&!is.na(sm)&!is.na(se)&!is.na(sa)&!is.na(ela)&!is.na(ela_post),]
# 
# nrow(sub1_data)
# # 34597
# nrow(sub2_data)
# # 34787
# nrow(sub3_data)
# # 34133

# choose those with no missing values of sel and math or ela
main_data <-  main_data[!is.na(gm)&!is.na(sm)&!is.na(se)&!is.na(sa)]
main_data[!is.na(math_post)&!is.na(math), has_math := 1 ]
main_data[!is.na(ela_post)&!is.na(ela), has_ela := 1 ]
main_data <- main_data[has_ela==1|has_math==1,]
nrow(main_data)
# 35251


# choose study grades
main_data<- main_data[grade==grade_set[1]|grade==grade_set[2]|grade==grade_set[3],]
nrow(main_data)
# 35242

#=====================================#
# ==== generate interaction terms ====
#=====================================#

# Test scores and SEL constructs
var_list <- c("math", "ela", "gm", "sm","se", "sa", "z_math", "z_ela", "z_gm", "z_sm","z_se", "z_sa")
# gender interaction terms
gender_inter_list <- lapply(var_list, function(p_var){
  main_data[, var1 := get(p_var) * gender]
  main_data[, var2 := get(p_var) * (1-gender)]
  data <- main_data[, c("var1", "var2")]
  setnames(data, "var1", paste0(p_var, "_female"))
  setnames(data, "var2", paste0(p_var, "_male"))
  return(data)
})
gender_inter <- do.call(cbind, gender_inter_list)
main_data <- cbind(main_data, gender_inter)


# race intercation terms
race_list <- c("white", "black", "hispanic", "asian", "other")
race_inter_list <- lapply(race_list, function(p_race){
  inter_list <- lapply(var_list, function(p_var){
    main_data[,var := get(p_var)*get(p_race)]
    data <- main_data[, "var"]
    setnames(data, "var", paste0(p_var, "_", p_race))
    return(data)
  })
  inter <- do.call(cbind, inter_list)  
  return(inter)
})
race_inter <- do.call(cbind, race_inter_list)
main_data <- cbind(main_data, race_inter)


# swd interaction terms
swd_inter_list <- lapply(var_list, function(p_var){
  main_data[, var1 := get(p_var) * swd]
  main_data[, var2 := get(p_var) * (1-swd)]
  data <- main_data[, c("var1", "var2")]
  setnames(data, "var1", paste0(p_var, "_swd"))
  setnames(data, "var2", paste0(p_var, "_nonswd"))
  return(data)
})
swd_inter <- do.call(cbind, swd_inter_list)
main_data <- cbind(main_data, swd_inter)


# frl interction terms
frl_inter_list <- lapply(var_list, function(p_var){
  main_data[, var1 := get(p_var) * frl]
  main_data[, var2 := get(p_var) * (1-frl)]
  data <- main_data[, c("var1", "var2")]
  setnames(data, "var1", paste0(p_var, "_frl"))
  setnames(data, "var2", paste0(p_var, "_nonfrl"))
  return(data)
})
frl_inter <- do.call(cbind, frl_inter_list)
main_data <- cbind(main_data, frl_inter)


# ell interaction term 
ell_inter_list <- lapply(var_list, function(p_var){
  main_data[, var1 := get(p_var) * ell]
  main_data[, var2 := get(p_var) * (1-ell)]
  data <- main_data[, c("var1", "var2")]
  setnames(data, "var1", paste0(p_var, "_ell"))
  setnames(data, "var2", paste0(p_var, "_nonell"))
  return(data)
})
ell_inter <- do.call(cbind, ell_inter_list)
main_data <- cbind(main_data, ell_inter)

#====================================#
# ==== generate high order terms ====
#====================================#

var_list <- c("math", "ela", "gm", "sm","se", "sa", "z_math", "z_ela", "z_gm", "z_sm","z_se", "z_sa")

var_hiorder_list <- lapply(var_list, function(p_var){
  main_data[, var2 := get(p_var)*get(p_var) ]
  main_data[, var3 := get(p_var)*get(p_var)*get(p_var) ]
  data <- main_data[, c("var2", "var3")]
  setnames(data, "var2", paste0(p_var,"2"))
  setnames(data, "var3", paste0(p_var,"3"))
  return(data)
})
var_hiorder <- do.call(cbind, var_hiorder_list)
main_data <- cbind(main_data, var_hiorder)


#=================#
# ==== export ====
#=================#

ea_save(main_data, p_dir_out, paste0("protective_",data_set, ".rdata"), p_timestamp)




