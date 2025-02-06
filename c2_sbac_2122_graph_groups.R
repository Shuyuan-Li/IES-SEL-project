
#====================================================================================================================================================#
# notes: plots estimates for different group pairs
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
library(RColorBrewer)
library(stringr)
#========================#
# ==== p2: set parms ====
#========================#

# set export toggle
p_opt_exp <- FALSE

# set timestamp
p_timestamp <- ea_timestamp()

# set output directories
p_dir_in <- "/projects/core/ies_sel_grant/year_1_2324/study_5/researcher/shu_study5/protective_factor/excel/sbac_2122/"
p_dir_out <- "/projects/core/ies_sel_grant/year_1_2324/study_5/researcher/shu_study5/protective_factor/ggplot/sbac_2122/"


# other key parameters
data_set <- "sbac_2122_1819"
grade_set <- c("04", "05", "08")

demo_set <- c("gender", "swd", "frl", "ell", "race")
subject_set <- c("z_math", "z_ela")
model_set <- c( "main", "fe", "hipoly1", "testinter")

#========================#
# ==== p3: load data ====
#========================#

grade_set <- c("4", "5","8")

for (p_model in model_set){
# p_model <- "main"
in_data_subject <- lapply(subject_set, function(p_subject){
  in_data_demo <- lapply(demo_set, function(p_demo){
    in_data_grade <- lapply(1:3, function(i){
      in_data<- as.data.table(read.xlsx(paste0(p_dir_in, p_subject, "_", p_demo,"_", p_model, ".xlsx"), sheetName = paste0("set_", i)))
      colnames(in_data) <- c("var", "est", "sd","t", "p")
      return(in_data)
    })
    names(in_data_grade) <- grade_set
    in_data <- rbindlist(in_data_grade, idcol = "grade")
    return(in_data)
  })
  names(in_data_demo) <- demo_set
  in_data <- rbindlist(in_data_demo, idcol = "demo" )
  return(in_data)
})

names(in_data_subject) <- subject_set
in_data <- rbindlist(in_data_subject, idcol = "subject" )

main_data <- copy(in_data)
#=========================================#
# ==== prepare the data for the plots ====
#=========================================#

main_data <- main_data[grepl("z_gm_|z_sm_|z_se_|z_sa_", main_data$var),]
main_data <- main_data[!grepl("_other", main_data$var),]
# grepl returns a logic vector
# grep returns the matches

main_data[, construct := str_extract(var, "^[a-z]+_[a-z]+")]
main_data[, group := str_extract(var, "[a-z]+$")]

main_data[ p > 0.1, star := ""]
main_data[ p <= 0.1, star := "*"]

main_data[ p <= 0.05, star := "**"]
main_data[ p <= 0.01, star := "***"]

main_data[, est := round(est,4)]
main_data[, est_star := paste(est,star)]


# define group id, g1 disadvantaged, g2 advantaged
main_data[group=="female"|group=="swd"|group=="frl"|group=="ell", group_id := "g1"]
main_data[group=="male"|group=="nonswd"|group=="nonfrl"|group=="nonell", group_id:="g2"]

# rename demo, gender to female
main_data[demo == "gender", demo := "Female vs. Male"]
main_data[demo == "frl", demo := "FRL vs. Non-FRL"]
main_data[demo == "ell", demo := "ELL vs. Non-ELL"]
main_data[demo == "swd", demo := "SWD vs. Non-SWD"]
main_data[demo == "race", demo := "Race Groups"]

main_data[, demo := factor(demo, levels = c("FRL vs. Non-FRL", "ELL vs. Non-ELL", "SWD vs. Non-SWD", "Female vs. Male","Race Groups"))]
main_data[, subject := factor(subject, levels = c("z_math", "z_ela"))]
main_data[, construct := factor(construct, levels = c("z_gm", "z_sm","z_se", "z_sa"))]

#============================#
# ==== generate the plot ====
#============================#

# define palette for race plot
palette <- brewer.pal(5, "YlGnBu")
palette <- palette[-1]


# generate plots by construct
construct_set <- c("z_gm", "z_sm", "z_se", "z_sa")

for (p_construct in construct_set){
  est_plot <- ggplot(main_data[construct==p_construct&demo!="Race Groups",], aes(grade, est, fill = group_id)) + geom_bar(stat = "identity", position = "dodge") + facet_wrap(~ demo + subject, nrow = 4) +
    geom_text(aes(label = est_star), size = 2, position =  position_dodge(width = 0.9))+  scale_fill_manual(values = c("cadetblue", "aquamarine4"))+ 
    labs(title = paste0(p_construct)) + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
  ggsave(paste0(p_dir_out,p_construct,"_",p_model,".png"), est_plot, width =8, height = 12)
}


# generate the plot for major race groups
main_data_race <- main_data[demo=="Race Groups",]
main_data_race[, group := factor(group, levels = c("black", "hispanic", "white", "asian"))]
est_plot <-  ggplot(main_data_race, aes(grade, est, fill = group)) + geom_bar(stat = "identity", position = "dodge")  +facet_wrap(~ construct+subject, nrow = 4) +
  geom_text(aes(label = est_star), size = 2, position =  position_dodge(width = 0.9))+  scale_fill_manual(values = palette) + theme(legend.position = "bottom")
ggsave(paste0(p_dir_out, "z_race_",p_model,".png"), est_plot, width = 10, height = 14)

}




