##
#http://lib.stat.cmu.edu/datasets/colleges/
library(car)
aaup_txt<-"E:/Revo_R_Data/College_Data/aaup.csv"
usnews_txt<-"E:/Revo_R_Data/College_Data/usnews.csv"
aaup <- RxTextData(aaup_txt, delimiter = ",",missingValueString = "*")
usnews<-RxTextData(usnews_txt, delimiter = ",",missingValueString = "*")
aaup_xdf<-rxImport(aaup,outFile = "E:/Revo_R_Data/College_Data/aaup.xdf",overwrite=TRUE)
names(aaup_xdf)<-c("FICE","College Name","State","Type","average_salary_full_professor","average_salary_associate_professor","average_salary_assistant_professor","average_salary_ALL","average_comp_full_professor","average_comp_associate_professor","average_comp_assistant_professor","average_all_ranks","number_of__full_professors","number_of_associate_professors","number_of_assistant_professors","number_of_instructors","number_of_total_faculty")
usnews_xdf<-rxImport(usnews,outFile = "E:/Revo_R_Data/College_Data/usnews.xdf",overwrite=TRUE)
names(usnews_xdf)<-c("FICE","College Name","State","Public_Private_Flag","Average_Math_SAT","Average_Verbal_SAT","Average_Combined_SAT","Average_ACT","Quartile_1_MATH_SAT","Quartile_3_MATH_SAT","Quartile_1_VERBAL_SAT","Quartile_3_VERBAL_SAT","Quartile_1_ACT","Quartile_3_ACT","Number_of_applicants_received","Number_of_applicants_accepted","Number_of_applicants_enrolled","pct_new_students_in_top_10_percent_of_HS","pct_new_students_in_top_25_percent_of_HS","FT_Undergrads","number_of_parttime_undergrads","in_state_tuition","out_of_state_tuition","room_and_board","room_cost","board_costs","additional_fees","estimate_Book_Costs","estimated_personal_spending","phd_percent_of_faculty","percent_of_faculty_w_terminal_degrees","student_faculty_ratio","alumni_donation_percent","instructional_expenses","graduation_rate")
collegeData<-rxMerge(aaup_xdf,usnews_xdf,matchVars = "FICE",type="inner")

#Stepwise Regression to Determine Important Factors in graduation rate
varsel <- rxStepControl(method = "stepwise",stepCriterion = "AIC",refitEachStep = TRUE,test = "Chisq")
transforms <- expression(list(
    full_prof_percentage = number_of__full_professors/number_of_total_faculty,
    ft_percentage = FT_Undergrads/(FT_Undergrads+number_of_parttime_undergrads)))
	#individual assessment of independent variables, not maintained on the transformation that boosted model performance.
	collegeData_LM <- rxLinMod( graduation_rate^2  ~ log(student_faculty_ratio),data = collegeData, verbose = 1, dropMain = FALSE,transforms = transforms, covCoef = TRUE)
	collegeLM_pred<-rxPredict(collegeData_LM,data = collegeData,computeResiduals=TRUE)
	summary(collegeData_LM)
	plot(collegeLM_pred)
	plot(collegeLM_pred$graduation_rate_Pred,collegeData$graduation_rate)
	#The above model increases R2 to .18
	#Review final variable selection
collegeData_LM <- rxLinMod( graduation_rate^2  ~ ft_percentage + log(student_faculty_ratio) + Average_Verbal_SAT + F(Public_Private_Flag) + phd_percent_of_faculty ,data = collegeData, verbose = 1, transforms = transforms,variableSelection = varsel)
#The summary Stats indicate that student facult ratio, Full time percentage, and phd_percent are all poor predictors.  Without the boost from the interaction with student faculty ratio, squaring the DV is unnecessary.
collegeData_LM <- rxLinMod( graduation_rate  ~ Average_Verbal_SAT + F(Public_Private_Flag) ,data = collegeData, verbose = 1, transforms = transforms,variableSelection = varsel)
collegeLM_pred<-rxPredict(collegeData_LM,data = collegeData,computeResiduals=TRUE)
#Plot of residuals Data appears normal, verified with density function
plot(collegeLM_pred)
residual_collegeLM_pred<-collegeLM_pred$graduation_rate_Resid
collegeLM_pred_densprep<-collegeLM_pred$graduation_rate_Resid[!is.na(collegeLM_pred$graduation_rate_Resid)]
collegeLM_densityFunction<-density(collegeLM_pred_densprep)
plot(density(collegeLM_pred_densprep), type = "l", col = "blue", main = "Density Plot of Residuals", xlab = "residual", ylab = "probability density")
#The model has a slight positive skew but for the most part is very normal


#Standard LM
as.factor(collegeData$Public_Private_Flag)
collegeData_LM <- lm( graduation_rate  ~ Average_Verbal_SAT + Public_Private_Flag  ,data = collegeData)
summary(collegeData_LM)
#QQ plot shows the range of data, in this case this is a very normal QQ plot.
qqPlot(collegeData_LM, main="QQ Plot")
#influencePlot shows key points in model
influencePlot(collegeData_LM, id.method = "identify")
#Passes Tests except for Global Statistic and Kurtosis
gvlma(collegeData_LM)
#Showing Multi-Collinearity
Multi_collinearity<-lm(graduation_rate~Average_Math_SAT+Average_Verbal_SAT+Average_Combined_SAT, data=collegeData) 
summary(Multi_collinearity) #Shows approximately the same R2 and the values independently
vif(Multi_collinearity)

#Decision Forest Model
collegeData_DecisionForest <- rxDTree( graduation_rate ~ Average_Verbal_SAT + Average_Math_SAT + Average_ACT ,data = collegeData, transforms=transforms)	
plot(createTreeView(collegeData_DecisionForest))
DC_pred<-rxPredict(collegeData_DecisionForest,data = collegeData,computeResiduals=TRUE)
plot(DC_pred)
residual_collegeLM_pred<-collegeLM_pred$graduation_rate_Resid
collegeLM_pred_densprep<-collegeLM_pred$graduation_rate_Resid[!is.na(collegeLM_pred$graduation_rate_Resid)]
collegeLM_densityFunction<-density(collegeLM_pred_densprep)
plot(density(collegeLM_pred_densprep), type = "l", col = "blue", main = "Density Plot of Residuals", xlab = "residual", ylab = "probability density")