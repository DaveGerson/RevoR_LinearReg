##
#http://lib.stat.cmu.edu/datasets/colleges/
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

collegeData_LM <- rxLinMod( graduation_rate  ~ ft_percentage + full_prof_percentage + alumni_donation_percent + student_faculty_ratio + Average_Math_SAT + Average_Verbal_SAT + Average_Combined_SAT + Average_ACT + F(Public_Private_Flag) + phd_percent_of_faculty ,data = collegeData, verbose = 1, dropMain = FALSE, coefLabelStyle = "R",transforms = transforms, variableSelection = varsel)

collegeData_DecisionTree <- rxDTree( graduation_rate  ~ ft_percentage + full_prof_percentage + alumni_donation_percent + student_faculty_ratio + Average_Math_SAT + Average_Verbal_SAT + Average_Combined_SAT + Average_ACT + Type + phd_percent_of_faculty ,data = collegeData, transforms=transforms)	

for(i
collegeData_DecisionTree <- rxDTree( graduation_rate  ~ ft_percentage + full_prof_percentage + alumni_donation_percent + student_faculty_ratio + Average_Math_SAT + Average_Verbal_SAT + Average_Combined_SAT + Average_ACT + Type + phd_percent_of_faculty ,data = collegeData, transforms=transforms,pruneCp="auto",cp=rxDTreeBestCp(collegeData_DecisionTree))

Item_Cube<-rxCube(salesDollars ~ F(chain) : F(dept), data = ItemInfo_xdf,removeZeroCounts = TRUE,returnDataFrame = TRUE)
storeFile<-"E:/Revo_R_Data/Store_Info/Store_data.csv"
storeText <- RxTextData(storeFile, delimiter = ",")
StoreInfo_xdf<-rxImport(storeText,outFile = "E:/Revo_R_Data/Store_Info/store_info.xdf",overwrite=TRUE)
names(StoreInfo_xdf)<-