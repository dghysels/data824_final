
proc import 
	datafile="D:\Users\dongh\source\repos\kumc\county_health_analytic_data2022.csv"
	out=ch_analytics_raw
	replace; 
run;
	


data ch_analytics (keep=County_name
						State
						Adult_obesity 
						Adult_smoking 
						Broadband_access
						Child_mortality
						Free_or_reduced_lunch_eligible
						Children_in_poverty
						Excessive_drinking
						Food_insecurity
						High_School_completion
						Home_ownership
						Income_inequality
						Life_expectancy
						Math_scores
						Median_income
						Poor_fair_health
						Premature_death
						Primary_care_physicians
						reading_scores
						School_funding_adequacy
						Severe_housing_problems
						Some_college
						Teen_births
						Unemployment
						Uninsured_children);
			rename Name = County_name
				State_abbreviation=state
				Adult_obesity_raw_value = Adult_obesity
				Adult_smoking_raw_value = Adult_smoking
				Broadband_access_raw_value=Broadband_access
				Child_mortality_raw_value = Child_mortality
				Children_eligible_for_free_or_re = Free_or_reduced_lunch_eligible
				Children_in_poverty_raw_value = Children_in_poverty
				Excessive_drinking_raw_value = Excessive_drinking
				Food_insecurity_raw_value = Food_insecurity
				High_school_completion_raw_value = High_School_completion
				Home_ownership_raw_value = Home_ownership
				Income_inequality_raw_value = Income_inequality
				Life_expectancy_raw_value = Life_expectancy
				Math_scores_raw_value = Math_scores
				Median_household_income_raw_valu = Median_income
				Poor_or_fair_health_raw_value = Poor_fair_health
				Premature_death_raw_value = Premature_death
				Primary_care_physicians_raw_valu = Primary_care_physicians
				Reading_scores_raw_value = reading_scores
				School_funding_adequacy_raw_valu = School_funding_adequacy
				Severe_housing_problems_raw_valu = Severe_housing_problems
				Some_college_raw_value = Some_college
				Teen_births_raw_value = Teen_births
				Unemployment_raw_value = Unemployment
				Uninsured_raw_value = Uninsured
				Uninsured_children_raw_value = Uninsured_children;
	set work.ch_analytics_raw;
run;

proc contents data=ch_analytics;
run;
	
proc univariate data=ch_analytics plots;
	var Poor_fair_health High_School_Completion;
run;
