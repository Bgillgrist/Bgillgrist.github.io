# Bayesian Linear Regression Project

A Bayesian logistic regression model predicting the win percentage of NFL teams from cap space management.

# Project Summary: 

The variable I wanted to predict throughout this project is the win percentage for each NFL team for the upcoming NFL season, however the main goal of this project is to explore the relationship between the management of cap space and team performance. My candidate predictor variables are the previous season win percentage and the percent of cap space specific position groups take up. The positions I used were quarterbacks, running backs, wide receivers, offensive linemen, and defense. For example, the Buffalo Bills in 2023 had two quarterbacks on roster: Josh Allen ($18,636,281) and Kyle Allen ($1,092,500). The total of the quarterbacks’ salary divided by the team salary cap for that season will equal 0.0866 (Or 8.66% of the team salary cap). This is calculated for every position group listed for every team seasons 2020-2023. 
	
I applied a Normal Bayesian regression model with this data set using informative priors on the intercept and previous win percentage variables, and semi-uninformative priors on each of the salary cap percentages. I know that my response variable falls only between 0-1 so I ensured that my intercept and betas have means very close to zero and small variances to keep the values very close to this range. After using a Gibbs sampler to calculate which models have the highest probability, I found that the best model with these variables does not include the salary percentage for running backs and wide receivers. After creating a new model, I got an MAE of 0.088 which means that my predictions will be 8.8% away from the true win percentage. 

While I can never completely predict win percentage for an upcoming season, this MAE does tell me that there is a relationship between managing cap space and team performance. Further analysis into more season predictor variables would obviously provide a better prediction for upcoming performance but this analysis here tells us that an NFL general manager negotiating contracts directly influences the season outcome. We can also see that a good defense is one of the best predictors compared to spending big on any other position group. Managing the cap space well will help a team perform better and negotiating good deals is key to a team’s performance.

# Full Project Report Document: 

- [Project Report](Project.Report)  
