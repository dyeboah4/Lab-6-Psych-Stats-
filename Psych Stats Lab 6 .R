> model_2 <- lm(MARIJAN1 ~ Age_midpt + X_PRACE2 + X_HISPANC + EDUCA, data = brfss_marijan)
> summary(model_2)
> model_3 <- lm(MARIJAN1 ~ Age_midpt + I(Age_midpt^2) + X_PRACE2 + X_HISPANC + EDUCA, data = brfss_marijan)
> summary(model_3) 
> to_be_predicted_2 <- data.frame(Age_midpt = 25:55, X_PRACE2 = "Black or African American",
                                  +                                 X_HISPANC = "no", EDUCA = "Grade 12 or GED (High school graduate)")
> 
  > to_be_predicted_2$yhat <- predict(model_2, newdata = to_be_predicted_2) 
  > model_4 <- lm(MARIJAN1 ~ Age_midpt*(X_PRACE2 + X_HISPANC) + I(Age_midpt^2)*(X_PRACE2 + X_HISPANC) + X_PRACE2 + X_HISPANC + EDUCA, data = brfss_marijan)
  > summary(model_4)
  > anova(model_2,model_3,model_4) 
  > to_be_predicted_2$yhat3 <- predict(model_3, newdata = to_be_predicted_2)
  > to_be_predicted_2$yhat4 <- predict(model_4, newdata = to_be_predicted_2)
  > 
    > 
    > d_for_graphing <- data.frame(Age_midpt = 25:55, 
                                   +                              to_be_predicted_2$yhat,
                                   +                              to_be_predicted_2$yhat3,
                                   +                              to_be_predicted_2$yhat4)
    > 
      > p_predvals <- ggplot(d_for_graphing, aes(Age_midpt))
      > p_predvals + geom_line(aes(y = to_be_predicted_2.yhat)) + 
        +     geom_line(aes(y = to_be_predicted_2.yhat3), color = 'blue') +
        +     geom_line(aes(y = to_be_predicted_2.yhat4), color = 'red') 
      > quantile(brfss_marijan$MARIJAN1, probs = c(0.99,0.95,0.94,0.93,0.92,0.91,0.9)) 
      > brfss_marijan$MARIJAN_factor <- as.factor(brfss_marijan$MARIJAN1)
      > summary(brfss_marijan$MARIJAN_factor) 
      > brfss_marijan$Marijan_01 <- as.numeric(brfss_marijan$MARIJAN1 > 0)
        > 
        > model_5 <- lm(Marijan_01 ~ Age_midpt*(X_PRACE2 + X_HISPANC) + I(Age_midpt^2)*(X_PRACE2 + X_HISPANC) + X_PRACE2 + X_HISPANC + EDUCA, data = brfss_marijan)
        > summary(model_5) 
        > model_7 <- lm(Marijan_01 ~ Age_midpt*(X_PRACE2 + X_HISPANC) + I(Age_midpt^2)*(X_PRACE2 + X_HISPANC) + X_PRACE2 + X_HISPANC + EDUCA + GENHLTH + LSATISFY, data = brfss_marijan)
        > summary(model_7) 
        
No Lab Partner today, Rafeal did not come to class. After re writing the code from Lab 5, I was able to complete the rest of the code for Lab 6 and 
starting looking at my own interactions. A interaction I wanted to focus on was how "General Health" and "Life Satification" interacts with the other three varibales, (Race, Hispanic, and Education) for Marijuana use. 
For General Health it seemed that the more people use Marijuana the worst their health is and the better they rate their health the less Marijuana they will be using. This isn't a direct correlation
however it is good to know for example, if your a substance abuse counselor, that for your clients who use Marijuana they will be more likely to have health issues which can effect how you engage them in counseling.
For Life satifaction there were a statitcally signifant amount of responses for satified and also a statitically signifant amount of responses for i dont know and refused responses. Before running the code I assumed that 
more satified people will be using marijuana more than the not satified people due to the euphoric effects of the drug. However the data shows that the responeses for Marijuana users and their life satification most users
did not answer or did not know which shows that there are most likelty other things interacting with satification beside Marijuana use that have an effect on people's answers. 
