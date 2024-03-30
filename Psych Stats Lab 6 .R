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


The article that I read this week for HW was "Smartphones, social media use and youth mental health" by Elia Abi-Jaoude MSc MD, Karline Treurnicht Naylor MPH MD, Antonio Pignatiello MD. This was a review paper, that looked at a
multitude of studies on social media usage and teens mental health. The researchers looked at various cross-sectional longitudinal, and emprirical studies 
that looked at social media and smartphone use among North American youth. Their research finds that North American youth spend a lot of time on social media and that there has been a rise of social media usage
and cell phone usage over the last couple of years. Their findings show that social media use can lead to increase in mental distress, self injurous behaviors, and negative self view. 
Their reasoning for this came from how social media will "normalize" self harm and even suicide. Another factor that the reseachers acknowledge is that youth are using social media and multiple apps during the night
and are sleeping less than normal, having negative effects on cognitive control and academic performance. Their statstical data the paper is using comes from various cross-sectional longitudinal, and emprirical studies that have looked
at social media/cell phone usage and North American Teens. I hope to use the data here for my project and create charts and grapghs explaining how social media use are effecting North American youth negatively. I want to specifically look at
the relationship between academics, life statifaction, mental health, physical health, sleep patterns and how they compare wit social media usage among youth. 
