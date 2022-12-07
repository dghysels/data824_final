

library(shiny)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(pheatmap)

#dfall<- read.csv("../selected_county_health_data2022.csv")
dfall <- read.csv("https://raw.githubusercontent.com/dghysels/data824_final/main/selected_county_health_data2022.csv")

options(scipen = 100)

shinyServer(function(input, output) {
  
   output$stateout <-renderText(input$state)

#---- DEMOGRAPHCICS ----------------     

   #display a message to the user indicating the number of counties
   output$demo_alert <- renderText({
     pop_rows <- nrow(dfall %>% filter(State.Abbreviation == input$state))
     paste(pop_rows, "Counties in the state of ", input$state)
   })
   
   #show population by county ordered from lowest population to highest
   output$popByCounty <- renderPlot({
      
      df_pop <- dfall %>% filter(State.Abbreviation == input$state)
      
      df_pop %>%
        arrange(desc(Population )) %>%
        slice_tail(n=input$topn) %>%
        ggplot(aes(x = Name, y=Population )) + 
        geom_bar(aes(x=fct_reorder(Name, Population ), 
                     y=Population ),
                 stat = "identity", 
                 position = "stack") + 
        ylab("Population") + 
        xlab("County") + 
        scale_y_continuous(labels = label_comma()) + 
        ggtitle("State Population by County (ordered by increasing population)") + 
        theme(axis.text.y = element_text(size = 8),
              axis.text.x = element_text(angle=75,size = 8, vjust = 1, hjust = 1),
              axis.title = element_text(size = 12),
              plot.title = element_text(size = 16, face = "bold"),
              panel.background = element_rect(fill = 'white'),
              panel.grid.major = element_line(color = 'light grey', size=0.25, linetype = 'solid'),
              #panel.grid.minor = element_line(color = 'light blue', size = .5),
              panel.border = element_rect(color = "light grey", fill = NA, size = 0.5))
        
      
    })
   
   #bar graph of percent rural, ordered by increasing population to
   #match the population bar graph
   output$ruralByCounty <- renderPlot({

     df_rural <- dfall %>% filter(State.Abbreviation == input$state)
     
     df_rural %>%
       arrange(desc(Population) ) %>%
       slice_tail(n=input$topn) %>%
       ggplot(aes(x = Name, y=Rural)) + 
       geom_bar(aes(x=fct_reorder(Name, Population), y=Rural),
                stat = "identity", position = "stack") + 
       ylab("Rural (%)") + 
       xlab("County") + 
       ggtitle("Percent Rural by County (ordered by increasing population)") + 
       scale_y_continuous(labels = label_comma()) + 
       theme(axis.text.y = element_text(size = 8),
             axis.text.x = element_text(angle=75,size = 8, vjust = 1, hjust = 1),
             axis.title = element_text(size = 12),
             plot.title = element_text(size = 16, face = "bold"),
             panel.background = element_rect(fill = 'white'),
             panel.grid.major = element_line(color = 'light grey', size=0.25, linetype = 'solid'),
             #panel.grid.minor = element_line(color = 'light blue', size = .5),
             panel.border = element_rect(color = "light grey", fill = NA, size = 0.5))
   
   })
   
#----- POPULATION HEALTH -------------
   output$pophealth_alert <- renderText({
     "Note: US view is not affected by state selection"
   })
   
   #heatmap of health related behaviours
   output$healthHeatmap <- renderPlot({
     
     df_hm <- dfall %>% 
       group_by(State.Abbreviation) %>% 
       summarise(`Premature death` = mean(Premature.death),
                `Life Expectancy` = mean(Life.expectancy),
                `Child Mortality` = mean(Child.mortality),
                `Poor Or Fair Health` = mean(Poor.or.fair.health))
     
     df_hm <- as.data.frame(df_hm)
     rownames(df_hm) <-df_hm$State.Abbreviation
     
     df_hm <- scale(df_hm[,2:5])
     pheatmap(t(df_hm), cuttree_cols=4)
     
   })
   
   output$lifeExpectancy <- renderPlot({
     
     max_life <- max(dfall$Life.expectancy )
     min_life <- min(dfall$Life.expectancy )
     
     if (input$popHealthZeroScale){
       min_life = 0
     }
     
     dfall %>% 
       select(State.Abbreviation, Life.expectancy ) %>%
       group_by(State.Abbreviation) %>%
       summarise(`Avg Life Expectancy` = mean(Life.expectancy ),
                 `Max Life Expectancy` = max(Life.expectancy ),
                 `Min Life Expectancy` = min(Life.expectancy )) %>%
       arrange(`Avg Life Expectancy`) %>%
       ggplot() + 
       geom_point(aes(x=fct_reorder(State.Abbreviation,`Avg Life Expectancy`), y=`Avg Life Expectancy`, color='Average'), size=3) +
       geom_point(aes(x=State.Abbreviation, y=`Min Life Expectancy`, color="Minimum"), size=3) + 
       geom_point(aes(x=State.Abbreviation, y=`Max Life Expectancy`, color="Maximum"), size=3) + 
       scale_color_manual(name="Life Expectancy", values=c('Average'='blue', 'Minimum'='red', 'Maximum'='green')) +
       ylab("Life Expectancy (Years)") + 
       xlab("State") + 
       ylim(min_life,max_life) + 
       ggtitle("Life Expectancy by State") + 
       theme(axis.text.y = element_text(size = 8),
             axis.text.x = element_text(angle=75,size = 8, vjust = 1, hjust = 1),
             axis.title = element_text(size = 12),
             plot.title = element_text(size = 16, face = "bold"),
             panel.background = element_rect(fill = 'white'),
             panel.grid.major = element_line(color = 'light grey', size=0.25, linetype = 'solid'),
             #panel.grid.minor = element_line(color = 'light blue', size = .5),
             panel.border = element_rect(color = "light grey", fill = NA, size = 0.5))
   })
   
   #plot of life expectancy by county
   output$lifeExpectancyCounty <- renderPlot({
     
     df_life_st <- dfall %>% filter(State.Abbreviation == input$state)
     
     max_life <- max(df_life_st$Life.expectancy )
     min_life <- min(dfall$Life.expectancy )
     
     #the user can choose to start the scale ate zero
     #or 'zoom' in
     if (input$countyHealthZeroScale){
       min_life = 0
     }
     
     df_life_st %>%
       select(Name, Life.expectancy ) %>%
       arrange(Life.expectancy) %>%
       ggplot() + 
       geom_point(aes(x=fct_reorder(Name,Life.expectancy), y=`Life.expectancy`, color='Life Expectancy'), size=3) +
       scale_color_manual(name="Life Expectancy", values=c('Life Expectancy'='blue')) +
       ylab("Life Expectancy (Years)") + 
       xlab("County") + 
       ylim(min_life,max_life) + 
       ggtitle("Life Expectancy by County") + 
       theme(axis.text.y = element_text(size = 8),
             axis.text.x = element_text(angle=75,size = 8, vjust = 1, hjust = 1),
             axis.title = element_text(size = 12),
             plot.title = element_text(size = 16, face = "bold"),
             panel.background = element_rect(fill = 'white'),
             panel.grid.major = element_line(color = 'light grey', size=0.25, linetype = 'solid'),
             #panel.grid.minor = element_line(color = 'light blue', size = .5),
             panel.border = element_rect(color = "light grey", fill = NA, size = 0.5))
   })
   
   #plot poor health by count
   output$PoorHealthCounty <- renderPlot({
     
     df_poor_st <- dfall %>% filter(State.Abbreviation == input$state)
     
     max_life <- max(df_poor_st$Poor.or.fair.health )
     min_life <- min(df_poor_st$Poor.or.fair.health )
     
     #the user can choose to start the scale ate zero
     #or 'zoom' in
     if (input$countyHealthZeroScale){
       min_life = 0
     }
     
     df_poor_st %>% 
       select(Name, Poor.or.fair.health ) %>%
       arrange(Poor.or.fair.health) %>%
       ggplot() + 
       geom_point(aes(x=fct_reorder(Name,Poor.or.fair.health), y=Poor.or.fair.health, color='Poor Health'), size=3) +
       scale_color_manual(name="Poor/Fair Health", values=c('Poor Health'='blue')) +
       ylab("Poor or Fair Health (%)") + 
       xlab("County") + 
       ylim(min_life,max_life) + 
       ggtitle("Poor or Fair Health by County") + 
       theme(axis.text.y = element_text(size = 8),
             axis.text.x = element_text(angle=75,size = 8, vjust = 1, hjust = 1),
             axis.title = element_text(size = 12),
             plot.title = element_text(size = 16, face = "bold"),
             panel.background = element_rect(fill = 'white'),
             panel.grid.major = element_line(color = 'light grey', size=0.25, linetype = 'solid'),
             panel.border = element_rect(color = "light grey", fill = NA, size = 0.5))
   })
   
   #National plot of poor health by state
   output$PoorHealth <- renderPlot({

     max_life <- max(dfall$Poor.or.fair.health )
     min_life <- min(dfall$Poor.or.fair.health )
  #the user can choose to start the scale ate zero
  #or 'zoom' in
     if (input$popHealthZeroScale){
       min_life = 0
     }
     
     dfall %>% 
       select(State.Abbreviation, Poor.or.fair.health ) %>%
       group_by(State.Abbreviation) %>%
       summarise(`Avg Poor/Fair Health` = mean(Poor.or.fair.health ),
                 `Max Poor/Fair Health` = max(Poor.or.fair.health ),
                 `Min Poor/Fair Health` = min(Poor.or.fair.health )) %>%
       arrange(`Avg Poor/Fair Health`) %>%
       ggplot() + 
       geom_point(aes(x=fct_reorder(State.Abbreviation,`Avg Poor/Fair Health`), y=`Avg Poor/Fair Health`, color='Average'), size=3) +
       geom_point(aes(x=State.Abbreviation, y=`Min Poor/Fair Health`, color="Minimum"), size=3) + 
       geom_point(aes(x=State.Abbreviation, y=`Max Poor/Fair Health`, color="Maximum"), size=3) + 
       scale_color_manual(name="Poor/Fair Health", values=c('Average'='blue', 'Minimum'='red', 'Maximum'='green')) +
       ylab("Poor/Fair Health") + 
       xlab("State") + 
       ylim(min_life,max_life) + 
       ggtitle("Poor or Fair Health by State") + 
       theme(axis.text.y = element_text(size = 8),
             axis.text.x = element_text(angle=75,size = 8, vjust = 1, hjust = 1),
             axis.title = element_text(size = 12),
             plot.title = element_text(size = 16, face = "bold"),
             panel.background = element_rect(fill = 'white'),
             panel.grid.major = element_line(color = 'light grey', size=0.25, linetype = 'solid'),
             #panel.grid.minor = element_line(color = 'light blue', size = .5),
             panel.border = element_rect(color = "light grey", fill = NA, size = 0.5))
   })
   
#---- HEALTH BEHAVIORS ---------
   #display a message to the user indicating the number of counties
   output$behav_alert <- renderText({
     pop_rows <- nrow(dfall %>% filter(State.Abbreviation == input$state))
     alert <- paste(pop_rows, "Counties in the state of ", input$state)
     if (input$smoking_topn > 60){
       alert <- paste(alert, "Note: More than 60 counties selected, the legend has been dropped")
     }
     alert
   })
   
   #render a line plot of health behaviors by county
   #the user can select the number of counties to display
   output$healthBehaviors <- renderPlot({
     
     df_hb <- dfall %>% filter(State.Abbreviation == input$state)
     
     #remove the legend when more than 60 counties are selected for display
     legend_loc <- theme(legend.position="bottom")
     if (input$smoking_topn > 60){
       legend_loc <- theme(legend.position="none")
     }
     
     df_hb %>% 
       arrange(desc(Name )) %>%
       slice_tail(n=input$smoking_topn) %>%
       select(Name,
              Adult.smoking,
              Adult.obesity,
              Physical.inactivity,
              Excessive.drinking) %>%
       tidyr::pivot_longer(-Name, names_to = "behavior", values_to = "value") %>% # to long format
       group_by(behavior) %>% 
       mutate(`max val` = max(value)) %>%
       ungroup() %>%
       mutate(`val scaled` = value / `max val`) %>%
       ggplot(aes(x = behavior, 
                  y = `val scaled`,
                  group = Name, 
                  color = Name)) +
       geom_line() + 
       ylim(-0.1,1.1) +
       xlab("Health Behavior for State") + 
       ylab("Scaled Value for Behavior") + 
       ggtitle("Health Behaviors") + 
       theme(
             axis.text.x = element_text(angle=70, hjust = 1, vjust = 1, size = 12),
             axis.text.y = element_text(size = 12),
             axis.title = element_text(size = 12),
             plot.title = element_text(size = 16, face = "bold"),
             panel.background = element_rect(fill = 'white'),
             panel.border = element_rect(color = "light grey", fill = NA, size = 0.5)) + 
             legend_loc
      
   },height=600)
   
   #correlate smoking with poor health
   output$smokeHealthCorr <- renderPlot({
     df_sm_corr <- dfall %>% filter(State.Abbreviation == input$state)
     
     if (input$smokeOption == 1){
       max_smoke_corr_y <- max(df_sm_corr$Poor.or.fair.health)
       g <- df_sm_corr %>% 
         mutate(`Poor or Fair Health Percent` = Poor.or.fair.health ) %>%
         ggplot(aes(x=Adult.smoking , 
                    y=`Poor or Fair Health Percent`))  
             
     }
     else if (input$smokeOption == 2){
       max_smoke_corr_y <- max(df_sm_corr$Premature.death)
       g <- df_sm_corr %>% 
         mutate(`Premature Death - Years Lost per 100K Pop` = Premature.death ) %>%
         ggplot(aes(x=Adult.smoking , 
                    y=`Premature Death - Years Lost per 100K Pop`))  
     }
     
     g + geom_bin2d(bins=30) + 
       scale_fill_gradient(low="green",high="darkblue") +
       scale_x_continuous() +
       scale_y_continuous() +
       ylim(-0.1,max_smoke_corr_y) + 
       xlab("Adult Smoking (Percent of Population)") + 
       ggtitle("Adult Smoking vs Health Effects (All Counties for state)") + 
       theme(axis.text = element_text(size = 12),
             axis.title = element_text(size = 12),
             plot.title = element_text(size = 16, face = "bold"),
             panel.background = element_rect(fill = 'white'),
             panel.grid.major = element_line(color = 'light grey', size=0.25, linetype = 'solid'),
             #panel.grid.minor = element_line(color = 'light blue', size = .5),
             panel.border = element_rect(color = "light grey", fill = NA, size = 0.5))
   })
   
   #smoking by county
   output$smokingbarchart <- renderPlot({
     df <- dfall %>% filter(State.Abbreviation == input$state)
     
     df %>%
       arrange(desc(Name )) %>%
       slice_tail(n=input$smoking_topn) %>%
       ggplot(aes(x = Name, y=Adult.smoking )) + 
       geom_bar(stat = "identity", position = "stack")+
       #geom_bar(aes(x=fct_reorder(Name, Adult.smoking ), binwidth=0.8, y=Adult.smoking ),stat = "identity", position = "stack") + 
       ylab("Adult Smoking (%)") + 
       xlab("County") + 
       ggtitle("Adult Smoking by County") + 
       theme(axis.text.y = element_text(size = 8),
             axis.text.x = element_text(angle=75,size = 8, vjust = 1, hjust = 1),
             axis.title = element_text(size = 12),
             plot.title = element_text(size = 16, face = "bold"),
             panel.background = element_rect(fill = 'white'),
             panel.grid.major = element_line(color = 'light grey', size=0.25, linetype = 'solid'),
             #panel.grid.minor = element_line(color = 'light blue', size = .5),
             panel.border = element_rect(color = "light grey", fill = NA, size = 0.5))
   })
   
#---- HEALTHCARE ACCESS -----
   
#Uninsured ,
#Uninsured.children ,
#Primary.care.physicians ,
    output$access_density <- renderPlot({
      df_access <- dfall %>% 
        filter(State.Abbreviation == input$state) %>%
        mutate(`Percent Uninsured` = Uninsured ) %>%
        mutate(`Percent Uninsured Children` = Uninsured.children ) %>%
        mutate(`Percent Poor/Fair Health` = Poor.or.fair.health ) %>%
        mutate(`Ratio Physicians` = Primary.care.physicians )
      
#select the healthcare access factor to display
      if (input$accessOption == 1){
        g <- df_access %>% 
          ggplot(aes(y=`Percent Poor/Fair Health`,
                     x=`Percent Uninsured`))
      }
      else if (input$accessOption == 2) {
        g <- df_access %>% 
          ggplot(aes(y=`Percent Poor/Fair Health`,
                     x=`Percent Uninsured Children`))
      }
      else if (input$accessOption == 3){
        g <- df_access %>% 
          ggplot(aes(y=`Percent Poor/Fair Health`,
                     x=`Ratio Physicians`))
      }
      
#generate a plot of the selected healthcare access factor
      g +
        geom_point()+
        geom_density2d_filled(alpha=0.5, bins=10) +
        ylab("Poor/Fair Health(%)") + 
        ggtitle("Access to Healthcare (select data point for details)") + 
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 12),
              plot.title = element_text(size = 16, face = "bold"),
              panel.background = element_rect(fill = 'white'),
              panel.grid.major = element_line(color = 'light grey', size=0.25, linetype = 'solid'),
              #panel.grid.minor = element_line(color = 'light blue', size = .5),
              panel.border = element_rect(color = "light grey", fill = NA, size = 0.5))
      
    })
    
#generate a table of access factors for the selected point
#on the plot
    output$hc_data <- renderTable({
      df_hc_near <- dfall %>% 
        filter(State.Abbreviation == input$state) %>%
        mutate(`Percent Uninsured` = Uninsured ) %>%
        mutate(`Percent Poor/Fair Health` = Poor.or.fair.health ) %>%
        mutate(`Percent Uninsured Children` = Uninsured.children ) %>%
        mutate(`Ratio Physicians` = format(Primary.care.physicians ,nsmall=4)) %>%
        select(Name, 
               `Percent Poor/Fair Health`,
               `Percent Uninsured`,
               `Percent Uninsured Children`,
               `Ratio Physicians`)
      req(input$plot_click)
      print(input$plot_click)
      nearPoints(df_hc_near, input$plot_click)
    })
    
#generate a distribution plot for the selected access factor
    output$access_dist <- renderPlot({
      
      df_access_dist <- dfall %>% 
        filter(State.Abbreviation == input$state) %>%
        mutate(`Percent Uninsured` = Uninsured ) %>%
        mutate(`Percent Poor/Fair Health` = Poor.or.fair.health ) %>%
        mutate(`Percent Uninsured Children` = Uninsured.children ) %>%
        mutate(`Ratio Physicians` = Primary.care.physicians )
      
      if (input$accessOption == 1){
        g_access_dist <- df_access_dist %>% ggplot(aes(`Percent Uninsured`)) +
          geom_histogram( bins = 100,fill='blue')
      }
      else if (input$accessOption == 2) {
        g_access_dist <- df_access_dist %>% ggplot(aes(`Percent Uninsured Children`)) +
          geom_histogram( bins = 100,fill='blue')
      }
      else if (input$accessOption == 3){
        g_access_dist <- df_access_dist %>% ggplot(aes(`Ratio Physicians`)) +
          geom_histogram( bins = 100,fill='blue')
      }
      
        g_access_dist  + 
        ylab("Count") + 
        ggtitle("Distribution of Healthcare Access Factors") + 
        theme(axis.text.y = element_text(size = 12),
              axis.text.x = element_text(angle=75,size = 12, vjust = 1, hjust = 1),
              axis.title = element_text(size = 12),
              plot.title = element_text(size = 16, face = "bold"),
              panel.background = element_rect(fill = 'white'),
              panel.grid.major = element_line(color = 'light grey', size=0.25, linetype = 'solid'),
              #panel.grid.minor = element_line(color = 'light blue', size = .5),
              panel.border = element_rect(color = "light grey", fill = NA, size = 0.5))
    })
   
#---- EDUCATION ----
   output$ed_scatter <- renderPlot({
     df <- dfall %>% filter(State.Abbreviation == input$state)
    
#select the data to display at the state level
     if (input$ed_option == 1){
       gg_ed <- df %>% ggplot(aes(x= Reading.scores , 
                         y=Poor.or.fair.health ,
                         color=Life.expectancy )) + 
         xlab("Reading Scores") +
         ylab("Poor or fair Health (%)") 
        
     }
     else if (input$ed_option == 2){
       gg_ed <- df %>% ggplot(aes(x= School.funding.adequacy , 
                         y=Poor.or.fair.health ,
                         color=Life.expectancy )) + 
         xlab("School Funding Adequacy") +
         ylab("Poor or fair Health (%)") 
     }
     else{
       gg_ed <- df %>% ggplot(aes(x= High.school.completion , 
                         y=Poor.or.fair.health ,
                         color=Life.expectancy )) + 
         xlab("High School Completion (%)") +
         ylab("Poor or fair Health (%)")  
     }

#generate the state education correlation plot
     gg_ed +   
       ggtitle("Selected State - Poor or Fair Health") + 
       geom_point(position = "jitter",
                  alpha = .8,
                  size = 1.5) + 
       geom_smooth(method="lm", se=TRUE) +
       scale_color_gradientn(colours=c("red","green","blue")) +
       scale_x_continuous() +
       scale_y_continuous() +
       theme(axis.text = element_text(size = 12),
             axis.title = element_text(size = 12),
             plot.title = element_text(size = 16, face = "bold"),
             panel.background = element_rect(fill = 'white'),
             panel.grid.major = element_line(color = 'light grey', size=0.25, linetype = 'solid'),
             #panel.grid.minor = element_line(color = 'light blue', size = .5),
             panel.border = element_rect(color = "light grey", fill = NA, size = 0.5))
     
   })
   
#select the education correlation plot for the national level
#Note the state and national plots use the same input option so
#they will always match
   output$edall_scatter<- renderPlot({
     
     if (input$ed_option == 1){
       gg_ed <- dfall %>% ggplot(aes(x= Reading.scores , 
                                  y=Poor.or.fair.health ,
                                  color=Life.expectancy )) + 
               xlab("Reading Scores") +
               ylab("Poor/Fair Health(%)") 
     }
     else if (input$ed_option == 2){
       gg_ed <- dfall %>% ggplot(aes(x= School.funding.adequacy , 
                                  y=Poor.or.fair.health ,
                                  color=Life.expectancy )) + 
         xlab("School Funding Adequacy") +
         ylab("Poor/Fair Health(%)") 
     }
     else{
       gg_ed <- dfall %>% ggplot(aes(x= High.school.completion , 
                                  y=Poor.or.fair.health ,
                                  color=Life.expectancy )) + 
         xlab("High School Completion (%)") +
         ylab("Poor/Fair Health(%)") 
     }

#generate the education correlation plot at the national level   
     gg_ed +   
       ggtitle("All US Counties - Poor or Fair Health") + 
       geom_point(position = "jitter",
                          alpha = .8,
                          size = 1.5) + 
       geom_smooth(method="lm", se=TRUE) +
       scale_color_gradientn(colours=c("red","green","blue")) +
       scale_x_continuous() +
       scale_y_continuous() +
       theme(axis.text = element_text(size = 12),
             axis.title = element_text(size = 12),
             plot.title = element_text(size = 16, face = "bold"),
             panel.background = element_rect(fill = 'white'),
             panel.grid.major = element_line(color = 'light grey', size=0.25, linetype = 'solid'),
             #panel.grid.minor = element_line(color = 'light blue', size = .5),
             panel.border = element_rect(color = "light grey", fill = NA, size = 0.5))
     
   })

# ----ECONOMICS------   

   output$EconomicDataTable <- renderDataTable({
     
     econ_df <- dfall %>% filter(State.Abbreviation == input$state)
     
     econ_df <- econ_df %>%
       select(Name,
              Poor.or.fair.health ,
              Income.inequality , 
              Food.insecurity ,
              Severe.housing.problems ,
              Unemployment ) %>%
       rename("County Name"=Name,
              "Percent Poor/Fair Health"=Poor.or.fair.health ,
              "Income Inequality" = Income.inequality ,
              "Percent Food Insecurity"=Food.insecurity ,
              "Percent Housing Problems"=Severe.housing.problems ,
              "Percent Unemployment"=Unemployment )
     
     
       econ_df
     
   },options = list(pageLength = 10)
   )
   
   #alert the user when more than 60 counties are selected
   #display the number of counties in the selected state
   output$econ_alert <- renderText({
     pop_rows <- nrow(dfall %>% filter(State.Abbreviation == input$state))
     alert <- paste(pop_rows, "Counties in the state of ", input$state)
     if (input$econ_topn > 60){
       alert <- paste(alert, "Note: More than 60 counties selected, the legend has been dropped")
     }
     alert
   })
   
   output$EconomicPlot <- renderPlot({
     
     econ_df <- dfall %>% filter(State.Abbreviation == input$state)
     
     #The plot is too cluttered for the legend when more than 80 counties are displayed
     legend_loc <- theme(legend.position="bottom")
     if (input$econ_topn > 60){
       legend_loc <- theme(legend.position="none")
     }
     
     econ_df %>%
       select(Name,
              Poor.or.fair.health,
              Income.inequality,
              Food.insecurity,
              Severe.housing.problems,
              Unemployment) %>%
       arrange(desc(Name )) %>%
       slice_tail(n=input$econ_topn) %>%
       tidyr::pivot_longer(-Name, names_to = "Economic Factor", values_to = "value") %>% # to long format
       group_by(`Economic Factor`) %>%
       mutate(`max value` = max(value)) %>% # max value calculation
       mutate(`Relative value` = value / `max value`) %>%
       # mutate(`min value` = min(value), 
       #        `Value modif` = value - `min value`,
       #        `max value modif` = max(`Value modif`), 
       #        `Relative value modif` = `Value modif` / `max value modif`) %>%  
       ungroup() %>%
       ggplot(aes(x = `Economic Factor`, 
                  y = `Relative value`,
                  group = Name, 
                  color = Name)) +
       geom_line(linewidth=0.4) + 
       ylim(-0.1,1.1) +
       xlab("Economic Factor") + 
       ylab("Scaled Value for Economic Factors") + 
       ggtitle("Scaled Economic Factors by County") + 
       theme(axis.text.x = element_text(angle=70,hjust = 1, vjust=1, size = 12),
             
             axis.title = element_text(size = 16),
             plot.title = element_text(size = 16, face = "bold"),
             panel.background = element_rect(fill = 'white'),
             panel.border = element_rect(color = "light grey", fill = NA, size = 0.5))+ 
     legend_loc
     
   },height=800)
   
})
