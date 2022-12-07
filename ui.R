
library(shiny)


shinyUI(
  fluidPage(

    # Application title
    titlePanel("Health Metrics for US Counties"),

    fluidRow(
      selectInput("state", "state:",c( "Alaska"="AK","Arizona"="AZ","Arkansas"="AR","California"="CA","Colorado"="CO",            
                                       "Connecticut"="CT","Delaware"="DE","District Of Columbia"="DC","Florida"="FL","Georgia"="GA",             
                                       "Hawaii"="HI","Idaho"="ID","Illinois"="IL","Indiana"="IN","Iowa"="IA",                
                                       "Kansas"="KS","Kentucky"="KY","Louisiana"="LA","Maine"="ME","Maryland"="MD",            
                                       "Massachusetts"="MA","Michigan"="MI","Minnesota"="MN","Mississippi"="MS","Missouri"="MO",            
                                       "Montana"="MT","Nebraska"="NE","Nevada"="NV","New Hampshire"="NH","New Jersey"="NJ",          
                                       "New Mexico"="NM","New York"="NY","North Carolina"="NC","North Dakota"="ND","Ohio"="OH",                
                                       "Oklahoma"="OK","Oregon"="OR","Pennsylvania"="PA","Rhode Island"="RI","South Carolina"="SC",     
                                       "South Dakota"="SD","Tennessee"="TN","Texas"="TX","Utah"="UT","Vermont"="VT",             
                                       "Virginia"="VA","Washington"="WA","West Virginia"="WV","Wisconsin"="WI","Wyoming"="WY"), 
                  selected="KS")
    ),
    navlistPanel(
        tabPanel("Demographics",
            fluidRow(
               textOutput("demo_alert"),
               tags$head(tags$style("#demo_alert{color: blue;
                                 font-size: 16px;
                                 font-style: bold;
                                 }")),
               sliderInput("topn",
                          "Number of counties to display:",
                          min = 10,
                          max=254,
                          value = 20)
            ),
            fluidRow(
                plotOutput("popByCounty")
            ),
            fluidRow(
               plotOutput("ruralByCounty")
            )
        ),
        tabPanel("US Population Health",
            fluidRow(
              textOutput("pophealth_alert"),
              tags$head(tags$style("#pophealth_alert{color: blue;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"))
            ),
            fluidRow(
                plotOutput("healthHeatmap")
            ),
            fluidRow(
             checkboxInput("popHealthZeroScale", "Start Scale at zero (truthful view)", TRUE),
           ),    
           fluidRow(
                plotOutput("lifeExpectancy")
            ),
           fluidRow(
                plotOutput("PoorHealth")
           ),
        ),
        tabPanel("State Population Health",
           fluidRow(
             checkboxInput("countyHealthZeroScale", "Start Scale at zero (truthful view)", TRUE),
           ),
           fluidRow(
                plotOutput("lifeExpectancyCounty")
           ),
           fluidRow(
                plotOutput("PoorHealthCounty")
           )
        ),
        tabPanel("Health Behaviors",

          fluidRow(
            radioButtons("smokeOption", "Adult Smoking / Health Correlation:",
                         c("Poor Health" = "1",
                           "Premature Death" = "2")),
          ),
          fluidRow(
              plotOutput("smokeHealthCorr")
          ),
          fluidRow(
            textOutput("behav_alert"),
            tags$head(tags$style("#behav_alert{color: blue;
                                 font-size: 16px;
                                 font-style: bold;
                                 }")),
            sliderInput("smoking_topn",
                        "Number of counties to display:",
                        min = 10,
                        max=254,
                        value = 20)
          ),
          fluidRow(
            plotOutput("smokingbarchart")
          ),
          fluidRow(
            plotOutput("healthBehaviors")
          )

        ),
        tabPanel("Healthcare Access",
            fluidRow(
               radioButtons("accessOption", "Health Access Type",
                            c("Uninsured" = "1",
                              "Uninsured Children" = "2",
                              "Physicians" = "3")),
            ),
            fluidRow(
              plotOutput("access_density", click = "plot_click")
            ),   
            fluidRow(
              tableOutput("hc_data")
            ),
            fluidRow(
              plotOutput("access_dist")
            )
         
        ),
        tabPanel("Education",
       
          fluidRow(
             column(12,
                    radioButtons("ed_option", "Household Income / Reading Score Correlation:",
                                 c("Reading Scores" = "1",
                                   "School Funding" = "2",
                                   "High School Completion" = "3")),
             ),
          ),
          fluidRow(
             column(12,
                    plotOutput("ed_scatter"),
             )
          ),
          fluidRow(
             column(12,
                    plotOutput("edall_scatter")
             )
           )       
        ),
       tabPanel("Economics Table",
           fluidRow(
              dataTableOutput('EconomicDataTable') 
           )
       ),
       tabPanel("Economics Plot",
          fluidRow(
              textOutput("econ_alert"),
              tags$head(tags$style("#econ_alert{color: blue;
                                 font-size: 16px;
                                 font-style: bold;
                                 }")),
              sliderInput("econ_topn",
                    "Number of counties to display:",
                    min = 10,
                    max=254,
                    value = 20)
           ),
           fluidRow(
             plotOutput('EconomicPlot')
           )
        )
    )
))
