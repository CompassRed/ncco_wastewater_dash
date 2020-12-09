
# ONLOAD JAVASCRIPT -------------------------------------------------------

onload_js <-
    '$(document).on("shiny:connected", function(e) {
      var jsWidth = window.innerWidth;
      Shiny.onInputChange("screen_width",jsWidth);
    });
    '


# ABOUT THIS DATA DIV -------------------------------------------------------------

about_div <- 
    column(width = 4,class = "col-md-4 col-lg-3 col-xl-3",
           tags$div(class = "ui card",style = "margin-top: 0; padding: 0px; width: 100%;",
                    tags$div(class = "content",
                             tags$p(tags$a("View Data by Sampling Site",id = "go_to_site_tab",onclick = 'document.querySelector("a[data-value=\'map\']").click()'),style = "text-align:left;",
                                    tags$a("View County Overview",id = "go_to_site_tab",onclick = 'document.querySelector("a[data-value=\'overview\']").click()',style = "float:right;")),
                             tags$h3("About this Data",class = "text-center",style = "margin-top: 3px; margin-bottom: 3px;"),
                             
                             tags$p(tags$i("“We will look anywhere and everywhere to collect data to make this invisible enemy more visible.”",HTML("<br>"),style = "font-weight: bold;"),"– County Executive Matt Meyer",class = "text-center"),
                             tags$p("Have feedback? Email us at ",tags$a(href = "mailto:COVID19@newcastlede.org?subject=Feedback on NCC Biobot Dashboard","COVID19@newcastlede.org"),class = "text-center"),
                             
                             tags$p("In April of 2020, New Castle County Executive Matt Meyer announced a partnership with ",tags$a("Biobot Analytics, Inc.",href = "https://www.biobot.io/",target = "_blank"), "to begin regular analysis of county and city of Wilmington wastewater in order to estimate the level of novel coronavirus in the sewer service area. In August, those efforts are being continued through a parternship with the University of Delaware's newly created",tags$a("Center for Environmental and Wastewater-based Epidemiological Research (CEWER).",href = "https://www.udel.edu/academics/colleges/canr/departments/animal-and-food-sciences/affiliated-centers/cewer/",target = "_blank")),
                             
                             tags$p(HTML("The <span style = 'font-weight: bold;'>Northern New Castle County Aggregate Sewer System</span> sampling site includes all the wastewater in New Castle County except for Delaware City/St. Georges, South of the Canal (excl. Middletown) and Port Penn. The sampling covers approximately 480K of the 560K residents in New Castle County.")),
                             
                             tags$div(tags$span(tags$strong("———"),style = "color: black; font-weight: bolder;"),tags$span("Northern New Castle County Aggregate Sewer System Area Outline (",icon("info"),")") %>% bsplus::bs_embed_tooltip("The Wilmington Wastwater Treatment Plant processes wastewater for the outlined area on the map and includes the pump stations",placement = "bottom"),class = "text-center",style = "padding-bottom:5px;"),
                             
                             tags$div(tags$span(tags$strong("———"),style = "color: white; background-color: black; font-weight: bolder;"),tags$span(HTML("&nbsp;City of Newark Area Outline ("),icon("info"),")") %>% bsplus::bs_embed_tooltip("The area included in the City of Newark sampling site is also included by the Greater Newark/Pike Creek/Hockessin area",placement = "bottom"),class = "text-center"),
                             tags$hr(),
                             tags$p("The amount of virus detected in wastewater is shown on a graph with a logarithmic (or log) scale. This is a way of showing a wide range of numerical data in a relatively compact scale. The numbers are shown in increments of a factor of 10."),
                             tags$p("The log scale chart allows for a better understanding of the coronavirus pandemic over rime. Here specifically, the amount of virus detected over multiple areas in New Castle County varies and the log scale helps depict this data most accurately."),
                             tags$p("Samples are collected during the middle of the work week and are graphed to Wednesday’s date for
that week."),
                             tags$div(id = "faq_click",tags$p(tags$a("Click here to view the FAQ section",),class = "text-center"))
                    )
           )
    )

about_div2 <- about_div


# CHARTS DIV --------------------------------------------------------------

chart_div <- 
    column(width = 4,class = "col-sm-4 col-sm-offset-4 col-md-4 col-md-offset-4 col-lg-4 col-lg-offset-5 col-xl-4 col-xl-offset-5",
           tags$div(class = "ui card",style = "margin-top: 0; padding: 0px; width: 100%;",
                    tags$div(class = "content",
                             uiOutput("as_of_date_ps",style = "margin-bottom: 10px;"),
                             
                             uiOutput("n_ncco_scorecard",class = "text-center",style = "margin-bottom: 7px;")  %>% bsplus::bs_embed_tooltip("This is an overall aggregate value for all areas above the canal, excluding Delaware City/St. Georges",placement = "bottom"),
                             tags$p(HTML("<i>Delaware City/St. Georges, South of the Canal (excl. Middletown), and Port Penn sampling sites are <span style = 'font-weight: bold;'>not</span> included in the Aggregate Sewer System</i>"),class = "text-center"),
                             tags$hr(),
                             
                             tags$h3("Latest Virus Levels (viral copies/L) by Sampling Site",class = "text-center",style = "margin-bottom: 0; margin-top: 5px;"),
                             plotlyOutput("bar_chart",height = "215px"),
                             tags$h3("Virus Levels (viral copies/L) Trended by Sampling Site",class = "text-center",style = "margin-bottom: 5px;margin-top:5px;"),

                             uiOutput("station_select"),
                             plotlyOutput("line_chart",height = "215px"),
                             tags$p(tags$i("08/13/20: University of Delaware begins sampling.",style = "color: grey;"),class="text-center"),
                             tags$p(tags$i("Y-axis represented on a log scale, shading represents 95% confidence interval.",style = "color: grey;"),class="text-center")
                             
                    )
           )
    )

footer_logos <- tags$div(class = "text-center ui card text-center",style = "position: fixed; left: 50%; bottom:0; margin: 0px 0px 10px 0px; transform: translateX(-50%); padding: 5px; width:150px;",
                         splitLayout(cellWidths = c("20%","80%"),tags$a(href = "https://www.udel.edu/academics/colleges/canr/departments/animal-and-food-sciences/affiliated-centers/cewer/",target = "_blank",tags$img(src = "ud_logo.png",height = "100%",width = "20em")),
                                     tags$a(href = "https://www.compassred.com",target = "_blank",tags$img(src = "cr_logo.png",height = "100%",width = "80em"))
                         )
)

chart_div2 <- column(width = 4,class = "col-sm-4 col-sm-offset-4 col-md-4 col-md-offset-4 col-lg-4 col-lg-offset-5 col-xl-4 col-xl-offset-5",
                     tags$div(class = "ui card",style = "margin-top: 0; padding: 0px; width: 100%;",
                              tags$div(class = "content",
                                       uiOutput("as_of_date_ps2",style = "margin-bottom: 10px;"),
                                       tags$h3(HTML("New Castle County Confirmed COVID-19 Cases<br>(Rolling 7-Day Average)"),class = "text-center",style = "margin-bottom: 0; margin-top: 5px;"),
                                       tags$p(tags$a("Source: DHHS My Healthy Community",href = "https://myhealthycommunity.dhss.delaware.gov/locations/county-new-castle#cases",target = "_blank"),class = "text-center"),
                                       plotlyOutput("county_cases_line_chart",height = "215px"),
                                       
                                       tags$h3(HTML("Northern New Castle County Aggregate Sewer System<br>Virus Levels (viral copies/L)"),class = "text-center",style = "margin-bottom: 0; margin-top: 5px;"),
                                       tags$p(tags$i("Represents ~85% of the population in New Castle County"),class = "text-center"),
                                       plotlyOutput("total_sample_line_chart",height = "215px"),
                                       tags$p(tags$i("08/13/20: University of Delaware begins sampling.",style = "color: grey;"),class="text-center"),
                                       tags$p(tags$i("Y-axis represented on a log scale, shading represents 95% confidence interval.",style = "color: grey;"),class = "text-center")
                                       
                                       
                              )
                     )
)

# FAQ SECTION -------------------------------------------------------------

faq_div <- fluidRow(
    column(width = 6,
  tags$h2("FAQ"),
  tags$h3("How can I give feedback on this dashboard and ask questions?"),
  tags$p("Please email us at ",tags$a(href = "mailto:COVID19@newcastlede.org?subject=Feedback on NCC Biobot Dashboard","COVID19@newcastlede.org")," for all feedback and questions."),
  tags$h3("Why test for COVID-19 in wastewater?"),
  tags$p("COVID-19 is shed in the stool of those infected with the virus and into the New Castle County sewer system. Sampling the sewer system provides a valuable insight into the health of the entire sewer service population and is a cost-effective means of testing hundreds of thousands of people at once. People that are asymptomatic or otherwise not sick enough to be tested, but that still may be spreading the virus, can be quantified by this testing."),
  
  tags$h3("Why does the dashboard no longer present prevalence rates?"),
  tags$p("As knowledge of the COVID-19 virus and how it is shed from the body has grown, it has been found that
current models predicting prevalence rate may not be as accurate as once thought. Instead, comparing
trends of viral concentration in wastewater compared to clinical cases provides helpful data that can be
used to help predict the magnitude, direction and extent of the pandemic."),
  
  tags$h3("How can I interpret a logarithmic scale?"),
  tags$p("You may be familiar with the Richter Scale, which is a logarithmic scale used to measure the magnitude of an earthquake. For example, you might not notice a great difference in an earthquake measuring a 2 and one measuring a 3, but an earthquake measuring a 6 is incredibly different, and actually is 10,000 times stronger than an earthquake measuring at a 2 on the Richter Scale."),
  
  tags$h3("How does this process work?"),
  tags$ol(
    tags$li("Personnel set up specialized equipment that collects a representative sample from points in the sewer system over the course of a 24-hour period. Population data and wastewater flow rates are computed."),
    tags$li("Collected samples are preserved with ice and delivered to the laboratory for Polymerase Chain Reaction (PCR) analysis- the same type of analysis performed to test if humans are infected."),
    tags$li("Through PCR, the laboratory looks for and quantifies the unique genetic signature of the COVID-19 virus in our
samples.")
  ),
  tags$h3("How can this data be used?"),
  tags$p("Because this sampling covers the entire sewered populations at scale, the data can be used to identify:"),
  tags$ul(
    tags$li("Trends of infection in the population"),
    tags$li("Where to deploy resources and additional clinical testing"),
    tags$li("Warning signs of a new outbreak")
  ),
  tags$h3("Should I be concerned about contracting COVID-19 from the sewer system?"),
  tags$p("Wastewater COVID-19 analysis detects only the genetic material from the virus. It is not able to detect whether the virus is still alive and viable in our wastewater. Regardless, the World Health Organization states that “there is no evidence to date that COVID-19 virus has been transmitted via sewerage systems, with or without wastewater treatment.” Please note that the County’s sewer system is contained and inaccessible to the public, and all samples collected are prior to treatment and disinfection at the wastewater treatment plant."),
  tags$h3("Where is other COVID-19-related data for Delaware?"),
  tags$p("To see estimates with actual positive test results in the State of Delaware, go to the ",tags$a("Delaware Health and Human Services website at coronavirus.delaware.gov.",href = "https://coronavirus.delaware.gov/",target = "_blank")),
  
  tags$br(),tags$br()),
  column(width = 6,
tags$div(
  tags$h2("Relevant Links"),
  
  tags$h5("New Castle County, Delaware"),
  tags$p(tags$a(href = "https://www.nccde.org/",target = "_blank","New Castle County Homepage"),
         tags$br(),tags$a(href = "https://www.nccde.org/DocumentCenter/View/38959/covidupdate",target = "_blank","Raw Data Collected")),
  tags$h5("BioBot Analytics"),
  tags$p(tags$a(href = "https://www.biobot.io",target = "_blank","Biobot Homepage")),
  
  tags$h5("University of Delaware Center for Environmental and Wastewater-based Epidemiological Research (CEWER)"),
  tags$p(tags$a(href = "https://www.udel.edu/academics/colleges/canr/departments/animal-and-food-sciences/affiliated-centers/cewer/",target = "_blank","CEWER Homepage")),
  
  tags$h5("CompassRed"),
  tags$p(tags$a(href = "https://www.compassred.com",target = "_blank","CompassRed Homepage"),tags$br(),
  tags$a(href = "https://www.github.com/CompassRed/ncco_wastewater_dash/",target = "_blank","This Application's Code on GitHub")),
  
  tags$h5("Delaware Data Innovation Lab (DDIL)"),
  tags$p(tags$a(href = "https://ddil.ai",target = "_blank","DDIL Homepage"))
  
  )),footer_logos)


# BUILD THE UI ------------------------------------------------------------

tagList(
    shinyUI(
        navbarPage(collapsible = TRUE,
            selected = "overview",
            id = "parent_page",
            title = "NCCo COVID-19 Wastewater Testing",
            windowTitle = "New Castle County COVID-19 Wastewater Testing Dashboard",
            theme = shinytheme("simplex"),
            tabPanel("Overview",value = "overview",
                     fluidPage(
                       tags$head(
                         shinyjs::useShinyjs()
                         ,use_bs_tooltip()
                         ,tags$link(rel="stylesheet", type="text/css", href="style.css")
                         ,tags$script(src = "semantic/semantic.js")
                         ,tags$link(rel="stylesheet", type="text/css", href="semantic/semantic.css")
                         ,tags$script(onload_js)
                         ,includeHTML("www/google_analytics.html")
                         ,tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; } #inline .form-group { display: table-row;}")
                       )
                       ,fluidRow(id = "map_div2",
                                 fillRow(leafletOutput("map2", width = "100%") %>% withSpinner(type = 4,color = "#576080"))
                                 ,fluidRow(about_div2,chart_div2)
                       )
                       ,footer_logos
                     )),
            tabPanel("By Sampling Site",value = "map",id = "map",
                     fluidPage(
                       tags$head(
                         shinyjs::useShinyjs()
                         ,use_bs_tooltip()
                         ,tags$link(rel="stylesheet", type="text/css", href="style.css")
                         ,tags$script(src = "semantic/semantic.js")
                         ,tags$link(rel="stylesheet", type="text/css", href="semantic/semantic.css")
                         ,tags$script(onload_js)
                         ,includeHTML("www/google_analytics.html")
                         ,tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; } #inline .form-group { display: table-row;}")
                       )
                       ,fluidRow(id = "map_div",
                                 fillRow(leafletOutput("map", width = "100%") %>% withSpinner(type = 4,color = "#576080"))
                                 ,fluidRow(about_div,chart_div)
                       )
                       ,footer_logos
                     )),   
            tabPanel("FAQ",value = "faq",fluidPage(faq_div)),
            tabPanel("NCCo",value = "NCCo")
            
        )
    )
)



