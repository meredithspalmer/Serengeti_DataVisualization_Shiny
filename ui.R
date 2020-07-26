# Serengeti Data Visualization App


# User interface ---------------------------------

source("serengeti_shiny.R")

# Sidebar ----------------------------------------

sidebar <- dashboardSidebar(
    
    sidebarMenu(
        menuItem("Overview", tabName = "overview"),
        menuItem("By Species", tabName = "species"),
        menuItem("Pairwise Comparison", tabName = "pairwise_compare")
    ),
    
    tags$footer(
        p(
            "Maintained by ",
            a(href = 'https://www.meredithspalmer.weebly.com', "Meredith S. Palmer.")
        ),
        
        align = "left",
        style = "
            position:absolute;
            bottom:0;
            width:100%;
            height:50px; /* Height of the footer */
            color: white;
            padding: 10px;
            background-color: black;
            z-index: 1000;"
    )
)

# Body -------------------------------------------

body <- dashboardBody(
    
    tabItems(
        
        tabItem(
            
            tabName = "overview",
            
            fluidRow(
                box(width = 12,
                    h1("Serengeti Camera Traps"),
                    "This dashboard facilitates exploration of the first five years of camera trap data (2010-
                    2014) from the systematic grid in Serengeti National Park, Tanzania. These data represent 
                    a collaborative effort between Drs. ", a(href = 'www.lionresearch.org', "Craig Packer,"), 
                    " Ali Swanson, Margaret Kosmala, and ", a(href = 'www.meredithspalmer.weebly.com', "Meredith Palmer."), 
                    "Data presented here were classified by citizen scientists on the website",  
                    a(href = 'www.SnapshotSerengeti.org', "Snapshot Serengeti."), "App is based heavily on code
                    originally provided by ", a(href = 'http://www.kaitlyngaynor.com/', "Dr. Kaitlyn Gaynor."))), 
            
            fluidRow(
                box(width = 12,
                    title = "Camera trap study design",
                    status = "primary",
                    "The camera trap study area covers ~1200 km2 in the center of Serengeti National Park, 
                    Tanzania, covering an area of woodlands and short-grass plains. The figure of the study site
                    /setup below is from Palmer et al. 2020:",
                    div(img(src="https://meredithspalmer.files.wordpress.com/2020/06/screen-shot-2020-06-17-at-2.58.02-pm.png", width=600), style = "text-align: center;"),
                    "More camera trap information to come. For details on survey methodology, please read ", 
                    a(href = 'https://www.nature.com/articles/sdata201526?origin=app', "Swanson et al. 2015;"), 
                    "to participate in on-going citizen science classifications of Serengeti data, visit ", 
                    a(href = 'https://www.snapshotserengeti.org', "Snapshot Serengeti."), "Raw data can be
                    accessed at the", a(href = 'http://lila.science/datasets/snapshot-serengeti', "LILA image
                                        repository."), 
                    "Please contact Meredith Palmer for additional information, (meta)data, or questions about
                    collaborations.")), 
            
            fluidRow(
                box(title = "Funding and collaboration",
                    width = 12,
                    status = "primary",
                    "Acknowledgements coming soon."))
        ),
        
# By species -------------------------------------

tabItem(
    
    tabName = "species",
    
    fluidRow(
        box(h2("INDIVIDUAL SPECIES PATTERNS"), width = 12)),
    
    fluidRow(
        box(
            title = "Choose a species",
            selectInput(inputId = "species_select",
                        label = "Select species:",
                        selected = "Eland",
                        choices = sort(unique(dat$species))), 
            
           radioButtons(inputId = "juvenile_select", 
                         label = "Select presence/absence of juveniles (optional):",
                         choices = list("All" = 1, "With_Junveniles" = 2, "Without_Juveniles" = 3), 
                         selected = 1), 
            "Select for records with or without juveniles. Default is all images of the species.", 
            br(),
            br(),
            
            checkboxGroupInput(inputId = "behavior_select", 
                               label = "Select performance of behaviors (optional):",
                               choices = c("Eating", "Standing", "Resting", "Moving",
                                           "Interacting")),
            "Select for records of animals performing certain behaviors. Default (unchecked) is all images of the
                    species.",
           br(), 
           br(), 
           
           strong("Selection for horns/no horns visible not available with current data.")
        ),
        
        box(
            title = "Subset records further (optional)",
            dateRangeInput(inputId = "date_range",
                           label = "Date Range:",
                           start = "2010-08-01",
                           end = "2012-05-29"),
            "Provided here are the first ~4 seasons of publicly available Snapshot Serengeti data. 
                    If you choose dates outside of this range, it will generate an error. No individual 
                    camera was operable for this entire period. If you would like to incorporate additional 
                    years of data (-2020), please contact Meredith Palmer",
            br(),
            br(),
            
            numericInput(inputId = "independent_min",
                         label = "Set quiet period for independent detections (minutes):",
                         value = 15,
                         min = 0,
                         max = 1440),
            "Records of a given species will only be counted as one detection if they occur within
                    the set quiet period. This setting addresses bias caused by a single animal sitting in 
                    front of a camera for a long period of time and repeatedly triggering the camera. 
                    The default setting is 15 minutes.", 
            br(),
            br(), 
            
            sliderInput(inputId = "evenness_level",
                        label = "Select consensus level of images (Pielous's evenness):",
                        min = 0, max = 1, step=0.1,
                        value = c(0,0.5)),
            "Multiple user classifications are aggregated into a single 'consensus' answer with an
            associated level of evenness. Lower values of evenness indicate more consensus among users.
            This metric is constrained between 0 (complete consensus) and 1 (each answer different).
            slider values are inclusive."
        )),
    
    fluidRow(
        box(title = "Relative Activity Index (RAI) across camera grid",
            collapsible = TRUE,
            
            selectInput(inputId = "rai_select",
                        label = "Select response variable:",
                        selected = "Detections",
                        choices = list("Detections" = 1, "Total_Counts" = 2)), 
            
            leafletOutput(outputId = "rai_map"),
            "Detections or total animals seen per trap-night at each camera. Note that greyed-out squares
            were not operable during the selected period. Switch to log scale for easier viewing (small 
            value of 0.001 added to all RAI to address issue with 0s).",
            br(), 
            br(), 
            radioButtons(inputId = "log_select_map", 
                         label = "Select scale:",
                         choices = list("RAI" = 1, "log(RAI)" = 2), 
                         selected = 1)),
        
        box(title = "Basic Occupancy Modeling (OM) across camera grid",
            collapsible = TRUE,
            
            selectInput(inputId = "om_cov",
                        label = "Select occupancy covariate:",
                        selected = "Kopje_Distance",
                        choices = c("River_Distance", "Kopje_Distance", "Road_Distance", 
                                    "Percent_Tree_Cover")), 
            
            leafletOutput(outputId = "om_map"),
            "Occupancy modeling accounts for imperfect detection of camera trap method. In this analysis, 
            there is only ONE covariate on occupancy and NO covariates ondetection probability. More 
            in-depth analyses examining how habitat features affect occupancy and detection are strongly 
            recommended. Note: these calculations may fail if date range selected is too large. Consider
            what makes a `closed` season when selecting date range.", 
            br(), 
            br(),
            
            radioButtons(inputId = "detection_window",
                        label = "Select length of detection window:",
                        selected = 2,
                        choices = list("Day" = 1, "Week" = 2))
        )),
    
    fluidRow(
        box(title = "Environmental covariates of Relative Activity Index",
            collapsible = TRUE,
            selectInput(inputId = "metadata_select",
                        label = "Choose an environmental covariate:",
                        choices = c("Lion_Density", "River_Distance", "Kopje_Distance",
                                    "Road_Distance", "Percent_Tree_Cover")), 
            
            plotlyOutput(outputId = "rai_metadata"),
            "All distance covariates are in meters. Please contact Meredith Palmer if you are interested 
                    in how these data layers were generated, or have additional spatial covariates to contribute."), 
        
        box(title = "Diel activity pattern",
            collapsible = TRUE,
            "Kernel density distribution of the timing of the detections across all cameras across 
                    the 24-hour period. All times are scaled to solar time based on the date of the detection.",
            plotOutput(outputId = "activity_plot"))
    ),
    
    fluidRow(
        
        box(title = "RAI over time: months",
            collapsible = TRUE,
            "Monthly RAI for the selected time period, calculated for the entire grid network (total
                    detections per total trap-nights across all operating cameras). An RAI of 0 indicates that 
                    there were no detections during that month.",
            plotlyOutput(outputId = "monthly_rai_hist"))
        
       ##put years here soon
        
        )),
    
# Species comparison -----------------------------

        tabItem(
            
            tabName = "pairwise_compare",
            
            fluidRow(
                box(h2("COMPARISON TOOL"), width = 12,
                    "This page enables the comparison of two data subsets. It can be used to compare 
                    patterns for a given species across seasons or behaviors, or to compare two species.")),
            
            fluidRow(
                box(
                    title = "Data Subset A:",
                    
                    selectInput(inputId = "species_select_A",
                                label = "Choose species for dataset A:",
                                selected = "Impala",
                                choices = sort(unique(dat$species))),
                    
                    dateRangeInput(inputId = "date_range_A",
                                   label = "Date Range:",
                                   start = "2010-08-01",
                                   end = "2014-12-31"),
                    
                    numericInput(inputId = "independent_min_A",
                                 label = "Set quiet period for independent detections (minutes):",
                                 value = 15,
                                 min = 0,
                                 max = 1440)),
                
                box(
                    title = "Data Subset B:",
                    
                    selectInput(inputId = "species_select_B",
                                label = "Choose species for dataset B:",
                                selected = "Wildebeest",
                                choices = sort(unique(dat$species))),
                    
                    dateRangeInput(inputId = "date_range_B",
                                   label = "Date Range:",
                                   start = "2010-08-01",
                                   end = "2014-12-31"),
                    
                    numericInput(inputId = "independent_min_B",
                                 label = "Set quiet period for independent detections (minutes):",
                                 value = 15,
                                 min = 0,
                                 max = 1440))),
            
            fluidRow(
                box(title = "Diel overlap",
                    collapsible = TRUE,
                    textOutput(outputId = "activity_overlap"),
                    plotOutput(outputId = "activity_plot_compare")
                ),
                
                box(title = "Side-by-side trend over time",
                    collapsible = TRUE,
                    plotlyOutput(outputId = "rai_monthly_AB"))),
            
            fluidRow(
                box(title = "Plot of RAI A vs B",
                    collapsible = TRUE,
                    plotlyOutput(outputId = "rai_AB"),
                    "Option to switch to log scale for easier viewing (small value of 0.001 added 
                    to all RAI to address issue with 0s).",
                    radioButtons(inputId = "log_select", label = "",
                                 choices = list("RAI" = 1, "log(RAI)" = 2), 
                                 selected = 1))) 
 )))

# Dashboard --------------------------------------

dashboardPage(
    dashboardHeader(title = "SNP Cameras"),
    sidebar,
    body
)