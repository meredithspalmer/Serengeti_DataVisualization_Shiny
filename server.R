# Serengeti Data Visualization App


# Server -----------------------------------------

server <- function(input, output, session) { 
    
# SINGLE SPECIES ---------------------------------
    
    # Subset based on widgets --------------------
    
    # create reactive object 'records_subset' that changes based on delta time and date range 
    records_subset <- reactive({
        dat %>%
            filter(delta.time.secs == 0 | delta.time.secs >= (60 * input$independent_min),
                   Date >= input$date_range[1], Date <= input$date_range[2]) %>%
            filter(species == input$species_select) %>% 
            {if (input$juvenile_select == 3) filter(., juveniles == "No_Juveniles") else .} %>%
            {if (input$juvenile_select == 2) filter(., juveniles == "Juveniles") else .} %>%
            {if ("Standing" %in% input$behavior_select) filter(., standing_yn == "Standing") else .} %>%
            {if ("Eating" %in% input$behavior_select) filter(., eating_yn == "Eating") else .} %>%
            {if ("Resting" %in% input$behavior_select) filter(., resting_yn == "Resting") else .} %>% 
            {if ("Moving" %in% input$behavior_select) filter(., moving_yn == "Moving") else .} %>% 
            {if ("Interacting" %in% input$behavior_select) filter(., interacting_yn == "Interacting") else .} %>%
            filter(evenness >= input$evenness_level[1], evenness <= input$evenness_level[2])
    })
    
    # Summarize subset data ----------------------
    
    # summarize species counts aross cameras 
    species_summary <- reactive({
        records_subset() %>%
            group_by(site) %>%
            dplyr::summarise(detections = n(), counts = sum(count)) 
    })
    
    # calculate RAI
    rai <- reactive({
        rai.calculate(records_subset(), camera_operation, input$date_range[1], input$date_range[2])
    })
    
    # calculate monthly RAI
    monthly_rai <- reactive({
        rai.monthly(records_subset(), camera_operation, input$date_range[1], input$date_range[2])
    })
    
    # combine RAI and metadata
    rai_metadata <- reactive({
        left_join(rai(), camera_metadata)
    })
    
    # calculate basic occupancy model
    om <- reactive({
        om.calculate(records_subset(), camera_operation, input$date_range[1], input$date_range[2], 
                     input$om_cov, input$detection_window, camera_metadata)
    })
    
    # Map outputs --------------------------------
    
    # merge grid with RAI
    grid_rai <- reactive({
        full_join(grid, rai())
    })
    
    # make color palette for map
    pal.det <- reactive({  
        colorNumeric(palette = "viridis", domain = grid_rai()$RAI.det)  
    })
    pal_log.det <- reactive({
        colorNumeric(palette = "viridis", domain = log(grid_rai()$RAI.det + 0.001))  
    })
    pal.count <- reactive({  
        colorNumeric(palette = "viridis", domain = grid_rai()$RAI.count)  
    })
    pal_log.count <- reactive({  
        colorNumeric(palette = "viridis", domain = log(grid_rai()$RAI.count + 0.001))  
    })
    
    # create map labels
    map_labels_det <- reactive({
        sprintf(
            "<strong>Camera: %s</strong><br/>Detections: %i<br/>Days operating: %i<br/>RAI: %g",
            grid_rai()$site, grid_rai()$Detections, grid_rai()$Operation, grid_rai()$RAI.det, 0) %>% 
            lapply(htmltools::HTML)
    }) 
    
    # create map labels
    map_labels_count <- reactive({
        sprintf(
            "<strong>Camera: %s</strong><br/>Total Counts: %i<br/>Days operating: %i<br/>RAI: %g",
            grid_rai()$site, grid_rai()$TotalCounts, grid_rai()$Operation, grid_rai()$RAI.count, 0) %>% 
            lapply(htmltools::HTML)
    }) 
    
    # generate leaflet map
    output$rai_map <- renderLeaflet({
        
        if(input$rai_select == 1) { 
            
            #generate detections map 
            if(input$log_select_map == 1) {  
                
                leaflet(grid_rai(), options=leafletOptions(zoomSnap=0.25)) %>%
                    
                    setView(34.9253, -2.4978, 10.5) %>% 
                    addTiles() %>% # or satellite image: addProviderTiles(providers$Esri.WorldImagery)
                    
                    addPolygons(
                        data = grid_rai(),
                        fillColor = ~pal.det()(grid_rai()$RAI.det),
                        fillOpacity = 1, 
                        weight = 1, # stroke weight of lines
                        color = "gray", # color of lines
                        label = map_labels_det(),
                        highlight = highlightOptions(
                            weight = 2,
                            color = "white",
                            fillOpacity = 1,
                            bringToFront = TRUE)
                    ) %>% 
                    
                    addLegend_decreasing(pal = pal.det(), 
                                         values = ~RAI.det,
                                         opacity = 1, 
                                         title = "RAI",
                                         position = "topleft",
                                         decreasing = TRUE)
            } else {
                leaflet(grid_rai(), options=leafletOptions(zoomSnap=0.25)) %>%
                    
                    setView(34.9253, -2.4978, 10.5) %>% 
                    addTiles() %>% # or satellite image: addProviderTiles(providers$Esri.WorldImagery)
                    
                    addPolygons(
                        data = grid_rai(),
                        fillColor = ~pal_log.det()(log(grid_rai()$RAI.det + 0.001)),
                        fillOpacity = 1, 
                        weight = 1, # stroke weight of lines
                        color = "gray", # color of lines
                        label = map_labels_det(),
                        highlight = highlightOptions(
                            weight = 2,
                            color = "white",
                            fillOpacity = 1,
                            bringToFront = TRUE)
                    ) %>% 
                    
                    addLegend_decreasing(pal = pal_log.det(), 
                                         values = ~log(RAI.det + 0.001),
                                         opacity = 1, 
                                         title = "log(RAI)",
                                         position = "topleft",
                                         decreasing = TRUE)    
            }
            
        } else {
             
            #generate counts map    
            if(input$log_select_map == 1) {  
                
                leaflet(grid_rai(), options=leafletOptions(zoomSnap=0.25)) %>%
                    
                    setView(34.9253, -2.4978, 10.5) %>% 
                    addTiles() %>% # or satellite image: addProviderTiles(providers$Esri.WorldImagery)
                    
                    addPolygons(
                        data = grid_rai(),
                        fillColor = ~pal.count()(grid_rai()$RAI.count),
                        fillOpacity = 1, 
                        weight = 1, # stroke weight of lines
                        color = "gray", # color of lines
                        label = map_labels_count(),
                        highlight = highlightOptions(
                            weight = 2,
                            color = "white",
                            fillOpacity = 1,
                            bringToFront = TRUE)
                    ) %>% 
                    
                    addLegend_decreasing(pal = pal.count(), 
                                         values = ~RAI.count,
                                         opacity = 1, 
                                         title = "RAI",
                                         position = "topleft",
                                         decreasing = TRUE)
            } else {
                leaflet(grid_rai(), options=leafletOptions(zoomSnap=0.25)) %>%
                    
                    setView(34.9253, -2.4978, 10.5) %>% 
                    addTiles() %>% # or satellite image: addProviderTiles(providers$Esri.WorldImagery)
                    
                    addPolygons(
                        data = grid_rai(),
                        fillColor = ~pal_log.count()(log(grid_rai()$RAI.count + 0.001)),
                        fillOpacity = 1, 
                        weight = 1, # stroke weight of lines
                        color = "gray", # color of lines
                        label = map_labels_count(),
                        highlight = highlightOptions(
                            weight = 2,
                            color = "white",
                            fillOpacity = 1,
                            bringToFront = TRUE)
                    ) %>% 
                    
                    addLegend_decreasing(pal = pal_log.count(), 
                                         values = ~log(RAI.count + 0.001),
                                         opacity = 1, 
                                         title = "log(RAI)",
                                         position = "topleft",
                                         decreasing = TRUE)    
            }
        }
    }) 
        
    # merge grid with OM
    grid_om <- reactive({
        full_join(grid, om())
    })
    
    # make color palette for map
    pal.om <- reactive({  
        colorNumeric(palette = "magma", domain = grid_om()$Predicted)  
    })
    
    # create map labels
    map_labels_om <- reactive({
        sprintf(
            "<strong>Camera: %s</strong><br/>Predicted OM: %g<br/>SE: %g<br/>Covariate value: %g",
            grid_om()$site, grid_om()$Predicted, grid_om()$SE, grid_om()$siteCovs, 0) %>% 
        lapply(htmltools::HTML)
    }) 
    
    # generate leaflet map
    output$om_map <- renderLeaflet({
        
        leaflet(grid_om(), options=leafletOptions(zoomSnap=0.25)) %>%
            
            setView(34.9253, -2.4978, 10.5) %>% 
            addTiles() %>% # or satellite image: addProviderTiles(providers$Esri.WorldImagery)
            
            addPolygons(
                data = grid_om(),
                fillColor = ~pal.om()(grid_om()$Predicted),
                fillOpacity = 1, 
                weight = 1, # stroke weight of lines
                color = "gray", # color of lines
                label = map_labels_om(),
                highlight = highlightOptions(
                    weight = 2,
                    color = "white",
                    fillOpacity = 1,
                    bringToFront = TRUE)
            ) %>% 
            
            addLegend_decreasing(pal = pal.om(), 
                                 values = ~Predicted,
                                 opacity = 1, 
                                 title = "Predicted Occupancy",
                                 position = "topleft",
                                 decreasing = TRUE)
        
    }) 
    
    
    # Render outputs -----------------------------
    
    # render a reactive table that shows a summary by species
    output$species_table <- renderTable({
        species_summary()
    })
    
    # render a reactive graph with RAI against other variable
    output$rai_metadata <- renderPlotly({
        ggplotly(ggplot(data = rai_metadata(),
                        aes_string(x = input$metadata_select, y = "RAI.det", label = "site")) +
                     geom_point() +
                     geom_smooth(method = "lm", col = "gray") +
                     theme_bw()
        )
    }) 
    ### ^ update this so have choices of whether use counts or detections
    
    # render a reactive graph with RAI every month
    output$monthly_rai_hist <- renderPlotly({
        ggplotly(ggplot(data = (monthly_rai()),
                        aes(x = Month, y = RAI.det, fill = Season)) +
                     geom_bar(stat = "identity") +
                     scale_fill_manual(values=c("#999999", "#00BFC4")) + 
                     theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
        )
    })
    ### ^ update this so have choices of whether use counts or detections
    
    
    # render a reactive graph with the activity patterns of the selected species
    output$activity_plot <- renderPlot({
        timeplot(records_subset()$Time.Sun)
    })
    
# DATASET COMPARISON -----------------------------
    
    # Subset based on widgets comparison ---------
    
    # create reactive object records_subset that changes based on delta time and date range
    records_subset_A <- reactive({
        dat %>%
            filter(delta.time.secs == 0 | delta.time.secs >= (60 * input$independent_min_A),
                   Date >= input$date_range_A[1], Date <= input$date_range_A[2]) %>%
            filter(species == input$species_select_A)
    })
    
    records_subset_B <- reactive({
        dat %>%
            filter(delta.time.secs == 0 | delta.time.secs >= (60 * input$independent_min_B),
                   Date >= input$date_range_B[1], Date <= input$date_range_B[2]) %>%
            filter(species == input$species_select_B)
    })
    
    # calculate RAI
    rai_A <- reactive({
        cbind(rai.calculate(records_subset_A(), camera_operation, input$date_range_A[1], 
                            input$date_range_A[2]), Subset = "A") 
    })
    
    rai_B <- reactive({
        cbind(rai.calculate(records_subset_B(), camera_operation, input$date_range_B[1], 
                            input$date_range_B[2]), Subset = "B")
    })
    
    rai_AB <- reactive({
        bind_rows(rai_A(), rai_B()) %>%
            select(site, RAI.det, Subset) %>%  ## ADD OPTIONS FOR RAI.COUNT 
            spread(key = Subset, value = RAI.det)  ## ADD OPTIONS FOR RAI.COUNT 
    }) 
    
    # calculate monthly RAI
    monthly_rai_A <- reactive({
        cbind(rai.monthly(records_subset_A(), camera_operation, input$date_range_A[1], 
                          input$date_range_A[2]), Subset = "A")
    })
    
    monthly_rai_B <- reactive({
        cbind(rai.monthly(records_subset_B(), camera_operation, input$date_range_B[1], 
                          input$date_range_B[2]), Subset = "B")
    })
    monthly_rai_AB <- reactive({
        bind_rows(monthly_rai_A(), monthly_rai_B())
    })
    
    # Render outputs for comparison -------------------------------------------
    
    # render a reactive graph with the activity patterns of the selected species
    output$activity_plot_compare <- renderPlot({
        overlapPlot2(records_subset_A()$Time.Sun, records_subset_B()$Time.Sun)
        legend('top', c("Subset A", "Subset B"), lty=c(1,1), col = c("#F8766D", "#00BFC4"), bty='n')
    })  
    
    # calculate overlap value
    output$activity_overlap <- renderText({
        paste("Overlap of daily activity density (ranging from 0 to 1) = ", round(overlapEst(records_subset_A()$Time.Sun, records_subset_B()$Time.Sun, type = "Dhat4"), digits = 3))
    })
    
    # render a reactive graph with both RAI against each other
    # switch to log scale based on radio button
    
    output$rai_AB <- renderPlotly({
        
        if(input$log_select == 1) {
            ggplotly(ggplot(data = rai_AB(),
                            aes(x = A, y = B, label = site)) +
                         geom_point() +
                         geom_smooth(method = "lm", col = "gray") +
                         theme_bw())
        } else {
            ggplotly(ggplot(data = rai_AB(),
                            aes(x = log(A + 0.001), y = log(B + 0.001), label = site)) +
                         geom_point() +
                         geom_smooth(method = "lm", col = "gray") +
                         theme_bw())      
        }
        
        
    })
    
    # render a reactive graph with side-by-side barplot
    output$rai_monthly_AB <- renderPlotly({
        ggplotly(ggplot(data = (monthly_rai_AB()),
                        aes(x = Month, y = RAI.det, fill = Subset)) + ##UPDATE TO CHOICE COUNT 
                     geom_bar(stat = "identity", position = "dodge") +
                     scale_fill_manual(values=c("#F8766D", "#00BFC4")) +
                     theme(axis.text.x = element_text(angle = 45, hjust = 1)))
    })
    
}
