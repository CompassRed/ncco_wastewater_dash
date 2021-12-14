shinyServer(function(input, output, session) {

# JAVASCRIPT DECLARATION --------------------------------------------------

    #turn menu elements into linkedimages
    observe({
    shinyjs::runjs('
    
    function tabNameImage(value,link,image_src,width) {
        let div = document.querySelector(\'[data-value=\'+value+\']\');
        div.setAttribute("href",link);
        div.setAttribute("target","_blank");
        div.removeAttribute("data-toggle");
        div.innerHTML = "";
        let elem = document.createElement("img");
        elem.setAttribute("src", image_src);
        elem.setAttribute("height", "100%");
        elem.setAttribute("width", width);
        div.appendChild(elem);
    }
    
    tabNameImage(value = "NCCo",link = "https://www.nccde.org/",image_src = "ncco_logo.png",width = "80em");
    //tabNameImage(value = "CompassRed",link = "https://www.compassred.com/",image_src = "cr_logo.png",width = "80em");
    //tabNameImage(value = "BioBot",link = "https://www.biobot.io/",image_src = "biobot_logo.png",width = "20em");
    
                   ')
    })
    
    #When FAQ text is clicked, change tabs
    shinyjs::onclick("faq_click",{
        shinyjs::runjs("$('a[data-value=\\'faq\\']').trigger('click');")
    })
    
    #FIX DIV HEIGHTS/WIDTHS WHEN DEVICE WIDTH CHANGES
    observeEvent(map_data(),{
        #when browser window is mobile width, remove the absolute position-based element that is included in the map div. also on mobile, add some space between cards.
        shinyjs::runjs("if ($(window).width() < 768) {
            let map_div = document.querySelector('#map_div .flexfill-item-inner');
            //map_div.setAttribute(\"class\", \"row\");
            map_div.removeAttribute(\"style\");
            
            let map_div2 = document.querySelector('#map_div2 .flexfill-item-inner');
            map_div2.removeAttribute(\"style\");
            
            $('div.ui.card').slice(0,2).attr('style','margin-top: 20px; margin-bottom:30px; padding: 0; width: 100%;');
        }")
        
        #when browser is desktop width, make the map the height of the page
        shinyjs::runjs("if ($(window).width() >= 768) {
            let div = document.querySelector('#map');
            div.setAttribute(\"style\",\"height: calc(100vh - 80px) !important;\");
            
            let div2 = document.querySelector('#map2');
            div2.setAttribute(\"style\",\"height: calc(100vh - 80px) !important;\");
        }")
    })

    # 
# DATA LOAD ---------------------------------------------------------------
    
    download.file("https://www.nccde.org/DocumentCenter/View/38959/covidupdate", "saved_data.xlsx", mode = "wb")
    if(Sys.getenv("LOCAL")){
        filename <- "test_data_new.xlsx"
        print("LOCAL")
    }else{
        filename <- "saved_data.xlsx"
    }
    
    data_init <- read_excel(filename,
                            sheet = "Clean Data"
    ) %>% 
        rename(log_levels = `SARS-CoV-2 Copies/L`,
               lower = `Lower Limit (Copies/L)`,
               upper = `Upper Limit (Copies/L)`) %>% 
        janitor::clean_names() %>% 
        mutate(date = as.Date(date)) %>% 
        mutate(upper = case_when(source == "Biobot" ~ log_levels,
                                 T ~ upper),
               lower = case_when(source == "Biobot" ~ log_levels,
                                 T ~ lower)
        )
               
    
    data_lookup <- read_excel(filename,
                              sheet = "Parent Lookup"
    ) %>% janitor::clean_names()
    
    data_county_overview <- read_excel(filename,
                                sheet = "County Level Case Data"
    ) %>% janitor::clean_names() %>% 
        #rename(log_levels = value) %>% 
        mutate(date = as.Date(date))
    
    data_raw <- data_init %>% left_join(data_lookup,by = c("station" = "station"))
    
    
    # ADD IN SHAPES/LOCATIONS -------------------------------------------------
    
    data_shapes <- readRDS("data/site_shapes.RDS")
    data_locations <- readRDS("data/site_locations.RDS")
    
    wilmington_combined_shape <- geojsonsf::geojson_sf("data/data_raw/wilmington_combined_shape.geojson") %>% 
        as.data.frame() %>% 
        pull(geometry)
    
    ncc_shape <- readRDS("data/ncc_shape.RDS")
    
    total_sample_outline <- readRDS("data/total_sample_outline.RDS")
    
    # MATCH SAMPLING SITES TO FRIENDLY NAMES ----------------------------------
    
    match_friendly_names <- function(df,field){
        
        field <- enquo(field)
        
        df %>% 
            mutate(!!field := case_when(
                !!field == "Airport PS" ~ "Bear/Glasgow/Churchmans",
                !!field == "Edgemoor PS" ~ "Brandywine Hundred/Claymont",
                !!field == "Richardson Park PS" ~ "Greenville/Elsmere/Newport",
                !!field == "South Market PS" ~ "Minquadale/N. New Castle",
                !!field == "Terminal Avenue PS" ~ "New Castle/Red Lion/Wrangle Hill",
                !!field == "White Clay Creek PS" ~ "Greater Newark/Pike Creek/Hockessin",
                !!field == "MOT Water Farm Influent" ~ "South of the Canal (excl. Middletown)",
                !!field == "Wilmington WWTP Influent" ~ "N. New Castle Co. Aggregate Sewer System",
                !!field == "City of Newark (subset of WCC PS)" ~ "City of Newark",
                !!field == "Delaware City WWTP Influent" ~ "Delaware City/St. Georges",
                !!field == "Port Penn WWTP Influent" ~ "Port Penn",
                !!field == "Christiana Hospital (subset of WCC PS)" ~ "Christiana Hospital",
                TRUE ~ !!field
            ))
    }
    
    # LOAD GLOBAL DATA ----------------------------------
    
    data <- data_raw %>% 
        left_join(data_shapes,by = c("station" = "name")) %>% 
        left_join(data_locations,by = c("station" = "name")) %>% 
        mutate(station_proper = station) %>% 
        match_friendly_names(station) %>% 
        match_friendly_names(parent)
    
    de_counties_shape <- readRDS("data/de_counties_shape.RDS")
        
    #add labels to master df
    data_rc_master <- reactive({
        
        data %>% 
            mutate(date = lubridate::as_date(date)) %>% 
            mutate(label_both = paste0(station,": ",scales::comma(log_levels,1)),
                   label_both_date = paste0("Date: ",format(date,"%m/%d"), "<br>","Virus Levels (viral copies/L): ",scales::comma(log_levels,1)),
                   parent_label = ifelse(parent != "None",glue::glue("Subset of: {parent}<br>"),""),
                   label_full = glue::glue("<b>{station}</b><br>
                                           {parent_label}
                                           Date: {format(date,\"%m/%d\")}<br>
                                            <i>Virus Levels (viral copies/L): {scales::comma(log_levels,1)}</i><br>"),
                   label_full = map(.x = label_full,
                                    .f =  htmltools::HTML),
                   label_sampling_site = glue::glue("<b>{station}</b><br>
                                                    {parent_label}
                                                    <u>Sampling Site Location</u><br>
                                                    Date: {format(date,\"%m/%d\")}<br>
                                                    <i>Virus Levels (viral copies/L): {scales::comma(log_levels,1)}</i><br>"),
                   label_sampling_site = map(.x = label_sampling_site,
                                    .f =  htmltools::HTML),
                   )
    })
    
    data_rc <- reactive({
        data_rc_master() %>% 
            filter(station != "N. New Castle Co. Aggregate Sewer System")
    })
    
    #update station selectize input
    output$station_select <- renderUI({
        choices <- data_rc_master() %>%
            select(station) %>%
            unique() %>% pull()
        
        selectizeInput("station_select",NULL,choices = choices,selected = choices[1],width = "100%")
    })

# AS OF DATE RIBBON --------------------------------------------------------------
    
    as_of_date <- reactive({
        req(data_rc_master()) %>% filter(date == max(date)) %>% select(date)  %>% mutate(date = as.character(date)) %>% unique()
    })
    
    output$as_of_date_ps <- renderUI({
        tags$a(icon("calendar")," ",paste0("Data as of or before: ",req(as_of_date())),class = "ui grey ribbon label")
    })
    
    as_of_date2 <- reactive({
        req(data_rc_master()) %>% filter(date == max(date)) %>% select(date)  %>% mutate(date = as.character(date)) %>% unique()
    })
    
    output$as_of_date_ps2 <- renderUI({
        tags$a(icon("calendar")," ",paste0("Data as of or before: ",req(as_of_date())),class = "ui grey ribbon label")
    })

# MAPPING -----------------------------------------------------------------

    map_data <- reactive({
        
    screen_width <- input$screen_width
    if(!is.null(screen_width)){
       
        #get current screen width, adjust view coordinates based on this          
        print(paste0("Current screen width: ",screen_width,"px"))
        if(screen_width <= 768){
            set_view_fun <- function(data) {setView(data,-75.6, 39.65, 10)}
        }else{
            set_view_fun <- function(data) {setView(data,-75.55, 39.5, 10)}
        }
        
        #data prep
        data_rc_master <- data_rc_master()
        
        data_wilmington_wwtp <- data_rc_master %>%
            filter(station == "N. New Castle Co. Aggregate Sewer System") %>% 
            filter(date == max(date))
        
        data_christiana <- data_rc_master %>%
            filter(station == "Christiana Hospital") %>% 
            filter(date == max(date))
        
        data_newark <- data_rc_master %>%
            filter(station == "City of Newark") %>% 
            filter(date == max(date))
        
        data_prep <- data_rc_master %>% group_by(station) %>% filter(date == max(date)) %>% ungroup() %>% arrange(desc(log_levels)) %>% 
            filter(station != "N. New Castle Co. Aggregate Sewer System",
                   station != "Christiana Hospital")
        
        data_counties <- de_counties_shape %>% filter(name %in% c("New Castle","Kent","Sussex"))
        
        data_all <- data_rc_master %>% group_by(station) %>% filter(date == max(date)) %>% ungroup()
        
        p_all <- data_all
        
        # trend_plot <- function(station){
        # 
        #   data_rc_master %>% filter(station == station) %>%
        #     ggplot(aes(date,log_levels))+
        #     geom_line()
        # }
        # 
        # p_all <- lapply(unique(data_rc_master$station), trend_plot)
        # 
        
        #make color palettes
        col_pal <- colorNumeric(
            palette = colorRampPalette(viridis(3))(length(data_all$log_levels)), 
            domain = data_all$log_levels)
        col_pal_rev <- colorNumeric(
            palette = colorRampPalette(rev(viridis(3)))(length(data_all$log_levels)), 
            domain = data_all$log_levels)
        
        data_prep %>% 
            leaflet(options = leafletOptions(zoomControl = FALSE,attributionControl = FALSE,worldCopyJump = TRUE)) %>% 
            addTiles() %>% 
            
            ##Leaflet Attribution
            # leaflet::addControl('<a href="leaflet.js">Leaflet</a> | &copy; <a href="openstreetmap.org">OpenStreetMap</a> contributors',position = "bottomleft") %>% 
            addProviderTiles(providers$CartoDB.Positron, group = "Canvas") %>% 
            set_view_fun() %>% 
            
            #COUNTY OUTLINES
            addPolygons(data = data_counties$geometry,fillColor = "grey",fillOpacity = 0.2,color = "black",opacity = .5,weight = 1,label = HTML("Area not part of a sampled site")) %>%
            
            #WILMINGTON WWTP OUTLINE
            addPolygons(data = data_wilmington_wwtp$geometry,weight = 4,label = data_wilmington_wwtp$label_full,fillColor = "white",opacity = .7,color = "black",fillOpacity = 0,dashArray = "13",layerId = data_wilmington_wwtp$station,
                        highlight = highlightOptions(
                            weight = 5,
                            opacity = 1,
                            stroke = TRUE,
                            fillOpacity = 0,
                            bringToFront = FALSE)) %>% 
            
            #ALL SHAPES OUTLINE
            addPolygons(data = data_prep$geometry,weight = 1,fillColor = col_pal(data_prep$log_levels),fill = T,opacity = .5,label = data_prep$label_full,fillOpacity = .5,color = "black",layerId = data_prep$station,group = "test",
                        highlight = highlightOptions(
                            weight = 1,
                            stroke = TRUE,
                            fillOpacity = .7,
                            bringToFront = FALSE)) %>%
          #addPopupGraphs(p_all,group = "test") %>% 
            
            #NEWARK OUTLINE
            addPolygons(data = data_newark$geometry,weight = 2,label = data_newark$label_full,fillColor = "white",opacity = 1,color = "white",fillOpacity = 0,dashArray = "6",layerId = paste0(data_newark$station,"_layerid1"),
                        highlight = highlightOptions(
                            weight = 3,
                            opacity = 1,
                            stroke = TRUE,
                            fillOpacity = 0,
                            bringToFront = FALSE)) %>% 
            
            #WILMINGTON OUTLINE
            addPolygons(data = wilmington_combined_shape,weight = 1,label = HTML("<b>City of Wilmington</b><br>
            • Not sampled individually<br>• Included in the N. New Castle Co. Aggregate Sewer System sample"),fillColor = col_pal(data_wilmington_wwtp$log_levels),opacity = .5,color = "black",fillOpacity = .5,layerId = paste0(data_wilmington_wwtp$station,"_layerid2"),
                        highlight = highlightOptions(
                            weight = 1,
                            opacity = 1,
                            stroke = TRUE,
                            fillOpacity = .7,
                            bringToFront = FALSE)) %>% 
            
            #MARKERS FOR EACH SAMPLING SITE
            addCircleMarkers(~longitude, ~latitude, label = ~label_sampling_site,
                             opacity = .5,color = "black", weight = 2,radius = 7,
                             fillColor = ~col_pal(log_levels),fillOpacity = .8,
                             layerId = ~station) %>% 
            #CHRISTIANA HOSPITAL MARKER
            addCircleMarkers(data = data_christiana,~longitude, ~latitude, label = ~label_sampling_site,
                             opacity = .7,color = "white",fillColor = ~col_pal(log_levels), fillOpacity = .8, weight = 2,radius = 7,layerId = ~station) %>% 
            #WILMINGTON MARKER
            addCircleMarkers(data = data_wilmington_wwtp,~longitude, ~latitude, label = ~label_sampling_site,
                             opacity = .5,color = "black", weight = 2,radius = 7,fillColor = ~col_pal(log_levels), fillOpacity = 1,layerId = ~station) %>% 
            
            ##SEARCH FUNCTIONALITY
            # addSearchOSM(options = searchOptions(autoCollapse = TRUE,
            #                                      minLength = 2,
            #                                      zoom = 10,
            #                                      position = "topright")) %>% 
            # #LEGEND (overlaps with divs, would like to put this in the plotting area)
            # addLegend("bottomleft", pal = col_pal_rev, values = data_prep$prevelance_rate,
            #           title = "Estimated<br>Prevalance Rate",
            #           labFormat = labelFormat(transform = function(x) sort(x*100,decreasing = TRUE),suffix = ".0%"),
            #           opacity = 1
            # ) %>% 
            
            #Put +/- zoom functionality at bottom left
            htmlwidgets::onRender("function(el, x) {
                                    L.control.zoom({position:'bottomleft'}).addTo(this);
                                  }")
    }
        
    })
    
    #if FAQ is selected and the user navigates back to the map, reload leaflet
    map_click_counter <- reactiveVal(0)
    observeEvent(input$parent_page,{
        if(isolate(input$parent_page) == "map"){
            new_val <- map_click_counter() + 1
            map_click_counter(new_val)
        }
    },ignoreInit = T)
    
    output$map <- renderLeaflet({
        map_click_counter()
        req(map_data())
    })
    
    output$map2 <- renderLeaflet({
        screen_width <- input$screen_width
        if(!is.null(screen_width)){
            
            county_data <- data_county_overview %>% 
                      mutate(cases_roll7 = zoo::rollmean(cases,k = 7,fill = NA)) %>% 
                      filter(!is.na(cases_roll7)) %>% 
                      filter(date == max(date))
            
            cases <- county_data %>% 
                     select(cases_roll7) %>% 
                     pull() %>% 
                     scales::comma(1)
            
            #get current screen width, adjust view coordinates based on this          
            if(screen_width <= 768){
                set_view_fun <- function(data) {setView(data,-75.6, 39.65, 10)}
            }else{
                set_view_fun <- function(data) {setView(data,-75.55, 39.5, 10)}
            }
            
        county_outline_label <- paste0("<b>New Castle County</b><br>
                                        Date: ",format(unique(county_data$date),"%m/%d"),"<br>",
                                       "<i>Latest Confirmed COVID-19 Cases: ",cases,"</i>")
        
        # total_sample_outline_label <- paste0("<b>Total Sampled Area</b><br>
        #                                       <i>Total Log Levels: ",total_log_levels,"</i>")
        
        data_wilmington_wwtp <- data_rc_master() %>%
            filter(station == "N. New Castle Co. Aggregate Sewer System") %>% 
            filter(date == max(date))
            
        leaflet(options = leafletOptions(zoomControl = FALSE,attributionControl = FALSE,worldCopyJump = TRUE)) %>% 
            addTiles() %>% 
            addProviderTiles(providers$CartoDB.Positron, group = "Canvas") %>% 
            set_view_fun() %>% 
            addPolygons(data = ncc_shape,fillColor = "blue",fillOpacity = 0.1,color = "blue",opacity = .4,weight = 1,label = HTML(county_outline_label)) %>% 
            # addPolygons(data = total_sample_outline,weight = 1,opacity = .4,fillColor = "orange",color = "grey",fillOpacity = .4,label = HTML(total_sample_outline_label))
            addPolygons(data = data_wilmington_wwtp$geometry,weight = 4,label = data_wilmington_wwtp$label_full,fillColor = "orange",opacity = .7,color = "black",fillOpacity = .4,dashArray = "13",layerId = "total_view",
                        # highlight = highlightOptions(
                        #     weight = 5,
                        #     opacity = 1,
                        #     stroke = TRUE,
                        #     fillOpacity = 0,
                        #     bringToFront = FALSE)
                        )
        }
    })
    
    map_proxy <- leafletProxy("map", session = session)

# CLICKED STATION EVENTS --------------------------------------------------
    
    #when a shape is clicked, update selectize input
    observe({

        if(!is.null(req(input$map_shape_click$id))){
            selected_station <- input$map_shape_click$id
            selected_station <- stringr::str_remove(selected_station,pattern = "_layerid[0-9]")
            shiny::updateSelectizeInput(session,"station_select",selected = selected_station)
        }
        
    })
    
    #when a marker is clicked, update selectize input
    observe({

        if(!is.null(req(input$map_marker_click$id))){
            selected_station <- input$map_marker_click$id
            shiny::updateSelectizeInput(session,"station_select",selected = selected_station)
        }
        
    })
    
    #when a barchart is clicked, update selectize input
    observe({

        if(!is.null(event_data("plotly_click", source = "bar_chart"))){
            selected_station <- event_data("plotly_click", source = "bar_chart")$key
            shiny::updateSelectizeInput(session,"station_select",selected = selected_station)
        }
        
    })
    
    #when the selectize input changes, fly to the relevant area on the map
    observeEvent(req(input$station_select),{
        
        #get geometry data
        filtered_data <- isolate(data_rc_master()) %>% filter(station == req(input$station_select))
        shape_geometry <- filtered_data %>% select(geometry) %>% unique() %>% pull()
        
        #if there is no shape geometry, fly to point instead
        if(is.na(st_bbox(filtered_data$geometry)[1])){
            shape_geometry_centroid <- filtered_data %>% select(longitude,latitude) %>% unique() %>% unlist() %>% unname()
        } else{
            shape_geometry_centroid <- st_centroid(shape_geometry[1])
            shape_geometry_centroid <- shape_geometry_centroid[[1]]
        }
        
        flyTo(map_proxy, shape_geometry_centroid[1], shape_geometry_centroid[2], zoom = 12)
    },ignoreInit = T,ignoreNULL = T)
    

# CHARTS ------------------------------------------------------------------
    
    render_bar_chart <- function(data_input,selected_field){
        selected_field <- sym(selected_field)
        
        data_input_prep <- data_input %>% 
            group_by(station) %>% 
            filter(date == max(date)) %>% 
            ungroup() %>% 
            arrange(desc(!!selected_field)) %>% 
            mutate(field_normalized = scales::rescale(!!selected_field))
        
        col_pal_fun <- colorNumeric(
            palette = colorRampPalette(viridis(3))(nrow(data_input_prep)), 
            domain = data_input_prep %>% select(!!selected_field) %>% pull()
            )
        
        data_input_color <- data_input_prep %>% 
            mutate(hex_code = map(!!selected_field,col_pal_fun) %>% unlist()) %>% 
            mutate(hex_code_darker = colorspace::darken(hex_code,amount = .5))
        
        data_input_clean <- data_input_color %>% 
            filter(station != "N. New Castle Co. Aggregate Sewer System") %>% 
            mutate(na_add = ifelse(is.na(log_levels)," (no data)","")) %>% 
            mutate(station_na_prep = paste0(station,na_add)) %>% 
            mutate(station_html = case_when(
                #station %in% c("South of the Canal (excl. Middletown)", "Port Penn", "Delaware City/St. Georges") ~ paste0("<i>",station_na_prep,"</i>"),
                TRUE ~ station_na_prep
            ))
        
        ggplotly(data_input_clean %>% 
            ggplot(aes(reorder(station_html,!!selected_field),!!selected_field,text = label_both_date,key = station)) +
            geom_col(aes(fill = hex_code)) +
            geom_errorbar(aes(ymin=lower, ymax=upper,color = hex_code_darker),alpha = .7, width=.1
                              #,position=position_dodge(.9)
                          ) +
            coord_flip(ylim = c(100,NA)) + #to do: bar coming in from below 4
            # scale_y_continuous(labels = scales::comma_format(1)) +
            scale_y_log10(labels = scales::comma_format(1)) +
            labs(x = "",
                 y = "") +
            theme_compassred() +
            scale_fill_identity() +
            scale_color_identity() +
            theme(legend.position = "none"),
            tooltip = "text",
            source = "bar_chart") %>% 
            config(displayModeBar = F) %>% 
            layout(margin = list(t = 5))
    }
    
    output$bar_chart <- renderPlotly({
        render_bar_chart(data_input = data_rc_master(),selected_field = "log_levels")
    })
    
    output$line_chart <- renderPlotly({
        
        data <- data_rc_master() %>% filter(station == req(input$station_select)) %>% 
            mutate(raw_log_levels = 10^log_levels)
        
        annotation_y_value <- max(data$log_levels)
        
        break_dates <- data$date %>% unique()
        break_dates <- break_dates[c(TRUE, FALSE)]
        
        secondary_y <- list(
            #tickfont = list(size=11.7),
            #titlefont=list(size=14.6),
            overlaying = "y",
            #tickvals = c(10000,100000,10000000),
            #type = "log",
            #nticks = 5,
            side = "right",
            title = ""
        )
        

        plot_output <- 
            ggplotly(data %>% 
            ggplot(aes(date,log_levels,group = station,text = label_both_date)) +
            geom_ribbon(aes(ymin = lower,ymax = upper),fill = "lightblue",alpha = .3) +
            geom_hline(yintercept=4, linetype="dashed", color = "grey") +
            geom_line(color = "#666666") +
            geom_point(color = "#666666",size = 1) +
            geom_vline(xintercept=as.numeric(as.Date("2020-08-13")), linetype="dashed", color = "black",alpha = .4) +
            geom_vline(xintercept=as.numeric(as.Date("2021-10-01")), linetype="dashed", color = "black",alpha = .4) +
            geom_segment(aes(x = as.Date("2020-05-07"),xend = as.Date("2021-09-23"),y = 10000, yend = 10000),linetype="dashed",color = "black",alpha = 0.1,size = .5) +
            scale_x_date(date_labels = "%b '%y") +
            scale_y_log10(labels = scales::comma_format(1)) +
            coord_cartesian(ylim = c(100,NA)) +
            expand_limits(y = 0) +
            labs(x = "",
                 y = "") +
            theme_compassred() +
            theme(legend.position = "none"),
            tooltip = "text") %>% 
            config(displayModeBar = F)
        
        plot_output <- plot_output %>% 
            layout(margin = list(t = 0,b = 0))
        
        plot_output
        
        # line <- list(
        #     type = "line", 
        #     x0 = as.Date("2021-01-01") %>% as.numeric(), x1 = as.Date("2021-05-01") %>% as.numeric(),
        #     y0 = 5000, y1 = 5000,
        #     line = list(color = "green",width="1",dash="solid")
        # )
        # 
        # plot_output$x$layout$shapes <- list(
        #     if (is.null(plot_output$x$layout$shapes)) list() else plot_output$x$layout$shapes,
        #     line)
        # 
        # print(plot_output$x$layout$shapes)
        
    })
    
    output$county_cases_line_chart <- renderPlotly({
        
        min_date <- data_rc_master() %>% filter(station == "N. New Castle Co. Aggregate Sewer System") %>% 
            pull(date) %>% unique() %>% min()
        
        data <- data_county_overview %>% 
            filter(date >= min_date) %>% 
            mutate(cases_roll7 = zoo::rollmean(cases,k = 7,fill = NA))
        
        break_dates <- data$date %>% unique()
        break_dates <- break_dates[c(TRUE, FALSE)]
        
        plot_output <- 
            ggplotly(data %>% 
                         ggplot() +
                         geom_line(aes(x = date, y = cases,group=1,text = paste0("Date: ",format(date,"%m/%d"),"<br>Confirmed Cases: ",scales::comma(cases,1))),color = "blue",alpha = .2) +
                         geom_line(aes(x = date,y = cases_roll7,group=2,text = paste0("Date: ",format(date,"%m/%d"),"<br>Confirmed Cases (Rolling 7-Day Avg.): ",scales::comma(cases_roll7,1))),color = "blue") +
                         # scale_x_date(breaks = function(x) c(seq.Date(from = min(break_dates), to = max(break_dates), by = "8 weeks"),max(data$date)),labels = scales::date_format("%m/%d")) +
                         scale_x_date(date_labels = "%b '%y") +
                         scale_y_continuous(labels = scales::comma_format(1)) +
                         labs(x = "",
                              y = "") +
                         theme_compassred() +
                         theme(legend.position = "none"),
                     tooltip = "text") %>% 
            config(displayModeBar = F) %>% 
            layout(margin = list(t = 0,
                                 l = 65.5))
        
        plot_output
    })
    
    output$total_sample_line_chart <- renderPlotly({
        
        data <- data_rc_master() %>% filter(station == "N. New Castle Co. Aggregate Sewer System")

        break_dates <- data$date %>% unique()
        break_dates <- break_dates[c(TRUE, FALSE)]
        
        plot_output <- 
            ggplotly(data %>% 
                         ggplot(aes(date,log_levels,group = station,text = label_both_date)) +
                         geom_ribbon(aes(ymin = lower,ymax = upper),fill = "orange",alpha = .3) +
                         geom_hline(yintercept=4, linetype="dashed", color = "grey") +
                         geom_line(color = "#505050") +
                         geom_point(color = "#505050",size = 1) +
                         geom_vline(xintercept=as.numeric(as.Date("2020-08-13")), linetype="dashed", color = "black",alpha = .4) +
                         geom_vline(xintercept=as.numeric(as.Date("2021-10-01")), linetype="dashed", color = "black",alpha = .4) +
                         geom_segment(aes(x = as.Date("2020-05-07"),xend = as.Date("2021-09-23"),y = 10000, yend = 10000),linetype="dashed",color = "black",alpha = 0.1,size = .5) +
                         scale_x_date(date_labels = "%b '%y") +
                         scale_y_log10(labels = scales::comma_format()) +
                         ggplot2::coord_cartesian(ylim = c(100,NA)) +
                         expand_limits(y = 0) +
                         labs(x = "",
                              y = "") +
                         theme_compassred() +
                         theme(legend.position = "none"),
                     tooltip = "text") %>% 
            config(displayModeBar = F)
        
        plot_output %>% 
            layout(
                margin = list(t = 0,b = 0)
                # shapes=list(list(
                #     type = "line", 
                #     x0 = 1, x1 = .5, xref = "paper",
                #     y0 = 5000, y1 = 5000,yref = "y",
                #     line = list(color = "red",width="5",dash="solid")
                # ))
            )
    })
    
    

# N NEW CASTLE CO SCORECARD -----------------------------------------------

    output$n_ncco_scorecard <- shiny::renderUI({
        data <- data_rc_master() %>% 
            filter(station == "N. New Castle Co. Aggregate Sewer System") %>% 
            filter(date == max(date))
        
        text_out <- paste0("<span style = 'font-weight:bold;'>","Northern New Castle County Aggregate Sewer System","</span><br><i>Virus Levels (viral copies/L): ",scales::comma(data$log_levels,1),"</i> ( ",icon("info")," )") %>%
            HTML()
            
        tags$p(tags$span(tags$strong("———"),style = "color: black; font-weight: bolder;"),text_out)
        
    })
})
