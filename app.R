library(shiny)
library(leaflet)
library(shinythemes)
library(shinyWidgets)
library(fontawesome)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(tidyr)

score_data = readRDS("data/score_data.rds")
imp_attribute = readRDS("data/imp_attributes_data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cyborg"),
                setBackgroundColor("#d32323"),

    # Application title
    tags$h1(fa("yelp", fill = "black"), "Yelp Data Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("cuisines",
                        tags$p(fa("yelp", fill = "red"), "Select Your Favorite Cuisine:"),
                        choices = c("Italian", "Korean", "Mexican", "Thai", "Japanese", "Ethnic Food")
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Map", leafletOutput("map")),
                tabPanel("Stars", plotOutput("plot1")),
                tabPanel("Ambience Attributes", plotOutput("plot2")),
                tabPanel("Sentiment Score", plotOutput("sent"),
                         textOutput("txt"),
                         tags$head(tags$style("#txt{color: #333333;
                                 font-size: 30px;
                                 font-style: calibri;
                                 background-color: #f5f5f5;
                                 }"
                         )
                         ),
                         textOutput("txt2"),
                         tags$head(tags$style("#txt2{color: #999999;
                                 font-size: 30px;
                                 font-style: calibri;
                                 background-color: #f5f5f5;
                                 }"
                         )
                         ))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    score_data_filter = reactive({
        aligned = score_data[str_detect(score_data$categories, input$cuisines),]
        aligned$name = factor(aligned$name,
                                  levels = unique(aligned$name[order(aligned$tot, decreasing = TRUE)])
                                  
        )
        
        aligned
                   
    })
    
    icons <- awesomeIcons(
        icon = 'ios-close',
        library = 'ion',
        markerColor = "black"
    )
    
    output$map <- renderLeaflet({
        leaflet(score_data_filter()) %>% 
            addProviderTiles(providers$Stamen.Toner) %>% 
            setView(lat = 40.116421, lng = -88.243385, zoom = 12) %>%
            addAwesomeMarkers(~longitude, ~latitude, label = score_data_filter()$name, icon=icons)
    })
    
    plot_stars = reactive({
        score_data_filter() %>% 
        group_by(name) %>% 
        summarise(Stars = mean(stars)) %>% 
            mutate(name = factor(name,
                          levels = name[order(Stars, decreasing = TRUE)])
            )
    })
    

    output$plot1 = renderPlot({
        ggplot(plot_stars()) +
            aes(x = name, y = as.numeric(Stars), fill = Stars) + 
            geom_col() +
            scale_fill_continuous(low = "#cccccc", high = "black") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            coord_cartesian(ylim = c(0,1,2,3,4,5)) +
            labs(x = "Restaurant Name",
                 y = "Stars")
    })
    
    data_tile_plot = reactive({
    
    join_attr_cuisine = inner_join(imp_attribute, score_data_filter(), by = "name")[,seq_len(ncol(imp_attribute))] %>% 
        distinct()
    
    #check which attribute has more than half NA values
    rm_col_ind = which(sapply(join_attr_cuisine, function(x) sum(is.na(x))) >= floor(nrow(join_attr_cuisine) - nrow(join_attr_cuisine)/2) )
    
    join_attr_cuisine = join_attr_cuisine[,-rm_col_ind]
    
    col_split_ambi = str_split(join_attr_cuisine$Ambience, ",") 
    
    colname_ambi = map(col_split_ambi, function(x) gsub("\\{|\\'|\\:","",str_extract(x, pattern = ".*\\:")))
    
    uni_colnames = colname_ambi[which(unlist(lapply(colname_ambi, length)) == max(unlist(lapply(colname_ambi, length))))[1]]
    
    ambi_val = map(col_split_ambi, function(x) gsub(" |\\:|\\}","",str_extract(x, pattern = "\\:.*")))
    
    conv_na = lapply(ambi_val, function(x){
        if(sum(is.na(x)) >= 1) { 
            rep(NA, length(colname_ambi[[1]]))
        } else { 
            x
        }
    })
    
    c = as.data.frame(matrix(unlist(conv_na), nrow=length(conv_na), byrow=T))
    
    colnames(c) = gsub(" ", "", unlist(uni_colnames))
    c$name = c()
    c$name = join_attr_cuisine$name
    
    cm = c %>% 
        gather(key = "attributes", value = "value", romantic:casual)
    })
    
    
    output$plot2 = renderPlot({
        ggplot(data_tile_plot()) +
            aes(attributes, name, fill = factor(value)) + 
            geom_tile() +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.key = element_rect(color= "black")) +
            scale_fill_manual(
                values = c("black", "grey"),
                breaks = waiver(),
                na.value = "white",
                labels = c("Present", "Absent", "Unknown"),
                guide = "legend"
                ) +
            labs(x = "Ambience Attribute",
                 y = "Restaurant Name",
                 fill = "Legend")
    })
    
    
    output$sent = renderPlot({
        ggplot(score_data_filter()) +
            aes(x = name, y = tot, fill = tot) +
            scale_fill_continuous(low = "#e6e6e6", high = "#333333") +
            geom_col() +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            labs(x = "Restaurant",
                 y = "Sentiment Score",
                 fill = "Sentiment Score") 
            
    })
    
    output$txt = renderText({
        paste("Based on the above sentiment score plot, we would recommend", score_data_filter()[which.max(score_data_filter()$tot),]$name,
              "for ", input$cuisines ," cuisine because it has the highest number of positive comments.")
    })
    
    output$txt2 = renderText({
        paste("However, we would not recommend", score_data_filter()[which.min(score_data_filter()$tot),]$name, "because reviewers left negative comments for it.")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)





