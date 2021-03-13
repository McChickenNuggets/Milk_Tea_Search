library(tidyverse)
library(jsonlite)
library(lubridate)
library(httr)
library(shiny)
library(shinythemes)
library(shinyjs)
library(leaflet)
library(rvest)
library(tidytext)
library(wordcloud)
readRenviron(".Renviron")
YELP_TOKEN = Sys.getenv("YELP_TOKEN")
# Define UI for application that draws a histogram
ui <- navbarPage(title = "Yelp Search",
    tabPanel(title = "General Milk Tea Search",
        sidebarLayout(
            sidebarPanel(
                title = "General Search",
                textInput(inputId = "search",label = "Enter a location"),
                p('(E.g. "Davis", "Sacramento", "Woodland")'),
                actionButton(inputId = "search_go",label = "go"),
                selectInput(inputId = "select_milk_tea",label = "Select a Milk Tea Store",choices = ""),
                actionButton(inputId = "click", label = "click"),
                width = 4
            ),
            mainPanel(
                h2("Milk Tea Stores"),
                DT::dataTableOutput("Store_List_Table"),
                hr(nrows =3),
                h4("Milk Tea Store Info"),
                tabsetPanel(
                    tabPanel(title = "Location",
                        leafletOutput("mymap")
                    ),
                    tabPanel(
                        title = "Photos",
                        h3("photos"),
                        h5("photo 1"),
                        uiOutput("picture1"),
                        h5("photo 2"),
                        uiOutput("picture2"),
                        h5("photo 3"),
                        uiOutput("picture3")
                        ),
                    tabPanel("Operating time",tableOutput("operating_time")),
                    tabPanel("Customer Review",tableOutput("customer_reviews"),textOutput("Note"),plotOutput("Wordcloud"))
                )
               ,
                width = 8
            ),
        ),
    ),

    tabPanel(title = "Other",
             h1("I Like Milk Tea")
    ),

    theme = shinytheme("flatly"),
    useShinyjs()
)


server <- function(input, output,session) {
    rv1<-reactiveValues(data=NULL)

    observe(
        if(is.null(input$search) || input$search == ""){
            disable("search_go")
            disable("click")
        }
        else{
            enable("search_go")
        }
    )

    observe(
        if(!is.null(input$select_milk_tea)&&input$select_milk_tea != ""){
            enable("click")
        }
    )

    observeEvent(input$search_go,{
            data<-GET(
                "https://api.yelp.com/v3/businesses/search",
                add_headers(Authorization = paste("Bearer", YELP_TOKEN)),
                query = list(
                    term = "Bubble Tea",
                    location = input$search
                )
            )
            json <- content(data, as = "text",encoding = "UTF-8")
            stores<-fromJSON(json, flatten = TRUE)$businesses %>% select(1,3:5,7:11,13:14,17)
            rv1$store_list<-stores
            updateSelectInput(session, "select_milk_tea", choices = c("-", stores$name, selected = "-"))

        }
    )

    output$Store_List_Table<-DT::renderDataTable(
        if(input$search==" "){
            NULL
        }else{
            if(is_null(rv1$store_list)){
               NULL
            }else{
                rv1$store_list %>% select(name,is_closed,review_count,rating,price,location.address1)
            }
        },
        options = list(pageLength = 5)
    )

    observeEvent(input$click,{
        req(input$select_milk_tea !="-", cancelOutput = TRUE)
            id_selected<-rv1$store_list %>% filter(name==input$select_milk_tea) %>% pull(id)
            url<-str_glue("https://api.yelp.com/v3/businesses/{id}",id=id_selected)
            data<-GET(
                 url,
                 add_headers(Authorization = paste("Bearer", YELP_TOKEN))
            )
            json <- content(data, as = "text",encoding = "UTF-8")
            rv1$operating_hours<-fromJSON(json, flatten = TRUE)$hours$open[[1]]
            rv1$photos<-fromJSON(json,flatten=TRUE)$photos
            rv1$coordinates<-fromJSON(json,flatten = TRUE)$coordinates
            r<-GET(
                str_glue("https://api.yelp.com/v3/businesses/{id}/reviews",id=id_selected),
                add_headers(Authorization = paste("Bearer", YELP_TOKEN))
            )
            json2<-content(r, as = "text",encoding = "UTF-8")
            rv1$reviews<-fromJSON(json2, flatten = TRUE)$reviews %>% select(user.name,text,rating)
            rv1$token <- rv1$reviews %>% unnest_tokens(word,text) %>% anti_join(stop_words) %>% count(word, sort = TRUE)
        }
    )

    output$mymap<-renderLeaflet(
        if(is_null(rv1$coordinates)){
            NULL
        }else{
            leaflet() %>%
                addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
                ) %>%
                addMarkers(data = cbind(rv1$coordinates$longitude,rv1$coordinates$latitude))
        }
    )
    
    output$Note<-renderText(
        if(is_null(rv1$token)){
            NULL
        }else{
            "Word Cloud on Reviews"
        }
    )
    
    output$Wordcloud<-renderPlot(
        if(is_null(rv1$token)){
            NULL
        }else{
            rv1$token %>% 
                with(wordcloud(
                    word, n, min.freq = 1, max.words = 25, random.order = FALSE,
                    colors = brewer.pal(8, "Dark2")))
        }
    )

    output$operating_time<-renderTable(
        if(input$select_milk_tea=="-"){
            NULL
        }else{
            rv1$operating_hours
        },
        caption = "Operation Time",
        caption.placement = getOption("xtable.caption.placement", "top"), 
        caption.width = getOption("xtable.caption.width", NULL)
    )

    output$picture1<-renderUI(
        if(is_null(input$select_milk_tea)){
            NULL
        }else{
            tags$img(src = rv1$photos[1])
        }
    )

    output$picture2<-renderUI(
        if(is_null(input$select_milk_tea)){
            NULL
        }else{
            tags$img(src = rv1$photos[2])
        }
    )

    output$picture3<-renderUI(
        if(is_null(input$select_milk_tea)){
            NULL
        }else{
            tags$img(src = rv1$photos[3])
        }
    )
    
    output$customer_reviews<-renderTable(
        if(is_null(input$select_milk_tea)){
            NULL
        }else{
            rv1$reviews
        },
        caption = "Customer Reviews",
        caption.placement = getOption("xtable.caption.placement", "top"), 
        caption.width = getOption("xtable.caption.width", NULL)
    )
    
}

# Run the application
shinyApp(ui = ui, server = server)
