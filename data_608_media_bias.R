## app.R ##
library(shinydashboard)
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(tidyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(XML)
library(wordcloud)
library(rquery)
library(markdown)
library(knitr)
library(DT)
library(dslabs)
library(tm)
## Code for Word Clouds-------------------------------------http://www.sthda.com/english/wiki/word-cloud-generator-in-r-one-killer-function-to-do-everything-you-need







rquery.wordcloud <- function(x, type=c("text", "url", "file"),    
                             lang="english", excludeWords=NULL, 
                             textStemming=FALSE,  colorPalette="Dark2",
                             min.freq=3, max.words=200)
{ 
    # library("tm")
    # library("SnowballC")
    # library("wordcloud")
    # library("RColorBrewer") 
    
    if(type[1]=="file") text <- readLines(x,warn=FALSE)
    else if(type[1]=="url") text <- html_to_text(x)
    else if(type[1]=="text") text <- x
    
    # Load the text as a corpus
    docs <- Corpus(VectorSource(text))
    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    # Remove stopwords for the language 
    docs <- tm_map(docs, removeWords, stopwords(lang))
    #####################   docs <- tm_map(docs, removeWords, c("sanders","bernie"))
    # Remove punctuations
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)
    # Remove your own stopwords
    if(!is.null(excludeWords)) 
        docs <- tm_map(docs, removeWords, excludeWords) 
    # Text stemming
    if(textStemming) docs <- tm_map(docs, stemDocument)
    # Create term-document matrix
    tdm <- TermDocumentMatrix(docs)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    # check the color palette name 
    if(!colorPalette %in% rownames(brewer.pal.info)) colors = colorPalette
    else colors = brewer.pal(8, colorPalette) 
    # Plot the word cloud
    set.seed(1234)
    wordcloud(d$word,d$freq, min.freq=min.freq, max.words=max.words,
              random.order=FALSE, rot.per=0.35, 
              use.r.layout=FALSE, colors=colors)
    
    invisible(list(tdm=tdm, freqTable = d))
}

## Code for first user choice-------------------------------------
df <- read.csv("final_proj_df.csv")

colnames(df) <- c("Index","Publication","Candidate","Positive","Negative","Score")
df <- df %>% 
    filter(Publication!="Bloomberg")
df2 <- df
df$source1 <- "Publication"
df2$source1 <- "Candidate"
df <- as.data.frame(rbind(df,df2))
df$cand_image <- paste('https://raw.githubusercontent.com/justinherman42/data608/master/images/',df$Candidate,".PNG", sep="")
df$Publication_image <- paste('https://raw.githubusercontent.com/justinherman42/data608/master/images/Bloomberg.PNG',df$Publication,".PNG", sep="")
cand_text_files=c("Biden.txt","Bernie.txt","Kamala.txt","Warren.txt")



## graph word count for vocab
datatabledf <- read.csv("pos_words_counts.csv")
bernie <- datatabledf %>% 
    filter(X.1== "Bernie") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE)%>% 
    t(.)
biden <- datatabledf %>% 
    filter(X.1== "Biden") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE)%>% 
    t(.)
Kamala <- datatabledf %>% 
    filter(X.1== "Kamala") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE)%>% 
    t(.)
Warren <- datatabledf %>% 
    filter(X.1== "Warren") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
    t(.)
colnames(Warren) <-  "Warren_Counts"
colnames(Kamala) <-  "Kamala_Counts"
colnames(biden) <-  "Biden_Counts"
colnames(bernie) <-  "Bernie_Counts"
pos_datatabledf2 <- datatable(cbind(bernie,biden,Kamala,Warren))

##Grab word count by publication
Breitbart_News <- datatabledf %>% 
    filter(X == "Breitbart News") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE)%>% 
    t(.)
CNN <- datatabledf %>% 
    filter(X== "CNN") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE)%>% 
    t(.)
Fox_News <- datatabledf %>% 
    filter(X== "Fox News") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE)%>% 
    t(.)
MSNBC <- datatabledf %>% 
    filter(X== "MSNBC") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
    t(.)
The_New_York_Times <- datatabledf %>% 
    filter(X== "The New York Times") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
    t(.)
The_Washington_Post <- datatabledf %>% 
    filter(X== "The Washington Post") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
    t(.)


colnames(Breitbart_News) <-  "Breitbart_News_Counts"
colnames(CNN) <-  "CNN_Counts"
colnames(Fox_News) <-  "Fox_News_Counts"
colnames(MSNBC) <-  "MSNBC_Counts"
colnames(The_New_York_Times) <-  "The_New_York_Times_Counts"
colnames(The_Washington_Post) <-  "The_Washington_Post_Counts"



pos_df_3 <- datatable(cbind(Breitbart_News,CNN,Fox_News,MSNBC,The_New_York_Times,The_Washington_Post))


## graph word count for vocab
neg_datatabledf <- read.csv("neg_words_counts.csv")
bernie <- neg_datatabledf %>% 
    filter(X.1== "Bernie") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE)%>% 
    t(.)
biden <- neg_datatabledf %>% 
    filter(X.1== "Biden") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE)%>% 
    t(.)
Kamala <- neg_datatabledf %>% 
    filter(X.1== "Kamala") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE)%>% 
    t(.)
Warren <- neg_datatabledf %>% 
    filter(X.1== "Warren") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
    t(.)
colnames(Warren) <-  "Warren_Counts"
colnames(Kamala) <-  "Kamala_Counts"
colnames(biden) <-  "Biden_Counts"
colnames(bernie) <-  "Bernie_Counts"
neg_datatabledf2 <- datatable(cbind(bernie,biden,Kamala,Warren))

##Grab word count by publication
Breitbart_News <- neg_datatabledf %>% 
    filter(X == "Breitbart News") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE)%>% 
    t(.)
CNN <- neg_datatabledf %>% 
    filter(X== "CNN") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE)%>% 
    t(.)
Fox_News <- neg_datatabledf %>% 
    filter(X== "Fox News") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE)%>% 
    t(.)
MSNBC <- neg_datatabledf %>% 
    filter(X== "MSNBC") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
    t(.)
The_New_York_Times <- neg_datatabledf %>% 
    filter(X== "The New York Times") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
    t(.)
The_Washington_Post <- neg_datatabledf %>% 
    filter(X== "The Washington Post") %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
    t(.)


colnames(Breitbart_News) <-  "Breitbart_News_Counts"
colnames(CNN) <-  "CNN_Counts"
colnames(Fox_News) <-  "Fox_News_Counts"
colnames(MSNBC) <-  "MSNBC_Counts"
colnames(The_New_York_Times) <-  "The_New_York_Times_Counts"
colnames(The_Washington_Post) <-  "The_Washington_Post_Counts"



neg_df_3 <- datatable(cbind(Breitbart_News,CNN,Fox_News,MSNBC,The_New_York_Times,The_Washington_Post))




ui <- dashboardPage(skin = 'green',
                    
                    dashboardHeader( title = "Exploring Media Bias", titleWidth = 280),
                    dashboardSidebar(width = 240,  
                                     sidebarMenu(id = "sidebarmenu",
                                                 menuItem(text = "Introduction", tabName = "Introduction", icon = icon("newspaper")),
                                                 menuItem(text = "Methodology", tabName = "Markdown", icon = icon("dashboard")),
                                                 menuItem(text = "Data Collection", tabName = "DataCollection", icon = icon("dashboard")),
                                                 menuItem(text = "Positive vocabulary Counts", tabName = "WordCounts2", icon = icon("newspaper")),
                                                 menuItem(text = "Negative Vocabulary Counts", tabName = "NegWordCounts", icon = icon("newspaper")),

                                                 menuItem(text = "Scoring", tabName = "dashboard", icon = icon("dashboard")),
                                                 conditionalPanel("input.sidebarmenu === 'dashboard'",
                                                                  selectInput('Choice', 'Publication or Candidate Level', unique(df$source1), selected='Candidate',width ='150')),
                                                 conditionalPanel("input.sidebarmenu === 'dashboard'",
                                                                 selectInput('Color_choice', 'Color by', unique(df$source1), selected='Publication',width ='150')),
                                                 menuItem(text = "Fancy Scoring", tabName = "dashboard2", icon = icon("dashboard")),
                                                 menuItem(text = "Word Cloud", tabName = "widgets", icon = icon("th")),
                                                 conditionalPanel("input.sidebarmenu === 'widgets'",
                                                                  selectInput('Choice2', 'Wordclouds', cand_text_files, selected="Bernie.txt",width ='150')),
                                                 menuItem(text = "Contact Me", tabName = "Contact", icon = icon("dashboard"))
                                                 #  selectInput('Choice', 'Publication or Candidate Level', unique(df$source1), selected='Candidate',width ='150'),
                                                 #  selectInput('Choice2', 'Wordclouds', cand_text_files, selected="1.txt",width ='150')
                                     )),
                    
                    dashboardBody( tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
                        tabItems(
                            tabItem(tabName = "Introduction",
                                    fluidRow(infoBox(width=12,
                                                     
                                                     title = tags$p(style = "font-size: 20px;", "Media Bias in The Democratic Primaries")
                                                     
                                    )),
                                    
                                    
                                    fluidRow(uiOutput('mymarkdown')),
                                    fluidRow(box(width=12,
                                                 value = tags$p(style = "font-size: 2px;", "Media Bias in The Democratic Primaries"),
                                                 title = ""
                                    )))
                            ,
                            tabItem(tabName = "DataCollection",
                                    br(), 
                                    fluidRow(width = 12,uiOutput('Table_Counts'))
                                    ),
                            tabItem(tabName = "WordCounts2",
                                    br(), 
                                    fluidRow(width = 12,uiOutput('Explain_word_counts')),
                                    br(), 
                                    fluidRow(width = 12,DT::dataTableOutput('table')),
                                    fluidRow(width = 12,DT::dataTableOutput('table2'))
                                    
                            ),
                            
                            
                            tabItem(tabName = "NegWordCounts",
                                    br(), 
                                    fluidRow(width = 12,uiOutput('Explain_neg_words')),
                                    br(), 
                                    fluidRow(width = 12,DT::dataTableOutput('table3')),
                                    fluidRow(width = 12,DT::dataTableOutput('table4'))
                                    
                            ),
                            
                            
                            
                            tabItem(
                                tabName = "Markdown",
                                fluidRow(uiOutput('Methodology')
                                )),
                            
                           # tabItem(
                        #        tabName = "WordCounts",
                        #        fluidRow(uiOutput('WordCounts')
                        #        )),
                            
                            tabItem(
                                tabName = "Contact",
                                fluidRow(
                                    infoBox(width=8,
                                            title = "Justin Herman",
                                            value = tags$p(style = "font-size: 20px;", "JustinHerman@gmail.com"),
                                            href = "https://github.com/mburke65/Data_698_Final",
                                            subtitle = "please visit github for full project at https://github.com/mburke65/Data_698_Final ",
                                            icon = icon("envelope"),
                                            color = "purple"
                                    )
                                )),
                            
                            tabItem(tabName = "dashboard",
                                    br(), 
                                    fluidRow(uiOutput('mymarkdown3')),
                
                                    fluidRow(column(width = 12, plotlyOutput("plot1")
                                    )),br(),br(),br(),br(),fluidRow(uiOutput('mymarkdown4'))
                                    
                                    
                                    ),
                            tabItem(tabName = "dashboard2",
                                    br(), 
                                    fluidRow(width = 12,uiOutput('cool_sentiment')
                                    )),
                            
                            tabItem(tabName = "widgets",
                                    fluidRow(
                                        column(5,'WordCloud')),
                                    
                                    br(),
                                    
                                    fluidRow(column(width = 12, plotOutput("plot2"))
                                    ),
                                    br()
                            )
                        )
                    )
)

server <- shinyServer(function(input, output) {
    
    output$plot1 <- renderPlotly({df%>%
            filter(source1 == input$Choice) %>% 
            ggplot() +
            # remove axes and superfluous grids
            theme_classic() +
            theme(axis.title = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.line = element_blank()) +
            # add a dummy point for scaling purposes
            geom_point(aes(x =.7, y = !!sym(input$Choice)), 
                       size = 0, col = "white") + 
            # add the horizontal discipline lines
            geom_hline(yintercept = 1:9, col = "grey80") +
            
            # add a point for each male success rate
            geom_point(aes(x = Score, y =  !!sym(input$Choice), colour = !!sym(input$Color_choice),label =Publication,label2=Candidate, 
                           size = Positive)) +
            scale_color_brewer(palette="Spectral")
        
        
        
        #  ggplotly(gg_dot)
    })
    
    output$table <- DT::renderDataTable({pos_datatabledf2})
    output$table2 <- DT::renderDataTable({pos_df_3}) 
    output$table3 <- DT::renderDataTable({neg_datatabledf2})
    output$table4 <- DT::renderDataTable({neg_df_3})                               
                           
    
    
    output$mymarkdown <- renderUI({  
        k <- knitr::knit(input = "Introduction2.Rmd", quiet = T)
        HTML(markdown::markdownToHTML(k, fragment.only = T,stylesheet = getOption("markdown.HTML.mystyle.css")))
    })
    output$Methodology <- renderUI({  
        k <- knitr::knit(input = "Methodology.Rmd", quiet = T)
        HTML(markdown::markdownToHTML(k, fragment.only = T))
    })
    
     
    
    output$mymarkdown3 <- renderUI({  
        k <- knitr::knit(input = "sentiment.Rmd", quiet = T)
        HTML(markdown::markdownToHTML(k, fragment.only = T))
    })
    
    output$Table_Counts <- renderUI({  
        k <- knitr::knit(input = "Table_Counts.Rmd", quiet = T)
        HTML(markdown::markdownToHTML(k, fragment.only = T))
    })
    
    
    output$mymarkdown4 <- renderUI({  
        k <- knitr::knit(input = "scored_printout.Rmd", quiet = T)
        HTML(markdown::markdownToHTML(k, fragment.only = T))
    })
    
    output$WordCounts <- renderUI({  
        k <- knitr::knit(input = "word_counts.Rmd", quiet = T)
        HTML(markdown::markdownToHTML(k, fragment.only = T))
    })
    
    output$Explain_word_counts <- renderUI({  
        k <- knitr::knit(input = "Explain_word_counts.Rmd", quiet = T)
        HTML(markdown::markdownToHTML(k, fragment.only = T))
    })
    
    output$Explain_neg_words <- renderUI({  
        k <- knitr::knit(input = "Explain_neg_words.Rmd", quiet = T)
        HTML(markdown::markdownToHTML(k, fragment.only = T))
    })
    
    
    output$cool_sentiment <- renderUI({  
        k <- knitr::knit(input = "cool_sentiment.Rmd", quiet = T)
        HTML(markdown::markdownToHTML(k, fragment.only = T))
    })
    
    output$plot2 <- renderPlot({
        rquery.wordcloud(input$Choice2, type="file", 
                         lang="english", excludeWords = c("warren's","sanders's",'sanders', "biden","joe","bernie","harris","kamala","warren","sanders","sanders's","said","can","going","also","says","many","much","told",
                                                          "one","two","three","way","just","sen","campaign",'elizabeth','warren'), 
                         textStemming = FALSE,  colorPalette="Dark2",
                         max.words=35)
    })
    output$plot3 <- renderPlot({df%>%
           # filter(source1 == input$Choice) %>% 
            ggplot() +
            theme_classic() +
            theme(axis.title = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.line = element_blank()) +
            geom_point(aes(x =.7, y = Candidate),size = 0, col = "white")+
            geom_hline(yintercept = 1:9, col = "grey80") +
            geom_image(aes(x = Score, y = Candidate,image=Publication_image), size=.08)
        #  scale_colour_manual(name = 'Score > .5', values = setNames(c('green','red'),c(T, F)))
    })
    output$plot4 <- renderPlot({df%>%
           # filter(source1 == Publication) %>% 
            ggplot() +
            # remove axes and superfluous grids
            theme_classic() +
            theme(axis.title = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.line = element_blank()) +
            # add a dummy point for scaling purposes
            geom_point(aes(x =.7, y = Publication), 
                       size = 0, col = "white") + 
            
            # add the horizontal discipline lines
            geom_hline(yintercept = 1:9, col = "grey80") +
            geom_image(aes(x = Score, y =  Publication,image=cand_image), size=.05)
    })
})

shinyApp(ui, server)




