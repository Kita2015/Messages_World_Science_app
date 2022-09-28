#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### --- Set up data and attributes

## -- Load libraries

library(shiny)
library(tidyverse)
library(gridExtra)

# -- set working directory 

##Fill out your own working directory between "" in setwd()
##Place data and app in the same directory.
##Place the logo file (.png or .jpg) in a separate folder "www" in the working directory.

setwd("...")


## -- Import and prepare data

##Import a text file (.csv) containing at least the following columns, separated by semi-colons:

#column names
#abstract; doi; year

#column classes
#character; character; numeric

#my data set
text_import <- read_delim("...", delim = ";", col_names = TRUE)

# select only relevant columns

text <- text_import %>%
  select(abstract, doi, year)

# remove references without doi (or full reference)

text <-text %>%
  na.omit()

# --- Separate sentence #1, #2, #3, and #4 in new, separate columns

## By default the code strips the four first sentences in the abstract. 
## You can adjust this number by changing n = n
## and adjust the number of columns that messages2 contains.


messages2 <- str_split_fixed(text$abstract, pattern = boundary("sentence"), n = 4)

messages2 <- as_tibble(messages2)

messages2 <- messages2 %>%
  rename("sent1" = "V1",
         "sent2" = "V2",
         "sent3" = "V3",
         "sent4" = "V4")

text <- cbind(text,messages2)

# pivot data such that all sentences are in one column, they get a coordinate, and still have a ref

text_final <- text %>%
  pivot_longer(cols = c(sent1, sent2, sent3, sent4),
               names_to = "sentence_nr",
               values_to = "messages") %>%
  select(!abstract)

#remove strings of messages that are too long for one sentence

text_final <- text_final %>%
  mutate(string_length = str_length(messages)) %>%
  filter(string_length < 200) #adjust this number according to the average sentence length in your data set 

#correct for weird symbols
text_final$messages <- stringi::stri_enc_toutf8(text_final$messages, is_unknown_8bit = FALSE, validate = TRUE)

#limit length of line to display a sentence
text_final$messages <- str_wrap(text_final$messages, 125) #adjust this number if you want a wider canvas

## based on particular words in these sentences: text mining exercise

text_final_env <- text_final %>%
  filter(str_detect(messages, 'planet|climate|greenhouse gas|environment')) %>%
  mutate(topic = "environment")

text_final_health <- text_final %>%
  filter(str_detect(messages, 'health')) %>%
  mutate(topic = "health")

text_final_welfare <- text_final %>%
  filter(str_detect(messages, 'welfare|animal|ethic')) %>%
  mutate(topic = "welfare")

text_final_food <- text_final %>%
  filter(str_detect(messages, 'plant|meat|protein|substitut|alternat')) %>%
  mutate(topic = "food")

text_final_agriculture <- text_final %>%
  filter(str_detect(messages, 'livestock|cattle|farming|production')) %>%
  mutate(topic = "agriculture")

list_text_topics <- list(text_final_agriculture,text_final_env,text_final_food,text_final_health,text_final_welfare)


text_final1 <- bind_rows(list_text_topics)

#Set up elements for further use in user interface
topics <- unique(text_final1$topic)
colours <- c("black","grey","cadetblue","chocolate","aquamarine4","darkorchid4","firebrick4")
canvas_colours <- c("white", "lightblue", "aquamarine", "darkolivegreen1", "gray", "lavenderblush", "khaki", "plum")
years <- unique(text_final1$year)


# --- Create a data frame that shows either all sentences or a selection


### --- Set up user interface

# Define UI for application that draws a canvas of messages and its references
ui <- fluidPage(
  
  # Application title
  titlePanel("Messages from the World of Science"),
  
  hr(),
  
  h5("Total messages in database"),
  h6("NOTE If a warning appears below, the required 'Number of messages' is larger than the total number of messages selected. Please adjust selection criteria."),
  
  fluidRow(
    column(3,
           textOutput(outputId = "total_messagesID"))
  ),
  
  br(),
  
  h4("Create your own plot of messages from science here"),
  
  fluidRow(
    h5("__Select messages"),
    column(3,
           selectInput(inputId = "topicID", label = "  Topic", choices = topics)
    ),
    column(3,
           sliderInput(inputId = "number_of_messagesID", label = "  Number of messages", min = 1, max = 30, value = 15)
    ),
    
    column(3,
           selectInput(inputId = "publication_yearID", "  Year of publication", choices = years, multiple = TRUE,
                       selected = years))
    
  ),
  
  
  fluidRow(
    h5("__Style your canvas"),
    column(3,
           selectInput(inputId = "canvas_colourID", label = "  Colour your canvas", choices = canvas_colours)
    ),
    column(3,
           sliderInput(inputId = "text_sizeID", label = "  Text size", min = 5, max = 10, value = 7, step = 0.5)
    ),
    column(3,
           numericInput(inputId = "plot_widthID", "  Canvas width (only for .png)", value = 1700, min = 1000, max = 2000, step = 100)
    )
  ),
  
  
  fluidRow(
    h5("__Export your canvas"),
    column(3,
           textInput(inputId = "canvas_nameID", label = "  Name your canvas", value = "")
    ),
    column(3,
           selectInput(inputId = "download_optionID", label = "  Select type of download", choices = c("png","pdf"))
    ),
    column(3,
           downloadButton(outputId = "messages_plotID", label = "  Download your canvas")
    )
    
  ),
  
  hr(),
  
  tabsetPanel(
    tabPanel("Canvas", 
             plotOutput("messagesPlot",
                                  width = "1350px",
                                  height = "800px")
    ),
    tabPanel("References", 
             fluidRow(
               column(3,
                      selectInput(inputId = "ref_download_optionID", label = "Select type of download", choices = c("csv"))
               ),
               column(3,
                      downloadButton(outputId = "references_tableID", label = "Download the references")
               )
             ),
             tableOutput("referencesTable")
    ),
    tabPanel("About", 
             h4("Why this app?"),
             p("During the interviews for my first study, one of my respondents mentioned that scientists do not communicate one clear message about effects of meat consumption and production, confusing consumers. However, within the world of science, consensus on the adverse effects of meat consumption and production on the environment, animal welfare, and health is very present. When screening literature for my second study I could not help but notice how often a similar message was communicated in article abstracts. So I decided to collect these messages to show that the academic world itself is well-aligned."),
             br(),
             h4("What does this app do?"),
             p("The Shiny app displays a selection of sentences extracted from article abstracts. Users can select number of messages, main topic, year of publication, background colour, font size, and canvas width. When satisfied, users can download their selection of messages and share it on social media. Since science prefers traceability and transparency, all URLs of the articles the selected messages were retrieved from, can be downloaded as well."),
             br(),
             h4("How does the app work?"),
             p("The Shiny app first imports the data set, consisting of at least the columns: abstract, doi, year, separated by a semi-colon."),
             p("The first four sentences of each abstract are extracted, since in this part of the abstract usually statements are made on the context of the research, in this case the urgency to reduce animal-based protein consumption. The selected sentences are added to the data frame."),
             p("Then a small text mining exercise will take place: based on words relating to topics of consumption and production, sentences are filtered, creating the option for users to display messages relating to a topic of their choice."),
             p("Based on user input for topic, number of messages, and year of publication, sentences are plotted on the screen.
               What happens in the background: each sentence is appointed a coordinate so it can be plotted on a graph surface."),
             p("Some style options are available for the user: colour of the canvas, text size, and canvas width (only for .png; .pdf is scaled automatically)."),
             p("As a last step, users can download their canvas and choose to download the references as well."),
             br(),
             h4("Use"),
             p("Everyone can use the app for free to create his or her own canvas of messages. Want to use a different set of abstracts relating to your own topic of interest? Or would you like to modify this app so it fits your preferences? Find the code for this app at",
               a("my Github", href="https://github.com/Kita2015?tab=repositories", target = "_blank"))
             
             )
    
  ),
  
  hr(),
  
  ## If you want a logo at the bottom of your app, put the name of the file (.jpg or .png) between "".
  ## If you don't want the picture, put # in front of the line of code
  HTML('<center><img src="..."></center>'),

  p("Cite this app: Blokhuis, Christa. Messages from the world of science. Version 0.8, 2022-09-20. URL.")
)
  

### --- Set up server

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #select messages based on selected year(s) and selected topic(s)
  dataInput <- reactive({
    text_final1 %>%
      filter(year %in% input$publication_yearID) %>%
      filter(topic %in% input$topicID)
  })
  
  
  #sample the selected number of messages, add column with message + reference, and select year(s)
  text_selectionInput <- reactive({
    dataInput()%>%
      sample_n(input$number_of_messagesID) %>%
      mutate(x_coord = (input$number_of_messagesID/2 + runif(row_number())),
             y_coord = row_number(),
             message_ref = paste0(messages,"[",y_coord,"]"))
  })
  
  output$total_messagesID <- reactive({ 
    nrow(text_selectionInput())
  })
  
  canvas_colour <- reactive({
    input$canvas_colourID
  })
  
  
  
  
  
  #plot messages
  plotsave <- reactive ({
    
    ggplot(data = text_selectionInput(), aes(x = x_coord, y = y_coord, label = message_ref)) +
      geom_point(colour = canvas_colour()) +
      theme_void() +
      geom_text(size = input$text_sizeID) +
      xlim(0,input$number_of_messagesID) +
      theme(legend.position = "none") +
      ggtitle("- Messages from the world of science -") +
      theme(plot.title = element_text(color = "black", size = 22, face = "bold", hjust = 0.5),
            panel.background = element_rect(fill = canvas_colour()))
    
    
    
  })
  
  output$messagesPlot <- renderPlot({
    p <- plotsave()
    print(p)
  })
  
  plot_width <- reactive({
    input$plot_widthID
  })
  
  
  #make download of messages plot available
  output$messages_plotID <- downloadHandler(
    filename = function() {
      paste(input$canvas_nameID, input$download_optionID, sep = ".")
    },
    content = function(file) {
      
      if (input$download_optionID == "png") {
        png(file, width = plot_width(), height = 800)
      } else {
        pdf(file, width = 25, height = 20)
      }
      
      print(plotsave())
      
      dev.off()
      
    }
    
  )
  
  #create table for references
  referencesInput <- reactive({
    text_selectionInput() %>%
      mutate(ref_number = paste0("[",y_coord,"]")) %>%
      select(ref_number, doi) %>%
      as_tibble()
  })
  
  #plot references
  output$referencesTable <- renderTable(referencesInput())
  
  #make download of references available
  
  output$references_tableID <- downloadHandler(
    filename = function() {"references.csv"},
    content = function(file) {
      
      write.csv(referencesInput(), file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
