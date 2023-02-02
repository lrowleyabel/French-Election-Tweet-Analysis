library(shiny)
library(stringr)
library(bslib)
library(dplyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemr)
library(MetBrewer)
library(utils)
library(htmltools)
library(htmlTable)


load("app_data_ignore.Rda")


ui <- fluidPage(
  

  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  tags$header(tags$style("
                         body {
                           padding:50px;
                         }
                         ")),
  
  card(h1("Exploring the Tweet Metrics Data")),

  
  layout_column_wrap(
        width = NULL, fill = FALSE,
        style = css(grid_template_columns = "1fr 2fr"),
        card(
          card_header("Select variables"),
          card_body(
          p("This dataset contains the main dependent and independent variables. The summarised party variable is called 'party_list'. Most of the Tweet metric variables have logged versions, which allows for more useful visualisation."),
          actionButton("show_descriptions", "Show variable descriptions"),
          br(),
          br(),
          selectInput("x_variable", label = "X Variable", choices = colnames(df), selected = "party_list"),
          selectInput("y_variable", label = "Y Variable", choices = colnames(df), selected = "log_total_campaign_tweets"),
          radioButtons("colour_option", label = "Group by..
                       .", choices = list("Nothing", "Main Parties", "Ensemble vs Non-Ensemble"), selected = "Main Parties"),
          checkboxGroupInput("parties", "Filter parties", choices = c("Ensemble", "Rassemblement National", "NUPES", "Les Républicains", "Reconquête", "Other party", "Independent"), selected = c("Ensemble", "Rassemblement National", "NUPES", "Les Républicains", "Reconquête", "Other party", "Independent")),
          radioButtons("linear_fit", label = "Add linear fit (scatter plots only)", choices = list("Yes", "No"), selected = "No"),
          sliderInput("alpha", "Point opacity (scatter plots only)", 0, 1,1)
          )
        ),
        layout_column_wrap(
          width = 1,
          card(
            card_header("Output"),
            card_body(
              plotOutput("plot", height = 500)
            )
          ),
          card(
            fluidRow(
                   downloadButton("download_plot")
            ),
            br(),
            br(),
            fluidRow(
              textInput("x_lab", "X Label", value = ""),
            textInput("y_lab", "Y Label", value = ""),
            textInput("title", "Title", value = ""),
            textInput("legend_position", "Legend position", value = ""),
            numericInput("legend_rows", "Legend rows", value = 3)
            )
          )
          
        )
  )
)

server <- function(input, output, session) {
  
  get_sub_df<- reactive({
    
    df%>%
      filter(party_list %in% input$parties)

   
  })
  
  ggtheme<- ggthemr("flat", spacing = 1.5, layout = "plain")
  
  hiro<- met.brewer("Hiroshige")%>%
    as.character()
  
  demuth<- met.brewer("Demuth")%>%
    as.character()
  
  degas<- met.brewer("Degas")%>%
    as.character()
  
  party_colours<- list("Other party" = degas[5],
                       "Ensemble" = hiro[3],
                       "Les Républicains" = hiro[9],
                       "NUPES" = hiro[1],
                       "Reconquête" = hiro[8],
                       "Rassemblement National" = hiro[6],
                       "Independent" = demuth[6])
  
  party_fill_scale<- scale_fill_manual(name = "", values = party_colours)
  party_colour_scale<- scale_color_manual(name = "", values = party_colours)
  
  binary_ensemble_colours<- list("Ensemble" = hiro[3],
                                 "Non-Ensemble" = degas[5])
  
  binary_ensemble_fill_scale<- scale_fill_manual(name = "", values = binary_ensemble_colours)
  binary_ensemble_colour_scale<- scale_color_manual(name = "", values = binary_ensemble_colours)
  
  createPlot<-reactive({
    
    sub_df<- get_sub_df()
    
    x_type<- ifelse(length(unique(sub_df[,input$x_variable])) <= 10, "categorical", "continuous")
    y_type<- ifelse(length(unique(sub_df[,input$y_variable])) <= 10, "categorical", "continuous")
    
    print(x_type)
    print(y_type)
    
    x_name<- input$x_variable%>%
      str_replace_all("_", " ")%>%
      str_to_title()
    
    y_name<- input$y_variable%>%
      str_replace_all("_", " ")%>%
      str_to_title()
    
    if(input$colour_option == "Main Parties"){
      sub_df$colour_variable<- sub_df[,"party_list"]
      colour_scale<- party_colour_scale
      fill_scale<- party_fill_scale
      legend<- theme(legend.position = "top")
    }
    if(input$colour_option == "Ensemble vs Non-Ensemble"){
      sub_df$colour_variable<- sub_df[,"ensemble"]
      colour_scale<- binary_ensemble_colour_scale
      fill_scale<- binary_ensemble_fill_scale
      legend<- theme(legend.position = "top")
    }
    if(input$colour_option == "Nothing"){
      sub_df$colour_variable<- "A"
      colour_scale<- {}
      fill_scale<- {}
      legend<- theme(legend.position = "None")
    }
    
    if(input$linear_fit == "Yes"){
      add_lm<- geom_smooth(aes(sub_df[,input$x_variable], sub_df[,input$y_variable], color = sub_df[,"colour_variable"]), se = F, method = "lm")
      add_lm_bg<- geom_smooth(aes(sub_df[,input$x_variable], sub_df[,input$y_variable], group = sub_df[,"colour_variable"]), color = "white", size = 2, se = F, method = "lm")
    } else {
      add_lm<- {}
      add_lm_bg<- {}
    }
    
    
    
    if (x_type == "continuous" & y_type == "continuous"){
    
      p<- ggplot()+
        geom_point(aes(sub_df[,input$x_variable], sub_df[,input$y_variable], color = sub_df[,"colour_variable"]), alpha = input$alpha)+
        labs(x = x_name, y = y_name)+
        add_lm_bg+
        add_lm+
        colour_scale+
        legend+
        guides(color=guide_legend(nrow=3,byrow=TRUE), fill = guide_legend(nrow=3,byrow=TRUE))+
        theme(text = element_text(size = 20))
        
    }
    
    if (x_type == "continuous" & y_type == "categorical"){
      
      p<- ggplot()+
        geom_boxplot(aes(sub_df[,input$x_variable], as.factor(sub_df[,input$y_variable]), fill = sub_df[,"colour_variable"]))+
        labs(x = x_name, y = y_name)+
        fill_scale+
        guides(color=guide_legend(nrow=3,byrow=TRUE), fill = guide_legend(nrow=3,byrow=TRUE))+
        legend+
        theme(text = element_text(size = 20))
      
    }
    
    if (x_type == "categorical" & y_type == "continuous"){
      
      p<- ggplot()+
        geom_boxplot(aes(as.factor(sub_df[,input$x_variable]), sub_df[,input$y_variable], fill = sub_df[,"colour_variable"]))+
        labs(x = x_name, y = y_name)+
        fill_scale+
        legend+
        guides(color=guide_legend(nrow=3,byrow=TRUE), fill = guide_legend(nrow=3,byrow=TRUE))+
        theme(text = element_text(size = 20),
              axis.text.x = element_text(angle = 45, hjust = 1))
      
    }
    
    if (x_type == "categorical" & y_type == "categorical"){
      
      if(input$x_variable == "party_list"){
        fill_scale<- party_fill_scale
      } else {
        fill_scale<- scale_fill_discrete(name = input$x_variable)
      }
      
      p<- ggplot()+
        geom_bar(aes(y = sub_df[,input$y_variable], group = as.factor(sub_df[,input$x_variable]), fill = as.factor(sub_df[,input$x_variable])), position = "dodge")+
        labs(x = "Count of candidates", y = y_name)+
        fill_scale+
        guides(color=guide_legend(nrow=3,byrow=TRUE), fill = guide_legend(nrow=3,byrow=TRUE))+
        theme(text = element_text(size = 20),
              axis.text.x = element_text(angle = 45, hjust = 1))
      
      
    }
    
    return(p)
    
    
    
  })
  
  output$plot<- renderPlot({createPlot()})
  
  observeEvent(input$show_descriptions, {
    
    showModal(modalDialog(title = "Variable Descriptions", size = "xl", htmlTable(read.csv("variable_descriptions.csv"), align = "l", rnames = F)))
    
  })
  
  output$download_plot<- downloadHandler(
    filename = "plot.png",
    content = function(con){
      
      print(input$legend_position)
    
      ggsave(con, createPlot()+
               guides(color=guide_legend(nrow=input$legend_rows,byrow=TRUE), fill = guide_legend(nrow=input$legend_rows,byrow=TRUE))+
               theme(text = element_text(size = 9),
                     plot.margin =  margin(t = 25, r = 20, b = 10, l = 5, unit = "pt"),
                     legend.position = input$legend_position)+
               labs(x = input$x_lab, y = input$y_lab, title = input$title),
             width = 4*1280, height = 4*720, units = "px", dpi = 700)
    })
  
}

shinyApp(ui, server)

 