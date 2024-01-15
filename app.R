

library(shiny)
library("DT")
library(readxl)
library( dplyr)
library (ggplot2)
library("ggrepel")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(shinyjs)


# Projekt rodzicielstwo 



# Wczytanie danych
d  <- read_excel("~/main table.xlsx")

# Zamiana nominanty na stymulante 
d <- d %>%
  mutate( female_income_share = (-1)/(female_income_share - 0.5 - 1))

# Średnie dla każdej zmiennej
means <- apply ( d[,-c(1)], 2, mean)
means <- unlist( means)
means <- unname ( means)

# Odhcylenia standardowe dla kazdej zmiennej
sd <- apply ( d[,-c(1)], 2, sd)
sd <- unname(unlist(sd))

# Funkcja standaryzująca 
Stan <- function(x) {
  (x - mean(x))/sd(x)
}

# Stworzenie tabeli z wartościami zestandaryzowanymi
d_standaryzacja <- d %>%
  mutate( forest_coverage = Stan(forest_coverage),
          mother_job_protect = Stan(mother_job_protect),
          benefits_for_disabled_children = Stan(benefits_for_disabled_children),
          health_leave = Stan(health_leave),
          breastfeed_duration = Stan(breastfeed_duration),
          father_job_protect = Stan(father_job_protect),
          paternal_leave = Stan(paternal_leave),
          maternal_leave = Stan(maternal_leave),
          CO2.emmision_pc= Stan(CO2.emmision_pc),
          equal_prental_leave= Stan(equal_prental_leave),
          maternal_mortality = Stan(maternal_mortality),
          Infant_mortality = Stan(Infant_mortality),
          mean_h_income = Stan(mean_h_income),
          Maternal_newborn_healthexpenditure = Stan(Maternal_newborn_healthexpenditure),
          x_satisafied_students = Stan(x_satisafied_students),
          global_peace_index = Stan(global_peace_index),
          female_income_share = Stan(female_income_share),
          X..of.children.who.reported.bulling = Stan(X..of.children.who.reported.bulling),
          education_expenditure = Stan(education_expenditure),
          suicide_rates = Stan(suicide_rates) ) 

# stworzenie wzorca idealnego państwa oraz antywzorca

maxes <- unname(unlist(apply(d_standaryzacja[,-c(1)],2,max)))
mins <- unname(unlist(apply(d_standaryzacja[,-c(1)],2, min)))

wzorzec_idealny <- c( maxes[1:8] , mins[9], maxes[10], mins[11:12], maxes[13:15], mins [16], maxes[17], mins[18], maxes[19], mins[20])
antywzorzec <- c (mins[1:8], maxes[9], mins[10], maxes[11:12], mins[13:15], maxes[16], mins[17], maxes[18], mins[19], maxes[20])

# odległośc między wzorcem idelanym i antywzorcem

odległosc_maksymalna <- (sum((wzorzec_idealny - antywzorzec) ^ 2))^0.5

# Stworzenie tabeli pomocniczej 
dd <- d_standaryzacja[, -c(1)]

for ( i in 1:38) {
  dd[i,] <- (dd[i,] - wzorzec_idealny)^2
}

# Funkcja pomocnicza

odleglosc <- function(x){
  (sum(x))^0.5
}


# Obliczenie odległości od wzorca idealnego 

r <- unname(unlist(apply(dd, 1, odleglosc)))

# Funkcja pomocnicza 

m_t <- function(x){
  1 - (x/odległosc_maksymalna)
}

# Miary taksonomiczne 

miary_tak <- m_t(r)

# Ranking 

Ranking <- d %>%
  select(country)%>%
  mutate ("miara_tak" = miary_tak) %>%
  arrange(desc(miary_tak))%>%
  mutate ("miejsce" = c(1:38) , 
          "grupa" = c( rep( "Najwyzszy poziom" , 7), rep("Poziom ponadprzecietny",12), rep("Poziom poniżej przecietnej", 13), rep( "Najniższy poziom",6))
  )

# Wizualizacja wyników 

# Podział państw 

I <- c ("Sweden","Portugal","Austria","Croatia","France","Slovenia","Finland")
II <- c ("Germany","Estonia","Lithuania","Italy","Hungary","Switzerland","Costa Rica","Latvia",
         "Bulgaria","Greece","Ireland","Belarus")
III <- c("Romania","Iceland","Uruguay","Poland","Serbia","Japan","Peru","Argentina",
         "Albania","Panama","Thailand","Ukraine","Georgia")
IV <- c("Malaysia","Dominican Republic","Moracco","Kazakhstan","Indonesia","Jordan")    

panstwa <- c ("Sweden","Portugal","Austria","Croatia","France","Slovenia","Finland",
              "Germany","Estonia","Lithuania","Italy","Hungary","Switzerland",
              "Costa Rica","Latvia",
              "Bulgaria","Greece","Ireland","Belarus", "Romania","Iceland",
              "Uruguay","Poland","Serbia","Japan","Peru","Argentina",
              "Albania","Panama","Thailand","Ukraine","Georgia","Malaysia",
              "Dominican Republic","Morocco","Kazakhstan","Indonesia","Jordan"
)

# Stworzenie mapy 



world <- ne_countries(scale = "medium", returnclass = "sf")


SE_country <- world %>%
  filter (name_long %in% panstwa) %>%
  select ( name_long,geometry)

SE_country <- SE_country %>%
  mutate ( country_group = ifelse ( name_long %in% I, "I",
                                    ifelse ( name_long %in% II, "II", 
                                             ifelse ( name_long %in% III ,"III","IV" ))))

SEA_map <- world %>% 
    ggplot() +
    geom_sf() +
    geom_sf(data=SE_country, aes(fill=SE_country$country_group), color = "black") +
    scale_fill_manual(values = c("#00CD00","#FFFF00","#EE9A00","#CD2626"))+
    labs( fill = NULL)+ 
    theme_dark()






ui <- fluidPage(
  
  
  useShinyjs(),
 
    # Application title
    titlePanel("Metoda wzorca ( porządkowanie liniowe )"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 10,
                        value = 6), 
            selectInput( "variable", "Names of variables:", 
                         list("Forest coverage" = "forest_coverage",
                              "Mother job protection" = "mother_job_protect",
                              "Benefits for disabled children"= "benefits_for_disabled_children",
                              "Health leave" = "health_leave",
                              "Breastfeed duration"= "breastfeed_duration",
                              "Father job protection" = "father_job_protect",
                              "Paternal leave" = "paternal_leave",
                              "Maternal leave" = "maternal_leave",
                              "CO2 emmision per capita" = "CO2.emmision_pc",
                              "Equal prental leave" = "equal_prental_leave",
                              "Maternal mortality" = "maternal_mortality",
                              "Infant mortality" = "Infant_mortality",
                              "Mean household income" = "mean_h_income",
                              "Maternal newborn healthexpenditure" = "Maternal_newborn_healthexpenditure",
                              "% of satisafied students" = "x_satisafied_students",
                              "Global peace index" = "global_peace_index",
                              "Female income share" = "female_income_share",
                              " % of children who reported bulling" = "X..of.children.who.reported.bulling",
                              "Education expenditure" = "education_expenditure",
                              "Suicide rates" = "suicide_rates"
                            
                              ), selected = 1),
            # Dodanie pola z opisem zmiennej pod wyborem zmiennych
            textOutput("variable_description"),
            actionButton("show_ranking_button", "Zobacz ranking"),
            actionButton("select_countries_button", "Sprawdż to sam !"),
            shinyjs::hidden(
              checkboxGroupInput("selected_countries", "Wybierz państwa:", choices = panstwa)
            ),
        ),
        
       
        mainPanel(
           h2(textOutput("name1")),
           h3(verbatimTextOutput("print1")),
           
          plotOutput("plot2"),
          DTOutput("table")
           ) )
  )



# Define server logic required to draw a histogram


server <- function(input, output, session) {
  
  
  d <- data.frame(d)
  output$name1<-renderText({input$variable})
  output$print1 <- renderPrint({summary(d[,input$variable])})
  
  variable_descriptions <- c( 
    "forest_coverage" = "Procent powieżchni jaki jest pokryty terenami lesnymi w danym państwie",
    "mother_job_protect" =  " Ta zmienna jest zakodowana ! 5 oznacza najlepszą sytuacji dla matek w sprawie utrzymania ich pracy, a 1 oznacza najgorszą",
    "benefits_for_disabled_children" = " Ta zmienna jest zakodowana! 5 oznacza najwiekszą kwote wypłacaną na zasiłek dla niepełnosprawnych dzicii, 1 oznacza njamniejszą",
    "health_leave" = " Ta zmienna jest zakodowana ! 5 oznacza najdłuzszy okres urlopu przyznawany rodzicą ze względu na chorobe dziecka, 1 oznacza brak takiego urlopu ",
    "breastfeed_duration" = " Ta zmienna jest zakodowana ! 5 oznacza najdłuższy okres urlopu przysługujący ze względu na okres karmienia dziecka, 1 oznacza brak takiego urlopu",
    "father_job_protect" = " Ta zmienna jest zakodowana ! 5 oznacza najlepszą sytuacji dla ojców w sprawie utrzymania ich pracy, a 1 oznacza najgorszą",
    "paternal_leave" = " Ta zmienna jest zakodowana ! 5 oznacza najdłuzsy okres urlopu tacierzyńskiego , a 1 najkrótszy",
    "maternal_leave" = " Ta zmienna jest zakodowana ! 5 oznacza najdłuzsy okres urlopu macierzyńskiego , a 1 najkrótszy",
    "CO2.emmision_pc" = "Wielkośc emisji na jednego mieszkańca",
    "equal_prental_leave"= " Ta zmienna jest zakodowana ! 5 oznacza że system jest zbudowany aby wspierać również ojców do zajmowania się dziećmi, a 1 oznacza że w ogólne nie jest w ten sposób zbudowany",
    "maternal_mortality" = " Umieralnośc matek , definiowana jako ilośc śmierci koboet w trkacie ciązy lub rok po urodzeniu na 1000 urodzeń",
    "Infant_mortality" = " Umieralnośc noworodków, definiowana jako ilość smierci noworodków na 1000 urodzeń",
    "mean_h_income" = "Średni dochód na gospodarstwo domowe",
    "Maternal_newborn_healthexpenditure"= " % wydatków państwowych, które idą na wydatki dotyczące zdrowia matek i dzieci",
    "x_satisafied_students" = " % dzieci które są ustysfakcjonowane swoim życiem",
    "global_peace_index" = "Global peace index",
    "female_income_share" = "Procentowy udział zarobków kobiecych w zarobkach ogółem",
    "X..of.children.who.reported.bulling" = "% dzieci które zgłosiły że ktoś się nad nimi znęca",
    "education_expenditure" = "% wydatków państwowych, które idą na edukację ",
    "suicide_rates" = "% sampbójstw wśród młodych ludzi ( 14 - 24 lata )" )
    
  output$variable_description <- renderText({
    selected_variable <- input$variable
    variable_descriptions[selected_variable]
  })
  
    output$plot2 <- renderPlot({
      x    <- d[,input$variable]
      bins <-  input$bins
      
      # draw the histogram with the specified number of bins
      d %>%
      ggplot(aes(x = x)) +
        geom_histogram(color = "black", fill = "olivedrab4", bins =bins) +
        theme_grey( base_size = 13)
      

    })
    
    output$table <- renderDT({
      selected_variable <- input$variable
      selected_data <- d %>% select(country, !!sym(selected_variable))
      sorted_data <- selected_data[order(-selected_data[[2]]), ]  # Sort by the selected variable column
      
      datatable(sorted_data, options = list(pageLength = 4))  # Display top 5 rows
    })
    
    
    observeEvent(input$show_ranking_button, {
      output$plot2 <- renderPlot({SEA_map})
      output$name1 <- renderText({"Ranking państw sprzyjających rodzicielstwu"})
      output$print1 <- NULL
      
      output$table <- renderDT({
        d_ranking <- d %>% select(country)
        d_ranking$miejsce <- 1:38
        d_ranking$grupa <- c(rep("Najwyzszy poziom", 7), rep("Poziom ponadprzecietny", 12), rep("Poziom poniżej przecietnej", 13), rep("Najniższy poziom", 6))
        datatable(d_ranking, options = list(pageLength = 38))
      })
    
      
    })
    
    observeEvent(input$select_countries_button, {
      
      shinyjs::toggle("selected_countries")
      updateCheckboxGroupInput(
        session, "selected_countries",
        choices = panstwa,
        selected = NULL
      )
      
      observe( {
        
        
        selected_countries <- input$selected_countries
        
        SE_country <- world %>%
          filter (name_long %in% selected_countries) %>%
          select ( name_long,geometry)
        
        SE_country <- SE_country %>%
          mutate ( country_group = ifelse ( name_long %in% I, "I",
                                            ifelse ( name_long %in% II, "II", 
                                                     ifelse ( name_long %in% III ,"III","IV" ))))
        
        SEA_map <- world %>% 
          ggplot() +
          geom_sf() +
          geom_sf(data=SE_country, aes(fill=SE_country$country_group), color = "black") +
          scale_fill_manual(values = c("#00CD00","#FFFF00","#EE9A00","#CD2626"))+
          labs( fill = NULL)+ 
          theme_dark()
        
        
        output$plot2 <- renderPlot({SEA_map})
        output$name1 <- renderText({"Sprawdź to sam ! "})
        output$print1 <- NULL
        output$table <- renderDT ({
          d_selected <- d %>% 
            mutate ("miara_tak" = miary_tak) %>%
            arrange(desc(miary_tak))%>%
            mutate ("miejsce" = c(1:38) , 
                    "grupa" = c( rep( "Najwyzszy poziom" , 7), rep("Poziom ponadprzecietny",12), rep("Poziom poniżej przecietnej", 13), rep( "Najniższy poziom",6))
            ) %>%
            filter ( country %in% selected_countries) %>%
            select(miejsce, grupa, everything())
          datatable(d_selected, options = list(pageLength = 38 , rownames = FALSE))
         
                  
        })
      })
      
    })
    

    
    
  
}



# Run the application 
shinyApp(ui = ui, server = server)
