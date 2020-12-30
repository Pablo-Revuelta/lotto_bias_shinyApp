# --------------------------------------
# TRABAJO FINAL DE VISUALIZACIÓN
# LOTTO BIAS
# --------------------------------------
# Matias Corredoira
# Pablo Revuelta
# Borja Martinez
# Beatriz Quevedo
# --------------------------------------
# 23 de diciembre de 2020


# Carga de librerías
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(gsubfn)
library(data.table)
library(readxl)
library(shinyalert)

# Carga del data frame

lottery <- as.data.frame(read_csv("Lottery_Take_5_Winning_Numbers.csv"))

# Eliminar la columna Bonus ya que no es de interes

lottery <- select(lottery, -"Bonus #")

# Cambiar el nombre de las columnas

names(lottery) <- c("date", "winning_numbers")

names(lottery)

#Crear un nuevo dataset con los numeros de la lotería por separado. 

split_lottery <- do.call('rbind', strsplit(as.character(lottery$winning_numbers), " ", fixed = TRUE))

split_lottery <- as.data.frame(split_lottery)

split_lottery["winning_numbers"] <- lottery$winning_numbers

split_lottery["date"] <- lottery$date

split_lottery$V1 <- as.numeric(split_lottery$V1)
split_lottery$V2 <- as.numeric(split_lottery$V2)
split_lottery$V3 <- as.numeric(split_lottery$V3)
split_lottery$V4 <- as.numeric(split_lottery$V4)
split_lottery$V5 <- as.numeric(split_lottery$V5)


# Carga de la base de datos de los números trampa.

numeros_trampa <- read_excel("numeros_trampa.xlsx")


# SHINY DASHBOARD
# --------------------------------------
# ui

ui <- dashboardPage(skin='black', # el tema del dashboard será negro
                    
  dashboardHeader(title = "Lotto Bias"), # título del dashboard
  
   dashboardSidebar( # dashboardSidebar es el menú de la izquierda
    
    sidebarMenu( # aquí aparecen las pestañas correspondienres a cada minijuego
      
      menuItem("Your lucky number",
               tabName = "1",
               icon = icon("heart-empty", lib= 'glyphicon')),
      
      menuItem("Guess the good one",
               tabName = "2",
               icon = icon("glyphicon glyphicon-thumbs-up", lib="glyphicon")),
      
      menuItem("More likely to win",
               tabName = "3",
               icon = icon("tower", lib= 'glyphicon'))
    )
  ),
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "1", # la ventana de la primera pestala: minijuego número 1
             
              h2('What is your favourite number?'), # título del minijuego
              
              h4('Here you can look how lucky is your favourite number, in other words; \n 
                 how many times it appears in the lottery. \n
                 Is your lucky number as lucky as you think? '),      # subtitulo del minijuego: breve explicación
             
               fluidRow( # aquí aparece lo que contendrá la primera fila del body
                
                 box(numericInput("num",                               # en box numericInput se recoge la primera caja,
                                 label = "Select your lucky number",  # donde se escogerá el número que se desee.
                                 value = 10,                          # el mínimo es 0 y el máximo es 40. El valor
                                 min = 0,                             # predeterminado es 10.
                                 max = 40)),
                 
                 actionButton("actualizarNumero", label = "GO!")       # Con este botón se actualizarán los elementos:
                ),                                                    # la tabla y el mensaje. 
              
              fluidRow( # aquí aparece lo que contendrá la segunda fila del body
                
                box(tableOutput("num_loteria")),                      # Esta es la tabla donde aparecerán los números de la lotería con el número dado
                
                box(verbatimTextOutput('percent'))                    # Este es el mensaje en el que se indicarán los porcentajes y estadísticas del 
              )                                                       # número dado.
              
      ),
      tabItem(tabName = "2", # la ventana de la segunda pestala: minijuego número 2
             
              h2('Which one is a real lotto number?'), # título del minijuego
             
              h4('Here you will find two numbers: one is a real lotto number and \n
              the other one is fake. Can you guess which is each one?'),  # subtitulo del minijuego: breve explicación
              
              fluidRow( # aquí aparece lo que contendrá la primera fila del body
                
                box(verbatimTextOutput("guess_real"), width = 4),         # la primera cajita donde aparecerá o un número de la lotería o uno aleatorio. 
                
                actionButton("actualizar1", label = "THIS ONE!"),         # El botón que servirá para indicar que el jugador ha escogido este número.
                useShinyalert()                                           # Para que salgan todos los mensajes en los minijuegos
               ),
              
              fluidRow( # aquí aparece lo que contendrá la segunda fila del body
               
                box(verbatimTextOutput("guess_random"), width = 4),       # la segunda cajita donde aparecerá o un número de la lotería o uno aleatorio.
               
                actionButton("actualizar2", label = "THIS ONE!")          # El botón que servirá para indicar que el jugador ha escogido este número.
              ),
              
              fluidRow( # aquí aparece lo que contendrá la tercera fila del body
                
                box(verbatimTextOutput('stats'), width = 6)              # Este es el mensaje con los porcentajes y estadísticas del jugador: 
                                                                         # Se van acumulando a medida que va jugando. 
              )
      ),
      tabItem(tabName = "3", # la ventana de la tercera pestala: minijuego número 3
              
              h2('Which number is more likely to win the lottery?'), # título del minijuego
              
              h4('Here you will find two random lotto numbers and one human-trap number. \n
                 A human-trap number is a number too pretty for people to think that it will
                 be a lotto number (ie.: 01 02 03 04 05) \n Try to guess which is which in this
                 fun minigame!'), # subtitulo del minijuego: breve explicación
              
              fluidRow( # aquí aparece lo que contendrá la primera fila del body
                
                box(verbatimTextOutput('option1')),            # la primera cajita donde aparecerá o un número de la lotería o uno aleatorio.
                
                actionButton('button1', label = 'THIS ONE!')   # El botón que servirá para indicar que el jugador ha escogido este número.
              ),
              
              fluidRow( # aquí aparece lo que contendrá la segunda fila del body
                
                box(verbatimTextOutput('option2')),           # la primera cajita donde aparecerá o un número de la lotería o uno aleatorio.       
                
                actionButton('button2', label = 'THIS ONE!')   # El botón que servirá para indicar que el jugador ha escogido este número.
              ),
              
              fluidRow( # aquí aparece lo que contendrá la tercera fila del body
                
                box(verbatimTextOutput('option3')),           # la primera cajita donde aparecerá o un número de la lotería o uno aleatorio.
                
                actionButton('button3', label = 'THIS ONE!')  # El botón que servirá para indicar que el jugador ha escogido este número.
                
              ),
              
              fluidRow( # aquí aparece lo que contendrá la cuarta fila del body
                
                box(verbatimTextOutput('stats_game3'), width = 6) # el mensaje con los porcentajes y estadísticas del jugador: 
                                                                  # Se van acumulando a medida que va jugando. 
              )
              )
    )
  )
)


server <- function(input, output) {
  
  # GRAME 1
  
  output$num_loteria <- renderTable({     # Este es el output para que salga la tabla con los números de la lotería

    input$actualizarNumero                # Botón para actualizar la tabla cuando el usuario lo clicka
    
    isolate(head(as.data.frame(split_lottery[split_lottery$winning_numbers %like% input$num, 7:6])))  # La tabla
    
    # Se hace un isolate para que varíe UNICAMENTE cuando se clicke el botón.
    # Se seleccionan las dos últimas columnas (la fecha y el número ganador) que coincidan con el número indicado por el jugador.
    # Esto se compara utilizando el dataframe split_lottery y un filtro (%like%)
    
  })
  
  output$percent <- renderText({          # Este es el output del mensaje del primer minijuego
    
    input$actualizarNumero                # Botón para actualizar el mensaje cuando el usuario lo clicla
    
    isolate(paste0('You have won ', count(split_lottery[split_lottery$winning_numbers %like% input$num, ]),
                   ' lottery numbers,\n', 'which is a ',
                   round(count(split_lottery[split_lottery$winning_numbers %like% input$num, ])/count(split_lottery), 4)*100, 
                   '% of the total'))     # El mensaje
    
    # Este mensaje solamente se actualiza cuando se hace click en el botón. Contiene las estadísticas de cuantas veces sale ese número en la lotería,
    # y el porcentaje que representa sobre el total.
    
  })
  
# ----------------------------------
  #GAME 2
  
  # Random boxes
  # Aquí es donde se ordenan a las cajitas de los números que vayan cambiando de posición de manera aleatoria.
  
  selection_box_game2 <- reactiveVal()  
  
  observeEvent(c(input$actualizar1,input$actualizar2),{
    
    selection_box_game2(sample(c(TRUE,FALSE), 1))
    
    # Se hace mediante un reactiveVal que va guardando el valor True o False 
    # El observeEvent sirve para que cuando se clicke en los botones se genere un True o False de forma aleatoria,
    # guardándolo en el reactiveVal
    
  })
  
  
  # Counter of times the user choose the correct number
  # Aquí se almacenan cuántas veces el usuario elige el número correcto, y define cuándo es correcto y cuando no (0 Y 1)
  
  counter <- reactiveValues(countervalue = 0)
  
  observeEvent(input$actualizar1,{
    
    acierto <- 
      if (selection_box_game2()){
        0
      } else 1
    
    counter$countervalue <- counter$countervalue + acierto 
    
    # Se genera un observeEvent para el primer botón, o el botón asociado a la primera caja en el cual
    # se indica que devuelva el valor 0 si no es correcto, y 1 si ha acertado el usuario
    
  })
  
    # Aquí se escribe el código para que salten los pop-ups o mensajes cuando el jugador hace click en 
    # uno de los botones. Si acierta saldrá un mensaje con 'Well done!' y si falla con 'Oh snap!'.
    # Los mensajes se pueden cerrar tanto cliclando en la OK como fuera de la cajita del pop-up.
    # Es meramente informativo. 
    
  observeEvent(input$actualizar1,{
    
    acierto <- 
      if (selection_box_game2()){
        shinyalert(
          title = "Oh snap!",
          text = "You chose the wrong one",
          size = "s", 
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "error",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      } else   shinyalert(
        title = "Well done!",
        text = "You guessed the real lotto number",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    
  })
  
  # Aquí se realiza lo mismo que comentado anteriormente pero con el segundo botón.
  
  observeEvent(input$actualizar2,{
    
    acierto <- 
      if (selection_box_game2()){
          shinyalert(
            title = "Oh snap!",
            text = "You chose the wrong one",
            size = "s", 
            closeOnEsc = TRUE,
            closeOnClickOutside = FALSE,
            html = FALSE,
            type = "error",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#AEDEF4",
            timer = 0,
            imageUrl = "",
            animation = TRUE
          )
      } else shinyalert(
        title = "Well done!",
        text = "You guessed the real lotto number",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    
  })
  
  # De nuevo, Se genera un observeEvent para el segundo botón, o el botón asociado a la segunda caja en el cual
  # se indica que devuelva el valor 0 si no es correcto, y 1 si ha acertado el usuario, al igual que con el primer 
  # botón.
  observeEvent(input$actualizar2,{
    
    acierto <- 
      if (selection_box_game2()){
        0
      } else 1
    
    counter$countervalue <- counter$countervalue + acierto
    
  })
  
  # Counter of times the user clicks a button.
  # Aquí se suma el número total de veces que el jugador está clickando uno de los botones, 
  # independientemente de si acierta o no.
  # Se utilizará en el texto con las estadísticas.
  
  counter_total <- reactiveValues(countervalue = 0)
  
  observeEvent(c(input$actualizar1, input$actualizar2),{
    counter_total$countervalue <- counter_total$countervalue + 1
    
  })
  
  # Output game 2
  # Se generan los outputs para las cajitas 
  
  output$guess_random <- renderText({ 
    
    input$actualizar1
    input$actualizar2
    
    if (selection_box_game2()){
      return(sample(10:39, 5, replace = FALSE))
    }
    else return(lottery[[sample(nrow(lottery), 1), 2]])
  
  # Se añaden los botones para que se actualice cuando se clicken. 
  # Se hace un if para que el objeto creado anteriormente, que daba un True/False
  # aleatorio, indique si en esa cajita se pondrá un número aleatorio o un número real de la lotería
   
  })
  
  # Se hace lo mismo que en el anterior, pero para la segunda cajita
  
  output$guess_real <- renderText({
    
    input$actualizar1
    input$actualizar2
    
    if (selection_box_game2()){
      return(lottery[[sample(nrow(lottery), 1), 2]])
      
    }
    else return(sample(10:39, 5, replace = FALSE))

  })
    
  # Aquí se codifica el mensaje del segundo minijuego, que contiene las estadísticas de cuántas veces ha clicado,
  # si lo ha hecho de manera correcta o incorrecta, y cuál es su porcentaje de acierto. 
  
  output$stats <- renderText({
    
    paste0('You have found ',  counter$countervalue , ' real lottery number(s)! \n', 'That is ',
           counter$countervalue ,' out of ', counter_total$countervalue -1,
           ' (', (counter$countervalue/(counter_total$countervalue-1))*100, '%)\n', 'Congratulations!')
  
    })

  
 # -------------------------------- 
  #GAME 3
  
  # Random boxes
  # De manera análoga que en el minijuego 2, se busca que el orden de los valores de las cajitas
  # vaya alternando de manera aleatoria. La única diferencia en este caso, es que se hace con tres cajitas y
  # tres botones, por lo que no se puede hacer con booleanos, si no con valores (1, 2 y 3)
  
  selection_box_game3 <- reactiveVal()
  
  observeEvent(c(input$button1,input$button2, input$button3),{
    
    selection_box_game3(sample(c(1, 2, 3), 1))
    
  })
  
  # Counter of times the user chooses the correct number. 
  # Como se desea llevar la cuenta de cuántos han sido random y cuantos trampa,
  # se realizan dos reactiveValues. 
  
  counter_trampa <- reactiveValues(countervalue = 0)
  counter_random <- reactiveValues(countervalue = 0)
  
  # De casi la misma manera que en el segundo minijuego, se codifica para que los valores 
  # sean 1 si es un human-trap number y 0 si es un número aleatorio para el objeto_trampa y viceversa
  # para el objeto_random. Esto se realiza con los tres botones.
  
  observeEvent(input$button1,{
    
    acierto_trampa <- 
     
      if (selection_box_game3() == 3){
        1
      } else 0 
    
    counter_trampa$countervalue <- counter_trampa$countervalue + acierto_trampa
    
     acierto_random <- 
      if (selection_box_game3() == 3){
        0
      } else 1
     
    counter_random$countervalue <- counter_random$countervalue + acierto_random
    
  })
  
#--------- Messagges -------
# Esto se hará igual para los tres botones, de forma similar que en el segundo minijuego, solo que en esta ocasión,
# en lugar de decir si has acertado o no, te informa de si esta random o trap number.

  observeEvent(input$button1,{
    
    acierto_trampa <- 
      
      if (selection_box_game3() == 3){
        shinyalert(
          title = "That was a human-trap number!",
          text = "Did you get it right?",
          size = "s", 
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "info",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      } else   shinyalert(
        title = "That was a random number!",
        text = "Did you get it right?",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      ) 

    
  })
#---------------------------
  
  observeEvent(input$button2,{
    
    acierto_trampa <- 
      
      if (selection_box_game3() == 2){
        1
      } else 0 
    
    counter_trampa$countervalue <- counter_trampa$countervalue + acierto_trampa
    
    acierto_random <- 
      if (selection_box_game3() == 2){
        0
      } else 1
    
    counter_random$countervalue <- counter_random$countervalue + acierto_random
    
  })
  
#--------- Messagges -------
  observeEvent(input$button2,{
    
    acierto_trampa <- 
      
      if (selection_box_game3() == 2){
        shinyalert(
          title = "That was a human-trap number!",
          text = "Did you get it right?",
          size = "s", 
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "info",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      } else  shinyalert(
        title = "That was a random number!",
        text = "Did you get it right?",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    
 
  })
#---------------------------
  
  observeEvent(input$button3,{
    
    acierto_trampa <- 
      
      if (selection_box_game3() == 1){
        1
      } else 0 
    
    counter_trampa$countervalue <- counter_trampa$countervalue + acierto_trampa
    
    acierto_random <- 
      if (selection_box_game3() == 1){
        0
      } else 1
    
    counter_random$countervalue <- counter_random$countervalue + acierto_random
    
  })

#--------- Messagges -------
  observeEvent(input$button3,{
    
    acierto_trampa <- 
      
      if (selection_box_game3() == 1){
        shinyalert(
          title = "That was a human-trap number!",
          text = "Did you get it right?",
          size = "s", 
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "info",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      } else shinyalert(
        title = "That was a random number!",
        text = "Did you get it right?",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    

  })
#---------------------------
  
  # Game 3 output
  # Similar al minijuego 2, salvo que esta vez se decidirá el valor que aparece en cada caja
  # dependiendo de si selection_box_game3 tiene un valor de 1, 2, o 3.
  
  output$option1 <- renderText({
    
    input$button1
    input$button2
    input$button3
    
    if (selection_box_game3() == 1){
      return(sample(10:39, 5, replace = F))
    }
    if (selection_box_game3() == 2){
      return(sample(10:39, 5, replace = F))
    } else return(numeros_trampa[[sample(nrow(numeros_trampa), 1), 1]])
    
  })
  
  output$option2 <- renderText({
    
    input$button1
    input$button2
    input$button3
    
    if (selection_box_game3() == 1){
      return(sample(10:39, 5, replace = F))
    }
    if (selection_box_game3() == 2){
      return(numeros_trampa[[sample(nrow(numeros_trampa), 1), 1]])
    } else return(sample(10:39, 5, replace = F))
    
  })
  
  output$option3 <- renderText({
    
    input$button1
    input$button2
    input$button3
    
    if (selection_box_game3() == 1){
      return(numeros_trampa[[sample(nrow(numeros_trampa), 1), 1]])
    }
    if (selection_box_game3() == 2){
      return(sample(10:39, 5, replace = F))
    } else return(sample(10:39, 5, replace = F))
    
  })

  # Finalmente se codifica la cajita de mensaje con las estadísticas del minijuego, 
  # que indicará cuantas veces se ha clickado en un random y cu+ántas en un human-trap.
  output$stats_game3 <- renderText({
    paste0('You have found ',  counter_random$countervalue , ' random lotto number(s) \n', 'and ', 
           counter_trampa$countervalue ,' human-trap number(s). ')
  })
  
  
  
}

shinyApp(ui, server)

