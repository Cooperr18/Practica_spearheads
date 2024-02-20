nuevo_dir = "C:/Users/alber/Documents/Practica_spearheads"
setwd(nuevo_dir)

#1
spear = read_excel("C:/spearheads.xlsx")
View(spear)
spear = as.data.frame(spear)

#2
#Con el operador "names(spear)" nos referimos a los nombres del data frame, y con el operador == y = "Materiales" estamos diciendo que todos aquellos vectores en los que "Mat" sea TRUE vamos a darle el nombre de "Materiales"
names(spear)[names(spear) == "Mat"] = "Materiales"
names(spear)[names(spear) == "Con"] = "Contexto"
names(spear)[names(spear) == "Cond"] = "Conservación"
names(spear)[names(spear) == "Loo"] = "Loop"
names(spear)[names(spear) == "Peg"] = "Remache"
names(spear)[names(spear) == "Date"] = "Fecha"
names(spear)[names(spear) == "Maxle"] = "Longitud_max"
names(spear)[names(spear) == "Socle"] = "Longitud_encaje"
names(spear)[names(spear) == "Maxwi"] = "Ancho_max"
names(spear)[names(spear) == "Upsoc"] = "Ancho_encaje"
names(spear)[names(spear) == "Maxwit"] = "Ancho_max_encaje"
names(spear)[names(spear) == "Weight"] = "Peso"

#3
#Con el operador "factor()" convertimos valores de tipo numérico a valores de texto, y seleccionamos cuáles queremos que sean los valores que se sustituyan
spear$Contexto = factor(spear$Contexto, levels = c('1','2','3'), labels = c("s/c", "Habitacional", "Funerario"))
spear$Conservación = factor(spear$Conservación, levels = c('1','2','3','4'), labels = c("Excelente","Bueno","Regular","Malo"))
spear$Remache = factor(spear$Remache, levels = c('1','2'), labels = c("Sí","No"))
spear$Materiales = factor(spear$Materiales, levels = c('1','2'), labels = c("Bronce","Hierro"))
View(spear)

#4
#Creamos el objeto "tabla_Materiales" como una tabla de frecuencia, señalando la variable "Materiales" con el operador "$" dentro de nuestro data frame
#Aquí lo que hacemos es indicar la frecuencia numérica de cada variable en el data frame, es decir, el número de veces que se repite
tabla_Materiales = table(spear$Materiales)
tabla_Contextos = table(spear$Contexto)
tabla_Conservacion = table(spear$Conservación)
View(tabla_Conservacion)

#5
#Para una tabla cruzada de dos variables creamos el objeto y le damos el valor con la función "xtabs()"; añadimos un operador de suma de las dos variables, y el operador "~" precedido del valor que queremos que se cruce con otro; y finalmente, para indicar la procedencia de los datos utilizamos el operador "data ="
#Aquí estamos indicando la frecuencia de dos variables, es decir, cuántas veces se repite cada una de las variables "Materiales" con cada una de las variables "Contexto"
tabla_Materiales_Contexto = xtabs(~ Materiales + Contexto, data = spear)
tabla_Materiales_Conservacion = xtabs(~ Materiales + Conservación, data = spear)
View(tabla_Materiales_Contexto)

#6
#Con la función "prop.table()" lo que hacemos es dividir cada valor por el total de variables del vector, y lo multiplicamos por 100 para obtener el porcentaje de esa tabla de frecuencia.
#Aquí estamos creando una tabla que indica el procentaje de cada valor en su columna
tabla_porcentaje_Materiales = prop.table(tabla_Materiales) *100
tabla_porcentaje_Contexto = prop.table(tabla_Contextos) *100
tabla_porcentaje_Conservacion = prop.table(tabla_Conservacion) *100
View(tabla_porcentaje_Conservacion)

#7
#Calculamos los porcentajes de los valores de las tablas cruzadas de la actividad 5 con el operador "prop.table()" y *100, añadiendo el operador "margin = 1" que nos indica que los porcentajes se obtienen relativos a las filas, es decir, de Materiales (filas) sobre Contexto (columnas), y Materiales (filas) sobre Conservación (columnas)
porcentajes_M_Cx = prop.table(tabla_Materiales_Contexto, margin = 1) *100
porcentajes_M_Cons = prop.table(tabla_Materiales_Conservacion, margin = 1) *100

#8
#Con la función "barplot()" creamos gráficos de barras de la tabla de frecuencia de la variable "Conservación", indicando con la función "xlab" el valor del eje X (Grado de Conservación), y la función "ylab" el valor del eje y (Frecuencia en la tabla)
#Aquí lo que hacemos es indicar con qué porcentaje aparece cada uno de nuestros valores de forma relativa a la variable correspondiente. Por ejemplo, lo más común es encontrar artefactos en buen estado de conservación, y lo menos común en mal estado de conservación. O es más común encontrar artefactos sin contexto.
grafico_barras_conservacion = barplot(tabla_Conservacion,
        main = "Frecuencia de Conservación",
        xlab = "Grado de Conservación",
        ylab = "Frecuencia (%)",
        col = "khaki1")

grafico_barras_contextos = barplot(tabla_Contextos,
        main = "Frecuencia de Contextos",
        xlab = "Contextos",
        ylab = "Frecuencia (%)",
        col = "khaki1")

#9
#Seguimos el mismo procedimiento que en la 8 pero añadiendo la función "horiz = TRUE" para obtener un gráfico horizontal, y cambiando el orden de los valores en los ejes
grafico_barras_materiales = barplot(tabla_Materiales,
        horiz = TRUE,
        main = "Frecuencia de materiales",
        xlab = "Frecuencia (%)",
        ylab = "Tipos de materiales",
        col = "khaki1")

tabla_Remaches = table(spear$Remache)
grafico_barras_remache = barplot(tabla_Remaches,
                                 horiz = TRUE,
                                 main = "Frecuencia de remaches",
                                 xlab = "Frecuencia (%)",
                                 ylab = "Remaches",
                                 col = "khaki1")

#10
#Aquí lo que hacemos es crear un gráfico de barras en el que, con la función "beside = TRUE" estamos agrupando los valores de la tabla cruzada "Materiales sobre Conservación". Añadimos la función "legend =" para poner una leyenda, y le indicamos que los datos de la leyenda se correspondan con los de la tabla de frecuencia cruzada
#Con esto obtenemos la frecuencia (%) de grados de conservación por cada tipo de material. Por ejemplo, se ve que en objetos de bronce lo más común es encontrarlo en buen estado, y lo menos, en malo. Y el hierro es más común encontrarlo en bueno o regular, y menos común en excelente.
barplot(tabla_Materiales_Conservacion,
        beside = TRUE,
        main = "Frecuencia de grado de conservación por tipo de material",
        xlab = "Grado de conservación",
        ylab = "Frecuencia (%)",
        col = c("darkblue","khaki"),
        legend = rownames(tabla_Materiales_Conservacion))

#11
#Con la función "pie()" creamos el gráfico de sectores, utilizando como referencia la tabla de frecuencia que realizamos en el ejercicio 4.
#Con la función "labels =" nos referimos a los nombres en la tabla de frecuencia, y lo concatenamos con los porcentajes de la tabla del ejercicio 6. Le añadimos el valor de texto "(%)" para indicar porcentajes.
pie(tabla_Conservacion,
    main = "Distribución de los grados de conservación",
    col = c("green","darkblue","yellow","red"),
    labels = paste(names(tabla_Conservacion),"(", (tabla_porcentaje_Conservacion),"%)"))

#12
#Primero seleccionamos únicamente las variables que son continuas, es decir, que son arbitrarias, en este caso, los valores numéricos
#Unificamos todas las variables continuas en un único histograma con la función "unlist()"
#Con el argumento "prob = TRUE" le estamos diciendo que muestre la densidad de probabilidad
var_continuas = spear[sapply(spear, is.numeric)]

windows(width = 10, height = 10)

histograma_probabilidad = hist(unlist(var_continuas), 
     main = "Histograma de Probabilidad de Variables Continuas",
     xlab = "Valor", 
     prob = TRUE)


  
  
  
  
  
  
  
  