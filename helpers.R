

# Center-align legends in graphs ------------------------------------------

align_legend <- function(p, hjust = 0.5)
{
  # extract legend
  g <- cowplot::plot_to_gtable(p)
  grobs <- g$grobs
  legend_index <- which(sapply(grobs, function(x) x$name) == "guide-box")
  legend <- grobs[[legend_index]]
  
  # extract guides table
  guides_index <- which(sapply(legend$grobs, function(x) x$name) == "layout")
  
  # there can be multiple guides within one legend box  
  for (gi in guides_index) {
    guides <- legend$grobs[[gi]]
    
    # add extra column for spacing
    # guides$width[5] is the extra spacing from the end of the legend text
    # to the end of the legend title. If we instead distribute it by `hjust:(1-hjust)` on
    # both sides, we get an aligned legend
    spacing <- guides$width[5]
    guides <- gtable::gtable_add_cols(guides, hjust*spacing, 1)
    guides$widths[6] <- (1-hjust)*spacing
    title_index <- guides$layout$name == "title"
    guides$layout$l[title_index] <- 2
    
    # reconstruct guides and write back
    legend$grobs[[gi]] <- guides
  }
  
  # reconstruct legend and write back
  g$grobs[[legend_index]] <- legend
  g
}


#################################################
# Function to calculate the NBI from indicators #
#################################################

library(zoo)

nbi_calc <- function(data) {
  
  #############################
  # 1. Acceso albergue digno
  #############################
  
  # 1.1. Calidad de la vivienda
  
  # 1.1.1 Hogar en vivienda eventual o tugurio.
  
  data <- data %>%mutate(NBI_111 = ifelse(V01_TIPO_VIVIENDA == "Tugurio" | V01_TIPO_VIVIENDA == "Otro (Local, casa móvil, barco, camión)", 1, 0))
  
  
  # 1.1.2 Hogar en vivienda de paredes de desecho u otro o techo de desecho o piso de tierra.
  
  data <- data %>% mutate(NBI_112 = ifelse(V04_MATERIAL_PAREDES == "... material de desecho" |V04_MATERIAL_PAREDES == "Otro (Zinc, adobe)", 1,
                                           ifelse(V05_MATERIAL_TECHO == "... material de desecho", 1,
                                                  ifelse(V07_MATERIAL_PISO=="Piso de tierra",1, 0))))
  
  
  # 1.1.3 Hogar en vivienda con materiales en mal estado simultáneamente en paredes, techo y piso.
  
  data <- data %>% mutate(NBI_113 = ifelse(V08A_ESTADO_PAREDES== "Malo" & V08B_ESTADO_TECHO== "Malo"& V08C_ESTADO_PISO=="Malo", 1,0))
  
  
  # 1.2 Hacinamiento
  
  # 1.2.1 Hogares en viviendas con más de dos personas por aposento
  
  data <- data %>% mutate(NBI_121 = ifelse(V32_HACINAMIENTO_APOSENTOS== "Vivienda hacinada según aposentos", 1,0))
  
  
  # 1.3 Electricidad
  
  # 1.3.1 Hogares en viviendas sin electricidad para alumbrado
  
  data <- data %>% mutate(NBI_131 = ifelse(V14_PROVENIENCIA_ELECTRICIDAD == "No hay luz eléctrica", 1,0))
  
  
  # 1.1. Calidad de la vivienda
  
  data <- data %>% mutate(NBI_1 = ifelse(NBI_111 == 1 | NBI_112 == 1 | NBI_113 == 1 | NBI_121==1 |NBI_131==1, 1,0))
  
  
  
  
  #############################
  # 2. Acceso a vida saludable 
  #############################
  
  #2.1 Infraestructura físico sanitaria
  
  #2.1.1 Hogar en vivienda urbana que consume agua de pozo, río o lluvia.
  
  data <- data %>% mutate(NBI_211 = ifelse(ID_ZONA=="Urbano" & V11_PROVENIENCIA_AGUA== "... un pozo", 1, 
                                           ifelse(ID_ZONA=="Urbano" & V11_PROVENIENCIA_AGUA== "... un río o quebrada",1,
                                                  ifelse(ID_ZONA=="Urbano" & V11_PROVENIENCIA_AGUA== "Otra fuente (Lluvia, camión cisterna, hidrante)",1,0))))
  
  #2.1.2 Hogar en vivienda rural que consume agua de río o lluvia.
  
  data <- data %>% mutate(NBI_212 = ifelse(ID_ZONA=="Rural" & V11_PROVENIENCIA_AGUA== "... un río o quebrada", 1,
                                           ifelse(ID_ZONA=="Rural" & V11_PROVENIENCIA_AGUA== "Otra fuente (Lluvia, camión cisterna, hidrante)",1,0)))
  
  #2.1.3 Hogar en vivienda rural que consume agua de pozo y no tiene cañería dentro de la vivienda
  
  data <- data %>% mutate(NBI_213 = ifelse(ID_ZONA=="Rural" & V11_PROVENIENCIA_AGUA== "... un pozo" & V12_TUBERIA_AGUA=="No",1,0))
  
  #2.1.4 Hogar en vivienda urbana con eliminación de excretas por pozo o u otro sistema o no tien.
  
  data <- data %>% mutate(NBI_214 = ifelse(ID_ZONA=="Urbano" & V13_SERVICIO_SANITARIO == "... es de hueco, de pozo negro o letrina",1, 
                                           ifelse(ID_ZONA=="Urbano" & V13_SERVICIO_SANITARIO == "... tiene salida directa a acequia, zanja, río o estero", 1, 
                                                  ifelse(ID_ZONA=="Urbano" & V13_SERVICIO_SANITARIO == "No tiene servicio sanitario", 1,0))))
  
  #2.1.5 Hogar en vivienda rural con eliminación de excretas por otro sistema o no tiene.
  
  data <- data %>% mutate(NBI_215 = ifelse(ID_ZONA=="Rural" & V13_SERVICIO_SANITARIO == "... tiene salida directa a acequia, zanja, río o estero", 1, 
                                           ifelse(ID_ZONA=="Rural" & V13_SERVICIO_SANITARIO == "No tiene servicio sanitario", 1,0)))
  
  
  
  #2. acceso a vida saludable
  
  data <- data %>% mutate(NBI_2 = ifelse(NBI_211 == 1 | NBI_212 == 1 | NBI_213 == 1 | NBI_214==1 |NBI_215==1, 1,0))
  
  
  
  
  #############################
  #3 Acceso al conocimiento 
  #############################
  
  #3.1. Asistencia escolar 
  
  #3.1.1. Hogares con uno o más miembros de 7 a 17 años que no asiste a la educación regular.
  
  data <- data %>%mutate(NBI_310 = ifelse(P03_EDAD > 6 & P03_EDAD < 18 & P13_ASISTE_EDUCACION_CUIDO =="No asiste", 1, 0))
  
  data <- data %>%
    group_by(hhid) %>%
    mutate(NBI_311 = sort(NBI_310 , decreasing = TRUE)) %>% 
    ungroup
  
  data$NBI_311 <- ave(data$NBI_311 , data$hhid, FUN= function(x)unique(x[1]))
  
  #3.2 Logro escolar
  
  #3.2.1 Hogares con uno o más miembros de 7 a 17 años que asiste a la educación regular con rezago mayor a dos años
  
  data <- data %>%mutate(NBI_320 = ifelse(P03_EDAD > 6 & P03_EDAD < 18 & P13_ASISTE_EDUCACION_CUIDO !="No asiste" & P41_REZAGO_ESCOLAR >2, 1,0))
  
  data$NBI_320[is.na(data$NBI_320)] <- 0
  
  data <- data %>%
    group_by(hhid) %>%
    mutate(NBI_321 = sort(NBI_320 , decreasing = TRUE)) %>% 
    ungroup
  
  data$NBI_321 <- ave(data$NBI_321 , data$hhid, FUN= function(x)unique(x[1]))
  
  
  #3 Acceso a conocimiento 
  data <- data %>% mutate(NBI_3 = ifelse(NBI_311 == 1 | NBI_321 == 1, 1,0))
  
  
  ########################################
  # 4. Acceso a otros bienes y servicios 
  ########################################
  
  #4.1. Capacidad de consumo 
  
  #4.1.1 Hogares sin perceptores regulares (ocupados o pensionistas o rentistas) y cuyo jefe tiene 50 años o más y primaria completa o menos.
  
  # needed: age - head of the household 
  
  # data<-data%>% mutate(age_head = ifelse(P00_NUMERO_LINEA == 1 ,P03_EDAD,
  #                                               ifelse(P00_NUMERO_LINEA> 1 , NA, NA )))
  # data$age_head<- na.locf(data$age_head)
  
  data <- data %>% 
    group_by(hhid) %>% 
    mutate(age_head = P03_EDAD[P00_NUMERO_LINEA == 1]) %>% 
    ungroup
  
  
  #needed: education - head of the household 
  
  # data <- data %>%mutate(school_head = ifelse(P00_NUMERO_LINEA == 1, P40_ANOS_ESCOLARIDAD, 
  #                                                   ifelse(P00_NUMERO_LINEA>1, NA, NA)))
  # data$school_head <- na.locf(data$school_head)
  
  data <- data %>% 
    group_by(hhid) %>% 
    mutate(school_head = P40_ANOS_ESCOLARIDAD[P00_NUMERO_LINEA == 1]) %>% 
    ungroup
  
  #needed: earners 
  
  data <- data %>% mutate(earner0 = ifelse(P03_EDAD>15 &  P42_CONDACT =="Ocupados", 1,
                                           ifelse(P03_EDAD>15  & P42_CONDACT == "Inactivo vive de rentas o alquileres", 1,
                                                  ifelse(P03_EDAD>15  & P42_CONDACT == "Inactivo pensionado(a) o jubilado(a)", 1, 0))))
  
  data$recipient0[is.na(data$earner0)] <- 0
  
  data <- data %>% 
    group_by(hhid) %>% 
    mutate(earner = sum(earner0, na.rm = T)) %>% 
    ungroup
  
  
  data <- data %>%
    mutate(NBI_411 = ifelse(earner == 0 & age_head > 49 &  school_head < 7, 1,0))
  
  
  #4.1.2 Hogares urbanos con un perceptor y primaria incompleta y tres o más dependientes.
  
  #needed: dependents
  
  data <- data %>%mutate(depend = ifelse(P03_EDAD < 15 , 1,
                                         ifelse(P03_EDAD > 14 &  P42_CONDACT =="Inactivo otra situación", 1,
                                                ifelse(P03_EDAD > 14 &  P42_CONDACT == "Inactivo pensionado(a) o jubilado(a)",1, 
                                                       ifelse(P03_EDAD > 14 &  P42_CONDACT =="Inactivo se dedica a oficios domésticos", 1,
                                                              ifelse(P03_EDAD > 14 &  P42_CONDACT =="Inactivo sólo estudia", 1,
                                                                     ifelse(P03_EDAD > 14 &  P42_CONDACT =="Inactivo vive de rentas o alquileres", 1,0)))))))   
  
  data<- data %>% 
    group_by(hhid) %>% 
    mutate(dependents = sum(depend, na.rm = T)) %>% 
    ungroup
  
  #needed_ average education of earners
  
  data <- data %>% 
    group_by(hhid, earner0) %>% 
    mutate(educ = mean(P40_ANOS_ESCOLARIDAD, na.rm = T)) %>% 
    ungroup
  
  data$educ <- ave(data$educ , data$hhid, FUN= function(x)unique(x[1]))
  
  
  data <- data %>%
    mutate(NBI_412 = ifelse(ID_ZONA =="Urbano" & earner == 1  & educ < 6 & dependents >2, 1,0))
  
  
  #4.1.3 Hogares urbanos con dos perceptores y con menos de cinco años de educación en promedio y tres o más dependientes.
  
  data <- data %>%
    mutate(NBI_413 = ifelse(ID_ZONA =="Urbano" & earner== 2  & educ < 5 & dependents >2, 1,0))
  
  
  #4.1.4 Hogares urbanos con tres o más perceptores y con menos de cuatro años de educación en promedio y tres o más dependientes.
  
  data <- data %>%
    mutate(NBI_414 = ifelse(ID_ZONA =="Urbano" & earner >2  & educ < 4 & dependents > 2, 1,0))
  
  #4.1.5 Hogares rurales con un perceptor y menos de cuatro años de educación y tres o más dependientes.
  
  data <- data %>%
    mutate(NBI_415 = ifelse(ID_ZONA =="Rural" & earner ==1  & educ < 4 & dependents > 2, 1,0))
  
  
  #4.1.6 Hogares rurales con dos perceptores y con menos de tres años de educación en promedio y tres o más dependientes.
  
  data <- data %>%
    mutate(NBI_416 = ifelse(ID_ZONA =="Rural" & earner == 2  & educ < 3 & dependents > 2, 1,0))
  
  #4.1.7 Hogares rurales con tres o más perceptores y con menos de dos años de educación en promedio y tres o más dependiente
  
  data <- data %>%
    mutate(NBI_417 = ifelse(ID_ZONA =="Rural" & earner > 2  & educ < 2 & dependents > 2, 1,0))
  
  #4 Acceso a otros bienes 
  data <- data %>% 
    mutate(NBI_4 = ifelse(NBI_411 == 1 | NBI_412 == 1 |NBI_413 == 1 | NBI_414 == 1 | NBI_415 == 1 | NBI_416 == 1 | NBI_417 == 1, 1,0))
  
  #NBI FINAL 
  
  data <- data %>% 
    mutate(new_dep = NBI_1+ NBI_2+ NBI_3+ NBI_4)
  
  data <- data %>% 
    mutate(new_NBI = ifelse(new_dep>0,1,0))
  
  return(data)
}