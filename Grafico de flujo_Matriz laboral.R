
library(ggplot2)
library(networkD3)
library(dplyr)
library(htmltools)
library(htmlwidgets)

links <- data.frame(
  
  inicio = c("Total Desempleados", "Total Desempleados","Urbano","Urbano","Urbano",
    "Rural", "Rural", "Rural"),
  
  final = c( "Urbano", "Rural", "Empleado_2023", "Desempleado_2023", "Inactivo_2023",
    "Empleado_2023", "Desempleado_2023", "Inactivo_2023"),
  
  percent = c(89.57, 10.43, 89.57 * 0.395 , 89.57 * 0.170   ,89.57 * 0.436  ,
                          10.43 * 0.509 , 10.43 * 0.176,      10.43 * 0.315))

nodes <- data.frame(name = unique(c(links$inicio, links$final)) ,
                    label2 = c(
                      "Desempleados 2022 (298.294; 100%)",
                      "Desempleados en la zona urbana 2022 (267.186; 89,6%)",
                      "Desempleados en la zona rural 2022 (31.100; 10,4%)",
                      "Obtuvo empleo en 2023 (121.296; 40,7%)",
                      "Continuó desempleado en 2023 (50.813; 17%)",
                      "Salió de la fuerza laboral en 2023 (126.175; 42,3%)"
                    ))
links$IDinicio <- match(links$inicio,nodes$name) - 1
links$IDfinal <- match(links$final,nodes$name) - 1

nodes$grupo <- c(
  "Total",
  "Zona1",
  "Zona2",
  "Estado_emp",
  "Estado_des",
  "Estado_inac"
)

graf <- sankeyNetwork(Links = links,Nodes = nodes,Source = "IDinicio",Target = "IDfinal",Value = "percent",
                      NodeID = "label2",sinksRight = FALSE ,NodeGroup = "grupo", colourScale = JS(
                        'd3.scaleOrdinal()
.domain([
"Total",
"Zona1",
"Zona2",
"Estado_emp",
"Estado_des",
"Estado_inac"
])

.range([
"#2C7FB8",
"#A6BDDB",
"#FD8D3C",
"#FDBF6F",
"#33A02C",
"#B2DF8A"
])'
     ), fontSize = 13, nodeWidth = 20, width = 1150, height = 400)


browsable(
  tagList(
    tags$h2(
      "La mayoría de desempleados no encuentra trabajo después de un año",
      style = "text-align:center; font-size:18px; font-family:Arial; font-weight:bold; margin-bottom:5px;"),
    
    tags$h4(
      "Matriz de transición ENEMDU, por zona de residencia, 2022-2023",
      style = "text-align:center; color:gray; font-size:14px; font-family:Arial; font-weight:normal; margin-top:0;" ),

graf <- onRender(
  graf,
  '
  function(el) {
    d3.select(el)
      .selectAll(".node text")
      .style("font-family", "Arial")
  }
  '),

tags$p(
  "Fuente: INEC – Matrices de Transición Laboral. Elaboración: Angel Alava para El Quantificador de Laboratorio LIDE.",
  tags$br(),
  "Nota: Las personas que salieron de la fuerza laboral no trabajan y no están disponibles para trabajar por cualquier motivo (no se toma en cuenta la edad).
      El ancho de los flujos representa la proporción de personas que transitaron entre estados laborales.
      Los porcentajes corresponden a la transición laboral entre 2022 y 2023.
      Los valores pueden presentar pequeñas diferencias debido al redondeo.",
  style = "text-align:center; font-size:12px; color:#141414; margin-top:15px; font-family:Arial; line-height:1.5;"
)))



