# Fuente de datos: colada morada, Día de Difuntos 2026

Insumos de los gráficos 52 y 53. Consulta: 31 de octubre de 2026.

## `costo_litro_colada_2026_10.csv` (gráfico 52)

Costo por litro de colada morada hecha en casa y comprada en panadería.

**Hecha en casa.** Receta de Laylita's Recipes (10 porciones; se asume que rinde unos 4 litros: 12 tazas de agua, 14 oz de pulpa de naranjilla y la fruta).

| Ingrediente | Cantidad | Precio de supermercado | Fuente |
|---|---|---|---|
| Harina de maíz morado | 1 taza (~120 g) | $0,85 por libra | La Cobacha (tienda en línea) |
| Pulpa de naranjilla | 14 oz (397 g) | $1,67 por 500 g | Frutasi, Santa María (Rappi) |
| Mora | 2 tazas (~290 g) | $1,90 por 500 g | Allkupamba (tienda en línea) |
| Arándanos (sustituto de mortiño) | 2 tazas (~300 g) | $1,70 por 120 g | Coral Hipermercado (Rappi) |
| Frutilla | 2 tazas (~330 g) | $1,74 por 500 g | Santa María (Rappi) |
| Piña | 1 unidad | $1,89 | Santa María (Rappi) |
| Panela molida | 13 oz (~370 g) | $0,95 por 500 g | Madrina, Mi Comisariato (Rappi) |
| Canela, clavo de olor, pimienta dulce | paquetes completos | $1,99; $0,47; $0,75 | El Sabor e Ile (Rappi) |

- Todo de supermercado: $13,85 por receta, $3,46 por litro.
- Frutas al por mayor: mora ($2,26/kg), frutilla ($1,83/kg), naranjilla híbrida ($1,05/kg; ~800 g de fruta para la pulpa) y piña ($0,44/kg; ~1,8 kg) con la mediana de precios mayoristas de octubre de 2026 del SIPA (MAG); el resto a precio de supermercado. Total $11,28, $2,82 por litro.
- No incluye ishpingo, anís, hierbas, gas ni tiempo de preparación.

**Panaderías** (precio de 1 litro publicado en sitio web o app de reparto):

| Panadería | Canal | Precio por litro |
|---|---|---|
| Morada Colada | moradacolada.com | $5,00 |
| Guaguas Quito | guaguasquito.com | $5,25 |
| San Honoré | sanhonore.com | $7,71 ($8,48 por 1,1 litros) |
| Panificadora Ambato | Rappi | $7,94 |
| La Española | Rappi | $9,75 |
| Panpe | Rappi | $9,99 |
| Cyrano | Uber Eats | $16,10 |

El gráfico muestra la más barata, la mediana y la más cara. Los precios en apps de reparto pueden incluir recargo.

## `sipa_semana_difuntos_2021_2025.csv` (gráfico 53)

**Proveedor:** Ministerio de Agricultura y Ganadería (MAG), Sistema de Información Pública Agropecuaria (SIPA), precios mayoristas diarios por mercado. Compilación diaria del repositorio público `alexeperez/precios-agro-ecuador`, desde enero de 2021.

**Productos:** mora de Castilla (251), babaco (275), piña (270), naranjilla híbrida (266), frutilla (248) y maracuyá (263).

**Cálculo:**

1. Para cada producto, mercado y año (2021-2025), cada precio diario por kilo se divide para el promedio de septiembre de ese mercado y año.
2. Se toma la mediana de esa razón en la semana antes del feriado (26 de octubre a 1 de noviembre) y en la semana del 9 al 15 de noviembre.
3. `antes` y `despues` son esa mediana menos 1.

La mora subió la semana previa en todos los años: +46% (2021), +37% (2022), +44% (2023), +16% (2024) y +79% (2025). El babaco solo se registra en Ambato, Loja y Riobamba.
