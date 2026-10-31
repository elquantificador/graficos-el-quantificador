# Fuente de datos: colada morada, Día de Difuntos 2026

Insumos de los gráficos 52_a y 52_b. Consulta: 31 de octubre de 2026.

## `costo_litro_colada_2026_10.csv` (gráfico 52_a)

Costo por litro de colada morada hecha en casa y comprada en locales.

**Hecha en casa.** Receta de El Comercio, "Colada morada y guaguas de pan, tradición ecuatoriana" (Jenny Martínez, octubre de 2026): https://www.elcomercio.com/sabores/colada-morada-guaguas-pan-tradicion-ecuatoriana/. Usa 4 litros de agua y no indica el rendimiento; se asume que rinde unos 5 litros (agua más fruta, harina y azúcar, menos evaporación). Es un supuesto conservador: si rinde más, el costo por litro baja.

| Ingrediente | Cantidad | Precio de supermercado | Costo | Fuente |
|---|---|---|---|---|
| Harina de maíz morado | 250 g | $0,85 por libra | $0,47 | La Cobacha (tienda en línea) |
| Arándanos (sustituto de mortiño) | 500 g | $1,70 por 120 g | $7,08 | Coral Hipermercado (Rappi) |
| Mora | 250 g | $1,90 por 500 g | $0,95 | Allkupamba (tienda en línea) |
| Piña | 1 unidad | $1,89 | $1,89 | Santa María (Rappi) |
| Naranjilla | 8 unidades | $0,18 por unidad | $1,44 | Mi Comisariato (Rappi) |
| Frutilla | 450 g | $1,74 por 500 g | $1,57 | Santa María (Rappi) |
| Babaco | 1 unidad | $2,90 | $2,90 | Santa María Express (Rappi) |
| Canela, clavo de olor, pimienta dulce | paquetes completos | $1,99; $0,47; $0,75 | $3,21 | El Sabor e Ile (Rappi) |
| Ishpingo | paquete más pequeño | $1,25 | $1,25 | Productos La Cena (tienda en línea) |
| Azúcar | 2 tazas (~400 g) | $1,15 por kg | $0,46 | San Carlos, Tía (tienda en línea) |

- Todo de supermercado: $21,22 por receta, $4,24 por litro.
- Frutas al por mayor: mora ($2,26/kg), frutilla ($1,83/kg), naranjilla híbrida ($1,05/kg; ~800 g) y piña ($0,44/kg; ~1,8 kg) con la mediana de precios mayoristas de octubre de 2026 del SIPA (MAG); el resto a precio de supermercado. Total $18,39, $3,68 por litro.
- No incluye hojas de naranja, hierba luisa, cedrón, gas ni tiempo de preparación.
- Versión anterior (descartada): receta de Laylita's Recipes, $3,46 y $2,82 por litro.

**Locales** (seis panaderías y un restaurante; precio de 1 litro publicado en sitio web o app de reparto):

| Local | Canal | Precio por litro |
|---|---|---|
| Morada Colada (marca del Restaurante Micasa) | moradacolada.com | $5,00 |
| Guaguas Quito | guaguasquito.com | $5,25 |
| San Honoré | sanhonore.com | $7,71 ($8,48 por 1,1 litros) |
| Panificadora Ambato | Rappi | $7,94 |
| La Española | Rappi | $9,75 |
| Panpe | Rappi | $9,99 |
| Cyrano | Uber Eats | $16,10 |

El gráfico muestra el más barato (Restaurante Morada Colada), la mediana y el más caro (Cyrano, con nota de que el precio es de Uber Eats). Los precios en apps de reparto pueden incluir recargo.

## `sipa_precios_sep_oct_2025.csv` (gráfico 52_b)

**Proveedor:** Ministerio de Agricultura y Ganadería (MAG), Sistema de Información Pública Agropecuaria (SIPA), precios mayoristas diarios por mercado. Compilación diaria del repositorio público `alexeperez/precios-agro-ecuador`.

**Productos:** mora de Castilla (251), babaco (275), piña (270), naranjilla híbrida (266), frutilla (248) y maracuyá (263).

**Cálculo:**

1. Para cada producto y mercado, se promedia el precio por kilo de septiembre de 2025 y el de la semana del 26 de octubre al 1 de noviembre de 2025.
2. Se conservan solo los mercados con datos en ambos periodos.
3. Se promedian los mercados y se convierte a dólares por libra (1 kg = 2,20462 lb). `cambio` es la variación entre ambos promedios.

| Fruta | Mercados | Septiembre (USD/lb) | Fin de octubre (USD/lb) | Cambio |
|---|---|---|---|---|
| Mora | 10 | 0,72 | 1,10 | +52% |
| Babaco | 3 | 0,31 | 0,37 | +21% |
| Naranjilla | 11 | 0,41 | 0,49 | +19% |
| Piña | 7 | 0,26 | 0,29 | +10% |
| Maracuyá | 13 | 0,33 | 0,36 | +9% |
| Frutilla | 10 | 0,99 | 1,04 | +5% |

Se usa 2025 porque los datos de octubre de 2026 están incompletos: la mora solo tiene un mercado y el babaco y la piña no tienen registros ese mes.

Contexto adicional: con la mediana de 2021 a 2025, la mora sube cada año la semana previa al feriado (+46% en 2021, +37% en 2022, +44% en 2023, +16% en 2024 y +79% en 2025, frente a su promedio de septiembre por mercado).
