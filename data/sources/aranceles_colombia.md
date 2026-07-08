# Fuente de datos: Aranceles aplicados a importaciones desde Colombia

## Dataset
**Base de importaciones desde Colombia y listado de aranceles por subpartida**

## Proveedor
COMEX, SENAE y Arancel Nacional Integrado.

## Acceso
Archivos utilizados:

- `data/raw/aranceles_colombia/COL_completo_con_arancel.xlsx`
- `data/raw/aranceles_colombia/lista-de-ecuador.xlsx`

## Variables utilizadas

- `Periodo`: mes de referencia.
- `codigo_subpartida`: identificador de subpartida arancelaria.
- `Subpartida`: descripción declarada en la base de importaciones.
- `Arancel`: arancel aplicado, expresado como proporcion en la base cruda.
- `Descripción`: descripción de la subpartida en el listado arancelario.
- `Arancel_Base`: arancel base de la subpartida.

## Notas

- El gráfico se restringe a marzo de 2026.
- El ranking usa las cinco subpartidas con mayor diferencia entre el arancel aplicado y el arancel base.
