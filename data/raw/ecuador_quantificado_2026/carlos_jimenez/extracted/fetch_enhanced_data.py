#!/usr/bin/env python3
"""
Fetch enhanced data for Ecuador Quantificado contest:
- Ecuador GDP (to calculate remittances as % of GDP)
- Colombia and Peru remittances (for regional comparison)
- Enhanced analysis data
"""

import requests
import pandas as pd
import os

def fetch_world_bank_indicator(country_code, indicator_code, start_year=2000, end_year=2025):
    """Fetch indicator data from World Bank API."""
    url = f"https://api.worldbank.org/v2/country/{country_code}/indicator/{indicator_code}"
    params = {
        "format": "json",
        "per_page": 100,
        "date": f"{start_year}:{end_year}"
    }

    try:
        response = requests.get(url, params=params, timeout=30)
        response.raise_for_status()
        data = response.json()

        if len(data) < 2 or not data[1]:
            return pd.DataFrame()

        records = []
        for item in data[1]:
            if item["value"] is not None:
                records.append({
                    "anio": int(item["date"]),
                    "valor": item["value"],
                    "pais": country_code,
                    "indicador": indicator_code
                })

        return pd.DataFrame(records)
    except Exception as e:
        print(f"Error fetching {country_code} - {indicator_code}: {e}")
        return pd.DataFrame()


def main():
    """Fetch all enhanced data."""
    print("Fetching enhanced data from World Bank API...")

    # 1. Ecuador GDP (current US$)
    print("\n1. Fetching Ecuador GDP...")
    ecuador_gdp = fetch_world_bank_indicator("ECU", "NY.GDP.MKTP.CD")
    if not ecuador_gdp.empty:
        # Convert to millions
        ecuador_gdp['gdp_millones_usd'] = ecuador_gdp['valor'] / 1_000_000
        ecuador_gdp = ecuador_gdp[['anio', 'gdp_millones_usd']].sort_values('anio')
        print(f"   Retrieved {len(ecuador_gdp)} years of GDP data")

    # 2. Colombia remittances
    print("\n2. Fetching Colombia remittances...")
    colombia_remesas = fetch_world_bank_indicator("COL", "BX.TRF.PWKR.CD.DT")
    if not colombia_remesas.empty:
        colombia_remesas['remesas_millones_usd'] = colombia_remesas['valor'] / 1_000_000
        colombia_remesas = colombia_remesas[['anio', 'remesas_millones_usd']].sort_values('anio')
        colombia_remesas['pais'] = 'Colombia'
        print(f"   Retrieved {len(colombia_remesas)} years")

    # 3. Peru remittances
    print("\n3. Fetching Peru remittances...")
    peru_remesas = fetch_world_bank_indicator("PER", "BX.TRF.PWKR.CD.DT")
    if not peru_remesas.empty:
        peru_remesas['remesas_millones_usd'] = peru_remesas['valor'] / 1_000_000
        peru_remesas = peru_remesas[['anio', 'remesas_millones_usd']].sort_values('anio')
        peru_remesas['pais'] = 'Perú'
        print(f"   Retrieved {len(peru_remesas)} years")

    # 4. Load Ecuador remittances (already have this)
    print("\n4. Loading Ecuador remittances...")
    ecuador_remesas = pd.read_csv('data/remesas_ecuador.csv')
    ecuador_remesas = ecuador_remesas[['anio', 'remesas_millones_usd']].copy()
    ecuador_remesas['pais'] = 'Ecuador'
    print(f"   Loaded {len(ecuador_remesas)} years")

    # Save individual datasets
    os.makedirs('data', exist_ok=True)

    if not ecuador_gdp.empty:
        ecuador_gdp.to_csv('data/ecuador_gdp.csv', index=False)
        print("\n✓ Saved: data/ecuador_gdp.csv")

    # Combine regional remittances
    regional_remesas = pd.concat([ecuador_remesas, colombia_remesas, peru_remesas], ignore_index=True)
    regional_remesas.to_csv('data/remesas_regional.csv', index=False)
    print("✓ Saved: data/remesas_regional.csv")

    # Calculate remittances as % of GDP for Ecuador
    if not ecuador_gdp.empty:
        merged = pd.merge(ecuador_remesas, ecuador_gdp, on='anio', how='inner')
        merged['remesas_pct_pib'] = (merged['remesas_millones_usd'] / merged['gdp_millones_usd']) * 100
        merged = merged[['anio', 'remesas_millones_usd', 'gdp_millones_usd', 'remesas_pct_pib']]
        merged.to_csv('data/ecuador_remesas_pib.csv', index=False)
        print("✓ Saved: data/ecuador_remesas_pib.csv")

        print("\nPreview of remittances as % of GDP:")
        print(merged.tail(10).to_string(index=False))

    print("\n✓ Enhanced data fetch complete!")


if __name__ == "__main__":
    main()
