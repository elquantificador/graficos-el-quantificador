#!/usr/bin/env python3
"""
Fetch remittances data for Ecuador from World Bank API and merge with latest BCE figures.

Data source:
- World Bank API: Indicator BX.TRF.PWKR.CD.DT (Personal remittances, received - current US$)
- Banco Central del Ecuador (BCE): Latest official figures for recent years
"""

import requests
import pandas as pd
import os

# World Bank API endpoint for Ecuador remittances
WB_API_URL = "https://api.worldbank.org/v2/country/ECU/indicator/BX.TRF.PWKR.CD.DT"
WB_PARAMS = {
    "format": "json",
    "per_page": 100,
    "date": "2000:2025"  # Request full range
}

# Latest BCE official figures (source: Banco Central del Ecuador)
# These supplement the World Bank data with the most recent years
BCE_DATA = [
    {"anio": 2016, "remesas_millones_usd": 2601.0, "fuente": "BCE"},
    {"anio": 2020, "remesas_millones_usd": 3337.0, "fuente": "BCE"},
    {"anio": 2022, "remesas_millones_usd": 4743.0, "fuente": "BCE"},
    {"anio": 2023, "remesas_millones_usd": 5447.5, "fuente": "BCE"},
    {"anio": 2024, "remesas_millones_usd": 6539.8, "fuente": "BCE"},
    {"anio": 2025, "remesas_millones_usd": 7729.0, "fuente": "BCE"},
]


def fetch_world_bank_data():
    """Fetch remittances data from World Bank API."""
    print("Fetching data from World Bank API...")

    try:
        response = requests.get(WB_API_URL, params=WB_PARAMS, timeout=30)
        response.raise_for_status()
        data = response.json()

        # World Bank API returns [metadata, data]
        if len(data) < 2 or not data[1]:
            print("Warning: No data returned from World Bank API")
            return pd.DataFrame()

        records = []
        for item in data[1]:
            if item["value"] is not None:
                # Convert from USD to millions of USD
                remesas_millones = item["value"] / 1_000_000
                records.append({
                    "anio": int(item["date"]),
                    "remesas_millones_usd": round(remesas_millones, 1),
                    "fuente": "World Bank"
                })

        df = pd.DataFrame(records)
        print(f"Retrieved {len(df)} years from World Bank API")
        return df

    except requests.exceptions.RequestException as e:
        print(f"Error fetching World Bank data: {e}")
        print("Will use only BCE data as fallback")
        return pd.DataFrame()


def merge_data(wb_df, bce_data):
    """
    Merge World Bank and BCE data, prioritizing BCE for recent years.

    Strategy: Use World Bank for historical data, but replace with BCE
    figures where available (more up-to-date and authoritative for Ecuador).
    """
    bce_df = pd.DataFrame(bce_data)

    if wb_df.empty:
        print("Using only BCE data (World Bank API unavailable)")
        return bce_df

    # Get years from BCE to avoid duplicates
    bce_years = set(bce_df["anio"])

    # Keep World Bank data for years NOT in BCE dataset
    wb_historical = wb_df[~wb_df["anio"].isin(bce_years)].copy()

    # Combine: historical from WB + recent from BCE
    merged = pd.concat([wb_historical, bce_df], ignore_index=True)
    merged = merged.sort_values("anio").reset_index(drop=True)

    print(f"Merged dataset: {len(merged)} years total")
    print(f"  - {len(wb_historical)} years from World Bank")
    print(f"  - {len(bce_df)} years from BCE")

    return merged


def save_data(df, output_path):
    """Save the merged dataset to CSV."""
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    df.to_csv(output_path, index=False, encoding="utf-8")
    print(f"\nData saved to: {output_path}")
    print(f"Year range: {df['anio'].min()} - {df['anio'].max()}")
    print(f"\nPreview:")
    print(df.tail(10))


def main():
    """Main execution flow."""
    # Fetch World Bank data
    wb_df = fetch_world_bank_data()

    # Merge with BCE data
    merged_df = merge_data(wb_df, BCE_DATA)

    # Save to CSV
    output_path = os.path.join("data", "remesas_ecuador.csv")
    save_data(merged_df, output_path)

    print("\n✓ Data fetch complete!")


if __name__ == "__main__":
    main()
