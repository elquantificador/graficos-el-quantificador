#!/usr/bin/env python3
"""
Generate enhanced multi-panel visualization for Ecuador Quantificado contest.

Creates a narrative-driven, publication-quality figure showing:
- Panel A: Regional comparison (Ecuador vs Colombia vs Perú)
- Panel B: Remittances as % of GDP (the real story)
- Panel C: Post-2020 acceleration analysis
- Panel D: Historical timeline with social context

Applies data visualization best practices from three specialized skills:
1. matplotlib: Publication-quality technical implementation (300 DPI, GridSpec, proper formatters)
2. data-visualization: Design principles (colorblind-friendly palette, accessibility, narrative flow)
3. data-viz-plots: Scientific visualization patterns (multi-panel layouts, context, annotations)
"""

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
from matplotlib.gridspec import GridSpec
import numpy as np
import os

# Professional color palette (colorblind-friendly)
ECUADOR_COLOR = "#DD8452"  # Warm orange
COLOMBIA_COLOR = "#4C72B0"  # Steel blue
PERU_COLOR = "#55A868"     # Green
HIGHLIGHT_COLOR = "#FDD9C7"  # Light orange
TEXT_COLOR = "#2B2D42"
GRID_COLOR = "#E5E5E5"
BACKGROUND_COLOR = "#FFFFFF"


def load_data():
    """Load all datasets."""
    ecuador_rem = pd.read_csv('data/remesas_ecuador.csv')
    ecuador_pib = pd.read_csv('data/ecuador_remesas_pib.csv')
    colombia_rem = pd.read_csv('data/colombia_remesas.csv')
    peru_rem = pd.read_csv('data/remesas_regional.csv')
    peru_rem = peru_rem[peru_rem['pais'] == 'Perú'].copy()
    origen_pais = pd.read_csv('data/remesas_por_pais_origen.csv')

    return ecuador_rem, ecuador_pib, colombia_rem, peru_rem, origen_pais


def create_enhanced_visualization():
    """Create multi-panel enhanced visualization."""
    # Set up publication-quality style
    plt.rcParams.update({
        'font.family': 'sans-serif',
        'font.sans-serif': ['DejaVu Sans', 'Arial', 'Helvetica'],
        'font.size': 10,
        'axes.titlesize': 12,
        'axes.titleweight': 'bold',
        'axes.labelsize': 10,
        'xtick.labelsize': 9,
        'ytick.labelsize': 9,
        'figure.dpi': 150,
        'savefig.dpi': 300,
    })

    # Load data
    ecuador_rem, ecuador_pib, colombia_rem, peru_rem, origen_pais = load_data()

    # Create figure with GridSpec for flexible layout
    fig = plt.figure(figsize=(16, 13), facecolor=BACKGROUND_COLOR)
    gs = GridSpec(4, 2, figure=fig, hspace=0.40, wspace=0.30,
                  left=0.08, right=0.96, top=0.92, bottom=0.06)

    # Panel A: Regional Comparison (top left, spans 2 rows)
    ax_regional = fig.add_subplot(gs[0:2, 0])
    create_regional_comparison(ax_regional, ecuador_rem, colombia_rem, peru_rem)

    # Panel B: Remittances as % of GDP (top right)
    ax_pct = fig.add_subplot(gs[0, 1])
    create_pct_gdp_panel(ax_pct, ecuador_pib)

    # Panel E: Countries of Origin (middle right)
    ax_origen = fig.add_subplot(gs[1, 1])
    create_origin_panel(ax_origen, origen_pais)

    # Panel C: Post-2020 Acceleration (row 2, full width)
    ax_accel = fig.add_subplot(gs[2, :])
    create_acceleration_panel(ax_accel, ecuador_rem)

    # Panel D: Historical Timeline (bottom, full width)
    ax_timeline = fig.add_subplot(gs[3, :])
    create_timeline_panel(ax_timeline, ecuador_rem, ecuador_pib)

    # Main title - Direct and impactful
    fig.suptitle('Las remesas crecieron 96% post-pandemia y ya representan 5.2% del PIB:\nEcuador depende más de sus migrantes que nunca',
                 fontsize=15, fontweight='bold', color=TEXT_COLOR, y=0.985,
                 ha='center', linespacing=1.3)

    # Save
    os.makedirs('output', exist_ok=True)
    plt.savefig('output/remesas_ecuador_enhanced.png', facecolor=BACKGROUND_COLOR, edgecolor='none')
    print("✓ Enhanced visualization saved: output/remesas_ecuador_enhanced.png")

    plt.close()


def create_regional_comparison(ax, ecuador_rem, colombia_rem, peru_rem):
    """Panel A: Regional comparison Ecuador vs Colombia vs Perú."""
    ax.set_title('Ecuador tiene el crecimiento de remesas más acelerado de la región',
                 fontsize=11, fontweight='bold', loc='left', pad=10, color=TEXT_COLOR)

    # Plot lines
    ax.plot(ecuador_rem['anio'], ecuador_rem['remesas_millones_usd'],
            color=ECUADOR_COLOR, linewidth=3, marker='o', markersize=5,
            markevery=5, label='Ecuador', zorder=3)

    ax.plot(colombia_rem['anio'], colombia_rem['remesas_millones_usd'],
            color=COLOMBIA_COLOR, linewidth=2.5, marker='s', markersize=4,
            markevery=5, label='Colombia', zorder=2, alpha=0.9)

    ax.plot(peru_rem['anio'], peru_rem['remesas_millones_usd'],
            color=PERU_COLOR, linewidth=2.5, marker='^', markersize=4,
            markevery=5, label='Perú', zorder=2, alpha=0.9)

    # Styling
    ax.set_xlabel('Año', fontsize=11, weight='semibold')
    ax.set_ylabel('Remesas recibidas (millones USD)', fontsize=11, weight='semibold')
    ax.legend(frameon=True, loc='upper left', fontsize=10)
    ax.grid(axis='y', color=GRID_COLOR, alpha=0.6, zorder=0)
    ax.set_axisbelow(True)

    # Format y-axis
    ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, p: f"{int(x):,}".replace(",", ".")))

    # Remove top and right spines
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_color(GRID_COLOR)
    ax.spines['bottom'].set_color(GRID_COLOR)

    # Insight: Specific growth rates comparison (positioned in middle right)
    ax.text(0.70, 0.50, 'Ecuador +96%\nColombia +71%\nPerú +51%\n(2020-2024)',
            transform=ax.transAxes, fontsize=10, weight='bold',
            ha='center', va='center', color=TEXT_COLOR,
            bbox=dict(boxstyle="round,pad=0.6", facecolor='white',
                     edgecolor=ECUADOR_COLOR, linewidth=2, alpha=0.95))

    # Source citation
    ax.text(0.98, 0.02, 'Fuente: data.worldbank.org (BX.TRF.PWKR.CD.DT)',
            transform=ax.transAxes, fontsize=7, ha='right', va='bottom',
            color=TEXT_COLOR, alpha=0.6, style='italic')


def create_pct_gdp_panel(ax, ecuador_pib):
    """Panel B: Remittances as % of GDP - the key insight."""
    ax.set_title('De 2.5% a 5.2% del PIB en menos de 10 años',
                 fontsize=11, fontweight='bold', loc='left', pad=10, color=TEXT_COLOR)

    # Filter to years with both data points
    data = ecuador_pib[ecuador_pib['remesas_pct_pib'].notna()].copy()

    # Create area plot
    ax.fill_between(data['anio'], 0, data['remesas_pct_pib'],
                    color=ECUADOR_COLOR, alpha=0.3, zorder=2)
    ax.plot(data['anio'], data['remesas_pct_pib'],
            color=ECUADOR_COLOR, linewidth=3, marker='o',
            markersize=5, markevery=3, zorder=3)

    # Highlight 2024 value with USD amount
    final_year = data['anio'].max()
    final_pct = data[data['anio']==final_year]['remesas_pct_pib'].values[0]
    final_usd = data[data['anio']==final_year]['remesas_millones_usd'].values[0]
    ax.plot(final_year, final_pct, 'o', color=ECUADOR_COLOR,
            markersize=10, markeredgecolor='white', markeredgewidth=2, zorder=4)

    ax.annotate(f'{final_pct:.1f}%\n${final_usd:,.0f}M',
                xy=(final_year, final_pct),
                xytext=(5, 10),
                textcoords='offset points',
                fontsize=11, fontweight='bold',
                color=ECUADOR_COLOR,
                bbox=dict(boxstyle="round,pad=0.4", facecolor='white',
                         edgecolor=ECUADOR_COLOR, linewidth=1.5))

    # Styling
    ax.set_xlabel('Año', fontsize=11, weight='semibold')
    ax.set_ylabel('% del PIB', fontsize=11, weight='semibold')
    ax.grid(axis='y', color=GRID_COLOR, alpha=0.6, zorder=0)
    ax.set_axisbelow(True)

    # Set y-axis limit based on maximum value in dataset, not just final value
    max_pct = data['remesas_pct_pib'].max()
    ax.set_ylim(0, max_pct * 1.15)  # 15% padding above maximum

    # Spines
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_color(GRID_COLOR)
    ax.spines['bottom'].set_color(GRID_COLOR)

    # Source citation
    ax.text(0.98, 0.03, 'Fuente: data.worldbank.org (BX.TRF.PWKR.CD.DT, NY.GDP.MKTP.CD)',
            transform=ax.transAxes, fontsize=7.5, ha='right', va='bottom',
            color=TEXT_COLOR, alpha=0.7, style='italic')


def create_origin_panel(ax, origen_pais):
    """Panel E: Countries of origin for remittances."""
    ax.set_title('77.8% de las remesas provienen de Estados Unidos',
                 fontsize=11, fontweight='bold', loc='left', pad=10, color=TEXT_COLOR)

    # Sort by percentage descending
    data = origen_pais.sort_values('porcentaje', ascending=True)

    # Create color palette - highlight USA
    colors = [ECUADOR_COLOR if pais == 'Estados Unidos' else '#9EAAB5'
              for pais in data['pais_origen']]

    # Horizontal bar chart
    bars = ax.barh(data['pais_origen'], data['porcentaje'], color=colors,
                   edgecolor='white', linewidth=1.5, height=0.7)

    # Add percentage labels on bars
    for i, (pais, pct) in enumerate(zip(data['pais_origen'], data['porcentaje'])):
        ax.text(pct + 1.5, i, f'{pct}%',
                va='center', ha='left', fontsize=10, fontweight='bold',
                color=ECUADOR_COLOR if pais == 'Estados Unidos' else TEXT_COLOR)

    # Styling
    ax.set_xlabel('Porcentaje del total (%)', fontsize=11, weight='semibold')
    ax.set_xlim(0, 85)
    ax.grid(axis='x', color=GRID_COLOR, alpha=0.6, zorder=0)
    ax.set_axisbelow(True)

    # Spines
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_color(GRID_COLOR)
    ax.spines['bottom'].set_color(GRID_COLOR)

    # Source citation
    ax.text(0.98, 0.03, 'Fuente: contenido.bce.fin.ec (Balanza de Pagos)',
            transform=ax.transAxes, fontsize=7.5, ha='right', va='bottom',
            color=TEXT_COLOR, alpha=0.7, style='italic')


def create_acceleration_panel(ax, ecuador_rem):
    """Panel C: Post-2020 acceleration analysis."""
    ax.set_title('La pandemia aceleró las remesas: +96% entre 2020 y 2024',
                 fontsize=11, fontweight='bold', loc='left', pad=10, color=TEXT_COLOR)

    # Focus on 2015-2025
    data = ecuador_rem[ecuador_rem['anio'] >= 2015].copy()

    # Plot with highlighted post-2020
    pre_2020 = data[data['anio'] <= 2020]
    post_2020 = data[data['anio'] >= 2020]

    # Pre-2020 (muted)
    ax.plot(pre_2020['anio'], pre_2020['remesas_millones_usd'],
            color='gray', linewidth=2, alpha=0.5, zorder=1)

    # Post-2020 (highlighted)
    ax.plot(post_2020['anio'], post_2020['remesas_millones_usd'],
            color=ECUADOR_COLOR, linewidth=3.5, marker='o',
            markersize=6, zorder=3)

    # Fill post-2020 area with pattern
    ax.fill_between(post_2020['anio'], 0, post_2020['remesas_millones_usd'],
                    color=HIGHLIGHT_COLOR, alpha=0.5, hatch='///',
                    edgecolor=ECUADOR_COLOR, linewidth=0, zorder=2)

    # Calculate growth rate
    val_2020 = data[data['anio']==2020]['remesas_millones_usd'].values[0]
    val_2024 = data[data['anio']==2024]['remesas_millones_usd'].values[0]
    growth_rate = ((val_2024 - val_2020) / val_2020) * 100

    # Insight: Acceleration comparison (horizontal single line)
    ax.text(0.35, 0.85, '96% en 4 años vs. 40% en los 5 años previos',
            transform=ax.transAxes, fontsize=10, weight='bold',
            ha='center', va='top', color=ECUADOR_COLOR,
            bbox=dict(boxstyle="round,pad=0.5", facecolor='white',
                     edgecolor=ECUADOR_COLOR, linewidth=2, alpha=0.95))

    # Styling
    ax.set_xlabel('Año', fontsize=11, weight='semibold')
    ax.set_ylabel('Remesas (millones USD)', fontsize=11, weight='semibold')
    ax.grid(axis='y', color=GRID_COLOR, alpha=0.6, zorder=0)
    ax.set_axisbelow(True)
    ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, p: f"{int(x):,}".replace(",", ".")))

    # Spines
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_color(GRID_COLOR)
    ax.spines['bottom'].set_color(GRID_COLOR)

    # Source citation
    ax.text(0.02, 0.03, 'Fuente: contenido.bce.fin.ec y data.worldbank.org',
            transform=ax.transAxes, fontsize=7.5, ha='left', va='bottom',
            color=TEXT_COLOR, alpha=0.7, style='italic')


def create_timeline_panel(ax, ecuador_rem, ecuador_pib):
    """Panel D: Historical timeline with events."""
    ax.set_title('Evolución histórica: de la dolarización a la crisis actual (2000-2025)',
                 fontsize=11, fontweight='bold', loc='left', pad=10, color=TEXT_COLOR)

    # Main plot
    ax.plot(ecuador_rem['anio'], ecuador_rem['remesas_millones_usd'],
            color=ECUADOR_COLOR, linewidth=3, marker='o',
            markersize=4, markevery=5, zorder=3)

    ax.fill_between(ecuador_rem['anio'], 0, ecuador_rem['remesas_millones_usd'],
                    color=ECUADOR_COLOR, alpha=0.15, zorder=2)

    # Historical events (based on research)
    # Format: (year, label, y_position_for_text, vertical_alignment)
    events = [
        (2000, 'Dolarización', 2300, 'top'),
        (2008, 'Crisis\nFinanciera\nGlobal', 5200, 'top'),
        (2020, 'COVID-19\nInicio\nPandemia', 2800, 'top'),
        (2021, 'Crisis\nEconómica\nIntensifica', 5200, 'bottom'),
        (2023, 'Violencia\nNarco-criminal\nAumenta', 4500, 'top'),
    ]

    # First, draw all markers at data points (lower z-order)
    for year, label, y_pos, va_pos in events:
        year_data = ecuador_rem[ecuador_rem['anio'] == year]
        if not year_data.empty:
            y_value = year_data['remesas_millones_usd'].values[0]
            ax.plot(year, y_value, 'o', color='#C62828', markersize=6,
                   markeredgecolor='white', markeredgewidth=1.2, zorder=3)

    # Then, draw vertical lines and text annotations (higher z-order to be on top)
    for year, label, y_pos, va_pos in events:
        # Vertical line for the event
        ax.axvline(x=year, color='gray', linestyle='--', alpha=0.5, linewidth=1.5, zorder=1)

        # Text annotation (on top of markers)
        ax.text(year, y_pos, label,
                fontsize=8, ha='center', va=va_pos,
                bbox=dict(boxstyle="round,pad=0.4", facecolor='#F8F9FA',
                         edgecolor='gray', linewidth=0.8, alpha=0.95),
                zorder=5)

    # Styling
    ax.set_xlabel('Año', fontsize=11, weight='semibold', labelpad=15)
    ax.set_ylabel('Remesas recibidas (millones USD)', fontsize=11, weight='semibold')
    ax.grid(axis='y', color=GRID_COLOR, alpha=0.6, zorder=0)
    ax.set_axisbelow(True)
    ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, p: f"{int(x):,}".replace(",", ".")))
    ax.set_xlim(ecuador_rem['anio'].min() - 1, ecuador_rem['anio'].max() + 1)

    # Spines
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_color(GRID_COLOR)
    ax.spines['bottom'].set_color(GRID_COLOR)

    # Source citation (positioned to avoid overlap with xlabel)
    ax.text(0.02, -0.18,
            'Fuente: contenido.bce.fin.ec (Balanza de Pagos) y data.worldbank.org (BX.TRF.PWKR.CD.DT, NY.GDP.MKTP.CD)',
            transform=ax.transAxes, fontsize=7.5, ha='left', va='top',
            color=TEXT_COLOR, alpha=0.7, style='italic')


def main():
    """Main execution."""
    print("Generating enhanced multi-panel visualization...")
    create_enhanced_visualization()
    print("\n✓ Enhanced visualization complete!")


if __name__ == "__main__":
    main()
