import numpy as np
import plotly.graph_objects as go
import matplotlib.colors # Used for LinearSegmentedColormap and rgb2hex

stakeholder_records = []
policy_records = []

for city, B in bipartite_by_city.items():
    for n, d in B.nodes(data=True):
        deg = B.degree(n, weight="weight")
        if d["bipartite"] == 0:
            stakeholder_records.append((city, n, deg))
        else:
            policy_records.append((city, n, deg))

def gini(degrees):
    arr = np.sort(np.array(degrees, dtype=float))
    n = len(arr)
    if n == 0 or arr.sum() == 0:
        return 0
    index = np.arange(1, n + 1)
    return (2 * np.sum(index * arr) / (n * arr.sum())) - (n + 1) / n

def plot_lorenz_plotly(records, title):
    from collections import defaultdict
    city_data = defaultdict(list)
    for city, node, deg in records:
        city_data[city].append(deg)

    gini_scores = {city: gini(degrees) for city, degrees in city_data.items()}
    gini_vals = np.array(list(gini_scores.values()))

    # Handle cases where gini_vals might be empty or all same to avoid division by zero
    if len(gini_vals) == 0:
        fig = go.Figure()
        fig.update_layout(title=f"Lorenz Curve - {title} (No data available)")
        return fig
    vmin, vmax = gini_vals.min(), gini_vals.max()

    # Define the matplotlib colormap and normalizer to get colors
    cmap_mpl = matplotlib.colors.LinearSegmentedColormap.from_list("custom", ["#3B8BC2", "#e8872a"])
    norm_mpl = matplotlib.colors.Normalize(vmin=vmin, vmax=vmax)

    # Sort cities by Gini score ascending for consistent coloring/legend order
    sorted_cities = sorted(gini_scores.keys(), key=lambda c: gini_scores[c])

    fig = go.Figure()

    # Add Perfect equality line
    fig.add_trace(go.Scatter(
        x=[0, 1], y=[0, 1], mode='lines',
        line=dict(color='black', dash='dash'),
        name='Perfect equality', showlegend=True,
        hovertemplate="Perfect Equality Line"
    ))

    for city in sorted_cities:
        degrees = city_data[city]
        if not degrees: # Skip if no degrees for the city
            continue
        degrees_sorted = np.sort(degrees)
        cumulative = np.cumsum(degrees_sorted)
        # Ensure cumulative[-1] is not zero before division
        if cumulative[-1] == 0:
            cumulative_normalized = np.zeros_like(cumulative)
        else:
            cumulative_normalized = cumulative / cumulative[-1]
        x = np.linspace(0, 1, len(cumulative_normalized) + 1)
        y = np.concatenate([[0], cumulative_normalized])

        # Get color from the matplotlib colormap and convert to hex
        rgb_color = cmap_mpl(norm_mpl(gini_scores[city]))
        hex_color = matplotlib.colors.rgb2hex(rgb_color[:3])

        fig.add_trace(go.Scatter(
            x=x,
            y=y,
            mode='lines',
            name=f"{city} (Gini: {gini_scores[city]:.2f})", # Include Gini in name for legend
            line=dict(color=hex_color, width=2),
            hovertemplate="City: %{customdata[0]}<br>Gini Coefficient: %{customdata[1]:.2f}<br>Cumulative Nodes: %{x:.2f}<br>Cumulative Degree: %{y:.2f}",
            customdata=[[city, gini_scores[city]] for _ in x],
            showlegend=True
        ))

    fig.update_layout(
        title=f"Lorenz Curve - {title}",
        xaxis_title="Cumulative share of nodes",
        yaxis_title="Cumulative share of weighted degree",
        hovermode="closest",
        legend_title="City (Gini Coefficient)",
        xaxis=dict(showgrid=True, gridwidth=1, gridcolor='LightGrey', range=[0,1]),
        yaxis=dict(showgrid=True, gridwidth=1, gridcolor='LightGrey', range=[0,1]),
        width=700,
        height=500
    )
    return fig

# Generate interactive Plotly Lorenz curves
fig_stakeholder_gini = plot_lorenz_plotly(stakeholder_records, "Stakeholder Types")
fig_policy_gini = plot_lorenz_plotly(policy_records, "Policy Sectors")

print("Interactive Lorenz Curve - Stakeholder Types:")
fig_stakeholder_gini.show()

print("\nInteractive Lorenz Curve - Policy Sectors:")
fig_policy_gini.show()
