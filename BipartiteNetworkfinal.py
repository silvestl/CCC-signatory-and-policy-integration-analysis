import pandas as pd
import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import community as community_louvain
import matplotlib.cm as cm
from networkx.algorithms import bipartite
from collections import Counter
from matplotlib.patches import Rectangle, Patch
from matplotlib.lines import Line2D
from pathlib import Path
from adjustText import adjust_text
from matplotlib.colors import LinearSegmentedColormap

input = Path(r"C:\Users\silvestl\Desktop\NetworkNZC\NetworkstakeholdersCCC.xlsx")
df = pd.read_excel(input)
df = df.loc[:, ["City", "Type", "CAP_Topic"]].dropna()
df["CAP_Topic"] = df["CAP_Topic"].str[:-4]

results = []
bipartite_by_city = {}

for city, group in df.groupby("City", sort=False):
    B = nx.Graph()
    w_e = Counter(zip(group["Type"], group["CAP_Topic"]))
    stakeholdertypes = group["Type"].unique()
    policysectors = group["CAP_Topic"].unique()
    B.add_nodes_from(stakeholdertypes, bipartite=0)
    B.add_nodes_from(policysectors, bipartite=1)
    B.add_edges_from(zip(group["Type"], group["CAP_Topic"]))
    B.add_weighted_edges_from([(a, b, wt) for (a, b), wt in w_e.items()])
    bipartite_by_city[city] = B
    #weighted average degree
    n = B.number_of_nodes()
    total_w = sum(d["weight"] for _, _, d in B.edges(data=True))
    w_avg_deg = 2 * total_w / n if n else 0
    #network density
    stakeholder_nodes = {n for n, d in B.nodes(data=True) if d.get('bipartite') == 0}
    policy_nodes = {n for n, d in B.nodes(data=True) if d.get('bipartite') == 1}
    possible_edges = len(stakeholder_nodes) * len(policy_nodes)
    actual_edges = B.number_of_edges()
    density = (actual_edges / possible_edges) if possible_edges > 0 else 0.0
    wdensity = (total_w / possible_edges) if possible_edges > 0 else 0.0
    results.append({"City": city,
        "WeightedAvgDegree": w_avg_deg,
        "Density": density,
        "WeightedDensity": wdensity})

results_df = pd.DataFrame(results)
results_df.to_excel(r"c:\Users\silvestl\Desktop\NetworkNZC\NetworkMetrics_Cities.xlsx", index=False)

#degree distribution plotted
#lorenz curve for concentration of connections across nodes:
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

def plot_lorenz(ax, records, title):
    from collections import defaultdict
    city_data = defaultdict(list)
    for city, node, deg in records:
        city_data[city].append(deg)

    gini_scores = {city: gini(degrees) for city, degrees in city_data.items()}
    gini_vals = np.array(list(gini_scores.values()))

    vmin, vmax = gini_vals.min(), gini_vals.max()
    cmap = LinearSegmentedColormap.from_list("custom", ["#3B8BC2", "#e8872a"])
    norm = plt.Normalize(vmin=vmin, vmax=vmax)

    sorted_cities = sorted(gini_scores.keys(), key=lambda c: gini_scores[c])

    for city in sorted_cities:
        degrees = city_data[city]
        degrees_sorted = np.sort(degrees)
        cumulative = np.cumsum(degrees_sorted)
        cumulative = cumulative / cumulative[-1]
        x = np.linspace(0, 1, len(cumulative) + 1)
        y = np.concatenate([[0], cumulative])
        color = cmap(norm(gini_scores[city]))
        ax.plot(x, y, marker='o', label=city, color=color)

    ax.plot([0, 1], [0, 1], 'k--', linewidth=1, label="Perfect equality")

    ax.set_title(title)
    ax.set_xlabel("Cumulative share of nodes")
    ax.set_ylabel("Cumulative share of weighted degree")
    ax.grid(linestyle="--", alpha=0.4)
    
    return cmap, norm, vmin, vmax

fig, axes = plt.subplots(1, 2, figsize=(14, 6))
cmap, norm, vmin, vmax = plot_lorenz(axes[0], stakeholder_records, "Actor Types")
plot_lorenz(axes[1], policy_records, "Policy Sectors")

sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
sm.set_array([])
cbar_ax = fig.add_axes([0.92, 0.2, 0.02, 0.6])  # [left, bottom, width, height]
cbar = fig.colorbar(sm, cax=cbar_ax, label="Gini coefficient")

plt.subplots_adjust(left=0.08, right=0.9, wspace=0.3)
plt.show()

#plot for selected cities network
import seaborn as sns
sns.set_theme(style="whitegrid")
cities_to_plot = ["Bergamo", "Seville", "Torino", "Wroclaw", "Limassol", "Dublin"]

fig, axes = plt.subplots(3, 2, figsize=(16, 14))
axes = axes.flatten()

for ax, city in zip(axes, cities_to_plot):
    B = bipartite_by_city.get(city)
    if B is None:
        ax.set_title(f"{city} — no data", fontsize=12)
        ax.axis("off")
        continue
    # partitions
    left = [n for n, d in B.nodes(data=True) if d.get("bipartite") == 0]   # stakeholders
    right = [n for n, d in B.nodes(data=True) if d.get("bipartite") == 1]  # policies

    # order nodes by (weighted) degree for nicer spacing
    left_sorted = sorted(left, key=lambda n: B.degree(n, weight='weight'))
    right_sorted = sorted(right, key=lambda n: B.degree(n, weight='weight'))

    # node weighted degrees
    node_wdeg = {n: B.degree(n, weight='weight') for n in B.nodes()}

    # compute node "area" (matplotlib expects area in points^2) and clamp
    min_area = 25
    area_scale = 25
    node_area = {n: max(min_area, node_wdeg.get(n, 0) * area_scale) for n in B.nodes()}

    # create a size-aware column layout so nodes don't overlap:
    def col_positions(nodes, x, pad=0.03, gap=0.01):
        if not nodes:
            return {}
        radii = np.array([np.sqrt(node_area[n]) for n in nodes], dtype=float)
        # Calculate cumulative y positions starting from pad, with gap between nodes
        y_positions = [pad + radii[0]]
        for i in range(1, len(radii)):
            y_positions.append(y_positions[-1] + radii[i-1] + gap + radii[i])
        # Normalize to fit within [pad, 1-pad]
        total_height = y_positions[-1] + radii[-1] - pad
        scale = (1 - 2 * pad) / total_height if total_height > 0 else 1
        y_positions = [pad + (y - pad) * scale for y in y_positions]
        return {node: (x, float(y)) for node, y in zip(nodes, y_positions)}

    pos = {}
    pos.update(col_positions(left_sorted, 0.0))
    pos.update(col_positions(right_sorted, 1.0))

    # node sizes list (area in points^2)
    left_sizes = [node_area[n] for n in left_sorted]
    right_sizes = [node_area[n] for n in right_sorted]

    # edge widths scaled from weight (safe get)
    edge_weights = [d.get("weight", 1) for _, _, d in B.edges(data=True)]
    edge_widths = [max(0.5, np.log1p(w) * 2.5) for w in edge_weights]

    # draw edges behind nodes
    nx.draw_networkx_edges(B, pos, alpha=0.5, width=edge_widths, edge_color="#888888", ax=ax)
    nx.draw_networkx_nodes(B, pos, nodelist=left_sorted, node_color="#3B8BC2", 
                           node_size=left_sizes, label="Stakeholder Types", ax=ax)
    nx.draw_networkx_nodes(B, pos, nodelist=right_sorted, node_color="#35B779",
                           node_size=right_sizes, label="Policy Sectors", ax=ax)

    # draw labels offset outside columns to reduce overlap
    nx.draw_networkx_labels(B, {n: (pos[n][0], pos[n][1]) for n in left_sorted},
                            labels={n: wrap_label(n, width=15) for n in left_sorted}, font_size=7, horizontalalignment='right', ax=ax)
    nx.draw_networkx_labels(B, {n: (pos[n][0], pos[n][1]) for n in right_sorted},
                            labels={n: wrap_label(n, width=15) for n in right_sorted}, font_size=7, horizontalalignment='left', ax=ax)
    # aesthetics
    ax.set_title(f"{city}", fontsize=14)
    ax.set_xlim(-0.2, 1.2)
    ax.set_ylim(-0.05, 1.05)
    ax.axis("off")

# shared legend
legend_handles = [
    Patch(facecolor="#3B8BC2", edgecolor="k", label="Actor Types"),
    Patch(facecolor="#35B779", edgecolor="k", label="Policy Sectors")]
fig.legend(handles=legend_handles, loc='lower center', ncol=3, fontsize=12)

plt.tight_layout(rect=[0, 0.06, 1, 0.96])
plt.show()
