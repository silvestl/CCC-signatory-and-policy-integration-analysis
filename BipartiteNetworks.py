#Bipartite networks of Climate City Contract (CCC) signatories.
#Input: NetworkstakeholdersCCC.xlsx, one row per signatory (columns: City, Type, CAP_Topic)
#Output: NetworkMetrics_Cities.xlsx, weighted density per CCC (Sections 2.3, 3.2.2; Table S3)
#NodeDegrees_Cities.xlsx, degree and weighted degree per node and CCC
#NodeDegrees_Summary.xlsx, pooled shares of links and signatories (Figure 2)
#Figure_1_bipartite.pdf, Figure 1

from collections import Counter
from pathlib import Path
import textwrap
import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
import pandas as pd
from matplotlib.patches import Patch

DATA = Path("NetworkstakeholdersCCC.xlsx")

#create bipartite networks for each CCC

df = pd.read_excel(DATA)
df = df.loc[:, ["City", "Type", "CAP_Topic"]].dropna()
df["CAP_Topic"] = df["CAP_Topic"].str[:-4]

results, node_records, bipartite_by_city = [], [], {}

for city, group in df.groupby("City", sort=False):
    types = group["Type"].unique()
    sectors = group["CAP_Topic"].unique()
    # an actor type and a policy sector with the same name would merge into one node
    assert not set(types) & set(sectors), f"Name clash in {city}"

    B = nx.Graph()
    B.add_nodes_from(types, bipartite=0)
    B.add_nodes_from(sectors, bipartite=1)
    edge_weights = Counter(zip(group["Type"], group["CAP_Topic"]))   # signatories per pair
    B.add_weighted_edges_from([(a, s, w) for (a, s), w in edge_weights.items()])
    bipartite_by_city[city] = B

    #WD = signatories / (actor types present x policy sectors present)
    n_signatories = sum(w for *_, w in B.edges(data="weight"))
    possible_pairs = len(types) * len(sectors)
    results.append({
        "City": city,
        "Signatories": n_signatories,
        "Density": B.number_of_edges() / possible_pairs,       # share of possible pairs linked
        "WeightedDensity": n_signatories / possible_pairs,      # WD
    })

    # for H2: degree = distinct partners, weighted degree = signatories
    for node, attr in B.nodes(data=True):
        is_type = attr["bipartite"] == 0
        node_records.append({
            "City": city,
            "Side": "Actor type" if is_type else "Policy sector",
            "Node": node,
            "Degree": B.degree(node),
            "Signatories": B.degree(node, weight="weight"),
        })

pd.DataFrame(results).to_excel("NetworkMetrics_Cities.xlsx", index=False)

nodes_df = pd.DataFrame(node_records)
nodes_df.to_excel("NodeDegrees_Cities.xlsx", index=False)

# 2. Pooled shares of links (degree) and signatories (weighted degree) & Figure 2

summary = (nodes_df.groupby(["Side", "Node"])
           .agg(CCCs=("City", "nunique"),
                Signatories=("Signatories", "sum"),
                Links=("Degree", "sum"))
           .reset_index())
for col, share in [("Signatories", "Share_signatories"), ("Links", "Share_links")]:
    summary[share] = summary[col] / summary.groupby("Side")[col].transform("sum")
summary = summary.sort_values(["Side", "Share_links"], ascending=[True, False])
summary.to_excel("NodeDegrees_Summary.xlsx", index=False)
print(summary.round(3).to_string(index=False))

# 3. Figure 1: bipartite networks of selected CCC signatories

CITIES = ["Torino", "Limassol", "Suceava"]
TYPE_COL, SECTOR_COL = "#3B8BC2", "#35B779"
ROW_H = 0.19
FS = 7
WRAP = 22


def sides(B):
    """Actor types and policy sectors, largest (most signatories) on top."""
    key = lambda n: -B.degree(n, weight="weight")
    types = sorted((n for n, d in B.nodes(data=True) if d["bipartite"] == 0), key=key)
    sectors = sorted((n for n, d in B.nodes(data=True) if d["bipartite"] == 1), key=key)
    return types, sectors


def column(nodes, x, n_rows):
    """Evenly spaced, vertically centred positions for one node set."""
    ys = np.arange(len(nodes))[::-1] + (n_rows - len(nodes)) / 2
    return {n: (x, y) for n, y in zip(nodes, ys)}


def draw(ax, city, B):
    types, sectors = sides(B)
    n_rows = max(len(types), len(sectors))
    pos = {**column(types, 0.0, n_rows), **column(sectors, 1.0, n_rows)}
    w = dict(B.degree(weight="weight"))
    edges = list(B.edges(data="weight"))
    n_sig = sum(x for *_, x in edges)
    wd = n_sig / (len(types) * len(sectors))

    nx.draw_networkx_edges(B, pos, edgelist=[(a, b) for a, b, _ in edges],
                           width=[0.3 + 0.6 * np.sqrt(x) for *_, x in edges],
                           edge_color="#9a9a9a", alpha=0.6, ax=ax)
    for nodes, col in ((types, TYPE_COL), (sectors, SECTOR_COL)):
        nx.draw_networkx_nodes(B, pos, nodelist=nodes, node_color=col, linewidths=0,
                               node_size=[min(10 + 2.5 * w[n], 80) for n in nodes], ax=ax)
    for n in types:
        ax.text(-0.06, pos[n][1], textwrap.fill(f"{n} ({w[n]})", WRAP),
                ha="right", va="center", fontsize=FS, linespacing=0.9)
    for n in sectors:
        ax.text(1.06, pos[n][1], textwrap.fill(f"{n} ({w[n]})", WRAP),
                ha="left", va="center", fontsize=FS, linespacing=0.9)
    ax.set_title(f"{city}: {n_sig} signatories, WD = {wd:.2f}", fontsize=FS + 1)
    ax.set_xlim(-1.55, 2.55)
    ax.set_ylim(-0.7, n_rows - 0.3)
    ax.axis("off")


graphs = [bipartite_by_city[c] for c in CITIES]
rows = [max(map(len, sides(B))) for B in graphs]
left_h, right_h = rows[0] + 1.5, sum(r + 1.5 for r in rows[1:])

fig = plt.figure(figsize=(6.7, ROW_H * max(left_h, right_h) + 0.4))   # 6.7 in = 17 cm
gs = fig.add_gridspec(len(CITIES) - 1, 2, height_ratios=[r + 1.5 for r in rows[1:]], wspace=0.08)
draw(fig.add_subplot(gs[:, 0]), CITIES[0], graphs[0])
for i, (city, B) in enumerate(zip(CITIES[1:], graphs[1:])):
    draw(fig.add_subplot(gs[i, 1]), city, B)

fig.legend(handles=[Patch(color=TYPE_COL, label="Actor types"),
                    Patch(color=SECTOR_COL, label="Policy sectors")],
           loc="lower center", ncol=2, fontsize=FS + 1, frameon=False,
           bbox_to_anchor=(0.5, -0.04))
fig.savefig("Figure_1_bipartite.pdf", bbox_inches="tight")
plt.show()