from datetime import datetime
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import itertools
import numpy as np

benchmark_name_map = {
    "PRK_stencil": "PRK Stencil",
    "PRK_transpose": "PRK Transpose",
    "miniMD": "miniMD (RMA Port)",
    "miniVite": "miniVite",
    "lulesh": "LULESH (RMA Port)"
}

benchmark_paths = {
    "PRK Stencil": "benchmark_results/PRK_stencil/result/filterstats_result_csv.dat",
    "PRK Transpose": "benchmark_results/PRK_transpose/result/filterstats_result_csv.dat",
    "miniMD-RMA": "benchmark_results/miniMD//result/filterstats_result_csv.dat",
    "LULESH-RMA": "benchmark_results/lulesh/result/filterstats_result_csv.dat",
    "NPB BT-RMA": "benchmark_results/BT-RMA/result/filterstats_result_csv.dat",
    "miniVite": "benchmark_results/miniVite/result/filterstats_result_csv.dat"
}


plt.rc('pdf', fonttype=42)
plt.rcParams.update({
    "text.usetex": True,
    "font.family": "Helvetica",
    "mathtext.rm": "Helvetica",
    "mathtext.it": "Helvetica:italic",
    "mathtext.bf": "Helvetica:bold",
    "text.latex.preamble": r'\usepackage[cm]{sfmath}'
})


def read_csv(benchmark_name, csv_file) -> None:
    # Read csv
    df = pd.read_csv(csv_file, sep=";")
    # Drop columns which have no values
    df = df.dropna(axis=1, how='all')
    # Fill empty cells with empty string
    df = df.fillna('None')
    # Drop rows with compile_mode base as they are not of interest
    df = df.drop(df[df['compile_mode']=='base'].index)

    df['benchmark'] = benchmark_name

    df = df.sort_values(by=['optimizations'], key=lambda x: x.map({
        'None': 0,
        'CLUSTER': 1,
        'AL1000': 2,
        'ALX1000': 3,
        'ALX1000,CLUSTER': 4,
        'AL10': 5,
        'ALX10': 6,
        'ALX10,CLUSTER': 7
    }))

    df = df.drop(df[(df['optimizations'] == "AL6") |
                 (df['optimizations'] == "ALX6") | (df['optimizations'] == "ALX6,CLUSTER")].index)

    df['optimizations'] = df['optimizations'].replace('1000', r'($\\infty$)', regex=True).replace('10', '(10)', regex=True).replace(
        'CLUSTER', 'CL', regex=True).replace('ALX', 'BDX', regex=True).replace('AL', 'BD', regex=True)
    # df = df.replace('AL1000','AL(∞)').replace('ALX1000','ALX(∞)').replace('ALX6','ALX(6)').replace('ALX1000,CLUSTER','ALX(∞)+CLUSTER').replace('ALX6,CLUSTER','ALX(6)+CLUSTER')

    df["total_instr"] = df['instr_reads'] + \
        df['instr_writes'] - df['CLUSTER_ignored']
    df[benchmark_name] = df['instr_reads'] + \
        df['instr_writes'] - df['CLUSTER_ignored']

    # df[benchmark_name] = [df.loc[(df['optimizations'] == "None")]["total_instr"].iloc[0] if index == 0 else df.loc[(df['optimizations'] == "None")]["total_instr"].iloc[0] - row["total_instr"] for index, row in df.iterrows()]
    df["total_instr_rel"] = [row["total_instr"] /
                             df.loc[(df['optimizations'] == "None")]["total_instr"].iloc[0] for index, row in df.iterrows()]

    return df

def create_subplots(dfs, ax_sp, num_benchmarks):
    palette = ["#383838", "#ffffff", "#7c7c7c", "#7c7c7c",
               "#7c7c7c", "#d5d5d5", "#d5d5d5", "#d5d5d5"]
    ax = sns.barplot(
        x="benchmark", 
        y="total_instr_rel", 
        data=dfs, 
        hue="optimizations", 
        palette=palette, 
        edgecolor="black", 
        ax=ax_sp)
    ax.set_xlabel("", fontsize=10)
    ax.set_ylabel("Proportion of\nInstrumented\nMemory Accesses", fontsize=11)
    ax.set_ylim([0, 1])
    plt.xticks(fontsize=11)
    plt.yticks(np.arange(0, 1 + 0.2, 0.2), fontsize=11)
    ax.set_ymargin(0.20)

    # define hatches for the different bars
    hatches = ['', '//', '....', '', '//', '....', '', '//']
    hatches = [hatch for hatch in hatches for _ in range(num_benchmarks)] + hatches
    # Loop over the bars
    for i, thisbar in enumerate(ax.patches):
        # Set a different hatch for each bar
        thisbar.set_hatch(hatches[i])

    # disable axes legend
    ax.legend([],[], frameon=False)


    # Show values
    list_values = list(dfs["total_instr"])
    for i, container in enumerate(ax.containers):
        texts = ax.bar_label(container, labels=[
                             list_values[i+j*8] for j in range(num_benchmarks)], rotation=90, fontsize=10)
        for text in texts:
            text.set(y=4, zorder=2000)

    return ax


def create_plots(dfs) -> None:
    sns.set_style("whitegrid")
    fig = plt.figure(figsize=(7.5, 4), dpi=300)
    plt.subplots_adjust(bottom=0.5)

    plt.clf()
    sp = fig.add_subplot(211)
    ax = create_subplots(dfs[(dfs['benchmark'] == "PRK Stencil") | (dfs['benchmark'] == "PRK Transpose") | (dfs['benchmark'] == "miniMD-RMA")], sp, 3)
    sp = fig.add_subplot(212)
    ax = create_subplots(dfs[(dfs['benchmark'] == "LULESH-RMA") | (dfs['benchmark'] == "NPB BT-RMA") | (dfs['benchmark'] == "miniVite")], sp, 3)


    def flip(items, ncol):
        return list(itertools.chain(*[items[i::ncol] for i in range(ncol)]))
    
    # legend with unique items
    handles, labels = ax.get_legend_handles_labels()
    handles = flip(handles, 4)
    labels = flip(labels, 4)
    unique = [(h, l) for i, (h, l) in enumerate(zip(handles, labels)) if l not in labels[:i]]
    ax.legend(*zip(*unique), title="Applied Filter Optimizations", loc="lower center",
                    bbox_to_anchor=(0.5, -1.1), ncol=4, columnspacing=1)

    plt.tight_layout()
    plt.savefig(f"plots/static_filter_results.png", bbox_inches='tight')
    plt.savefig(f"plots/static_filter_results.pdf", bbox_inches='tight')


if __name__ == "__main__":
    dfs = []
    for (benchmark, path) in benchmark_paths.items():
        dfs.append(read_csv(benchmark, path))
    dfs = pd.concat(dfs, axis=0).drop_duplicates()
    create_plots(dfs)