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

benchmark_details = {
    "PRK Stencil": ["PRK Stencil\n{48 procs}\n{400 iters, Matrix: $20000^2$}", "benchmark_results/PRK_stencil/result/result_csv.dat"],
    "PRK Transpose": ["PRK Transpose\n{48 procs\n{400 iters, Matrix: $16320^2$}", "benchmark_results/PRK_transpose/result/result_csv.dat"],
    "miniMD-RMA": ["miniMD-RMA\n{48 procs\n{200 iters, LJ, $4 \\cdot 10^6$ atoms}", "benchmark_results/miniMD//result/result_csv.dat"],
    "LULESH-RMA": ["LULESH-RMA\n{27 procs\n{Mesh Size: $20^3$}", "benchmark_results/lulesh/result/result_csv.dat"],
    "NPB BT-RMA": ["NPB BT-RMA\n{36 procs\n{Class C}", "benchmark_results/BT-RMA/result/result_csv.dat"],
    "miniVite": ["miniVite\n{32 procs\n{nlpkkt240}", "benchmark_results/miniVite/result/result_csv.dat"]
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


def read_csv(benchmark_name, full_description, csv_file) -> None:
    # Read csv
    df = pd.read_csv(csv_file, sep=";")
    # Drop columns which have no values
    df = df.dropna(axis=1, how='all')
    # Fill empty cells with empty string
    df = df.fillna('None')
    # Drow row with base / must
    df = df.drop(df[(df['compile'] == "base") & (
        df['measurement'] == "must")].index)

    # Drop all columns except tasks, compile, measurement time_avg, time_std must_compile_opt
    df = df[['tasks', 'compile', 'measurement',
             'time_avg', 'time_std', 'must_compile_opt']]

    df['benchmark'] = benchmark_name
    df['full_description'] = full_description

    # Calculate tool slowdown
    target_column = "time_avg"
    df["tool slowdown"] = [row[target_column]/df.loc[(df['compile'] == "base")
                                                     & (df['measurement'] == "base")
                                                     & (df['tasks'] == row['tasks'])][target_column].iloc[0] for index, row in df.iterrows()]
    
    df["time_std_rel"] = df["time_std"] / df["time_avg"]
    target_column = "time_std_rel"
    df["tool slowdown std rel"] = [row[target_column] + df.loc[(df['compile'] == "base")
                                                     & (df['measurement'] == "base")
                                                     & (df['tasks'] == row['tasks'])][target_column].iloc[0] for index, row in df.iterrows()]
    df["tool slowdown std"] = df["tool slowdown"] * df["tool slowdown std rel"]

    # Drow row with base / base
    df = df.drop(df[(df['compile'] == "base") & (
        df['measurement'] == "base")].index)

    # Drow some rows
    df = df.drop(df[(df['must_compile_opt'] == "AL6") | (df['must_compile_opt'] == "ALX6") | (
        df['must_compile_opt'] == "AL1000") | (df['must_compile_opt'] == "AL10") | (df['must_compile_opt'] == "ALX6,CLUSTER")].index)

    df = df.sort_values(by=['must_compile_opt'], key=lambda x: x.map({
        'None': 0,
        'CLUSTER': 1,
        'AL1000': 2,
        'ALX1000': 3,
        'ALX1000,CLUSTER': 4,
        'AL10': 5,
        'ALX10': 6,
        'ALX10,CLUSTER': 7
    }))

    df['must_compile_opt'] = df['must_compile_opt'].replace('1000', r'($\\infty$)', regex=True).replace('10', '(10)', regex=True).replace(
        'CLUSTER', 'CL', regex=True).replace('ALX', 'BDX', regex=True).replace('AL', 'BD', regex=True)

    return df


def create_subplots(dfs, ax_sp, num_benchmarks):
    palette = ["#383838", "#ffffff", "#7c7c7c", "#7c7c7c",
               "#d5d5d5", "#d5d5d5"]
    ax = sns.barplot(
        x="full_description", y="tool slowdown", data=dfs, hue="must_compile_opt", palette=palette, edgecolor="black", ax=ax_sp)
    ax.set_xlabel("", fontsize=10)
    ax.set_ylabel("Tool Slowdown", fontsize=11)
    ax.set_ymargin(0.35)
    plt.xticks(fontsize=11)
    plt.yticks(np.arange(0, 20 + 5, 5), fontsize=11)
    plt.ylim([0,20])
    ax.set_ymargin(0.20)

    # define hatches for the different bars
    hatches = ['', '//', '', '//', '', '//']
    hatches = [hatch for hatch in hatches for _ in range(num_benchmarks)] + hatches
    # Loop over the bars
    for i, thisbar in enumerate(ax.patches):
        # Set a different hatch for each bar
        thisbar.set_hatch(hatches[i])

    # disable axes legend
    ax.legend([],[], frameon=False)


    # Show values
    for i in ax.containers:
        texts = ax.bar_label(i, fmt='%.1f', rotation=90, fontsize=10)
        for text in texts:
            text.set(y=4, zorder=2000)
    plt.tight_layout()


    return ax

def create_plots(dfs) -> None:
    palette = ["#383838", "#FFFFFF", "#7c7c7c",
               "#7c7c7c", "#d5d5d5", "#d5d5d5"]

    sns.set_style("whitegrid")
    fig = plt.figure(figsize=(7.5, 5), dpi=300)
    plt.subplots_adjust(bottom=0.2, top=0.4)

    plt.clf()
    sp = fig.add_subplot(211)
    ax = create_subplots(dfs[(dfs['benchmark'] == "PRK Stencil") | (dfs['benchmark'] == "PRK Transpose") | (dfs['benchmark'] == "miniMD-RMA")], sp, 3)
    sp = fig.add_subplot(212)
    ax = create_subplots(dfs[(dfs['benchmark'] == "LULESH-RMA") | (dfs['benchmark'] == "NPB BT-RMA") | (dfs['benchmark'] == "miniVite")], sp, 3)

    def flip(items, ncol):
        return list(itertools.chain(*[items[i::ncol] for i in range(ncol)]))
    
    # legend with unique items
    handles, labels = ax.get_legend_handles_labels()
    unique = [(h, l) for i, (h, l) in enumerate(zip(handles, labels)) if l not in labels[:i]]
    ax.legend(*zip(*unique), title="Applied Filter Optimizations", loc="lower center",
                    bbox_to_anchor=(0.5, -1.2), ncol=6, columnspacing=1)
    plt.tight_layout()

    plt.savefig(f"plots/performance_results.png", bbox_inches='tight')
    plt.savefig(f"plots/performance_results.pdf", bbox_inches='tight')

if __name__ == "__main__":
    dfs = []
    for (benchmark, [full_description, path]) in benchmark_details.items():
        dfs.append(read_csv(benchmark, full_description, path))
    dfs = pd.concat(dfs, axis=0)
    create_plots(dfs)
