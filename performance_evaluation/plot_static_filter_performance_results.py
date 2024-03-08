from datetime import datetime
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

benchmark_name_map = {
    "PRK_stencil": "PRK Stencil",
    "PRK_transpose": "PRK Transpose",
    "miniMD": "miniMD (RMA Port)",
    "miniVite": "miniVite",
    "lulesh": "LULESH (RMA Port)"
}

# benchmark_paths = {
#     "PRK Stencil\n{\small 48 procs, 400 iters}\n{\small Matrix: $20000^2$}": "/home/ss540294/research/RMA_Codes/jube/benchmarks/PRK_stencil/PRK_stencil.benchmarks/000006/result/result_csv.dat",
#     "PRK Transpose\n{\small 48 procs, 400 iters}\n{\small Matrix: $16320^2$}": "/home/ss540294/research/RMA_Codes/jube/benchmarks/PRK_transpose/PRK_transpose.benchmarks/000003/result/result_csv.dat",
#     "miniMD-RMA\n{\small 48 procs, 200 iters}\n{\small LJ, $4 \cdot 10^6$ atoms}": "/home/ss540294/research/RMA_Codes/jube/benchmarks/miniMD/miniMD.benchmarks/000007/result/result_csv.dat",
#     "LULESH-RMA\n{\small 27 procs}\n{\small Mesh Size: $20^3$}": "/home/ss540294/research/RMA_Codes/jube/benchmarks/lulesh/lulesh.benchmarks/000008/result/result_csv.dat",
#     "NPB BT-RMA\n{\small 36 procs}\n{\small Class B}": "/home/ss540294/research/RMA_Codes/jube/benchmarks/BT-RMA/BT-RMA.benchmarks/000042/result/result_csv.dat",
#     "miniVite\n{\small 32 procs}\n{\small nlpkkt240}": "/home/ss540294/research/RMA_Codes/jube/benchmarks/miniVite/miniVite.benchmarks/000025/result/result_csv.dat"
# }

benchmark_paths = {
    "PRK Stencil\n{\small 48 procs, 400 iters}\n{\small Matrix: $20000^2$}": "benchmark_results/PRK_stencil/result/result_csv.dat",
    "PRK Transpose\n{\small 48 procs, 400 iters}\n{\small Matrix: $16320^2$}": "benchmark_results/PRK_transpose/result/result_csv.dat",
    "miniMD-RMA\n{\small 48 procs, 200 iters}\n{\small LJ, $4 \cdot 10^6$ atoms}": "benchmark_results/miniMD//result/result_csv.dat",
    "LULESH-RMA\n{\small 27 procs}\n{\small Mesh Size: $20^3$}": "benchmark_results/lulesh/result/result_csv.dat",
    "NPB BT-RMA\n{\small 36 procs}\n{\small Class B}": "benchmark_results/BT-RMA/result/result_csv.dat",
    "miniVite\n{\small 32 procs}\n{\small nlpkkt240}": "benchmark_results/miniVite/result/result_csv.dat"
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
    # Drow row with base / must
    df = df.drop(df[(df['compile'] == "base") & (
        df['measurement'] == "must")].index)

    # Drop all columns except tasks, compile, measurement time_avg, time_std must_compile_opt
    df = df[['tasks', 'compile', 'measurement',
             'time_avg', 'time_std', 'must_compile_opt']]

    df['benchmark'] = benchmark_name

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


def create_plots(dfs) -> None:
    palette = ["#383838", "#FFFFFF", "#b0b0b0",
               "#b0b0b0", "#f0f0f0", "#f0f0f0"]

    sns.set_style("whitegrid")
    fig = plt.figure(figsize=(9, 2.2), dpi=300)
    #fig.subplots_adjust(bottom=0.9)

    num_benchmarks = dfs['benchmark'].nunique()
    ax = sns.barplot(
        x="benchmark", y="tool slowdown", data=dfs, hue="must_compile_opt", palette=palette, edgecolor="black")
    ax.set_xlabel("", fontsize=10)
    ax.set_ylabel("Tool Slowdown", fontsize=11)
    ax.set_ymargin(0.35)

    # define hatches for the different bars
    hatches = ['', '//', '', '//', '', '//']
    hatches = [hatch for hatch in hatches for _ in range(num_benchmarks)]
    # Loop over the bars
    for i, thisbar in enumerate(ax.patches):
        # Set a different hatch for each bar
        thisbar.set_hatch(hatches[i])
    # disable axes legend
    ax.legend([],[], frameon=False)
    # enable figure legend
    leg = fig.legend(title="Applied Filter Optimizations", loc="lower center",
                     bbox_to_anchor=(0.5, -0.2), ncol=6)

    # Show values
    for i in ax.containers:
        texts = ax.bar_label(i, fmt='%.1f', rotation=90, fontsize=8)
        for text in texts:
            text.set(y=5, zorder=2000)
    plt.tight_layout()

    plt.savefig(f"plots/static_filter_performance_results.png", bbox_inches='tight')
    plt.savefig(f"plots/static_filter_performance_results.pdf", bbox_inches='tight')

if __name__ == "__main__":
    dfs = []
    for (benchmark, path) in benchmark_paths.items():
        dfs.append(read_csv(benchmark, path))
    dfs = pd.concat(dfs, axis=0)
    create_plots(dfs)
