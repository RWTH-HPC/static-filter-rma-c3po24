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
#     "PRK Stencil": "/home/ss540294/research/RMA_Codes/jube/benchmarks/PRK_stencil/PRK_stencil.benchmarks/000006/result/filterstats_result_csv.dat",
#     "PRK Transpose": "/home/ss540294/research/RMA_Codes/jube/benchmarks/PRK_transpose/PRK_transpose.benchmarks/000003/result/filterstats_result_csv.dat",
#     "miniMD-RMA": "/home/ss540294/research/RMA_Codes/jube/benchmarks/miniMD/miniMD.benchmarks/000007/result/filterstats_result_csv.dat",
#     "LULESH-RMA": "/home/ss540294/research/RMA_Codes/jube/benchmarks/lulesh/lulesh.benchmarks/000008/result/filterstats_result_csv.dat",
#     "NPB BT-RMA": "/home/ss540294/research/RMA_Codes/jube/benchmarks/BT-RMA/BT-RMA.benchmarks/000042/result/filterstats_result_csv.dat",
#     "miniVite": "/home/ss540294/research/RMA_Codes/jube/benchmarks/miniVite/miniVite.benchmarks/000025/result/filterstats_result_csv.dat"
# }

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

    df['optimizations'] = df['optimizations'].replace('1000', r'($\\infty$)', regex=True).replace('10', '(10)', regex=True).replace('10', '(10)', regex=True).replace(
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


def create_plots(dfs) -> None:
    palette = ["#383838", "#FFFFFF", "#b0b0b0", "#b0b0b0",
               "#b0b0b0", "#f0f0f0", "#f0f0f0", "#f0f0f0"]

    sns.set_style("whitegrid")
    fig = plt.figure(figsize=(9, 2), dpi=300)
    plt.subplots_adjust(bottom=0.4)

    num_benchmarks = dfs['benchmark'].nunique()
    plt.clf()
    ax = sns.barplot(
        x="benchmark", y="total_instr_rel", data=dfs, hue="optimizations", palette=palette, edgecolor="black")
    ax.set_xlabel("", fontsize=10)
    ax.set_ylabel("Proportion of Instrumented\nMemory Accesses", fontsize=11)
    ax.set_ylim([0, 1])
    plt.xticks(fontsize=11)
    plt.yticks(fontsize=11)
    ax.set_ymargin(0.20)

    # define hatches for the different bars
    hatches = ['', '//', '..', '', '//', '..', '', '//']
    hatches = [hatch for hatch in hatches for _ in range(num_benchmarks)]
    # Loop over the bars
    for i, thisbar in enumerate(ax.patches):
        # Set a different hatch for each bar
        thisbar.set_hatch(hatches[i])

    # disable axes legend
    ax.legend([],[], frameon=False)
    # enable figure legend
    leg = fig.legend(title="Applied Filter Optimizations", loc="lower center",
                    bbox_to_anchor=(0.5, -0.25), ncol=8, columnspacing=1)
    leg.get_title().set_fontsize(11)
    # Show values
    list_values = list(dfs["total_instr"])
    for i, container in enumerate(ax.containers):
        texts = ax.bar_label(container, labels=[
                             list_values[i+j*8] for j in range(6)], rotation=90, fontsize=7.5)
        for text in texts:
            text.set(y=5, zorder=2000)
    plt.tight_layout()
    plt.savefig(f"plots/static_filter_results.png", bbox_inches='tight')
    plt.savefig(f"plots/static_filter_results.pdf", bbox_inches='tight')
    exit(1)


if __name__ == "__main__":
    dfs = []
    for (benchmark, path) in benchmark_paths.items():
        # dfs.append(read_csv(benchmark, path)[['optimizations',benchmark]].transpose())
        dfs.append(read_csv(benchmark, path))
    dfs = pd.concat(dfs, axis=0).drop_duplicates()
    create_plots(dfs)

    # Initialize parser
    # parser = argparse.ArgumentParser()
    # parser.add_argument(
    #     'benchmark_name', help="Name of the benchmark, e.g. 'lulesh, 'miniMD'")
    # parser.add_argument('-i', '--id', default=None, help="Id of the Jube run")
    # parser.add_argument('-t', '--target', default="time_avg", help="Name of the column that should be evaluated. \
    #                     The column that is used for evaluation is the first column that has the provided value as a prefix, \
    #                     thus if the column also has a unit in its name the unit does not need to be provided. \
    #                     E.g. if the column name is 'time_avg [s]' then providing 'time_avg' is sufficient")
    # parser.add_argument('-e', '--err', default="time_std",
    #                     help="Column that contains the error values")
    # parser.add_argument(
    #     '-p', '--path', help="Path to the result file, e.g. filterstats_result_csv.dat. The file has to be in csv format.")
    # args = parser.parse_args()
