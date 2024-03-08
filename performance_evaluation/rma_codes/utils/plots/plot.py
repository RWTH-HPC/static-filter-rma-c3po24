import os
import argparse
import itertools
from datetime import datetime
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import re
import shutil
import csv


ylabel_map = {
    "time_avg": "Execution Time [s]",
    "tool slowdown": "Tool Slowdown",
    "memory total (MiB)": "memory total (MiB)",
    "memory overhead": "Memory Overhead"
}

benchmark_name_map = {
    "PRK_stencil": "PRK Stencil",
    "PRK_transpose": "PRK Transpose",
    "miniMD": "miniMD (RMA Port)",
    "miniVite": "miniVite",
    "lulesh": "LULESH (RMA Port)"
}

plt.rc('pdf', fonttype=42)


class Plot():
    def __init__(self, benchmark_name, id: int = None, path: str = None, target: str = "time_avg", yerr: str = "time_std"):
        self.benchmark_name = benchmark_name
        self.target = target
        self.yerr = yerr
        self.result_path = path
        self.run_dir = self.result_path
        self.id = id
        self.file_path = os.path.dirname(os.path.abspath(__file__))

        if self.result_path == None:
            # path is set
            self.benchmark_dir = os.path.abspath(
                f"{self.file_path}/../../benchmarks/{self.benchmark_name}")
            if self.id == None:
                self.id = self.__get_current_id()
            self.id = str(self.id).zfill(6)
            self.run_dir = os.path.abspath(
                f"{self.benchmark_dir}/{self.benchmark_name}.benchmarks/{self.id}")
            if self.result_path == None:
                self.result_path = f"{self.run_dir}/result/result_csv.dat"
        else:
            # path is not set
            found_id = re.search(r'/.*/([0-9]+)/result', self.result_path)
            found_run_dir = re.search(r'(.*?)/result', self.result_path)
            self.id = "unkID"
            if found_id:
                self.id = found_id.group(1)
            self.run_dir = "UNK"
            if found_run_dir:
                self.run_dir = found_run_dir.group(1)

        self.result_path = os.path.abspath(self.result_path)

    # copied from JUBE (https://github.com/FZJ-JSC/JUBE/)
    def __get_current_id(self) -> int:
        # Return the highest id found in directory 'base_dir'
        filelist = sorted(os.listdir(os.path.abspath(
            f"{self.benchmark_dir}/{self.benchmark_name}.benchmarks")))
        try:
            filelist = sorted(os.listdir(os.path.abspath(
                f"{self.benchmark_dir}/{self.benchmark_name}.benchmarks")))
        except OSError as error:
            filelist = list()

        maxi = -1
        for item in filelist:
            try:
                maxi = max(int(re.findall("^([0-9]+)$", item)[0]), maxi)
            except IndexError:
                pass
        return maxi

    def is_csv_file(self, file_path):
        try:
            with open(file_path, 'r') as file:
                dialect = csv.Sniffer().sniff(file.read(1024))
                return True
        except csv.Error:
            return False

    def read_csv(self) -> None:
        if not self.is_csv_file(self.result_path):
            exit(
                f"[\033[91m\033[1mERROR\033[0m]: Result file is not in csv format! @{self.result_path}")
        # Read csv
        self.df = pd.read_csv(self.result_path)
        # print(self.df)
        # Drop columns which have no values
        self.df = self.df.dropna(axis=1, how='all')
        # Drop rows which have missing values
        self.df = self.df.dropna()

        # Find the first column name that has self.target as a prefix
        first_target_match = self.df.filter(
            like=self.target, axis=1).columns[0]
        # Rename that column to self.target
        self.df = self.df.rename(columns={first_target_match: self.target})

        # Rename column "compile_mode" to "compile". TODO make the column names consistent in the benchmarks
        if "compile_mode" in self.df.columns:
            self.df = self.df.rename(columns={"compile_mode": "compile"})

        # Rename Must branch values
        # Replace all occurences of feature/onesided with MUST-RMA
        self.df = self.df.replace({"feature/onesided": "MUST-RMA"})
        # Replace all occurences of rmasanitizer with RMA-Sanitizer
        self.df = self.df.replace({"rmasanitizer": "RMA-Sanitizer"})

        # Calculate tool slowdown
        self.df["tool slowdown"] = [row[self.target]/self.df.loc[(self.df['compile'] == "base")
                                                                 & (self.df['measurement'] == "base")
                                                                 & (self.df.get('RMA_target') == row.get('RMA_target'))
                                                                 & (self.df['tasks'] == row['tasks'])][self.target].iloc[0] for index, row in self.df.iterrows()]

        # Add memory overhead if column "memory total (MiB)" exists
        if "memory total (MiB)" in self.df.columns:
            self.df["memory overhead"] = [row["memory total (MiB)"]/self.df.loc[(self.df['compile'] == "base")
                                                                                & (self.df['measurement'] == "base")
                                                                                & (self.df.get('RMA_target') == row.get('RMA_target'))
                                                                                & (self.df['tasks'] == row['tasks'])]["memory total (MiB)"].iloc[0] for index, row in self.df.iterrows()]

        if "must branch" in self.df.columns:
            self.df["compile:measurement"] = self.df["must branch"] + \
                ":" + self.df["compile"] + ":" + self.df["measurement"]
        else:
            self.df["compile:measurement"] = self.df["compile"] + \
                ":" + self.df["measurement"]

        # Do we have multiple RMA_targets, e.g. runs with RMA Fence, RMA Lock_all, or no RMA
        rma_synchs = ["default"]
        if "RMA_target" in self.df:
            rma_synchs = self.df.get("RMA_target").unique()

        # Drop rows that are not of interrest and rename "compile:measurement" values
        # Do this for each RMA_target value, e.g. lulesh can be build without RMA, with fence, and with lock_all
        # and we want to consider each of these dataset on their own for dropping and renaming
        for rma_synch in rma_synchs:
            rows_to_drop = []
            for drop_value, keep_amount in [(":base:base", 1),
                                            (":base:must", 0),
                                            (":tsan:base", 0),
                                            (":tsan-opt:base", 0),
                                            ("RMA-Sanitizer:tsan:must", 0),
                                            ("MUST-RMA:tsan-opt:must", 0)]:
                for np in self.df["tasks"].unique():
                    indices = [index for index, row in self.df.iterrows() if
                               drop_value in row["compile:measurement"]
                               and (row.get('RMA_target', "default") == rma_synch)
                               and (row['tasks'] == np)]
                    if len(indices) > keep_amount:
                        rows_to_drop += indices[keep_amount:]

            for index in rows_to_drop:
                self.df = self.df.drop(index)
            # Rename "compile:measurement" values
            for old, new in [(":base:base", "Baseline"), (":tsan:base", "ThreadSanitizer"), ("MUST-RMA:tsan:must", "MUST-RMA"), ("RMA-Sanitizer:tsan-opt:must", "RMA-Sanitizer")]:
                if len(rows := [row["compile:measurement"] for index, row in self.df.iterrows() if old in row["compile:measurement"] and row.get('RMA_target', "default") == rma_synch]) in range(1, len(self.df["tasks"].unique())+1):
                    self.df.loc[(self.df['compile:measurement'] == rows[0]) & (self.df.get(
                        'RMA_target', "default") == rma_synch), 'compile:measurement'] = new

    def create_plots(self, root_folder=None) -> None:
        if not root_folder:
            root_folder = f"{self.benchmark_name}_{self.id}_{datetime.now().strftime('%Y%m%d-%H%M%S')}"

        path = os.path.abspath(f"{self.file_path}/images/{root_folder}")
        os.makedirs(path)

        palette = ["#8EBAE5", "#00549F", "#F6A800",
                   "#FDD48F", "#B8D698", "#57AB27"]
        sns.set_style("darkgrid", rc={
                      "grid.linestyle": "solid", "grid.color": "white"})
        plt.figure(figsize=(9, 6), dpi=300)
        plt.subplots_adjust(bottom=0.2)
        # plt.rc('text', usetex=True)
        # TODO make result table consistent regarding to RMA_target
        # for t, rma in itertools.product([(self.target, self.yerr), ("tool slowdown", "tool slowdown std"), ("memory overhead", None)], list(self.df.get("RMA_target", pd.Series([], dtype="float64")).unique()) or [""]):
        plot_types = [(self.target, self.yerr)]

        if all(col in self.df.columns for col in ["tool slowdown"]):
            plot_types.append(("tool slowdown", None))

        if "memory overhead" in self.df.columns:
            plot_types.append(("memory overhead", None))

        for t, rma in itertools.product(plot_types, list(self.df.get("RMA_target", pd.Series([], dtype="float64")).unique()) or [""]):
            y, yerr = t
            # TODO make result table consistent regarding to RMA_target
            df = self.df.loc[self.df["RMA_target"] ==
                             rma] if "RMA_target" in self.df.columns else self.df
            df = df.sort_values(['tasks', 'compile:measurement'])
            plt.clf()
            ax = sns.barplot(
                x="tasks", y=y, data=df, hue="compile:measurement", palette=palette, edgecolor="black")
            ax.set_xlabel("# Application Processes", fontsize=12)
            ax.set_ylabel(ylabel_map[y], fontsize=12)
            plt.xticks(fontsize=12)
            plt.yticks(fontsize=12)
            ax.set_ymargin(0.1)

            # plt.suptitle(f"LULESH (RMA Port) {ylabel_map[y]} on MUST-RMA", fontsize=12)
            ax.set_title(
                f"{benchmark_name_map[self.benchmark_name]} {ylabel_map[y]} on MUST-RMA", pad=20)
            # ax.set_title(f"CLAIX-18 MPI (Weak Scaling, Intel MPI 2022a, Clang 15)", pad=20)

            ax.title.set_size(16)
            # leg = ax.legend(title="Must branch : Compile Mode : Measurement Mode", loc="lower center", bbox_to_anchor=(0.5, -0.45), ncol=3)
            leg = ax.legend(title="Legend", loc="lower center",
                            bbox_to_anchor=(0.5, -0.45), ncol=2)
            leg._legend_box.align = 'left'
            leg.get_title().set_fontsize(11)
            # Show values
            for i in ax.containers:
                texts = ax.bar_label(i, fmt='%.3g', rotation=90, fontsize=10)
                for text in texts:
                    text.set(y=5, zorder=2000)
            self.__errorbars(df, ax, yerr)
            plt.tight_layout()
            plt.savefig(f"{path}/{y}_{rma}.png", )
            plt.savefig(f"{path}/{y}_{rma}.pdf", )

        # Move result.csv
        shutil.copy(self.result_path, f"{path}/result.dat")

        # Create text file to store path
        with open(f'{path}/metadata.txt', 'w') as f:
            f.write(f"Path: {self.run_dir}\n")
            f.write(f"id: {self.id}\n")
            f.write(f"target: {self.target}\n")
            f.write(f"yerr: {self.yerr}\n")

        print(f"\nThe plots are saved under '{path}'")

    def __errorbars(self, df, ax, yerr) -> None:
        if yerr not in df.columns:
            return
        x_coords = [p.get_x() + 0.5 * p.get_width() for p in ax.patches]
        y_coords = [p.get_height() for p in ax.patches]
        ax.errorbar(x=x_coords, y=y_coords,
                    yerr=df[yerr], fmt="none", capsize=3, c="black")

    def plot(self) -> None:
        # self.check_and_wait()
        self.read_csv()
        self.create_plots()


if __name__ == "__main__":
    # Initialize parser
    parser = argparse.ArgumentParser()
    parser.add_argument(
        'benchmark_name', help="Name of the benchmark, e.g. 'lulesh, 'miniMD'")
    parser.add_argument('-i', '--id', default=None, help="Id of the Jube run")
    parser.add_argument('-t', '--target', default="time_avg", help="Name of the column that should be evaluated. \
                        The column that is used for evaluation is the first column that has the provided value as a prefix, \
                        thus if the column also has a unit in its name the unit does not need to be provided. \
                        E.g. if the column name is 'time_avg [s]' then providing 'time_avg' is sufficient")
    parser.add_argument('-e', '--err', default="time_std",
                        help="Column that contains the error values")
    parser.add_argument(
        '-p', '--path', help="Path to the result file, e.g. result_csv.dat. The file has to be in csv format.")
    args = parser.parse_args()

    p = Plot(args.benchmark_name, id=args.id,
             target=args.target, path=args.path, yerr=args.err)
    p.plot()
