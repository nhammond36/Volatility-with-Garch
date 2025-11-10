# ===== IMPORTS =====
import os
import sys
import subprocess
import datetime
import pandas as pd
import matplotlib.pyplot as plt
from arch import arch_model

# ===== HELPER FUNCTION =====
def open_folder(path):
    """Open a folder in the system's file explorer."""
    if sys.platform.startswith("darwin"):   # macOS
        subprocess.Popen(["open", path])
    elif os.name == "nt":                   # Windows
        os.startfile(path)
    elif os.name == "posix":                # Linux
        subprocess.Popen(["xdg-open", path])

# ===== 1. Load data =====
data_path = r"C:\Users\naham\OneDrive\Documents\GARCHfolder\spread.csv"
spread = pd.read_csv(data_path, index_col=0, parse_dates=True)
print("✅ Data loaded:", spread.shape)
print("Columns:", spread.columns.tolist())

# Use first 6 columns as returns
returns = spread.iloc[:, :6].dropna().astype(float)
print("✅ Returns head:")
print(returns.head())
print("✅ Returns dtypes:")
print(returns.dtypes)

# ===== 2. Set dates =====
sdate = spread.index  # already datetime

# ===== 3. Create run folders =====
timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
run_folder = f"EGARCH_Run_{timestamp}"
os.makedirs(run_folder, exist_ok=True)

plots_folder = os.path.join(run_folder, "egarch_plots")
os.makedirs(plots_folder, exist_ok=True)

print("📂 Run folder:", os.path.abspath(run_folder))
print("📂 Plots folder:", os.path.abspath(plots_folder))

# ===== 4. Fit EGARCH models =====
egarch_results = []
rate_columns = returns.columns.tolist()

for col in rate_columns:
    print(f"Fitting EGARCH(1,1) for {col}...")
    series = returns[col].dropna()

    try:
        model = arch_model(series, vol='EGARCH', p=1, q=1, mean='Zero', dist='normal')
        res = model.fit(disp='off')

        # Save diagnostic plot
        plt.figure(figsize=(10, 5))
        plt.plot(series, label=f"{col} Returns")
        plt.title(f"{col} Returns Series")
        plt.legend()
        plot_path = os.path.join(plots_folder, f"{col}_plot.png")
        plt.savefig(plot_path, dpi=300, bbox_inches="tight")
        plt.close()
        print(f"✅ Plot saved: {plot_path}")

        # Collect results (without std_err if it causes errors)
        df = pd.DataFrame({
            "series": col,
            "parameter": res.params.index,
            "estimate": res.params.values,
            "std_error": res.std_err.values,
            "t_value": res.tvalues.values,
            "p_value": res.pvalues.values
        })
        egarch_results.append(df)

    except Exception as e:
        print(f"⚠️ Failed to fit {col}: {e}")

# ===== 5. Save results CSV =====
# ===== 5. Save results CSV =====
if egarch_results:
    results_df = pd.concat(egarch_results, ignore_index=True)
    csv_path = os.path.join(run_folder, "egarch_results.csv")
    results_df.to_csv(csv_path, index=False)
    print(f"✅ Results CSV saved: {os.path.abspath(csv_path)}")
    print(f"✅ Plots saved in: {os.path.abspath(plots_folder)}")

    # Auto-open run folder
    open_folder(run_folder)
else:
    print("⚠️ No EGARCH results produced.")

# Instructions to run
# cd "C:\Users\naham\OneDrive\Documents\GARCHfolder"
# python gsme_14.py 
if egarch_results:
    results_df = pd.DataFrame(egarch_results)

    # === Save CSV ===
    csv_path = os.path.join(run_folder, "egarch_results.csv")
    results_df.to_csv(csv_path, index=False)
    print(f"✅ EGARCH results saved: {csv_path}")

    # === Save LaTeX ===
    latex_path = os.path.join(run_folder, "egarch_results.tex")
    with open(latex_path, "w") as f:
        f.write(results_df.to_latex(index=False))
    print(f"✅ EGARCH LaTeX saved: {latex_path}")

"""
    # === Optional: print preview ===
    print("\nPreview of results:")
    print(results_df.head())

else:
    print("⚠️ No results found in egarch_results — CSV/LaTeX not created.")
    print("Message:", resultf.message if 'resultf' in locals() else "No optimization run.")

##
if egarch_results:
    results_df = pd.concat(egarch_results, ignore_index=True)
    csv_path = os.path.join(run_folder, "egarch_results.csv")
    results_df.to_csv(csv_path, index=False)
    print(f"✅ Results CSV saved: {os.path.abspath(csv_path)}")
    print(f"✅ Plots saved in: {os.path.abspath(plots_folder)}")
"""
       


latex_path = os.path.join(run_folder, "egarch_results.tex")
with open(latex_path, "w") as f:
    f.write(results_df.to_latex(index=False))
print(f"✅ EGARCH LaTeX saved: {latex_path}")

# Instructions to run
# cd "C:\Users\naham\OneDrive\Documents\GARCHfolder"
# python gsme_14.py