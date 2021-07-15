# Tailcuff analysis

## Installation
1. Install the latest version of R (https://cran.rstudio.com/bin/macosx/)
2. Install the latest version of R-studio (https://download1.rstudio.org/desktop/macos/RStudio-1.4.1717.dmg)
3. Install homebrew and git using terminal (my favorite is iTerm2 - https://iterm2.com/). Paste the following into your terminal: 
```
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew install git
```
4. Clone this repository into an accessible folder (e.g. ~/Desktop) again using terminal:
```
cd ~/Desktop # or your favorite directory
git clone https://github.com/nicholas-camarda/tailcuff.git
cd tailcuff
```

## Usage
1. Create `data` dir: create a directory called `data` inside this newly cloned directory using terminal:
```
mkdir data
```
2. Create `project_dir`: create another directory UNDER the `data` directory e.g. 'data/sorafenib-pilot3' using terminal:
```
mkdir data/<project_name>
```
3. Copy all of the .csv files from your tailcuff analysis to this `data` folder (using terminal or by hand with Finder):
4. Edit the `scripts/exec.R` file and change the variables accordingly inside Rstudio.


5. First, run all code up until 'RUN-2', including `process_data_fn(project_dir = my_project_dir)`. If all goes well, you will receive a file in `output/sor-pilot3/cleaned-data` called `my_data.csv`. Open this file with Excel, and save it as an `.xslx` file. NOTE the `.xlsx` extension! DO NOT change the name otherwise!

From this point you have two options:
A. Run the `run_low_overhead(my_project_dir)`
B. Proceed to step 6.

6. Next, edit the first sheet of this `.xlsx` file with the correct Phase information (training, baseline, or treatment). The date will be automatically extracted from the file name. If the date doesn't look correct, modify it.
7. Then, open a new sheet in this same document. Copy the metadata for your project into this sheet. See `examples/` for inspiration. The column names in this metadata sheet MUST NOT CHANGE. The `Specimen Name`, `Body weight (g)`, `Date` (of the weight measurement, to the right of weight), `Status`, `Date of death`, and `Machine ID` are critical columns that must be present and cannot be modified. All other columns are auxilliary. 
8. Finally, run the next two lines of code *after* 'RUN-2', mainly the functions:
```
res_lst <- run_main(my_project_dir)
run_plots_and_analysis(res_lst, my_project_dir)
```
9. The results will be displayed under `output/<my_project_dir>/results`, in addition to various additional information (e.g. `metadata`)





