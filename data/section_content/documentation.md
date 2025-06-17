
<h3>Contents</h3>

- [Overview](#overview)
- [Analysis Types and Visualizations](#analysis_types_visualizations)<span style="font-size:90%">
  - [Response Kinetic Analysis](#response_kinetic_analysis)
  - [Autophagy Competence Analysis](#autophagy_competence_analysis)</span>
- [Data Visualization Options](#data_viz_options)
- [Examples and Use Cases](#examples_use_cases)
- [Technical details](#technical_details)
- [Known Issues](#known_issues)

<h3 id="overview">Overview</h3>

AutoDRY is an interactive platform for exploring autophagy phenotypes in yeast gene deletion mutants. The portal supports two main types of phenotype analysis, each offering two complementary visualization modes: one for individual mutant response curves and one for comparing multiple mutants via dot plots. AutoDRY is built upon extensive high-content imaging data combined with
deep learning analyses to characterize autophagy responses in nearly 6,000 yeast gene mutants. Two distinct analytical approaches are provided:

-   <b>Response Kinetic Analysis</b>: Focuses on the time-resolved activation and inactivation of autophagy.

-   <b>Autophagy Competence Analysis</b>: Focuses on the dynamics of autophagy execution via quantitative scoring of autophagosome formation and clearance.

Both approaches offer visualizations for individual mutants and for comparing groups of mutants to benchmark control sets.

<h3 id="analysis_types_visualizations">Analysis Types and Visualizations</h3>

<h4 id="response_kinetic_analysis">Response Kinetic Analysis</h4>

This analysis dissects the dynamic response of yeast mutants during nutrient shifts. Two visualization modes are available:

##### *a) Individual Response Curve Plot*

-   **What It Displays**: For each mutant, view the activation (during nitrogen starvation, –N) and inactivation (after nitrogen replenishment, +N) response curves.

-   **Curve Fitting**: Each curve is modeled with a double-sigmoidal fit to capture:

    -   **Asymptotic Limits**: Autophagy levels at the start, maximum activation, and at the final time point.

    -   **Response Times**: Metrics such as lag time (t\_lag), half-maximal response (t50), and final response time (t\_final).

    -   **Tangent Slopes**: The fitted slopes during the activation and inactivation phases.

    -   **Differential Integral**: The area difference relative to the wild-type (WT) response.

-   **Usage**: Select an individual mutant to inspect its complete kinetic profile and compare specific points of interest with the WT.

<br>

##### *b) Dot Plot Comparison of Kinetic Parameters*

-   **What It Displays**: Explore kinetic parameters (e.g., t\_lag, t50, slopes, differential integrals) across multiple mutants in dot plots.

-   **Comparison Sets**: Data is compared against the genome-wide distribution as well as reference sets (e.g., ATG core and Fusion core genes).

-   **Visualization Options:**

    -   **Control Contours**: Overlay contours for KO and DAmP mutant
        distributions.

    -   **Perturbed Values**: Option to display normalized (perturbed)
        values derived from plate controls.

-   **Usage:** Use filtering and selection options to focus on subsets of mutants and assess how their kinetics deviate from controls.

<br>
<hr>

<h4 id="autophagy_competence_analysis">Autophagy Competence Analysis</h4>

This analysis quantifies the ability of a mutant to execute autophagy by scoring autophagosome formation and clearance. Two visualization modes are available:

##### *a) Individual Autophagy Execution Dynamics*

-   **What It Displays**: For each mutant, view time-resolved autophagy execution dynamics using log Bayes factors (BFs) calculated at each time point during –N and +N conditions.

-   **Comparison with WT**: Each mutant’s dynamics are directly compared to its respective WT response, highlighting deviations in autophagosome formation and clearance.

-   **Usage**: Select an individual mutant to review its temporal dynamics and identify specific stages where the response diverges from WT.

##### *b) Dot Plot Comparison of Autophagy Competence*

-   **What It Displays**: Explore the average autophagy execution competence (average log BFs over the entire time course) for multiple mutants in a dot plot format.

-   **Comparison Sets**: Data is compared against genome-wide distributions and as reference sets (ATG core and Fusion core genes). Additionally, densities for atg1Δ, vam6Δ, and WT/ORF controls are shown.

-   **Visualization Options:**

    -   **Control Contours**: Option to overlay the contours of KO and DAmP mutant distributions.

    -   **Library Correction**: Option to harmonize data between the KO and DAmP mutant collections.

-   **Usage**: Filter and select groups of mutants to compare their overall autophagy execution competence and observe shifts relative to controls.

<br>
<hr>

<h3 id="data_viz_options">Data Visualization Options</h3>

Across both analysis types, users can adjust several display settings:

-   *Contour Overlays*: Toggle contours to visualize the distribution of control groups.
-   *Normalization Settings*: Display raw or perturbed (normalized) data values.
-   *Library Correction*: For autophagy competence, apply correction to harmonize data between different mutant libraries.
-   *Interactive Filtering*: Use built-in filters to select mutants based on gene sets or specific phenotype classes.

<br>
<hr>

<h3 id="examples_use_cases">Examples and Use Cases</h3>

#### Example 1: Detailed Kinetic Profile of a Single Mutant

1.  Use the menu bar to navigate to the Response Kinetic Analysis module ( *Data Explorer* **&gt;&gt;** *Response kinetics - single*).
2.  Select a single/individual mutant to view its activation/inactivation curves with points of interest.
3.  Examine key parameters (asymptotic limits, t\_lag, t50, t\_final, tangent slopes) and compare them to the WT curve.

#### Example 2: Comparing Multiple Mutants for Autophagy Competence

1.  Use the menu bar to navigate to the Autophagy Competence Analysis module ( *Data Explorer* **&gt;&gt;** *Autophagy competence - global*).
2.  Use the dot plot to compare average log BF scores across multiple mutants.
3.  Overlay control contours and apply library correction to assess how selected mutants deviate from genome-wide distributions and control sets.

<br>
<hr>

<h3 id="technical_details">Technical Details</h3>

 * **Data and Methods**:
   The portal integrates high-content imaging data with deep learning and statistical scoring methods (e.g., log Bayes factors for autophagy competence).

 * **Modeling Approaches**:
   Double-Sigmoidal Fitting: Applied to autophagy activation (%) response curves to derive kinetic parameters.

 * **Bayesian Analysis**: Used to quantify the autophagy execution competence based on cell-level latent space evidence of autophagosome formation and clearance.

 * **Control Data**: WT, atg1Δ, and vam6Δ mutants serve as baseline controls to benchmark mutant phenotypes.

<br>
<hr>

<h3 id="known_issues">Known Issues</h3>

 * **Gene/ORF mutants with extreme values**:
   We have observed that a few gene deletion mutants exhibit extreme values in their autophagy response kinetics or competence scores. Some of these outliers are unfortunately not shown when trying to highlight them in the global views (dot plots). This is due to the way the data is normalized and visualized, which can obscure these extreme values. We are working on improving this aspect of the visualization to ensure that all relevant data points are clearly represented. 

<br>
<hr>

Authors: _Aram N. Andersen and Sigve Nakken_