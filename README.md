
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iNZight Addons

Below is a list of available addons for iNZight.

| Name          | Version    | Description                                                                                                                                                                                                                                                                                                                                                               | Author                      |
|:--------------|:-----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:----------------------------|
| Demo Module   | 0.1.0      | A demonstration of an iNZight add-on module.                                                                                                                                                                                                                                                                                                                              | Tom Elliott                 |
| Multivariate  | 1.0.1.9000 | A module used for multivariate graphics and analysis.                                                                                                                                                                                                                                                                                                                     | Daniel Barnett, Tom Elliott |
| Time Series   | 1.0.0      | Explore time series data with iNZight. The new version uses packages from the ‘tidyverts’ collection <https://tidyverts.org>. If you require access to the old version of iNZight (pre 2023), please choose the ‘legacy-version’ from the dropdown. We recommend trying the new version, however, as we do not have the resources to continue supporting the old version. | Tom Elliott                 |
| Quick Explore | 0.1.0      | Plots and summaries for quick exploration of a dataset.                                                                                                                                                                                                                                                                                                                   | Tom Elliott                 |

<!--
BRANCH: refs/heads/dev
MINVERSION: 0
-->

To install, download the file and install it from within iNZight:

- Open the **Modules** menu
- Choose **Manage …**
- Click the **Browse** button to locate the addon file
- Click **Install**

You can also **remove modules** from **Manage modules**.

## Manual installation

To install addons manually, simply download the raw R file and place it
in your modules folder:

- Windows: `My Documents\iNZightVIT\modules`
- macOS: `Documents/iNZightVIT/modules`
- Linux: probably `~/Documents/iNZightVIT/modules`

## Making your own

To make your own, check out [the
demo](https://github.com/iNZightVIT/demo-module.git).
