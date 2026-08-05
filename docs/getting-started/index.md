---
title: Getting started
description: Install pubdelays, prepare default paths, and run the local pipeline.
icon: octicons/rocket-16
---

# Getting started

Use this section when you are setting up a checkout or preparing a first pipeline run. The commands use the `pubdelays` console script declared in `pyproject.toml` and paths from `config/default.toml`.

<div class="grid cards" markdown>

-   :octicons-download-16: **Install the environment**

    Choose the reference Nix shell or the uv fallback used by collaborators.

    [Installation](installation.md)

-   :octicons-play-16: **Run the pipeline**

    Create directories, check raw data, run stages, and inspect outputs.

    [Quickstart](quickstart.md)

-   :octicons-gear-16: **Adjust defaults**

    Understand the TOML keys that control paths, formats, and SLURM resources.

    [Configuration](configuration.md)

</div>

!!! warning "Raw data is not bundled"
    `init-dirs` creates directories only. PubMed baseline/update XML files and manual metadata such as the Scopus Source List and Norwegian Publication Indicator snapshots must be placed in the configured raw-data paths before a full run.
