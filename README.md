# quarto.audiobook

## Overview

`quarto.audiobook` is an R package that helps to generate quarto books into audio files by using TTS APIs (including [OpenAI](https://platform.openai.com/docs/guides/text-to-speech) and [VoiceRSS](https://www.voicerss.org/)). This package hopes to make Quarto-generated products to be more accessible for vision impaired individuals as well as people who prefer to ingest content via audio.

## Installation

``` markdown
devtools::install_github("qngyn/quarto.audiobook")
```

## Usage

-   First, load the package

```{r}
library(quarto.audiobook)
```

Besides the main package, there are other components in order to successfully attach the audio files into the quarto file.

-   In `_quarto.yml` file, ensure to put `keep-md: true` in the format For example:

``` markdown
format:
  html:
    theme: cosmo
    keep-md: true
  pdf:
    documentclass: scrreprt
```

- In addition to this, please download [Node.js](https://nodejs.org/en/download/package-manager) in the terminal. To check if Node.js is installed correctly, you can run the following commands in the terminal
```markdown
node -v
npm -v
```

- Then, you need to initialize the Node.js in the current directory by using the following command:
```markdown
npm init -y
```

- And finally, in the `_quarto.yaml` file, make sure to put these commands under the project:
```markdown
post-render: 
    - bash -c "cp -a audio _book && mv _book/audio/index/* _book/audio/ && rmdir _book/audio/index"
    - node post_render.js
```
For example:
```markdown
project:
  type: book
  post-render: 
    - bash -c "cp -a audio _book && mv _book/audio/index/* _book/audio/ && rmdir _book/audio/index"
    - node post_render.js
  preview:
    port: 4200
    browser: true
  
book:
  title: "example_book"
  author: "Norah Jones"
  date: "8/1/2024"
  chapters:
    - index.qmd
    - intro.qmd
    - summary.qmd
    - references.qmd
    

bibliography: references.bib


format:
  html:
    theme: cosmo
    keep-md: true
  pdf:
    documentclass: scrreprt

editor: visual
```
