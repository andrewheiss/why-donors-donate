# Why Donors Donate

[Suparna Chaudhry](http://www.suparnachaudhry.com/) • Christopher Newport University  
[Marc Dotson](https://marriottschool.byu.edu/directory/details?id=50683) • Brigham Young University  
[Andrew Heiss](https://www.andrewheiss.com) • Brigham Young University

---

## Abstract

Abstract goes here


## Project layout

There are several subdirectories with specific purposes:

**Tracked with git**

- `/bib`: The BibTeX file lives here.
- `/data`: Data goes here. ***Temporarily* not tracked with git.**
- `/html`: Various CSS and HTML files used for building the website.
- `/lib`: Collections of functions and variables meant to be used throughout the project (like a ggplot theme, modeling parameters, etc.). 
- `/manuscript`: The actual writing goes here. A separate Makefile here generates the output as HTML, PDF (through XeTeX), and Word. For now, that Makefile depends on a bunch of helper scripts that only live on Andrew Heiss's computer, so he's the only one that can build the actual paper. Eventually he'll move those scripts here so anyone can build it.

**Not tracked with git**

These folders are ignored with `.gitignore` and instead live in a shared Dropbox folder.

- `/_site`: This is where the generated website lives. Don't mess with this—it gets deleted and rewritten all the time.
- `/admin`: For administrative tasks, like IRB, pre-registration, and other general files.
- `/output`: All exported tables and figures go here.
- `/readings`: PDFs of relevant articles go here.
- `/sandbox`: Miscellaneous experimental stuff goes here. Use this as your playground when developing new scripts or analysis code.
- `/submissions`: Manuscripts and figures prepared for conference and journal submission go here.


## Project workflow

This project is designed to work with [RStudio](https://www.rstudio.com/), and it is built as an [R Markdown website](https://bookdown.org/yihui/rmarkdown/rmarkdown-site.html) which gets [uploaded to the internet](https://stats.andrewheiss.com/why-donors-donate/) via a Makefile. Because of SSH keys and server credentials, only Andrew Heiss can upload the compiled site.

To build the site, click on the "Build Website" button in the Build tab in RStudio. All `.Rmd` files in the root of the project will be knit individually in isolated R sessions. `.Rmd` files not in the project root are not knit when building the site.

We follow a few style and workflow guidelines:

- Write code in R Markdown files. 
- Try to follow the [tidyverse style guide](https://style.tidyverse.org/)
- Use `here::here()` to specify file paths. This makes it easier to move work from the `sandbox` folder to an actual R Markdown file. Here are some examples of using it in scripts:

    ```r
    library(here)
    
    # Load graphics-related functions
    source(here("lib", "graphic-functions.R"))
    
    # Save a plot
    ggsave(plot_name, filename = here("output", "figures", "figure1.pdf"), 
           width = 6, height = 4)
    
    # Save a table
    some_data_frame %>% 
      pandoc.table.return(caption = "Some table {#tbl:table-id}") %>% 
      cat(file = here("output", "tables", "tbl-some-table.md"))
    ```


## Collaboration

Because not all collaborators use git, we use Dropbox as the main method for sharing code and data. Contact [Andrew Heiss](mailto:andrew@andrewheiss.com) to get access to the shared Dropbox folder.

At the same time, though, having actual version control with Git is nice. To make Git and Dropbox work correctly together, [we follow this system](http://www.math.cmu.edu/~gautam/sj/blog/20160406-dropbox-git.html). Here's how it works:

- The shared Dropbox folder contains everything for the project—the current git repository + all untracked files.
- **If you *are not* a git user**, make edits in the Dropbox folder and let Andrew know what you've done. He'll commit changes for you.
- **If you *are* a git user**, fork this repository to somewhere on your computer that's *not* in Dropbox. You'll work on your own local copy, pull any upstream changes, make edits, create branches, submit pull requests, and do all regular git things. Do not make any changes to the Dropbox folder itself, unless you're adding files to the folders that aren't tracked by git (e.g. adding a new paper to `/readings`).
- When pull requests are accepted or other commits are made, Andrew will pull those changes into the shared Dropbox folder.

tl;dr version:

- If you don't use git, edit files in the shared Dropbox folder and let Andrew know what you've done.
- If you do use git, fork this repository, edit files wherever you want on your computer (not in Dropbox, though), and make pull requests to the [main repository](https://github.com/andrewheiss/why-donors-donate). Only do stuff with the shared Dropbox folder if you're changing things that aren't tracked with git.
