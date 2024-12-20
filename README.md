# Org Exporter to Confluence

Features:
- org export backend to confluence storage format
- functions for updating pages and attachments to confluence using REST api

## What's Confluence?

Confluence is a product from Atlassian and allows users to make
their own wiki pages and collaborate and comment on the pages.

The main issue with confluence is that you must use 
the a what you see is what you get (WYSIWYG) editor to edit pages.
It allows you to make complicated wiki pages, but does not support
custom css like org export html. Although, most of the time, wiki pages
are plain html and might as well be plain markdown.

## Installation

After adding the file to load path,
either customize `org-export-backends` or load directly with
```elisp
(require 'ox-confluence-html)
```
On successful load, the backend adds new keys in the export dispatcher.

## Dependencies

The backend is a modified version of `ox-html` depends on it.
The exporter relies on the external program `curl` to export files and interact with confluence rest api.
We could use `request.el` but I want to minimize dependencies in case of corporate restrictions.

# Scoping

## How it works

Confluence storage format is similar to body only xhtml.
However, it does not recognize css and you cannot add css.
The exporter has to strip all attributes and give confluence plain html.
Some blocks also correspond to structured macros in confluence.

There is a pre-processing function that searches for the following pattern:
``` org
#+include "/path/to/file.html" export html
```
It then does the following:
- Upload the file as an attachment to the confluence page with rest api
- Replace the include statement with an `export html` block embeding the confluence attachment url
inside an `<iframe>` tag
- `export html` is then transcoded to a structured `html` macro.

## List of Transcoders or Supported Blocks

Implemented, barebones:
- bold
- italic
- underline
- strikethrough
- quote block
- example block
- headline
- section
- drawer
- verbatim
- paragraph
- plain-list
- item (no support for task lists)
- table
- table-row
- table-cell (no support for colored tables yet)
- template
- src-block
- line-break
- export-block (html only)

To be implemented:
- [ ] table of contents
- [ ] link and anchors
- [ ] task lists

## Confluence Storage Format Documentation

We will use the documentation of confluence 9.2 from this page
https://confluence.atlassian.com/doc/confluence-storage-format-790796544.html.
Since confluence is propertary software, the format can change and this exporter might break.

# Extra

## Why a new ox-confluence?

Confluence only supports its own wiki markdown in the legacy editor
which means you can still use the old
[package](https://github.com/aspiers/orgmode/blob/master/contrib/lisp/ox-confluence.el)
in org contrib is now deprecated. We must now convert to the storage format.

The alternative to converting to confluence markdown is to convert to the 
confluence html format, also called confluence storage format,
which is the format confluence uses to locally store pages.
This means you have access to all features while exporting.
This org exporter aims to export org files to the best of its abilities
to this format.

## Why even bother?

Since confluence is usually for corporate environments,
using of emacs or even downloading this package might not be worth the effort.
You could be locked into a specific work setup or even blocked from using emacs.
However, if the following applies to you, this package could make sense to use:
- you can and want to use emacs org mode,
- you use confluence to store documents,
- and you do not have to lock yourself into other editors or workflow environments.

# Acknowledgements

I would like to thank [nbconflux](https://github.com/vericast/nbconflux/tree/master) for inspiration.
I would also thank org-mode and emacs contributors for their generous contributions.
