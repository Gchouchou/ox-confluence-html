# Org Exporter to Confluence

## What's Confluence?

Confluence is a product from Atlassian and allows users to make
their own wiki pages and collaborate and comment on the pages.

The main issue with confluence is that you must use 
the a what you see is what you get (WYSIWYG) editor to edit pages.
It allows you to make complicated wiki pages, but does not support
custom css like org export html. Although, most of the time, wiki pages
are plain html and might as well be plain markdown.

## Why a new ox-confluence?

Confluence still supports inputing its own wiki markdown which means you can still
use the old [package](https://github.com/aspiers/orgmode/blob/master/contrib/lisp/ox-confluence.el)
in org contrib. It is no longer maintained.

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

# Dependencies

The backend is a modified version of `ox-html` depends on it.
The exporter relies on the external program `curl` to export files and interact with confluence rest api.
We could use `request.el` but I want to minimize dependencies in case of corporate restrictions.

# Scoping

## Documentation

We will use the documentation of confluence 9.2 from this page
https://confluence.atlassian.com/doc/confluence-storage-format-790796544.html.
Since confluence is propertary software, the format can change and this exporter might break.

## Goals

- Export org file to .html confluence storage abiding format,
- Upload attachments and update page using confluence page rest api,

## Testing

This repo is impossible to test because I do not have confluence locally to test it.

# Acknowledgements

I would like to thank [nbconflux](https://github.com/vericast/nbconflux/tree/master) for inspiration.
I would also thank org-mode and emacs contributors for their generous contributions.
