# Org Exporter to Confluence

Confluence has its own XHTML-based format to store page contents.
This org exporter aims to export org files to the best of its abilities
to this format.

# Scoping

## Documentation

We will use the documentation of confluence 9.2 from this page
https://confluence.atlassian.com/doc/confluence-storage-format-790796544.html.
Since confluence is propertary software, the format can change and this exporter might break.

## Goals

- Export org file to .xml confluence storage abiding format,
- Upload attachments and update page using confluence page rest api,

## Testing

This repo is impossible to test because I do not have confluence locally to test it.
