;;; ox-confluence-html.el --- Confluence Wiki Back-End for Org Export Engine

;; Copyright (C) 2024 Justin Yu <jusytinyu@gmail.com>

;; Author: Justin Yu
;; Keywords: tools, text, wp
;; Homepage: https://github.com/Gchouchou/ox-confluence-html
;; Created 12 Dec 2024
;; Version: 0.1

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;; This file is not part of GNU Emacs.

;;; Commentary:
;; ox-confluence-html tries to convert org file to confluence storage format.
;; Since confluence storage is quite limited, this export backend will
;; ignore many parameters. Many transcoders are taken from ox-slimhtml and
;; ox-html.

;; Two variables have to be set to access Confluence REST api:
;; `ox-confluence-html-host', the URI of the confluence server,
;; and `ox-confluence-html-token', the file containing the confluence
;; access token. See https://confluence.atlassian.com/enterprise/using-personal-access-tokens-1026032365.html
;; for instructions on how to create a personal access token.

;;; Requirements:
;;; Package-Requires: ((emacs "27.1"))

;;; Code:

(require 'ox-html)
(require 'cl-lib)
(require 'json)
(require 'url-parse)

(defcustom ox-confluence-html-host nil
  "The default confluence host name used for exporting.
It usually has the form of confluence.somewhere.com or somewhere.atlassian.net
We only want the host name so do not prepend with https or postpend with path."
  :safe 'stringp
  :type 'string
  :group 'confluence)

(defcustom ox-confluence-html-token nil
  "The file that contains the API token to access confluence."
  :safe 'file
  :type 'file
  :group 'confluence)

;;; Confluence Curl functions

(defun ox-confluence-html-get-page-id (title space &optional host)
  "Queries the pageid using TITLE and SPACE.
Uses HOST then ox-confluence-html-host or fails if both are nil.
Uses curl as a backend."
  (interactive "sPage title: \nsPage space: ")
  (let* ((host (or host ox-confluence-html-host (error "No host found")))
         (token (and ox-confluence-html-token
                     (file-exists-p ox-confluence-html-token)
                     (with-temp-buffer
                       (insert-file-contents ox-confluence-html-token)
                       (org-trim (buffer-string)))))
         (header (when token (format "Authorization: Bearer %s" token)))
         (url (format "https://%s/rest/api/content?title=%s&spaceKey=%s" host title space))
         (args (delete nil (list "--get" "-s" (when header "-H") header url))))
    (with-temp-buffer
      (if (zerop (apply 'call-process "curl" nil (current-buffer) nil args))
          (if-let* ((resp (progn (goto-char (point-min))
                                 (json-parse-buffer)))
                    (results (gethash "results" resp))
                    (result (unless (zerop (length results)) (aref results 0)))
                    (id (gethash "id" result)))
              (progn (message "Page found id=%s title=%s, space=%s" id title space)
                     id)
            (error "Could not locate %s in %s. Ensure that page exists.\n%s" title space (buffer-string)))
        (error "Error with curl\n%s" (buffer-string))))))

(defun ox-confluence-html-get-page-id-from-link (link)
  "Parse human readable LINK and retuns the page id.
It will also set ox-confluence-html-host for the rest of the session."
  (interactive "sConfluence Page Link: ")
  (let* ((parsed-uri (url-generic-parse-url link))
         (host (url-host parsed-uri))
         (filename (url-filename parsed-uri))
         (segs (split-string filename "/"))
         (query (car (last segs)))
         (query (when-let* ((match (string-match-p "\\?" query))) (substring query (+ match 1)))))
    ;; set host for the rest of the session
    (setq ox-confluence-html-host host)
    (cond
     ;; URL contains the pageId as a query arg
     ;; https://somewhere.com/pages/viewpage.action?pageId=123456
     ((and query (string-match "pageId=\\([0-9]+\\)" query)) (match-string 1 query))
     ;; URL on Confluence Cloud contains the page ID in the path under a space
     ;; https://somewhere.atlassian.net/wiki/spaces/ASPACE/pages/123456/Page+Title
     ((and (< 5 (length segs)) (string= (nth 4 segs) "pages")) (nth 5 segs))
     ;; URL on Confluence Server contains a space and page title requiring lookup
     ;; https://confluence.somewhere.com/display/ASPACE/Page+Title
     ((and (< 3 (length segs)) (string= (nth 1 segs) "display"))
      (ox-confluence-html-get-page-id (nth 3 segs) (nth 2 segs) host))
     (t (error "Unknown URL format: %s" link)))))

(defun ox-confluence-html-update-attachment (pageId attachment &optional override comment)
  "Upload ATTACHMENT to confluence PAGEID and return attachmentid.
If OVERRIDE is non-nil, overrides attachment if already present.
Adds COMMENT to upload."
  (interactive "sPageId: \nfFile: \nP")
  (if (not (file-exists-p attachment)) (error "Attachment %s does not exist" attachment))
  (let* ((basename (file-name-nondirectory attachment))
         (host (or ox-confluence-html-host (error "No host found")))
         (token (and ox-confluence-html-token
                     (file-exists-p ox-confluence-html-token)
                     (with-temp-buffer
                       (insert-file-contents ox-confluence-html-token)
                       (org-trim (buffer-string)))))
         (header (when token (format "Authorization: Bearer %s" token)))
         (uri (format "https://%s/rest/api/content/%s/child/attachment?filename=%s" host pageId basename))
         (args (delete nil (list "--get" "-s" (when header "-H") header uri)))
         (attachmentId (with-temp-buffer
                         (if (zerop (apply 'call-process "curl" nil (current-buffer) nil args))
                             (when-let* ((resp (progn (goto-char (point-min)) (json-parse-buffer)))
                                         (results (gethash "results" resp))
                                         (result (unless (zerop (length results)) (aref results 0)))
                                         (id (gethash "id" result)))
                               id)
                           (error "Error with curl\n%s" (buffer-string))))))
    (cond
     ;; adding new attachment
     ((not attachmentId)
      (with-temp-buffer
        (when (zerop (apply 'call-process
                            "curl" nil (current-buffer) nil
                            (delete nil (list "-sSX"
                                              "POST"
                                              (when header "-H") header
                                              "-H" "X-Atlassian-Token: no-check"
                                              "-F" (format "file=@%s" attachment)
                                              (when comment "-F") (when comment (format "comment=%s" comment))
                                              (format "https://%s/rest/api/content/%s/child/attachment" host pageId)))))
          ;; get the attachment id of the newly updated file
          (message "Successfully updated %s, getting attachment id from result." basename)
          (when-let* ((resp (progn (goto-char (point-min)) (json-parse-buffer)))
                      (results (gethash "results" resp))
                      (result (unless (zerop (length results)) (aref results 0)))
                      (id (gethash "id" result)))
            id))))
     ;; override attachment
     (override
      (with-temp-buffer
        (message "Override is set to true, overriding attachment %s, id=%s" basename attachmentId)
        (apply 'call-process
               "curl" nil (current-buffer) nil
               (delete nil (list "-sSX"
                                 "POST"
                                 (when header "-H") header
                                 "-H" "X-Atlassian-Token: no-check"
                                 "-F" (format "file=@%s" attachment)
                                 (when comment "-F") (when comment (format "comment=%s" comment))
                                 (format "https://%s/rest/api/content/%s/child/attachment/%s/data" host pageId attachmentId))))))
     ;; not overriding existing
     (t (progn
          (message "Attachment %s already exists, not overriding" basename)
          attachmentId)))))

(defun ox-confluence-html-update-content (pageId file &optional append)
  "Overwrite contents of confluence page PAGEID with FILE.
If APPEND is not nil, first query contents and prepend to FILE contents
before uploading."
  (interactive "sPageId: \nfFile: \nP")
  ;; Fetch version number of the page so we can increment it by 1
  (if (or (null pageId) (not (file-exists-p file)) (not ox-confluence-html-host))
      (error "Missing page id %s or file %s or host %s" pageId file ox-confluence-html-host))
  (let* ((token (and ox-confluence-html-token
                     (file-exists-p ox-confluence-html-token)
                     (with-temp-buffer
                       (insert-file-contents ox-confluence-html-token)
                       (org-trim (buffer-string)))))
         (header (when token (format "Authorization: Bearer %s" token)))
         (resp (with-temp-buffer
                 (if (zerop (apply 'call-process
                                   "curl" nil (current-buffer) nil
                                   (delete nil (list "-s"
                                                     (when header "-H") header
                                                     (format "https://%s/rest/api/content/%s?expand=body.storage,version" ox-confluence-html-host pageId)))))
                     (progn (goto-char (point-min))
                            (json-parse-buffer))
                   (error "Error with curl"))))
         (old-body (when-let* ((respb (gethash "body" resp))
                               (view (gethash "storage" respb))
                               (value (gethash "value" view)))
                     value))
         (page-ver (when-let* ((v (gethash "version" resp))
                               (number (gethash "number" v)))
                     number))
         (new-body (concat (when append (concat old-body "\n"))
                           (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))))
         (title (gethash "title" resp))
         (json `((version . ((number . ,(format "%s" (+ page-ver 1)))))
                 (title . ,title)
                 (type . "page")
                 (body . ((storage . ((representation . "storage")
                                      (value . ,(format "%s" new-body)))))))))
    (with-temp-buffer
      (if (zerop (apply 'call-process
                        "curl" nil "current-buffer" nil
                        (delete nil (list "-sSX" "PUT"
                                          (when header "-H") header
                                          "-H" "Content-Type: application/json"
                                          "--data"  (json-serialize json)
                                          (format "https://%s/rest/api/content/%s" ox-confluence-html-host pageId)))))
          (message "Updated Page Successfully")
        (error "Error with update\n%s" (buffer-string))))))

;;; Transcoders

;; formatting
;; #+BEGIN_EXAMPLE
;;   *bold*                                     # <strong>bold</strong>
;;   /italic/                                   # <em>italic</em>
;;   +strike-through+                           # <span style=\"text-decoration:line-through;\">strike-through</span>
;;   _underlined_                               # <u>underline</u>
;;   =verbatim=                                 # <code>verbatim</code>
;;   ~code~                                     # <code>verbatim</code>
;; #+END_EXAMPLE

(defun ox-confluence-html-bold (bold contents info)
  "Transcode BOLD from Org to confluence storage format.

CONTENTS is the text with bold markup.
INFO is a plist holding contextual information."
  (when contents (format "<strong>%s</strong>" contents)))

(defun ox-confluence-html-italic (italic contents info)
  "Transcode ITALIC from Org to confluence storage format.

CONTENTS is the text with italic markup.
INFO is a plist holding contextual information."
  (when contents (format "<em>%s</em>" contents)))

(defun ox-confluence-html-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to confluence storage format.

CONTENTS is the text with STRIKE-THROUGH markup.
INFO is a plist holding contextual information."
  (when contents (format "<span style=\"text-decoration:line-through;\">%s</span>" contents)))

(defun ox-confluence-html-underline (underline contents info)
  "Transcode UNDERLINE from Org to confluence storage format.

CONTENTS is the text with italic markup.
INFO is a plist holding contextual information."
  (when contents (format "<u>%s</u>" contents)))


(defun ox-confluence-html-verbatim (verbatim contents info)
  "Transcode VERBATIM string from Org to confluence storage format.

CONTENTS is nil.
INFO is a plist holding contextual information."
  (let ((contents (org-html-encode-plain-text
                   (org-element-property :value verbatim))))
    (when contents (format "<code>%s</code>" contents))))

(defun ox-confluence-html-code (code contents info)
  "Transcode CODE string from Org to confluence storage format.

CONTENTS is nil.
INFO is a plist holding contextual information."
  (let ((contents (org-html-encode-plain-text
                   (org-element-property :value code))))
    (when contents (format "<code>%s</code>" contents))))

;; plain text

(defun ox-confluence-html-plain-text (plain-text info)
  "Transcode a PLAIN-TEXT string from Org to confluence storage format.

PLAIN-TEXT is the string to transcode.
INFO is a plist holding contextual information."
  (org-html-encode-plain-text plain-text))

;; minimal template
(defun ox-confluence-html-template (contents info)
  "Wrap exported CONTENTS in a minimal template for the confluence backend.

CONTENTS is the string to be exported.
INFO is a plist containing export options."
  (concat
   (when-let* ((depth (plist-get info :with-toc)))
     ;; insert table of contents, could customize depth
     "<ac:structured-macro ac:name=\"toc\">\n</ac:structured-macro>\n\n")
   contents))

(defun ox-confluence-html-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to a simple paragraph wrapper.

CONTENTS is the paragraph's text.
INFO is a plist holding contextual information."
  (format "<p>%s</p>" contents))

(defun ox-confluence-html-section (section contents info)
  "Transcode SECTION element from Org to confluence storage format.

CONTENTS is the section.
INFO is a plist holding contextual information"
  contents)

(defun ox-confluence-html-headline (headline contents info)
  "Transcode HEADLINE from Org to confluence storage format.

CONTENTS is the section as defined under the HEADLINE.
INFO is a plist holding contextual information."
  (let* ((text (org-export-data (org-element-property :title headline) info))
         (level (org-export-get-relative-level headline info)))
    (format "<h%s>%s</h%d>\n%s" level text level (or contents ""))))

(defun ox-confluence-html-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST string from Org to confluence storage format.

CONTENTS is the contents of the list element.
INFO is a plist holding contextual information.
Currently does not support descriptive lists and filters out checkboxes."
  (when contents
    (let ((type (cl-case (org-element-property :type plain-list)
                  (ordered "ol")
                  (unordered "ul"))))
      (format "<%s>%s</%s>" type (or contents "") type))))

(defun ox-confluence-html-item (item contents info)
  "Transcode an ITEM element from Org to confluence storage format.

CONTENTS holds the contents of the item.
INFO is a plist holding contextual information."
  (format "<li>%s</li>\n" contents))

(defun ox-confluence-html-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to confluence storage format.

CONTENTS holds the contents of the block.
INFO is a plist holding contextual information."
  (format "<blockquote>\n%s</blockquote>" contents))

;;; Table stuff

(defun ox-confluence-html-table (table contents info)
  "Transcode a TABLE element from Org to confluence storage format.

CONTENTS is the contents of the table.
INFO is a plist holding contextual information."
  (format "<table>\n<tbody>\n%s</tbody>\n</table>" contents))


(defun ox-confluence-html-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to confluence storage format.

CONTENTS is the contents of the table.
INFO is a plist holding contextual information."
  (when contents (format "<tr>\n%s</tr>\n" contents)))

(defun ox-confluence-html-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to confluence storage format.

CONTENTS is the contents of the table.
INFO is a plist holding contextual information."
  (let* ((table-row (org-element-parent table-cell))
         (table (org-element-lineage table-cell 'table))
         ;; just set scope to col on first row blindly
         (attr (if (and (org-export-table-has-header-p table info)
                        (= 1 (org-export-table-row-group table-row info)))
                   " scope=\"col\""
                 "")))
    (format "<th%s>%s</th>\n" attr (or contents ""))))

(defun ox-confluence-html-example-block (example-block contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to HTML.

CONTENTS is nil.  INFO is a plist holding contextual
information."
(let ((contents (org-html-encode-plain-text
                   (org-element-property :value example-block))))
    (when contents (format "<pre>\n%s</pre>" contents))))

(defun ox-confluence-html-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.

CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lang (org-element-property :language src-block))
         (linenumbers (org-element-property :number-lines src-block))
         (name (org-element-property :name src-block))
         (contents (org-element-property :value src-block)))
    (concat
     "<ac:structured-macro ac:name=\"code\" ac:schema-version=\"1\">\n"
     (format "<ac:parameter ac:name=\"language\">%s</ac:parameter>\n" lang)
     (when linenumbers "<ac:parameter ac:name=\"linenumbers\">true</ac:parameter>\n")
     (when name (format "<ac:parameter ac:name=\"Title\">%s</ac:parameter>\n" name))
     (when contents (format "<ac:plain-text-body><![CDATA[%s]]></ac:plain-text-body>\n" (org-trim contents)))
     "</ac:structured-macro>")))

(defun ox-confluence-html-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to Confluence format.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "HTML")
    (format "<ac:structured-macro ac:name=\"html\" ac:schema-version=\"1\">\n%s</ac:structured-macro>"
           (format "<ac:plain-text-body><![CDATA[%s]]></ac:plain-text-body>\n"
		   (org-remove-indentation (org-element-property :value export-block))))))

(defun ox-confluence-html-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to Confluence format.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  contents)

(defun ox-confluence-html-keyword (keyword _contents info)
  "Transcode a KEYWORD element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "HTML") value)
     ((string= key "TOC")
      (format "<ac:structured-macro ac:name=\"toc\">\n</ac:structured-macro>")))))

;;; Pre processing functions

(defun ox-confluence-html-include-replace (backend)
  "If BACKEND is confluence, replace includes html with export html.
This before process function will upload include html files to confluence,
defined using page_id and host or with confluence_url and then replace
include html with export html with an iframe tag to the confluence attachment."
  (when (org-export-derived-backend-p backend 'confluence)
    (let* ((options (org-export-get-environment 'confluence))
           (url (plist-get options :confluence-url))
           (page_id (or (plist-get options :confluence-page-id)
                        (and url (ox-confluence-html-get-page-id-from-link url))))
           (host ox-confluence-html-host)
           (override-attachments (plist-get options :override-confluence-attachment))
           (upload-to-confluence (and (plist-get options :upload-to-confluence) host page_id)))
      (if upload-to-confluence
          (message "Page_id: %s found, host: %s found, uploading attachments to confluence" page_id host)
        (message "Page_id: %s, host: %s, not uploading to confluence" page_id host))
      (save-excursion
        ;; got to top of the page and scan for includes
        (goto-char (point-min))
        (while (re-search-forward "^#\\+include: \"\\([^\"]+\\)\" export html.*$" nil t)
          (let* ((file (match-string 1)))
            (when upload-to-confluence (ox-confluence-html-update-attachment page_id file override-attachments))
            ;; TODO confirm the confluence attachment url format
            (replace-match (format "\
#+begin_export html
<iframe src=\"https://%s/download/attachments/%s/%s\" width=\"100%%\" height=\"400px\" frameborder=\"0\"></iframe>
#+end_export"
                                   host
                                   page_id
                                   (file-name-nondirectory file)))))))))

(add-hook 'org-export-before-processing-functions #'ox-confluence-html-include-replace)

(org-export-define-backend
    'confluence
  '((bold . ox-confluence-html-bold)
    (italic . ox-confluence-html-italic)
    (strike-through . ox-confluence-html-strike-through)
    (code . ox-confluence-html-code)
    (verbatim . ox-confluence-html-verbatim)
    (underline . ox-confluence-html-underline)
    (paragraph . ox-confluence-html-paragraph)
    (section . ox-confluence-html-section)
    (headline . ox-confluence-html-headline)
    (plain-list . ox-confluence-html-plain-list)
    (item . ox-confluence-html-item)
    (quote-block . ox-confluence-html-quote-block)
    (table . ox-confluence-html-table)
    (table-row . ox-confluence-html-table-row)
    (table-cell . ox-confluence-html-table-cell)
    (example-block . ox-confluence-html-example-block)
    (template . ox-confluence-html-template)
    (src-block . ox-confluence-html-src-block)
    (link . org-html-link)
    (line-break . org-html-line-break)
    (export-block . ox-confluence-html-export-block)
    (drawer . ox-confluence-html-drawer)
    (fixed-width . ox-confluence-html-example-block)
    (keyword . ox-confluence-html-keyword)
    (subscript . org-html-subscript)
    (superscript . org-html-superscript))
  :menu-entry '(?f "Export to Confluence"
                ((?F "As Confluence buffer" ox-confluence-html-export-as-html)
                 (?f "As HTML file" ox-confluence-html-export-to-confluence)
                 (?p "As HTML file and update confluence page"
                     (lambda (a s v b)
                       (let* ((options (org-export-get-environment 'confluence))
                              (url (plist-get options :confluence-url))
                              (page_id (or (plist-get options :confluence-page-id)
                                           (and url (ox-confluence-html-get-page-id-from-link url)))))
                         (ox-confluence-html-export-to-confluence a s v b nil
                                                             (lambda (file)
                                                               (ox-confluence-html-update-content page_id file))))))
                 (?a "As HTML file and append to confluence page"
                     (lambda (a s v b)
                       (let* ((options (org-export-get-environment 'confluence))
                              (url (plist-get options :confluence-url))
                              (page_id (or (plist-get options :confluence-page-id)
                                           (and url (ox-confluence-html-get-page-id-from-link url)))))
                         (ox-confluence-html-export-to-confluence a s v b nil
                                                             (lambda (file)
                                                               (ox-confluence-html-update-content page_id file t))))))))
  :options-alist '((:confluence-page-id "PAGE_ID" nil nil)                                 ; page id to upload to
                   (:confluence-url "CONFLUENCE_URL" nil nil)                              ; or the url to upload
                   (:override-confluence-attachment "CONFLUENCE_OVERRIDE_ATTACH" nil nil)  ; override attachments on export
                   (:upload-to-confluence "UPLOAD_TO_CONFLUENCE" nil t)))                  ; override switch to not upload to confluence

;;;###autoload
(defun ox-confluence-html-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, nothing changes.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org CONFLUENCE Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'confluence "*Org CONFLUENCE Export*"
    async subtreep visible-only body-only ext-plist (lambda () (html-mode))))

;;;###autoload
(defun ox-confluence-html-export-to-confluence
    (&optional async subtreep visible-only body-only ext-plist post-process)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only return body
code, without surrounding template.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Optional argument POST-PROCESS is called with FILE as its
argument and happens asynchronously when ASYNC is non-nil.  It
has to return a file name, or nil.
Return output file's name."
    (interactive)
    (let ((outfile (org-export-output-file-name ".html" subtreep)))
      (org-export-to-file 'confluence outfile
        async subtreep visible-only body-only ext-plist post-process)))

(provide 'ox-confluence-html)
;;; ox-confluence-html.el ends here
