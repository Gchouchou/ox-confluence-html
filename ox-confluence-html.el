;;; ox-confluence-html --- Confluence Wiki Back-End for Org Export Engine

;; Author: Justin Yu <jusytinyu@gmail.com>
;; Keywords: confluence, wiki

;; This file is not part of GNU Emacs.

;;; Commentary:
;; ox-confluence tries to convert org file to confluence storage format.
;; Since confluence storage is quite limited, this export backend will
;; ignore many parameters. Many elements will be taken from ox-slimhtml or
;; ox-html.

;;; Code:
(require 'ox-html)
(require 'cl-lib)
(require 'json)
(require 'url-parse)


(defcustom ox-confluence-host nil
  "The default confluence host name used for exporting.
It usually has the form of confluence.somewhere.com or somewhere.atlassian.net
We only want the host name so do not prepend with https or postpend with path."
  :safe 'stringp
  :type 'string
  :group 'confluence)

(defcustom ox-confluence-token nil
  "The file that contains the API token to access confluence."
  :safe 'file
  :type 'file
  :group 'confluence)

;;; Confluence Curl functions

(defun ox-confluence-get-page-id (title space &optional host)
  "Queries the pageid using TITLE and SPACE.
Uses HOST then ox-confluence-host or fails if both are nil.
Uses curl as a backend."
  (interactive "sPage title: \nsPage space: ")
  (let* ((host (or host ox-confluence-host (error "No host found")))
         (token (and ox-confluence-token
                     (file-exists-p ox-confluence-token)
                     (with-temp-buffer
                       (insert-file-contents ox-confluence-token)
                       (buffer-string))))
         (header (when token (format "-H \"Authorization: Bearer %s\"" token)))
         (url (format "https://%s/rest/api/content?title=%s&spaceKey=%s" host title space)))
      (with-temp-buffer
        (if (zerop (call-process "curl" nil (current-buffer) nil "--get" "-s" header url))
            (progn (goto-char (point-min))
                   (let* ((result (gethash "results" (json-parse-buffer) nil)))
                     (if result
                         (gethash "id" result)
                       (error "Could not locate %s in %s. Ensure that page exists" title space))))
          (error "Error with curl\n%s" (buffer-string))))))

(defun ox-confluence-get-page-id-from-link (link)
  "Parse human readable LINK and retuns the page id.
It will also set ox-confluence-host for the rest of the session."
  (interactive "sConfluence Page Link: ")
  (let* ((parsed-uri (url-generic-parse-url link))
         (host (url-host parsed-uri))
         (filename (url-filename parsed-uri))
         (segs (string-split filename "/"))
         (query (car (last segs)))
         (query (when-let* ((match (string-match-p "\\?" query))) (substring query (+ match 1)))))
    ;; set host for the rest of the session
    (setq ox-confluence-host host)
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
      (ox-confluence-get-page-id (nth 3 segs) (nth 2 segs) host))
     (t (error "Unkown URL format: %s" link)))))

(defun ox-confluence-update-attachment (pageId attachment &optional override comment)
  "Upload ATTACHMENT to confluence PAGEID and return attachmentid.
If OVERRIDE is non-nil, overrides attachment if already present.
Adds COMMENT to upload."
  (interactive "sPageId: \nfFile: \nP")
  (if (not (file-exists-p attachment)) (error "Attachment %s does not exist" attachment))
  (let* ((basename (file-name-nondirectory attachment))
         (host (or ox-confluence-host (error "No host found")))
         (token (and ox-confluence-token
                     (file-exists-p ox-confluence-token)
                     (with-temp-buffer
                       (insert-file-contents ox-confluence-token)
                       (buffer-string))))
         (header (when token (format "-H \"Authorization: Bearer %s\"" token)))
         (uri (format "https://%s/rest/api/content/%s/child/attachment?filename=%s" host pageId basename))
         (attachmentId (with-temp-buffer
                         (if (zerop (call-process "curl" nil (current-buffer) nil "--get" "-s" header uri))
                             (progn (goto-char (point-min))
                                    (when-let* ((result (gethash "results" (json-parse-buffer) nil)))
                                      (gethash "id" result)))
                           (error "Error with curl\n%s" (buffer-string))))))
    (cond
     ;; adding new attachment
     ((not attachmentId)
      (with-temp-buffer
        (when (zerop (call-process "curl" nil (current-buffer) nil
                                   "-sSX POST"
                                   header
                                   "-H \"X-Atlassian-Token: nocheck\""
                                   (format "-F \"file=@%s\"" attachment)
                                   (when comment (format "-F \"comment=%s\"" comment))
                                   (format "https://%s/rest/api/content/%s/child/attachment" host pageId)))
          ;; get the attachment id of the newly updated file
          (message "Successfully updated %s, getting attachment id from result." basename)
          (goto-char (point-min))
          (when-let* ((result (gethash "results" (json-parse-buffer) nil)))
            (gethash "id" result)))))
     ;; override attachment
     (override (with-temp-buffer
                 (message "Override is set to true, overriding attachment %s, id=%s" basename attachmentId)
                 (call-process "curl" nil (current-buffer) nil
                               "-sSX POST"
                               header
                               "-H \"X-Atlassian-Token: nocheck\""
                               (format "-F \"file=@%s\"" attachment)
                               (when comment (format "-F \"comment=%s\"" comment))
                               (format "https://%s/rest/api/content/%s/child/attachment/%s/data" host pageId attachmentId))))
    ;; not overriding existing
    (t (progn
         (message "Attachment %s already exists, not overriding" basename)
         attachmentId)))))

(defun ox-confluence-update-content (pageId file &optional append)
  "Overwrite contents of confluence page PAGEID with FILE.
If APPEND is not nil, first query contents and prepend to FILE contents
before uploading."
  (interactive "sPageId: \nfFile: \nP")
  ;; Fetch version number of the page so we can increment it by 1
  (if (or (null pageId) (not (file-exists-p file)) (not ox-confluence-host))
      (error "Missing page id %s or file %s or host %s" pageId file ox-confluence-host))
  (let* ((token (and ox-confluence-token
                     (file-exists-p ox-confluence-token)
                     (with-temp-buffer
                       (insert-file-contents ox-confluence-token)
                       (buffer-string))))
         (header (when token (format "-H \"Authorization: Bearer %s\"" token)))
         (resp (with-temp-buffer
                 (if (zerop (call-process "curl" nil (current-buffer) nil
                                          "-sX GET"
                                          header
                                          (format "https://%s/rest/api/content/%s" ox-confluence-host pageId)))
                     (progn (goto-char (point-min))
                            (json-parse-buffer))
                   (error "Error with curl"))))
         (old-body (when-let* ((respb (gethash "body" resp))
                               (view (gethash "view" respb))
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
         (json `((version . (number . ,(+ page-ver 1)))
                 (title . ,title)
                 (type . page)
                 (body . (storage .
                                  ((representation . storage)
                                   (value . ,(format "%s" new-body))))))))
    (with-temp-buffer
      (if (zerop (call-process "curl" nil "current-buffer" nil
                               "-sSX POST"
                               header
                               (format "--json '%s'" (json-encode json))
                               (format "https://%s/restpai/content/%s" ox-confluence-host pageId)))
          (message "updated page successfully")
        (error "Error with update")))))

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

(defun ox-confluence-bold (bold contents info)
  "Transcode BOLD from Org to confluence storage format.

CONTENTS is the text with bold markup.
INFO is a plist holding contextual information."
  (when contents (format "<strong>%s</strong>" contents)))

(defun ox-confluence-italic (italic contents info)
  "Transcode ITALIC from Org to confluence storage format.

CONTENTS is the text with italic markup.
INFO is a plist holding contextual information."
  (when contents (format "<em>%s</em>" contents)))

(defun ox-confluence-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to confluence storage format.

CONTENTS is the text with STRIKE-THROUGH markup.
INFO is a plist holding contextual information."
  (when contents (format "<span style=\"text-decoration:line-through;\">%s</span>" contents)))

(defun ox-confluence-underline (underline contents info)
  "Transcode UNDERLINE from Org to confluence storage format.

CONTENTS is the text with italic markup.
INFO is a plist holding contextual information."
  (when contents (format "<u>%s</u>" contents)))


(defun ox-confluence-verbatim (verbatim contents info)
  "Transcode VERBATIM string from Org to confluence storage format.

CONTENTS is nil.
INFO is a plist holding contextual information."
  (let ((contents (org-html-encode-plain-text
                   (org-element-property :value verbatim))))
    (when contents (format "<code>%s</code>" contents))))

(defun ox-confluence-code (code contents info)
  "Transcode CODE string from Org to confluence storage format.

CONTENTS is nil.
INFO is a plist holding contextual information."
  (let ((contents (org-html-encode-plain-text
                   (org-element-property :value code))))
    (when contents (format "<code>%s</code>" contents))))

;; plain text

(defun ox-confluence-plain-text (plain-text info)
  "Transcode a PLAIN-TEXT string from Org to confluence storage format.

PLAIN-TEXT is the string to transcode.
INFO is a plist holding contextual information."
  (org-html-encode-plain-text plain-text))

;; minimal template
(defun ox-confluence-template (contents info)
  "Wrap exported CONTENTS in a minimal template for the confluence backend.

CONTENTS is the string to be exported.
INFO is a plist containing export options."
  contents)

(defun ox-confluence-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to a simple paragraph wrapper.

CONTENTS is the paragraph's text.
INFO is a plist holding contextual information."
  (format "<p>%s</p>" contents))

(defun ox-confluence-section (section contents info)
  "Transcode SECTION element from Org to confluence storage format.

CONTENTS is the section.
INFO is a plist holding contextual information"
  contents)

(defun ox-confluence-headline (headline contents info)
  "Transcode HEADLINE from Org to confluence storage format.

CONTENTS is the section as defined under the HEADLINE.
INFO is a plist holding contextual information."
  (let* ((text (org-export-data (org-element-property :title headline) info))
         (level (org-export-get-relative-level headline info)))
    (format "<h%s>%s</h%d>\n%s" level text level (or contents ""))))

(defun ox-confluence-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST string from Org to confluence storage format.

CONTENTS is the contents of the list element.
INFO is a plist holding contextual information.
Currently does not support descriptive lists and filters out checkboxes."
  (when contents
    (let ((type (cl-case (org-element-property :type plain-list)
                  (ordered "ol")
                  (unordered "ul"))))
      (format "<%s>%s</%s>" type (or contents "") type))))

(defun ox-confluence-item (item contents info)
  "Transcode an ITEM element from Org to confluence storage format.

CONTENTS holds the contents of the item.
INFO is a plist holding contextual information."
  (format "<li>%s</li>\n" contents))

(defun ox-confluence-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to confluence storage format.

CONTENTS holds the contents of the block.
INFO is a plist holding contextual information."
  (format "<blockquote>\n%s</blockquote>" contents))

;;; Table stuff

(defun ox-confluence-table (table contents info)
  "Transcode a TABLE element from Org to confluence storage format.

CONTENTS is the contents of the table.
INFO is a plist holding contextual information."
  (format "<table>\n<tbody>\n%s</tbody>\n</table>" contents))


(defun ox-confluence-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to confluence storage format.

CONTENTS is the contents of the table.
INFO is a plist holding contextual information."
  (when contents (format "<tr>\n%s</tr>\n" contents)))

(defun ox-confluence-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to confluence storage format.

CONTENTS is the contents of the table.
INFO is a plist holding contextual information."
  (when contents (format "<th>%s</th>\n" contents)))

(defun ox-confluence-example-block (example-block contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to HTML.

CONTENTS is nil.  INFO is a plist holding contextual
information."
(let ((contents (org-html-encode-plain-text
                   (org-element-property :value example-block))))
    (when contents (format "<pre>\n%s</pre>" contents))))

(defun ox-confluence-src-block (src-block contents info)
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

(defun ox-confluence-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to Confluence format.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "HTML")
    (format "<ac:structured-macro ac:name=\"html\" ac:schema-version=\"1\">\n%s</ac:structured-macro>"
            (org-remove-indentation (org-element-property :value export-block)))))

(defun ox-confluence-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to Confluence format.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  contents)

(defun ox-confluence-keyword (keyword _contents info)
  "Transcode a KEYWORD element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "HTML") value)
     ((string= key "TOC")
      ;; TODO confirm confluence's Table of content's macro format
      (format "<ac:structured-macro ac:name=\"TOC\"/>")))))

;;; Pre processing functions

(defun ox-confluence-include-replace (backend)
  "If BACKEND is confluence, replace includes html with export html.
This before process function will upload include html files to confluence,
defined using page_id and host or with confluence_url and then replace
include html with export html with an iframe tag to the confluence attachment."
  (when (org-export-derived-backend-p backend 'confluence)
    (let* ((options (org-export-get-environment 'confluence))
           (url (assoc :confluence-url options))
           (page_id (or (assoc :confluence-page-id options)
                        (and url (ox-confluence-get-page-id-from-link url))))
           (host ox-confluence-host)
           (override-attachments (assoc :override-confluence-attachment options))
           (upload-to-confluence (and (assoc :upload-to-confluence options) host page_id)))
      (if upload-to-confluence
          (message "Page_id: %s found, host: %s found, uploading attachments to confluence" page_id host)
        (message "Page_id: %s, host: %s, not uploading to confluence" page_id host))
      (save-excursion
        ;; got to top of the page and scan for includes
        (goto-char (point-min))
        (while (re-search-forward "^#\\+include: \"\\([^\"]+\\)\" export html.*$" nil t)
          (let* ((file (match-string 1)))
            (when upload-to-confluence (ox-confluence-update-attachment page_id file override-attachments))
            ;; TODO confirm the confluence attachment url format
            (replace-match (format "\
#+begin_export html
<iframe src=\"https://%s/%s/attachment/%s\" width=\"100%%\" height=\"400px\" frameborder=\"0\" scrolling=\"auto\"/>
#+end_export"
                                   host
                                   page_id
                                   (file-name-nondirectory file)))))))))

(add-hook 'org-export-before-processing-functions #'ox-confluence-include-replace)

(org-export-define-backend
    'confluence
  '((bold . ox-confluence-bold)
    (italic . ox-confluence-italic)
    (strike-through . ox-confluence-strike-through)
    (code . ox-confluence-code)
    (verbatim . ox-confluence-verbatim)
    (underline . ox-confluence-underline)
    (paragraph . ox-confluence-paragraph)
    (section . ox-confluence-section)
    (headline . ox-confluence-headline)
    (plain-list . ox-confluence-plain-list)
    (item . ox-confluence-item)
    (quote-block . ox-confluence-quote-block)
    (table . ox-confluence-table)
    (table-row . ox-confluence-table-row)
    (table-cell . ox-confluence-table-cell)
    (example-block . ox-confluence-example-block)
    (template . ox-confluence-template)
    (src-block . ox-confluence-src-block)
    (link . org-html-link)
    (line-break . org-html-line-break)
    (export-block . ox-confluence-export-block)
    (drawer . ox-confluence-drawer)
    (keyword . ox-confluence-keyword))
  :menu-entry '(?f "Export to Confluence"
                ((?F "As Confluence buffer" ox-confluence-export-as-html)
                 (?f "As HTML file" ox-confluence-export-to-confluence)
                 (?p "As HTML file and upload"
                     (lambda (a s v b)
                       (let* ((options (org-export-get-environment 'confluence))
                              (url (assoc :confluence-url options))
                              (page_id (or (assoc :confluence-page-id options)
                                           (and url (ox-confluence-get-page-id-from-link url))))
                              (file (ox-confluence-export-to-confluence nil s v b)))
                         (when (and file page_id)
                           (ox-confluence-update-content
                            page_id file)))))))
  :options-alist '((:confluence-page-id "PAGE_ID" nil nil)                                 ; page id to upload to
                   (:confluence-url "CONFLUENCE_URL" nil nil)                              ; or the url to upload
                   (:override-confluence-attachment "CONFLUENCE_OVERRIDE_ATTACH" nil nil)  ; override attachments on export
                   (:upload-to-confluence "UPLOAD_TO_CONFLUENCE" nil t)))                  ; override switch to not upload to confluence

;;;###autoload
(defun ox-confluence-export-as-html
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

(defun ox-confluence-export-to-confluence
    (&optional async subtreep visible-only body-only ext-plist)
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

When optional argument BODY-ONLY is non-nil, nothing changes.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
    (interactive)
    (let ((outfile (org-export-output-file-name ".html" subtreep)))
      (org-export-to-file 'confluence outfile
        async subtreep visible-only body-only ext-plist)))

(provide 'ox-confluence-html)
;;; ox-confluence-html.el ends here
