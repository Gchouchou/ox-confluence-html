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
  (interactive "sConfluence Page Link:")
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
If OVERRIDE is t, overrides attachment if already present.
Adds COMMENT to upload."
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
    (export-block . org-html-export-block)))


;;;###autoload
(defun ox-confluence-export ()
  "Export current buffer using ox-confluence backend."
  (interactive)
  (org-export-to-buffer 'confluence "*confluence*"))

(provide 'ox-confluence-html)
;;; ox-confluence-html.el ends here
