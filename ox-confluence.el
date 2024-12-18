;;; ox-confluence --- Confluence Wiki Back-End for Org Export Engine

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
  (let* ((host (or host ox-confluence-host))
         (token (and ox-confluence-token
                     (with-temp-buffer
                       (insert-file-contents ox-confluence-token)
                       (buffer-string))))
         )
    (unless (and host token)
      (cond
       ((not host) (error "No host provided"))
       ((not ox-confluence-token) (error "No token file defined"))
       ((not token) (error "Could not read token from file %s" ox-confluence-token))))
    (let* ((header (format "-H \"Authorization: Bearer %s\"" token))
           (url (format "https://%s/rest/api/content?title=%s&spaceKey=%s" host title space)))
      (with-temp-buffer
        (if (zerop (call-process "curl" nil (current-buffer) nil "--get" "-s" header url))
            (progn (goto-char (point-min))
                   (let* ((result (gethash "results" (json-parse-buffer) nil)))
                     (if result
                         (gethash "id" result)
                       (error "Could not locate %s in %s. Ensure that page exists" title space))))
          (error "Error with curl\n%s" (buffer-string)))))))

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
  (format "<blockquote>\n%s</blockquote>\n" contents))

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
    (table-cell . ox-confluence-table-cell)))

;;;###autoload
(defun ox-confluence-export ()
  "Export current buffer using ox-confluence backend."
  (interactive)
  (org-export-to-buffer 'confluence "*confluence*"))

(provide 'ox-confluence)
;;; ox-confluence.el ends here
