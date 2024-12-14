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

;; formatting
;; #+BEGIN_EXAMPLE
;;   ,*bold*                                    # <strong>bold</strong>
;;   /italic/                                   # <em>italic</em>
;;   =verbatim=                                 # <kbd>verbatim</kbd>
;; #+END_EXAMPLE

(defun ox-confluence-bold (bold contents info)
  "Transcode BOLD from Org to HTML.

CONTENTS is the text with bold markup.
INFO is a plist holding contextual information."
  (when contents (format "<strong>%s</strong>" contents)))

(defun ox-confluence-italic (italic contents info)
  "Transcode ITALIC from Org to HTML.

CONTENTS is the text with italic markup.
INFO is a plist holding contextual information."
  (when contents (format "<em>%s</em>" contents)))

(defun ox-confluence-verbatim (verbatim contents info)
  "Transcode VERBATIM string from Org to HTML.

CONTENTS is nil.
INFO is a plist holding contextual information."
  (let ((contents (org-html-encode-plain-text
                   (org-element-property :value verbatim))))
    (when contents (format "<kbd>%s</kbd>" contents))))

;; plain lists
;; #+BEGIN_EXAMPLE
;;   ,#+attr_html: :class this                   # <ul class="this">
;;   - item 1                                   # <li>item 1</li>
;;   - item 2                                   # <li>item 2</li>
;;                                              # </ul>

;;   + item 1                                   # <ol><li>item 1</li></ol>
;;   - definition :: list                       # <dl><dt>definition</dt><dd>list</dd></dl>
;; #+END_EXAMPLE

(defun ox-slimhtml-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST string from Org to HTML.

CONTENTS is the contents of the list element.
INFO is a plist holding contextual information."
  (when contents
    (let ((type (cl-case (org-element-property :type plain-list)
                  (ordered "ol")
                  (unordered "ul")
                  (descriptive "dl"))))
      (format "<%s%s>%s</%s>" type (ox-slimhtml--attr plain-list) contents type))))

;; paragraphs
;; #+BEGIN_EXAMPLE
;;   ,#+attr_html: :class this                   # <p class="this">content</p>
;;   content
;; #+END_EXAMPLE

(defun ox-slimhtml-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.

CONTENTS is the contents of the paragraph.
INFO is a plist holding contextual information."
  (when contents
    (if (or (ox-slimhtml--immediate-child-of-p paragraph 'item)
            (ox-slimhtml--immediate-child-of-p paragraph 'special-block))
        contents
      (if (ox-slimhtml--has-immediate-child-of-p paragraph 'link)
          (format "<p>%s</p>" contents)
        (format "<p%s>%s</p>" (ox-slimhtml--attr paragraph) contents)))))


(org-export-define-backend
    'confluence
  '((bold . ox-confluence-bold)
    (italic . ox-confluence-italic)
    (verbatim . ox-confluence-verbatim)
    ))


;;;###autoload
(defun ox-confluence-export-to-html ()
  "Export current buffer using ox-confluence backend."
  )

(provide 'ox-confluence)
;;; ox-confluence.el ends here
