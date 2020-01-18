;;; org-to-xml.el --- Converts an org-mode file to XML.

;; Copyright © 2019 Norman Walsh

;; Author: Norman Walsh <ndw at nwalsh dot com>
;; Keywords: org-mode, XML

;; This file is not part of GNU Emacs.

;; This program is Free Software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module has been superseded by `om-to-xml.el'. You almost
;; certainly want that one. This version is moribund.

;; This file converts an org-mode file to XML. The goal of this conversion
;; is a complete and accurate translation of the internal org-mode
;; data structures to XML. Only a minimal effort is made to clean up the
;; XML. The expectation is that down-stream XML processing tools will
;; take care of cleanup and transformation.
;;
;; NOTE: This function is destructive; for a file named "foo.org",
;; this function will overwrite any existing file named "foo.xml".
;;
;; Installation: put this to your load path and add
;;
;;   (require 'org-to-xml)
;;
;;  to your .emacs.
;;
;; I have 'org-to-xml bound to a key, but there's nothing remotely standard
;; about my binding.
;;
;; Change log:
;; v0.0.6: added conditional bindings for caaddr and caddr
;; v0.0.5: added autoload comments
;; v0.0.4: allow the caller to specify an alternate filename for the xml
;; v0.0.3: many improvements
;;         - move data out of @value and into content so whitespace won't be mangled
;;         - strip leading whitespace paragraphs
;;         - unindent src-block content so it's left aligned
;;         - handle titles correctly
;;         - etc. (0.0.2 wasn't really very good.)
;; v0.0.2: added default namespace
;; v0.0.1: initial release

;;; Code:

(require 'org)

(defvar org-to-xml-ignore-symbols
  '(:parent :begin :end
            :contents-begin :contents-end
            :pre-blank :post-blank
            :post-affiliated :raw-value
            :structure :value :title))

;;; Preliminary functions not available in raw emacs
;;; https://github.com/ndw/org-to-xml/pull/2
(if (not (fboundp 'caaddr))
    (progn
      (defun caaddr (x)
        (car (car (cdr (cdr x)))))

      (defun caddr (x)
        (car (cdr (cdr x))))))

;;;###autoload
(defun org-to-xml (&optional filename)
  "Convert an 'org-mode' buffer to XML.
If FILENAME is provided, then that filename is used to store the document.
Otherwise, the filename is derived from the name of the Org file."
  (interactive)
  (if (eq major-mode 'org-mode)
      (ndw/o2xml--org-to-xml (current-buffer) filename)
    (message "The org-to-xml function can only be applied to org-mode buffers")))

(defvar ndw/o2xml--org-to-xml-version "0.0.5")
(defvar ndw/o2xml--org-to-xml-uri "https://github.com/ndw/org-to-xml")

;;;###autoload
(defun org-to-xml-version ()
  "Displays the current version of org-to-xml."
  (interactive)
  (message (format "org-to-xml version %s (see %s)"
                   ndw/o2xml--org-to-xml-version
                   ndw/o2xml--org-to-xml-uri)))

(defun ndw/o2xml--org-to-xml (buffer &optional filename)
  "Convert the 'org-mode' BUFFER to XML; save the result in FILENAME.
If no FILENAME is given, the buffer filename will be used, with .org
removed and .xml added."
  (let* ((buffn (buffer-file-name buffer))
         (xmlfn (if filename
                    filename
                  (if (string-suffix-p ".org" buffn)
                      (concat (substring buffn 0 (- (length buffn) 4)) ".xml")
                    (concat buffn ".xml"))))
         (tree  (save-current-buffer
                  (set-buffer buffer)
                  (org-element-parse-buffer))))
    (ndw/o2xml--kill-xml-buffer xmlfn)
    (with-temp-buffer
      (delete-region (point-min) (point-max))
      (insert "<?xml version=\"1.0\"?>\n")
      (insert "<!-- Converted from org-mode to XML by org-to-xml version ")
      (insert ndw/o2xml--org-to-xml-version)
      (insert " -->\n")
      (insert "<!-- See https://github.com/ndw/org-to-xml -->\n")
      (ndw/o2xml--walk-tree tree)
      (ndw/o2xml--post-process)
      (write-region (point-min) (point-max) xmlfn))
    (message (concat "Converted org-mode: " xmlfn))))

(defun ndw/o2xml--kill-xml-buffer (filename)
  "Kill the buffer associated with FILENAME."
  (mapc
   (lambda (buf)
     (if (string= filename (buffer-file-name buf))
         (kill-buffer buf)))
     (buffer-list)))

(defun ndw/o2xml--walk-tree (tree)
  "Walk the 'org-mode' sexp TREE, inserting XML into the buffer for each expression."
  (if (listp tree)
      (let* ((tag   (car tree))
             (attrs (if (listp tree) (cadr tree) nil))
             (rest  (if (listp tree) (cddr tree) nil))
             (pre-blank (plist-get attrs :pre-blank))
             (post-blank (plist-get attrs :post-blank))
             (props (ndw/o2xml--property-drawer tree)))
        (progn
          (insert (format "<%s" tag))
          (ndw/o2xml--walk-attributes attrs props)
          (insert ">")
          (if (plist-get attrs :title)
              (progn
                (insert "<title>")
                (mapc
                 (lambda (element)
                   (ndw/o2xml--walk-tree element))
                 (plist-get attrs :title))
                (insert "</title>\n")))
          (if (plist-get attrs :value)
              (insert
                (ndw/o2xml--xml-content-escape
                 (plist-get attrs :value))))
          (mapc 'ndw/o2xml--walk-tree rest)
          (insert (format "</%s>" tag))
          (if (and post-blank (> post-blank 0))
              (insert (make-string post-blank ?\s)))
          ))
    (insert (ndw/o2xml--xml-content-escape
             (substring-no-properties tree 0 (length tree))))))

(defun ndw/o2xml--walk-attributes (xattrs props)
  "Walk the attribute list, XATTRS, outputing XML attributes as appropriate.
If PROPS is non-nil, it's assumed to be a list of node-property elements.
If an attribute has the same name as a node property, it's elided from
the attribute list."
  (let ((attrs (copy-sequence xattrs))
        (keys  (mapcar
                (lambda (prop)
                  (plist-get (cadr prop) :key))
                props)))
    (while attrs
      (let* ((sym  (car attrs))
             (name (substring (symbol-name sym) 1))
             (val  (cadr attrs)))
        (if (and val (not (member sym org-to-xml-ignore-symbols))
                 (not (member name keys)))
            (progn
              (insert (format " %s=\"" name))
              (ndw/o2xml--format-attribute val)
              (insert "\"")))
        (setq attrs (cddr attrs))))))

(defun ndw/o2xml--format-attribute (val)
  "Format each attribute value VAL as XML."
  (let ((avalue
         (cond ((and (listp val) (eq (car val) 'timestamp))
                (plist-get (cadr val) :raw-value))
               ((listp val)
                (substring-no-properties (car val) 0 (length (car val))))
               ((symbolp val)
                (symbol-name val))
               ((numberp val)
                (number-to-string val))
               ((stringp val)
                val)
               (t (format "%s" val)))))
    (insert (ndw/o2xml--xml-attribute-escape avalue))))

(defun ndw/o2xml--xml-attribute-escape (str)
  "Escape the STR value of an attribute appropriately."
  (replace-regexp-in-string "<" "&lt;"
    (replace-regexp-in-string ">" "&gt;"
      (replace-regexp-in-string "\"" "&quot;"
        (replace-regexp-in-string "&" "&amp;" str)))))

(defun ndw/o2xml--xml-content-escape (str)
  "Escape the STR value of element content appropriately."
  (replace-regexp-in-string "<" "&lt;"
    (replace-regexp-in-string ">" "&gt;"
      (replace-regexp-in-string "&" "&amp;" str))))

(defun ndw/o2xml--property-drawer (element)
  "If ELEMENT is a headline, return its properties."
  (if (eq (car element) 'headline)
      (if (and (listp element) (eq (caaddr element) 'section))
          (let ((section (caddr element)))
            (if (and (listp section) (eq (caaddr section) 'property-drawer))
                (cddr (caddr section)))))))

(defun ndw/o2xml--post-process ()
  "Post process the result to add a namespace declaration. A bit of a hack, really."
  (goto-char (point-min))
  (if (re-search-forward "<org-data>")
      (replace-match "<org-data xmlns=\"https://nwalsh.com/ns/org-to-xml\">"))
  ;; Replace elements with empty content with empty element tags
  (goto-char (point-min))
  (while (re-search-forward "<\\([-a-zA-Z]+\\)\\([^>]*\\)></\\1>" nil t)
    (replace-match "<\\1\\2/>"))
  ;; Remove leading whitespace from paragraphs
  (goto-char (point-min))
  (while (re-search-forward "<\\(paragraph[^>]*\\)>\\s-+" nil t)
    (replace-match "<\\1>"))
  (goto-char (point-min))
  (ndw/o2xml--undent-src-blocks)
)

(defun ndw/o2xml--undent-src-blocks ()
  "Search for all src-block elements and remove leading indentation."
  (while (re-search-forward "<src-block[^>]+>" nil t)
      (let ((start (point)))
        (insert "\n")
        (let ((min-spaces 99999) ; an absurd number
              (end nil)
              (more-lines t))
          (while more-lines
            (if (looking-at "^\\s-*$")
                ;; blank lines don't count
                nil
              (if (looking-at "^\\(\\s-+\\)")
                  (if (> min-spaces (length (match-string 1)))
                      (setq min-spaces (length (match-string 1))))
                (setq more-lines nil)))
            (if more-lines
                (forward-line)))
          (setq end (point))
          (if (and (looking-at "</src-block>") (> min-spaces 0))
              (progn
                (indent-rigidly (1+ start) end (- min-spaces))))
          (goto-char start)
          (delete-char 1)))))

(provide 'org-to-xml)

;;; org-to-xml.el ends here
