;;; org-to-xml.el --- Converts an org-mode file to XML.

;; Copyright © 2019 Norman Walsh

;; Author: Norman Walsh <ndw at nwalsh dot com>
;; Version: 0.0.1
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
;;
;; v0.0.2: added default namespace
;; v0.0.1: initial release

;;; Code:

(require 'org)

(defvar org-to-xml-ignore-symbols
  '(:begin :end :parent :contents-begin :contents-end :pre-blank :post-blank :post-affiliated
           :raw-value :structure))

(defun org-to-xml ()
  "Convert an 'org-mode' buffer to XML."
  (interactive)
  (message (concat "major mode is " major-mode))
  (if (eq major-mode 'org-mode)
      (ndw/o2xml--org-to-xml (current-buffer))
    (message "The org-to-xml function can only be applied to org-mode buffers")))

(setq ndw/o2xml--org-to-xml-version "0.0.2")

(defun ndw/o2xml--org-to-xml (buffer &optional filename)
  "Convert the 'org-mode' BUFFER to XML, save the result in FILENAME. If no FILENAME
is given, the buffer filename will be used, with .org removed and .xml added."
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
      (goto-char (point-min))
      ;; Slightly hacky approach to adding a default namespace
      (if (re-search-forward "<org-data>")
          (replace-match "<org-data xmlns=\"https://nwalsh.com/ns/org-to-xml\">"))
      (while (re-search-forward "<\\([-a-zA-Z]+\\)\\([^>]*\\)></\\1>" nil t)
        (replace-match "<\\1\\2/>"))
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
      (let ((tag   (car tree))
            (attrs (if (listp tree) (cadr tree) nil))
            (rest  (if (listp tree) (cddr tree) nil)))
        (progn
          (insert (format "<%s" tag))
          (ndw/o2xml--walk-attributes attrs)
          (insert ">")
          (mapcar 'ndw/o2xml--walk-tree rest)
          (insert (format "</%s>" tag))))
    (insert (ndw/o2xml--xml-content-escape (substring-no-properties tree 0 (length tree))))))

(defun ndw/o2xml--walk-attributes (xattrs)
  "Walk the attribute list, XATTRS, outputing XML attributes as appropriate."
  (let ((attrs (copy-sequence xattrs)))
    (while attrs
      (let* ((sym  (car attrs))
             (name (substring (symbol-name sym) 1))
             (val  (cadr attrs)))
        (if (and val (not (member sym org-to-xml-ignore-symbols)))
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

(provide 'org-to-xml)

;;; org-to-xml.el ends here
