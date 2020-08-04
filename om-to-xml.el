;;; om-to-xml.el --- Converts an org-mode file to XML. -*- lexical-binding: t -*-

;; Copyright © 2020 Norman Walsh

;; Author: Norman Walsh <ndw at nwalsh dot com>
;; Keywords: org-mode, org-ml, XML

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

;;; Prerequisites:

;; org-ml: https://github.com/ndwarshuis/org-ml

;;; Commentary:

;; This file converts an org-mode file to XML. It is a significant
;; improvement over `org-to-xml.el`, previously package, in that it
;; uses the excellent `org-ml` library by Nate Dwarshuis to parse the
;; Org data structures. I more-or-less abandoned attempting to improve
;; `org-to-xml.el` at the point where I realized I needed a better
;; parsing library for Org.

;; In addition to better parsing, `om-to-xml.el` provides a few
;; extensibility options:

;; `om-to-xml-plist-skip-keys':
;; The `om' library represents the properties of Org nodes with a
;; plist of properties. The properties named in this list will be
;; excluded from the XML output.

;; `om-to-xml-newline-before-start' and
;; `om-to-xml-newline-before-end':
;; The org node types named in these lists will have an extra
;; newline before their start and/or end tag. This makes the
;; XML output a little easier to read.

;; `om-to-xml-element-handlers':
;; This is an alist that maps node types to special functions for XML
;; conversion. By default, all of the Org nodes are converted to XML
;; in the same way: they get a start tag, the plist values become
;; attributes (or children), and any content is placed inside the
;; element. This list is an opportunity to insert special processing.
;;
;; If you define your own block types, for example, you may want to
;; define functions for converting them to XML. Four node types are
;; treated specially by default:
;;
;; + src-block: In the Org data structure, the content of the
;;   src-block is a value in the plist. For XML, I turn it into
;;   element content.
;; + macro: Org macros values are delimited by {{{ and }}}. There's no
;;   reason to leave those delimiters in the XML.
;; + subscript and superscript: This library isn't really intended
;;   as an export library exactly. But I treat sub and superscript
;;   specially for convenience. If a _ or ^ wouldn't be repesented as
;;   a sub or superscript on export, I ignore the Org data structure
;;   and output a literal _ or ^ instead.
;;
;; More special functions may be added in the future.

;; `om-to-xml-post-process'
;; If this is set to a function, that function will be run on the
;; output XML buffer before it is saved.

;; Installation: put this to your load path and add
;;
;;   (require 'om-to-xml)
;;
;;  to your .emacs.
;;
;; I have 'om-to-xml bound to a key, but there's nothing remotely
;; standard about my binding.
;;
;; Change log:
;; v0.0.7: Updated to use org-ml (the om.el refactored)
;; ...
;; v0.0.1: Initial release

;;; Code:

(require 'org)
(require 'org-ml)

(defconst om-to-xml--om-to-xml-version "0.0.7")
(defconst om-to-xml--om-to-xml-uri "https://github.com/ndw/org-to-xml")
(defconst om-to-xml--namespace "https://nwalsh.com/ns/org-to-xml")

(defvar om-to-xml-plist-skip-keys
  '(:begin
    :end
    :pre-blank
    :post-blank
    :contents-begin
    :contents-end
    :post-affiliated
    :use-brackets-p)
  "List of om node keys that should be ignored.
All key values that aren't ignored are turned into attributes." )

;; From an XML perspective, we can assume that whitespace
;; between "block" elements is irrelevant. These lists add
;; newlines to make the resulting XML a little more readable.

(defvar om-to-xml-newline-before-start
  '(keyword
    headline
    paragraph
    section
    property-drawer
    node-property)
  "List of elements that should be preceded by a newline.")

(defvar om-to-xml-newline-before-end
  '(property-drawer
    headline
    section)
  "List of elements whose end tag should be preceded by a newline.")

(defvar om-to-xml-element-handlers
  '((src-block . om-to-xml--om-src-block-to-xml)
    (macro . om-to-xml--om-macro-to-xml)
    (superscript . om-to-xml--om-sub-superscript-to-xml)
    (subscript . om-to-xml--om-sub-superscript-to-xml))
  "Special element handlers.")

(defvar om-to-xml-post-process nil
  "Post-processing hook.
If set to a function, that function will run after the XML is
generated.")

;;;###autoload
(defun om-to-xml (&optional filename)
  "Convert an 'org-mode' buffer to XML.
If FILENAME is provided, then that filename is used to store the
document. Otherwise, the filename is derived from the name of the
Org file."
  (interactive)
  (if (eq major-mode 'org-mode)
      (om-to-xml--org-to-xml (current-buffer) filename)
    (message "Error: om-to-xml can only be applied to org-mode buffers")))

(defun om-to-xml--org-to-xml (buffer &optional filename)
  "Convert the 'org-mode' BUFFER to XML; save the result in FILENAME.
If no FILENAME is given, the buffer filename will be used, with
.org removed and .xml added."
  (let* ((buffn (buffer-file-name buffer))
         (xmlfn (if filename
                    filename
                  (if (string-suffix-p ".org" buffn)
                      (concat (substring buffn 0 (- (length buffn) 4)) ".xml")
                    (concat buffn ".xml"))))
         (om-list (om-to-xml--parse-buffer buffer)))
    (om-to-xml--kill-xml-buffer xmlfn)
    (with-temp-buffer
      (delete-region (point-min) (point-max))
      (insert "<?xml version=\"1.0\"?>\n")
      (insert "<!-- Converted from org-mode to XML by om-to-xml version ")
      (insert om-to-xml--om-to-xml-version)
      (insert " -->\n<!-- See ")
      (insert om-to-xml--om-to-xml-uri)
      (insert " -->\n")
      (om-to-xml--om-to-xml om-list)

      (if (functionp om-to-xml-post-process)
          (funcall om-to-xml-post-process))

      (write-region (point-min) (point-max) xmlfn))
    (message (concat "Converted org-mode: " xmlfn))))

(defun om-to-xml--kill-xml-buffer (filename)
  "Kill the buffer associated with FILENAME."
  (mapc
   (lambda (buf)
     (if (string= filename (buffer-file-name buf))
         (kill-buffer buf)))
     (buffer-list)))

(defun om-to-xml--parse-buffer (&optional buffer)
  "Use om library to parse entire document in BUFFER."
  (save-current-buffer
    (if buffer
        (set-buffer buffer))
    (save-excursion
      (let ((om-point (point-min))
            (last-point -1)
            (om-list '())
            (elem nil))
        (while (< om-point (point-max))
          (setq elem (org-ml-parse-element-at om-point))
          (setq parent (plist-get (cadr elem) :parent))
          (setq om-point (plist-get (cadr elem) :end))
          ;; There are places where parsing seems to get stuck.
          ;; For example, if a paragraph precedes the first heading
          ;; or if the file ends with several blank lines. The
          ;; :end doesn't advance the cursor position and this
          ;; while loop never ends. To avoid that, we use last-point
          ;; to force the cursor to advance.
          ;; https://github.com/ndwarshuis/org-ml/issues/8
          (if (<= om-point last-point)
              (setq om-point (1+ last-point))
            (setq om-list (append om-list (list elem))))
          (setq last-point om-point)
          (goto-char om-point))
        om-list))))

(defun om-to-xml--plist-children (plist &optional exclude)
  "Return the names of keys in PLIST that must be child elements.
Any keys listed in EXCLUDE are ignored."
  (let ((list plist)
        (children nil))
    (while list
      (setq key (car list))
      (cond
       ((eq key :parent)
        nil)
       ((member key exclude)
        nil)
       ((and (cadr list) (listp (cadr list)))
        (if children
            (setq children (append children (list key)))
          (setq children (list key))))
       (t
        nil))
      (setq list (cddr list)))
    children))

(defun om-to-xml--plist-attributes (plist &optional exclude)
  "Insert atomic PLIST values as XML attributes.
Any keys whose names are listed in EXCLUDE are ignored.
The comparison is done by name because symbols are unintered."
  (let ((list plist) key value)
    (while list
      (setq key (car list))
      (setq value
            (cond
             ((member (symbol-name key) exclude)
              nil)
             ((eq (cadr list) nil)
              nil)
             ((integerp (cadr list))
              (number-to-string (cadr list)))
             ((symbolp (cadr list))
              (symbol-name (cadr list)))
             ((stringp (cadr list))
              (ndw/o2xml--om-xml-attribute-escape (cadr list)))
             (t nil)))
      (setq list (cddr list))
      (if value
          (let ((name (substring (symbol-name key) 1)))
            (if (not (member key om-to-xml-plist-skip-keys))
                (progn
                  (insert (concat " " name "=\"" value "\"")))))))))

(defun om-to-xml--property-drawer-properties (element)
  "If ELEMENT is a headline, return the names of its properties.
Properties in this case meaning any properties defined in a
property-drawer. The properties are returned as upper-case symbols
because that's how they appear in the headline plist."
  (if (and (eq (car element) 'headline)
           (eq (caaddr element) 'section))
      (let ((section (caddr element)))
        (if (eq (caaddr section) 'property-drawer)
            (let ((props (cddr (caddr section)))
                  (names '())
                  key name)
              (while props
                (setq key (plist-get (cadr (car props)) :key))
                (setq name (concat ":" (upcase key)))
                (setq names (append names (list name)))
                (setq props (cdr props)))
              names)))))

(defun om-to-xml--om-to-xml (om-list)
  "Convert OM-LIST to XML."
  (insert (concat "<document xmlns=\""
                  om-to-xml--namespace
                  "\">"))
  (om-to-xml--om-to-xml-list om-list)
  (insert "</document>\n"))

(defun om-to-xml--om-to-xml-list (om-list)
  "Convert OM-LIST to XML."
  (let ((list om-list)
        symbol)
    (while list
      (om-to-xml--om-element-to-xml (car list))
      (setq list (cdr list)))))

(defun om-to-xml--om-insert-blank (elem)
  "Insert blank line if ELEM has a non-zero :post-blank property."
  (let* ((plist (cadr elem))
         (pblank (plist-get plist :post-blank))
         (blank (and (integerp pblank) (> pblank 0))))
    (if blank
        (insert " "))))
  
(defun om-to-xml--om-src-block-to-xml (elem)
  "Convert src-block ELEM to XML.
In the Org data model, the contents of the src-block is in a
value in the plist. In the XML, it makes more sense to put the
value in the content of the element."
  (let ((symbol (car elem))
        (plist (cadr elem)))
    (if (member symbol om-to-xml-newline-before-start)
        (insert "\n"))
    (insert (concat "<" (symbol-name symbol)))
    (om-to-xml--plist-attributes plist '(":value"))

    (let ((value (plist-get plist :value)))
      (insert ">")
      (insert (om-to-xml--om-xml-content-escape (substring value 0 (1- (length value)))))
      (insert "</src-block>"))))

(defun om-to-xml--om-element-to-xml (elem)
  "Convert ELEM to XML.
Lists are recursively processed, perhaps by the appropriate
'om-to-xml-element-handler. Other values are inserted
directly."
  (cond
   ((and (listp elem) (symbolp (car elem)))
    (let* ((symbol (car elem))
           (handler (alist-get symbol om-to-xml-element-handlers)))
      (if (and handler (functionp handler))
          (funcall handler elem)
        (om-to-xml--om-base-element-to-xml elem))
      (om-to-xml--om-insert-blank elem)))
   ((listp elem)
    (let ((children elem))
      (while children
        (om-to-xml--om-element-to-xml (car children))
        (setq children (cdr children)))))
   (t
    (insert (om-to-xml--om-xml-content-escape elem)))))

(defun om-to-xml--om-xml-content-escape (value)
  "Escape the string VALUE appropriately for XML content."
  (let ((str
         (cond
          ((integerp value)
           (number-to-string value))
          (t
           value))))
    (replace-regexp-in-string "<" "&lt;"
      (replace-regexp-in-string ">" "&gt;"
        (replace-regexp-in-string "&" "&amp;" str)))))

(defun ndw/o2xml--om-xml-attribute-escape (str)
  "Escape the STR value appropriately for an XML attribute."
  (replace-regexp-in-string "<" "&lt;"
    (replace-regexp-in-string ">" "&gt;"
      (replace-regexp-in-string "\"" "&quot;"
        (replace-regexp-in-string "&" "&amp;" str)))))

(defun om-to-xml--om-macro-to-xml (elem)
  "Convert a macro ELEM to XML.
All this really does is trim off the leading and trailing
macro indicators '{{{' and '}}}'."
  (let* ((symbol (car elem))
         (plist (cadr elem))
         (value (plist-get plist :value)))
    (if (member symbol om-to-xml-newline-before-start)
        (insert "\n"))
    (insert (concat "<" (symbol-name symbol)))
    (om-to-xml--plist-attributes plist '(":value"))

    ;;; This is probably redundant, but safety first.
    (if (string= (substring value 0 3) "{{{")
        (setq value (substring value 3)))
    (if (string= (substring value (- (length value) 3)) "}}}")
        (setq value (substring value 0 (- (length value) 3))))

    (insert (concat
             " value=\""
             (ndw/o2xml--om-xml-attribute-escape value)
             "\""))
    (insert "/>")))

(defun om-to-xml--om-sub-superscript-to-xml (elem)
  "Convert subscript or superscript ELEM to XML.
Mostly, converting form Org to XML is a direct translation of the
data model, but superscripts and subscripts are interpreted a
little bit more like export. In particular, they are turned back
into `_` and `^` according to the setting of
`org-export-with-sub-superscripts'."
  (let* ((symbol (car elem))
         (plist (cadr elem))
         (rest (cddr elem))
         (brackets (plist-get plist :use-brackets-p)))

    (if (or (eq org-export-with-sub-superscripts t)
            (and (eq org-export-with-sub-superscripts '{})
                 brackets))
        (om-to-xml--om-base-element-to-xml elem)
      (progn
        (if (eq symbol 'superscript)
            (insert "^")
          (insert "_"))
        (insert (car rest))))))

(defun om-to-xml--om-base-element-to-xml (elem)
  "Convert ELEM to XML."
  (let* ((symbol (car elem))
         (plist (cadr elem))
         (rest (cddr elem))
         (exclude (om-to-xml--property-drawer-properties elem)))
    (if (member symbol om-to-xml-newline-before-start)
        (insert "\n"))
    (insert (concat "<" (symbol-name symbol)))
    (om-to-xml--plist-attributes plist exclude)

    (if rest
        (progn
          (insert ">")

          (let ((list (om-to-xml--plist-children plist))
                name)
            (while list
              (setq name (substring (symbol-name (car list)) 1))
              (insert (concat "<" name ">"))
              (om-to-xml--om-element-to-xml (plist-get plist (car list)))
              (insert (concat "</" name ">"))
              (setq list (cdr list))))

          (om-to-xml--om-to-xml-list rest)
          (if (member symbol om-to-xml-newline-before-end)
              (insert "\n"))
          (insert (concat "</" (symbol-name symbol) ">")))
      (progn
        (insert "/>")))))

(provide 'om-to-xml)

;;; om-to-xml.el ends here
