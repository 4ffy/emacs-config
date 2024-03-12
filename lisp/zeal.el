;;; zeal.el --- Lookup Zeal/Dash documentation.      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Cameron Norton

;; Author: Cameron Norton <cameron.norton@gmail.com>
;; Keywords: docs, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'dom)
(require 'sqlite)
(require 'xml)

(defgroup zeal nil
  "Browse Zeal docsets."
  :group 'external)

(defcustom zeal-docset-dir
  (file-name-concat (or (getenv "XDG_DATA_HOME")
                        (file-name-concat (getenv "HOME") ".local/share"))
                    "Zeal/Zeal/docsets")
  "Top-level directory for Zeal docsets."
  :type '(directory)
  :group 'zeal)

(defun zeal--get-index-file (docset)
  "Find the index html file for DOCSET.
This parses the docset's \"Info.plist\" XML file to find the
docset's index page.  Some docsets are weird and don't give a
path, so exit with an error if that happens."
  (unless (file-directory-p (file-name-concat zeal-docset-dir docset))
    (error "Docset %s does not exist" docset))
  (let* ((info-plist
          (file-name-concat zeal-docset-dir docset "Contents/Info.plist"))
         (xml-dom
          (with-temp-buffer
            (insert-file-contents info-plist)
            (libxml-parse-xml-region)))
         (index-filename
          (zeal--remove-path-metadata
           (or
            (cadr
             (assoc
              "dashIndexFilePath"
              (seq-partition
               (mapcar #'caddr (dom-children (dom-by-tag xml-dom 'dict))) 2)))
            (error "Index file not found")))))
    (file-name-concat zeal-docset-dir
                      docset
                      "Contents/Resources/Documents"
                      index-filename)))

(defun zeal--get-symbol-path-alist (db)
  "Create an alist of (symbol . path) from the Zeal database DB.
The docset database should have a searchIndex table with name and
path columns that can be used for symbol lookup.  However, some
docsets are weird and don't follow the schema.  These cases are
not handled."
  (let ((result '())
        (query
         (sqlite-select db "select name, path from searchIndex" nil 'set)))
    (while (sqlite-more-p query)
      (let ((row (sqlite-next query)))
        (push `(,(car row) . ,(cadr row)) result)))
    result))

(defun zeal--remove-path-metadata (path)
  "Remove Dash metadata from fetched PATH.
Paths retrieved from the docset DB may have extra metadata
enclosed in angle brackets before the actual path name.  This
does a simple replacement to strip these tags."
  (with-temp-buffer
    (insert path)
    (goto-char (point-min))
    (when (re-search-forward "\\`<.*>" nil t)
      (replace-match ""))
    (buffer-string)))

(defun zeal--url-from-file (filename)
  "Prepend \"file://\" to FILENAME."
  (format "file://%s" filename))

;;;###autoload
(defun zeal-lookup-symbol ()
  "Search Zeal docs for a symbol."
  (interactive)
  (let* ((selected-docset
          (completing-read "Select docset: "
                           (directory-files zeal-docset-dir nil "\\.docset\\'")
                           nil
                           t))
         (resource-path
          (file-name-concat zeal-docset-dir
                            selected-docset
                            "Contents/Resources"))
         (document-path (file-name-concat resource-path "/Documents"))
         (db (sqlite-open (file-name-concat resource-path "docSet.dsidx")))
         (symbol-alist (zeal--get-symbol-path-alist db))
         (html-file
          (file-name-concat document-path
                            (zeal--remove-path-metadata
                             (cdr
                              (assoc
                               (completing-read "Select symbol: " symbol-alist
                                                nil t)
                               symbol-alist))))))
    (other-window-prefix)
    (browse-url (zeal--url-from-file html-file))))

;;;###autoload
(defun zeal-display-docset (docset)
  "Prompt the user to display DOCSET's index page."
  (interactive (list
                (completing-read
                 "Select docset: "
                 (directory-files zeal-docset-dir nil "\\.docset\\'")
                 nil
                 t)))
  (other-window-prefix)
  (browse-url (zeal--url-from-file (zeal--get-index-file docset))))

(provide 'zeal)
;;; zeal.el ends here
