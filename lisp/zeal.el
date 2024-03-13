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

;; I use Zeal for offline documentation.  However, it is frustrating to have to
;; leave Emacs when I want to look something up.  I tried solving this by adding
;; extra Info manuals, but there aren't many of them out there, and using Pandoc
;; or whatever just isn't good enough.

;; Instead, I want to tap into my existing Zeal documentation.  It is possible
;; to just browse the offline html files, but then I miss out on features such
;; as symbol lookup.

;; So, I wrote this Zeal package.  Currently, it has `zeal-lookup-symbol' and
;; `zeal-display-docset' functions, which are analogous to `info-lookup-symbol'
;; and `info-display-manual', respectively.  It's a bit jank - I haven't really
;; done that much testing and some docsets are in weird formats.  However, it is
;; a decent start.

;;; Code:

(require 'dom)
(require 'sqlite)
(require 'xml)



;;; Customize entries

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



;;; Internal functions

(defun zeal--url-from-file (filename)
  "Prepend `file://' to FILENAME."
  (format "file://%s" filename))

(defun zeal--remove-path-metadata (path)
  "Remove Dash metadata from fetched PATH.
Paths retrieved from docsets may have extra metadata enclosed in
angle brackets before the actual path name.  This does a simple
replacement to strip these tags."
  (with-temp-buffer
    (insert path)
    (goto-char (point-min))
    (when (re-search-forward "\\`<.*>" nil t)
      (replace-match ""))
    (buffer-string)))

(defun zeal--index-from-info-plist (info-plist)
  "Parse the XML file INFO-PLIST to retrieve its `dashIndexFilePath' entry.
Error if the entry is not present."
  (unless (file-exists-p info-plist)
    (error "File %s does not exist" info-plist))
  (let ((xml-dom
         (with-temp-buffer
           (insert-file-contents info-plist)
           (libxml-parse-xml-region))))
    ;; The plist in the DOM has a flat mapping like
    ;; (dict nil (key nil "a") (string nil "1") (key nil "b") (string nil "2")).
    ;; I want to take dict's children and extract the values, resulting in
    ;; ("a" "1" "b" "2").  Then, I can pair off the values to form a proper
    ;; alist and retrieve the entry associated with dashIndexFilePath.
    (zeal--remove-path-metadata
     (or (cadr
          (assoc
           "dashIndexFilePath"
           (seq-partition
            (mapcar #'caddr (dom-children (dom-by-tag xml-dom 'dict))) 2)))
         (error "Index file path not found in %s" info-plist)))))

(defun zeal--get-index-file (docset)
  "Find the index html file for DOCSET.
This examines the docset's `Info.plist' XML file to find the
docset's index page.  Some docsets are weird and don't give a
path, so exit with an error if that happens."
  (unless (file-directory-p (file-name-concat zeal-docset-dir docset))
    (error "Docset %s does not exist" docset))
  (let ((info-plist
         (file-name-concat zeal-docset-dir docset "Contents/Info.plist")))
    (file-name-concat zeal-docset-dir
                      docset
                      "Contents/Resources/Documents"
                      (zeal--index-from-info-plist info-plist))))

(defun zeal--get-symbol-path-alist (db)
  "Create an alist of (symbol . path) from the Zeal database DB.
The docset database should have a `searchIndex' table with `name'
and `path' columns that can be used for symbol lookup.  However,
some docsets are weird and don't follow the schema.  These cases
are not handled."
  (let ((result '())
        (query
         (sqlite-select db "select name, path from searchIndex" nil 'set)))
    (while (sqlite-more-p query)
      (let ((row (sqlite-next query)))
        (push `(,(car row) . ,(cadr row)) result)))
    result))



;;; Public functions

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
