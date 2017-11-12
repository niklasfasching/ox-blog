;;; ox-blog.el --- Blog backend for Org Export Engine -*- lexical-binding: t -*-

;; Copyright (C) 2017 Niklas Fasching

;; Author: Niklas Fasching
;; Homepage: https://github.com/niklasfasching/ox-blog
;; Package-Requires: ((emacs "25") (org-plus-contrib "8.3.2") (htmlize "1"))
;; Version: 0.1
;; Keywords: files tools org blog
;; Homepage: https://github.com/magit/magit

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This package provides a blog backend for the org export engine.
;; See the README for more details.

;;; Code:

(require 'cl-lib)
(require 'htmlize)
(require 'mailcap) ; file extension mime types for server
(require 'ox)
(require 'subr-x)
(require 'wid-edit) ; validation based on defcustom types


;;; variables

(defgroup org-export-blog nil
  "Customizable default values for ox-blog-project properties."
  :tag "Org Export Blog"
  :group 'org-export)

(defcustom ox-blog-project nil
  "Project definition.  Should be set inside .dir-locals.el."
  :type '(plist :key-type symbol)
  :safe (lambda (_) t)
  :group 'org-export-blog)

(defcustom ox-blog-excluded-files nil
  "List of file and directory names to exclude from export.
See `ox-blog--directory-files'."
  :tag ":excluded-files"
  :type '(repeat string)
  :group 'org-export-blog)

(defcustom ox-blog-force-export nil
  "Whether to (re-)export unmodified files."
  :tag ":force-export"
  :type '(boolean)
  :group 'org-export-blog)

(defcustom ox-blog-with-drafts t
  "Whether to list drafts on the index."
  :tag ":with-drafts"
  :type '(boolean)
  :group 'org-export-blog)

(defcustom ox-blog-index-head nil
  "HTML string embedded into the html of the index page.
If set, use this value instead of :html-head for the index page."
  :tag ":index-head"
  :type '(choice (string) (const nil))
  :group 'org-export-blog)

(defcustom ox-blog-babel-header-args nil
  "See `org-babel-default-header-args' for details.
This variable is merged with and overrides the value
of `org-babel-default-header-args' during export."
  :tag ":babel-header-args"
  :type '(alist :key-type symbol :value-type string)
  :group 'org-export-blog)

(defcustom ox-blog-babel-evaluate t
  "See `org-export-babel-evaluate' for details.
This variable overrides the value of `org-export-babel-evaluate' during export."
  :tag ":babel-evaluate"
  :type '(boolean)
  :group 'org-export-blog)

(defcustom ox-blog-server-idle-seconds 60
  "The delevopment server is closed when no clients are connected.
This variable determines the interval (in seconds) at which the idle state of
the server is checked."
  :tag ":server-idle-seconds"
  :type '(number)
  :group 'org-export-blog)

(defcustom ox-blog-server-port 8000
  "Development server port."
  :tag ":server-port"
  :type '(number)
  :group 'org-export-blog)

(defcustom ox-blog-server-livereload-script "
<script>
fetch(location.pathname, {method: 'POST'}).then(() => location.reload());
</script>"
  "HTML string (livereload script) embedded in the html head by the development
server."
  :tag ":server-livereload-script"
  :type '(string)
  :group 'org-export-blog)

(defcustom ox-blog-server-base-url ""
  "Base url of the blog (without trailing slash).
During development the blog is served from /. If the base url of the blog
is different, requests have to be rewritten to remove the base-url."
  :tag ":server-base-url"
  :type '(string)
  :group 'org-export-blog)

(defcustom ox-blog-index-template "
#+TITLE: Home
#+BEGIN_EXPORT html
<ul class='index'>
%i
</ul>
<script>
const originalTitle = document.querySelector('.title').textContent;
function updateIndex() {
  const category = window.location.hash.slice(1);
  if (category) {
    document.querySelector('.title').textContent =
      category.charAt(0).toUpperCase() + category.slice(1);
    document.querySelectorAll('.index li')
      .forEach(li => {
        const categories = li.dataset.categories.split(' ');
        if (categories.includes(category)) {
          li.style.display = 'block';
        } else {
          li.style.display = 'none';
        }
      });
  } else {
    document.querySelector('.title').textContent = originalTitle;
    document.querySelectorAll('.index li')
      .forEach(li => li.style.display = 'block');
  }
}
window.addEventListener('hashchange', updateIndex);
updateIndex();
</script>
#+END_EXPORT"
  "Format string of the index body (org syntax).
%i is replaced with the index items."
  :tag ":index-template"
  :type '(string)
  :group 'org-export-blog)

(defcustom ox-blog-index-format-function 'ox-blog--index-format
  "Called to create the index body (org syntax). See `ox-blog-index-template'."
  :tag ":index-format-function"
  :type '(function)
  :group 'org-export-blog)

(defcustom ox-blog-index-item-format-function 'ox-blog--index-item-format
  "Called to create the index item HTML string."
  :tag ":index-item-format-function"
  :type '(function)
  :group 'org-export-blog)

(defcustom ox-blog-page-category "page"
  "Category for posts that Called to create the index item HTML string."
  :tag ":page-category"
  :type '(string)
  :group 'org-export-blog)


;;;; export backend

(org-export-define-derived-backend 'blog 'html
  :menu-entry
  '(?b "Export blog"
       ((?e "Export" (lambda (&rest _) (ox-blog-export :with-drafts nil)))
        (?d "Export with drafts" (lambda (&rest _) (ox-blog-export)))
        (?p "Export & publish" (lambda (&rest _) (ox-blog-publish)))))
  :options-alist '((:categories "CATEGORIES"))
  :filters-alist '((:filter-body . ox-blog--extend-body)))

(defun ox-blog--extend-body (body-html _ info)
  (if-let ((org-node (plist-get info :org-node))
           (categories (plist-get org-node :categories))
           (is-post (not (member (plist-get info :page-category) categories)))
           (path (concat "/" (plist-get org-node :href)))
           (path-to-root (file-relative-name "/" path))
           (category-to-li (lambda (category)
                             (format-spec "<li><a href='%r#%c'>%c</a></li>"
                                          `((?r . ,path-to-root)
                                            (?c . ,category)))))
           (lis (mapcar category-to-li categories))
           (html (string-join `("<ul class='categories'>" ,@lis "</ul>") "\n")))
      (string-join (list html body-html) "\n")
    body-html))


;;;; utility

(defun ox-blog--get-project (&optional overrides)
  "Return project plist.
The project plist is created by merging the following plists
1. defaults (customizable variables). See `org-export-blog' group
2. `ox-blog-project'
3. OVERRIDES"
  (unless ox-blog-project (error "Could not find ox-blog-project."))
  (let* ((constants '((:source-directory (directory))
                      (:export-directory (directory))))
         (custom-group (get 'org-export-blog 'custom-group))
         (customizables (cl-loop for (symbol) in custom-group
                                 if (get symbol 'custom-tag)
                                 collect (list (intern (get symbol 'custom-tag))
                                               (get symbol 'custom-type)
                                               (symbol-value symbol))))
         (defaults (append constants customizables))
         (default-project (cl-loop for (key _type value) in defaults
                                   nconc (list key value)))
         (project (org-combine-plists default-project ox-blog-project
                                      overrides)))
    (cl-loop for (key type _value) in defaults
             do (unless (widget-apply (widget-convert type)
                                      :match (plist-get project key))
                  (error (concat "Invalid project. "
                                 "Value %s for key %s does not match %s.\n"
                                 "Project: %s")
                         (plist-get project key) key type project)))
    project))

(defun ox-blog--directory-files (directory &optional excluded)
  "Return list of files under DIRECTORY excluding files in EXCLUDED.
EXCLUDED is a list of file and directory names.
Directories are marked by a trailing slash."
  (let ((files nil))
    (dolist (file (if directory (file-name-all-completions "" directory)))
      (unless (member file (append '("./" "../") excluded))
        (let ((full-file (expand-file-name file directory)))
          (if (directory-name-p full-file)
              (setq files (nconc files
                                 (ox-blog--directory-files full-file excluded)))
            (push full-file files)))))
    files))

(defmacro ox-blog--post-cache-wrap (project &rest body)
  (declare (indent 1))
  `(let* ((file (expand-file-name "ox-blog-post-cache.el"
                                  (plist-get project :export-directory)))
          (cache (ignore-errors (with-temp-buffer (insert-file-contents file)
                                                  (read (buffer-string)))))
          (cache (or cache (make-hash-table :test 'equal)))
          print-level
          print-length)
     ,@body
     (with-temp-file file (insert (prin1-to-string cache)))))

(defun ox-blog--after-save-hook ()
  (if-let ((ox-blog-project ox-blog-project)
           (source-directory (plist-get ox-blog-project :source-directory))
           (is-source-file (string-prefix-p (file-truename source-directory)
                                            default-directory)))
      (ox-blog-export)))

(define-minor-mode ox-blog-mode
  "Minor mode to assist ox-blog project management.
When enabled, the project containing the current file is exported on save."
  :lighter " ox-blog"
  (if ox-blog-mode
      (add-hook 'after-save-hook 'ox-blog--after-save-hook)
    (remove-hook 'after-save-hook 'ox-blog--after-save-hook)))


;;;; export

(defun ox-blog--export-file (project file-node)
  (let* ((export-path (plist-get file-node :export-path))
         (export-directory (file-name-directory export-path)))
    (unless (file-directory-p export-directory) (mkdir export-directory t))
    (pcase (plist-get file-node :extension)
      (".org" (ox-blog--export-org-file project file-node))
      (_ (ox-blog--export-asset-file project file-node)))))

(defun ox-blog--export-asset-file (project file-node)
  (copy-file (plist-get file-node :source-path)
             (plist-get file-node :export-path) t)
  file-node)

(defun ox-blog--export-org-file (project file-node)
  (let* ((org-inhibit-startup t)
         (babel-header-args (plist-get project :babel-header-args))
         (org-babel-default-header-args (org-combine-plists
                                         org-babel-default-header-args
                                         babel-header-args))
         (org-export-babel-evaluate (plist-get project :babel-evaluate))
         (org-file (plist-get file-node :source-path))
         (default-directory (file-name-directory org-file))
         (visited (find-buffer-visiting org-file))
         (buffer (find-file-noselect org-file)))
    (with-current-buffer buffer
      (let* ((org-node (ox-blog--org-node project file-node))
             (extended-project (append (list :org-node org-node) project))
             (html (org-export-as 'blog nil nil nil extended-project)))
        (with-temp-file (plist-get org-node :export-path) (insert html))
        (unless visited (kill-buffer buffer))
        org-node))))

(defun ox-blog--get-excerpt (project)
  (save-restriction
    (org-forward-heading-same-level 1)
    (narrow-to-region (point-min) (point))
    (org-export-as 'html nil t t project)))

(defun ox-blog--org-node (project file-node)
  (let* ((plist (org-export-get-environment 'blog))
         (excerpt (ox-blog--get-excerpt project))
         (org-date (assq 'timestamp (plist-get plist :date)))
         (ISO-8601 "%Y%m%dT%H%M%S")
         (date (if org-date (date-to-time (org-timestamp-format org-date
                                                                ISO-8601))))
         (draft (null date))
         (categories (split-string (or (plist-get plist :categories) "") " " t))
         (title (org-no-properties (car (plist-get plist :title))))
         (subtitle (org-no-properties (car (plist-get plist :subtitle)))))
    (org-combine-plists file-node (list :draft draft :date date
                                        :title title :subtitle subtitle
                                        :categories categories
                                        :excerpt excerpt))))

(defun ox-blog--file-node (project source-path)
  (let* ((source-directory (plist-get project :source-directory))
         (relative-path (file-relative-name source-path source-directory))
         (extension (file-name-extension source-path t))
         (relative-export-path
          (if (equal extension ".org")
              (concat (file-name-sans-extension relative-path) ".html")
            relative-path))
         (export-path (expand-file-name relative-export-path
                                        (plist-get project :export-directory)))
         (source-modified (nth 5 (file-attributes source-path)))
         (export-modified (nth 5 (file-attributes export-path)))
         (modified (or (not export-modified) (time-less-p export-modified
                                                          source-modified))))
    (list :source-path source-path :export-path export-path
          :href relative-export-path :extension extension :modified modified)))

(defun ox-blog-export (&rest options)
  (let ((start-time (float-time))
        (project (ox-blog--get-project options)))
    (ox-blog--post-cache-wrap project
      (ox-blog--export-files project cache)
      (ox-blog--index project cache)
      (ox-blog--server project)
      (message "Export finished in %s seconds" (- (float-time) start-time)))))

(defun ox-blog--export-files (project cache)
  (let* ((source-directory (plist-get project :source-directory))
         (export-directory (plist-get project :export-directory))
         (excluded-files (plist-get project :excluded-files))
         (source-files (ox-blog--directory-files source-directory
                                                 excluded-files))
         (source-nodes (mapcar (apply-partially 'ox-blog--file-node project)
                               source-files))
         (export-files (mapcar (lambda (node) (plist-get node :export-path))
                               source-nodes))
         (exported-files (ox-blog--directory-files export-directory
                                                   '(".git/" ".gitignore"
                                                     "ox-blog-post-cache.el")))
         (leftover-files (cl-set-difference exported-files export-files
                                            :test 'equal)))
    (cl-loop for node in source-nodes
             if (or (plist-get project :force-export)
                    (plist-get node :modified))
             for export-node = (ox-blog--export-file project node)
             if (and (equal (plist-get export-node :extension) ".org")
                     (not (member (plist-get project :page-category)
                                  (plist-get export-node :categories))))
             do (puthash (plist-get export-node :export-path) export-node cache))
    (mapc (lambda (file) (delete-file file) (remhash file cache))
          leftover-files)))


;;;; index

(defun ox-blog--index (project cache)
  (let* ((html-file (expand-file-name "index.html"
                                      (plist-get project :export-directory)))
         (html-head (or (plist-get project :index-head)
                        (plist-get project :html-head)))
         (project (plist-put (copy-tree project) :html-head html-head))
         (with-drafts (plist-get project :with-drafts))
         (nodes (hash-table-values cache))
         (nodes (cl-remove-if (lambda (x) (and (not with-drafts)
                                               (plist-get x :draft)))
                              nodes))
         (nodes (sort nodes (lambda (a b) (time-less-p (plist-get b :date)
                                                       (plist-get a :date)))))
         (index-format-function (plist-get project :index-format-function)))
    (with-temp-buffer
      (insert (funcall index-format-function project nodes))
      (let ((html (org-export-as 'html nil nil nil project)))
        (with-temp-file html-file (insert html))))))

(defun ox-blog--index-format (project org-nodes)
  (let* ((index-template (plist-get project :index-template))
         (item-format-function (plist-get project :index-item-format-function))
         (items (mapconcat (apply-partially item-format-function project)
                           org-nodes "\n")))
    (format-spec index-template `((?i . ,items)))))

(defun ox-blog--index-item-format (project org-node)
  (format-spec (concat "<li class='%s' data-categories='%c'>"
                       "<a href='%h'><date>%d</date><span>%t</span></a>"
                       "</li>")
               `((?s . ,(if (plist-get org-node :draft) "draft" "post"))
                 (?h . ,(plist-get org-node :href))
                 (?d . ,(format-time-string "%Y-%m-%d"
                                            (plist-get org-node :date)))
                 (?t . ,(plist-get org-node :title))
                 (?c . ,(string-join (plist-get org-node :categories) " ")))))


;;;; server

(defun ox-blog--server (project)
  "Start and return development server for PROJECT.
If server is already running, refresh its clients.
Files are served from :export-directory of PROJECT.
If :base-url is set, :base-url is removed from beginning of path if applicable."
  (let* ((name "ox-blog--server")
         (port (plist-get project :server-port))
         (directory (plist-get project :export-directory))
         (base-url (plist-get project :server-base-url))
         (idle-seconds (plist-get project :server-idle-seconds))
         (plist (list :directory directory :ox-blog t :server t :project project
                      :base-url base-url))
         (existing-server (car (ox-blog--server-find directory :server)))
         (server (or existing-server
                     (make-network-process
                      :name name :filter 'ox-blog--server-on-chunk
                      :log 'ox-blog--server-on-connect :plist plist
                      :server t :service port :family 'ipv4 :noquery t))))
    (if existing-server
        (ox-blog--server-refresh-clients project)
      (run-with-timer idle-seconds nil 'ox-blog--server-kill-on-idle
                      idle-seconds directory))
    server))

(defun ox-blog--server-kill-on-idle (idle-seconds directory)
  "Kill server for DIRECTORY :export-directory if no clients are connected.
Otherwise reschedule check by IDLE-SECONDS."
  (let ((clients (ox-blog--server-find directory :client))
        (server (car (ox-blog--server-find directory :server))))
    (cond (clients (run-with-timer idle-seconds nil 'ox-blog--server-kill-on-idle
                                   idle-seconds directory))
          (server (delete-process server)))))

(defun ox-blog--server-refresh-clients (project)
  "Respond to all clients awaiting a response from the server for PROJECT."
  (mapcar (lambda (client)
            (with-temp-buffer (ox-blog--server-respond client "text/plain" 200)))
          (ox-blog--server-find (plist-get project :export-directory) :client)))

(defun ox-blog--server-find (&optional directory property)
  "Return list of ox-blog-server processes.
Optionally, filter returned list by DIRECTORY (project :export-directory)
and PROPERTY (:client or :server)."
  (cl-remove-if-not
   (lambda (proc) (and (process-get proc :ox-blog)
                       (if directory (equal directory
                                            (process-get proc :directory))
                         t)
                       (if property (process-get proc property) t)))
   (process-list)))

(defun ox-blog--server-on-connect (server client message)
  (set-process-sentinel client
                        (apply-partially 'ox-blog--server-sentinel server))
  (set-process-plist client (list :buffer (generate-new-buffer "*ox-blog*")
                                  :directory (process-get server :directory)
                                  :base-url (process-get server :base-url)
                                  :project (process-get server :project)
                                  :ox-blog t :client t)))

(defun ox-blog--server-sentinel (server client message)
  (unless (string-match-p "^open" message)
    (kill-buffer (process-get client :buffer))))

(defun ox-blog--server-on-chunk (client chunk)
  "Handle HTTP requests from clients.
POST requests are logged and kept open (until the next export,
see `ox-blog--server-refresh-clients').
GET requests are answered with the corresponding file ath path."
  (with-current-buffer (process-get client :buffer)
    (setf (point) (point-max))
    (insert chunk)
    (setf (point) (point-min))
    (when-let ((match (looking-at "\\([^ ]+\\) +\\([^ ]+\\) +[^\r]+\r\n"))
               (method (match-string 1))
               (path (url-unhex-string (match-string 2)))
               (path (string-remove-prefix (process-get client :base-url) path))
               (parts (split-string path "/"))
               (sanitized-path (string-join (cl-set-difference parts '("" "..")
                                                               :test 'equal)
                                            "/"))
               (export-path (expand-file-name sanitized-path
                                              (process-get client :directory)))
               (full-path (if (file-directory-p export-path)
                              (expand-file-name "index.html" export-path)
                            export-path)))
      (erase-buffer)
      (cond ((string= method "POST")
             (let ((inhibit-message t))
               (message "ox-blog client %s connected" path)))
            ((string= method "GET")
             (ox-blog--server-send-file client full-path))))))

(defun ox-blog--server-send-file (client path)
  (with-temp-buffer
    (if (not (file-exists-p path))
        (ox-blog--server-respond client "" 400)
      (let ((mime (mailcap-extension-to-mime (file-name-extension path t)))
            (script (plist-get (process-get client :project)
                               :server-livereload-script)))
        (insert-file-contents path)
        (if (and (string= mime "text/html") (search-forward "<head>" nil t))
            (replace-match (concat "<head>\n" script)))
        (ox-blog--server-respond client mime 200)))))

(defun ox-blog--server-respond (client mime status)
  (let ((content-length (buffer-size)))
    (with-temp-buffer
      (insert (format "HTTP/1.1 %d\r\n" status))
      (insert (format "Content-Type: %s\r\n" mime))
      (insert (format "Content-Length: %s\r\n\r\n" content-length))
      (process-send-region client (point-min) (point-max)))
    (process-send-region client (point-min) (point-max)))
  (delete-process client))


;;; publish
(defun ox-blog-publish ()
  (let* ((project (ox-blog--get-project))
         (default-directory (plist-get project :export-directory))
         (git-root (string-trim
                    (shell-command-to-string "git rev-parse --show-toplevel")))
         (is-repository (file-equal-p default-directory git-root))
         (remote-exists (eq 0 (shell-command "git remote get-url origin"))))
    (unless (and is-repository remote-exists)
      (error "Export directory has to be a git repository with remote origin."))
    (ox-blog-export :with-drafts nil :force-export t)
    (shell-command "git add --all")
    (shell-command "git rm --cached .ox-blog-post-cache.el")
    (shell-command "git commit -m $(date +%Y-%m-%d)")
    (when (yes-or-no-p "Push to production?")
      (message "Pushing to production")
      (shell-command "git push -u origin $(git rev-parse --abbrev-ref HEAD)")
      (message "Done!"))))

(provide 'ox-blog)

;;; ox-blog.el ends here
