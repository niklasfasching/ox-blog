(require 'cl-lib)
(require 'ox-blog)
(require 'subr-x)


;;; Setup

(defvar ox-blog-test--test-directory
  (expand-file-name "test"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun ox-blog-test--with-file (name &rest body)
  (declare (indent 1))
  (let* ((content (string-join body "\n"))
         (file (expand-file-name name))
         (directory (file-name-directory file)))
    (unless (file-directory-p directory)) (mkdir directory t)
    (with-temp-file file (insert content))))

(defun ox-blog-test--with-project (project-cb)
  (let* ((default-directory ox-blog-test--test-directory)
         (source-directory (expand-file-name "source"))
         (export-directory (expand-file-name "export"))
         (post-file (expand-file-name "source/posts/post.org"))
         (draft-file (expand-file-name "source/drafts/draft.org"))
         (asset-file (expand-file-name "source/assets/style.css")))
    (ignore-errors
      (delete-directory source-directory 'recursive)
      (delete-directory export-directory 'recursive))
    (ox-blog-test--with-file draft-file
      "#+TITLE: Title"
      "#+SUBTITLE: Subtitle"
      "#+CATEGORIES: a b c")
    (ox-blog-test--with-file post-file
      "#+TITLE: Title"
      "#+SUBTITLE: Subtitle"
      "#+CATEGORIES: a b c"
      "#+DATE: <2017-01-01>"
      "content before first heading"
      "* First heading")
    (ox-blog-test--with-file asset-file)
    (make-directory export-directory)
    (let* ((ox-blog-project (list :source-directory source-directory
                                  :export-directory export-directory))
           (project (ox-blog--get-project)))
      (funcall project-cb))
    (delete-directory default-directory 'recursive)))

(defun ox-blog-test--with-server (server-cb)
  (ox-blog-test--with-project
   (lambda ()
     (mapc (lambda (p) (delete-process p)) (ox-blog--server-find nil nil))
     (let* ((test-port 9000)
            (project (plist-put project :server-port test-port))
            (server (ox-blog--server project))
            (base-url (format "http://localhost:%s" test-port)))
       (with-temp-file (expand-file-name "index.html" export-directory)
         (insert "html goes here"))
       (funcall server-cb)))))

(defun ox-blog-test--with-index (index-cb)
  (ox-blog-test--with-project
   (lambda ()
     (let* ((post-file (expand-file-name "source/posts/post.org"))
            (draft-file (expand-file-name "source/drafts/draft.org"))
            (org-files (list post-file draft-file))
            (file-nodes (mapcar (apply-partially 'ox-blog--file-node project)
                                org-files))
            (org-nodes (mapcar (lambda (file-node)
                                 (with-temp-buffer
                                   (insert-file-contents
                                    (plist-get file-node :source-path))
                                   (ox-blog--org-node project file-node)))
                               file-nodes)))
       (ox-blog--post-cache-wrap project
         (cl-loop for node in org-nodes
                  do (puthash (plist-get node :export-path) node cache))
         (funcall index-cb cache))))))



;;; directory-files

(ert-deftest ox-blog--directory-files/should-find-all-files-recursively ()
  (ox-blog-test--with-project
   (lambda ()
     (let* ((files (ox-blog--directory-files source-directory))
            (relative-files (mapcar (lambda (file)
                                      (file-relative-name file source-directory))
                                    files))
            (expected-relative-files '("posts/post.org" "assets/style.css"
                                       "drafts/draft.org")))
       (should (null (cl-set-difference relative-files expected-relative-files
                                        :test 'equal)))))))

(ert-deftest ox-blog--directory-files/should-exclude-excluded-files-and-directories ()
  (ox-blog-test--with-project
   (lambda ()
     (let* ((files (ox-blog--directory-files source-directory
                                             '("drafts/" "assets/")))
            (relative-files (mapcar (lambda (file)
                                      (file-relative-name file source-directory))
                                    files)))
       (should (equal relative-files '("posts/post.org")))))))


;;; get-project

(ert-deftest ox-blog--get-project/should-validate-project-definition ()
  (let ((get-project (lambda (ox-blog-project &optional overrides)
                       (condition-case err (ox-blog--get-project overrides)
                         (error (cadr err))))))
    (should (cl-search
             "Could not find ox-blog-project."
             (funcall get-project nil)))
    (should (cl-search
             "Value nil for key :source-directory does not match (directory)"
             (funcall get-project (list :export-directory "foo"))))
    (should (cl-search
             "Value nil for key :export-directory does not match (directory)."
             (funcall get-project (list :source-directory "/var/tmp/"))))
    (should (cl-search
             "Value foo for key :server-idle-seconds does not match (number)."
             (funcall get-project (list :source-directory "/var/tmp/"
                                        :export-directory "/var/tmp/"
                                        :server-idle-seconds "foo"))))))

(ert-deftest ox-blog--get-project/should-return-extended-project ()
  (let* ((ox-blog-project (list :source-directory "/var/tmp/"
                                :export-directory "/var/tmp/"
                                :server-idle-seconds 10))
         (overrides (list :server-idle-seconds 20))
         (project (ox-blog--get-project overrides)))
    (should (equal (plist-get project :source-directory) "/var/tmp/"))
    (should (equal (plist-get project :server-port) ox-blog-server-port))
    (should (equal (plist-get project :server-idle-seconds) 20))))



;;; file-node

(ert-deftest ox-blog--file-node/should-return-node-with-attributes ()
  (ox-blog-test--with-project
   (lambda ()
     (let ((asset-node (ox-blog--file-node project asset-file)))
       (should (equal (plist-get asset-node :source-path) asset-file))
       (should (equal (plist-get asset-node :export-path)
                      (expand-file-name "export/assets/style.css")))
       (should (equal (plist-get asset-node :href) "assets/style.css"))
       (should (equal (plist-get asset-node :extension) ".css"))
       (should (equal (plist-get asset-node :modified) t))))))

(ert-deftest ox-blog--file-node/should-correctly-set-modified-attribute ()
  (ox-blog-test--with-project
   (lambda ()
     (make-directory (expand-file-name "assets" export-directory))
     (with-temp-file (expand-file-name "assets/style.css" export-directory))
     (let* ((asset-file (expand-file-name "source/assets/style.css"))
            (asset (ox-blog--file-node project asset-file)))
       (should (equal (plist-get asset :modified) nil))))))


;;; org-node

(ert-deftest ox-blog--org-node/should-return-node-with-attributes ()
  (ox-blog-test--with-project
   (lambda ()
     (let* ((file-node (ox-blog--file-node project post-file))
            (org-node (with-temp-buffer
                        (insert-file-contents post-file)
                        (ox-blog--org-node project file-node))))
       (should (equal '() (cl-set-difference file-node org-node)))
       (should (equal (plist-get org-node :title) "Title"))
       (should (equal (plist-get org-node :subtitle) "Subtitle"))
       (should (equal (format-time-string "%Y-%m-%d" (plist-get org-node :date))
                      "2017-01-01"))
       (should (equal (plist-get org-node :categories) '("a" "b" "c")))
       (should (equal (plist-get org-node :excerpt)
                      "<p>\ncontent before first heading\n</p>\n"))
       (should (equal (plist-get org-node :draft) nil))))))

(ert-deftest ox-blog--org-node/should-correctly-set-draft-attribute ()
  (ox-blog-test--with-project
   (lambda ()
     (let* ((file-node (ox-blog--file-node project draft-file))
            (org-node (with-temp-buffer
                        (insert-file-contents draft-file)
                        (ox-blog--org-node project file-node))))
       (should (equal (plist-get org-node :draft) t))))))


;;; export-file

(ert-deftest ox-blog--export-file/should-copy-asset-to-export-directory-and-return-node ()
  (ox-blog-test--with-project
   (lambda ()
     (let ((file-node (ox-blog--file-node project asset-file)))
       (should (not (file-exists-p (plist-get file-node :export-path))))
       (should (equal file-node (ox-blog--export-file project file-node)))
       (should (file-exists-p (plist-get file-node :export-path)))))))

(ert-deftest ox-blog--export-file/should-export-org-to-export-directory-and-return-node ()
  (ox-blog-test--with-project
   (lambda ()
     (let* ((file-node (ox-blog--file-node project post-file)))
       (should (not (file-exists-p (plist-get file-node :export-path))))
       (let ((org-node (ox-blog--export-file project file-node)))
         (should (equal (plist-get org-node :title) "Title")))
       (should (file-exists-p (plist-get file-node :export-path)))))))


;;; server

(ert-deftest ox-blog--server/should-respond-to-get-with-file ()
  (ox-blog-test--with-server
   (lambda ()
     (let* ((url (concat base-url "/index.html"))
            (response (with-current-buffer (url-retrieve-synchronously url) (buffer-string)))
            (lines (split-string response "\n")))
       (should (equal lines '("HTTP/1.1 200"
                              "Content-Type: text/html"
                              "Content-Length: 14"
                              ""
                              "html goes here")))))))

(ert-deftest ox-blog--server/respond-to-get-with-index.html-if-file-does-not-exist ()
  (ox-blog-test--with-server
   (lambda ()
     (let* ((url (concat base-url "/"))
            (response (with-current-buffer (url-retrieve-synchronously url) (buffer-string)))
            (lines (split-string response "\n")))
       (should (equal lines '("HTTP/1.1 200"
                              "Content-Type: text/html"
                              "Content-Length: 14"
                              ""
                              "html goes here")))))))

(ert-deftest ox-blog--server/should-not-respond-to-post-until-explicitly-called ()
  (ox-blog-test--with-server
   (lambda ()
     (let* ((url (concat base-url "/index.html"))
            (url-request-method "POST")
            (resolve-time 0)
            (refresh-time 0)
            (response "")
            (response-cb (lambda (_status) (setq resolve-time (current-time)
                                                 response (buffer-string))))
            (timer-cb (lambda (project) (setq refresh-time (current-time))
                        (ox-blog--server-refresh-clients project)))
            refresh-time resolve-time response)
       (url-retrieve url response-cb)
       (run-with-timer 0.005 nil timer-cb project)
       (should (and (not refresh-time) (not resolve-time)))
       (sleep-for 0.01)
       (should (time-less-p refresh-time resolve-time))
       (should (equal (split-string response "\n") '("HTTP/1.1 200"
                                                     "Content-Type: text/plain"
                                                     "Content-Length: 0"
                                                     ""
                                                     "")))))))

(ert-deftest ox-blog--server/should-only-start-one-server-and-idle-kill-timer-per-project ()
  (ox-blog-test--with-server
   (lambda ()
     (let ((server-a server)
           (timer-count-before (length timer-list))
           (server-b (ox-blog--server project))
           (timer-count-after (length timer-list)))
       (should (equal timer-count-before timer-count-after))
       (should (equal server-a server-b))))))

(ert-deftest ox-blog--server/should-refresh-server-clients-if-server-already-exists ()
  (ox-blog-test--with-server
   (lambda ()
     (let* ((url (concat base-url "/index.html"))
            (url-request-method "POST")
            (responded nil)
            (cb (lambda (_status) (setq responded t))))
       (url-retrieve url cb)
       (sleep-for 0.01)
       (should (not responded))
       (ox-blog--server project)
       (sleep-for 0.01)
       (should responded)))))

(ert-deftest ox-blog--server/should-kill-server-on-idle ()
  (ox-blog-test--with-project
   (lambda ()
     (mapc (lambda (p) (delete-process p)) (ox-blog--server-find nil nil))
     (let* ((project (plist-put project :server-idle-seconds 0.01))
            (server (ox-blog--server project))
            (base-url (format "http://localhost:%s" ox-blog-server-port))
            (url (concat base-url "/index.html")))
       (ox-blog--server project)
       (with-current-buffer (url-retrieve-synchronously url)
         (should (cl-search "HTTP/1.1" (buffer-string))))
       (sleep-for 0.02)
       (with-current-buffer (url-retrieve-synchronously url)
         (should (not (cl-search "HTTP/1.1" (buffer-string)))))
       (should (null (ox-blog--server-find nil nil)))))))


;;; index

(ert-deftest ox-blog--index/should-use-index-head-over-html-head ()
  (ox-blog-test--with-index
   (lambda (cache)
     (let ((project (org-combine-plists project '(:html-head "HTMLHEAD"))))
       (ox-blog--index project cache)
       (with-temp-buffer
         (insert-file-contents (expand-file-name "index.html" export-directory))
         (should (search-forward "HTMLHEAD"))))
     (let ((project (org-combine-plists project (list :html-head "HTMLHEAD"
                                                      :index-head "INDEXHEAD"))))
       (ox-blog--index project cache)
       (with-temp-buffer
         (insert-file-contents (expand-file-name "index.html" export-directory))
         (should (search-forward "INDEXHEAD"))
         (should-error (search-forward "HTMLHEAD")))))))

(ert-deftest ox-blog--index/should-list-drafts-depending-on-with-drafts ()
  (ox-blog-test--with-index
   (lambda (cache)
     (ox-blog--index (plist-put project :with-drafts t) cache)
     (with-temp-buffer
       (insert-file-contents (expand-file-name "index.html" export-directory))
       (keep-lines "</li>")
       (let ((lis (split-string (buffer-string) "\n" t)))
         (should (equal (length lis) 2))))

     (ox-blog--index (plist-put project :with-drafts nil) cache)
     (with-temp-buffer
       (insert-file-contents (expand-file-name "index.html" export-directory))
       (keep-lines "</li>")
       (let ((lis (split-string (buffer-string) "\n" t)))
         (should (equal (length lis) 1))))
     )))


;;; export

(ert-deftest ox-blog-export/should-export-modified-only-correctly ()
  (ox-blog-test--with-project
   (lambda ()
     (let* ((post-node (ox-blog--file-node project post-file))
            (export-path (plist-get post-node :export-path))
            modified-before
            modified-not-forced
            modified-forced)
       (ox-blog-export :force-export nil)
       (setq modified-before (nth 5 (file-attributes export-path)))
       (ox-blog-export :force-export nil)
       (setq modified-not-forced (nth 5 (file-attributes export-path)))
       (should (equal modified-before modified-not-forced))
       (ox-blog-export :force-export t)
       (setq modified-forced (nth 5 (file-attributes export-path)))
       (should (not (equal modified-before modified-forced)))))))

(ert-deftest ox-blog-export/should-delete-leftover-files-from-export-directory-and-cache ()
  (ox-blog-test--with-project
   (lambda ()
     (let* ((post-node (ox-blog--file-node project post-file))
            (export-path (plist-get post-node :export-path)))
       (ox-blog-export)
       (should (file-exists-p export-path))
       (ox-blog--post-cache-wrap project
         (should (gethash export-path cache)))
       (delete-file post-file)
       (ox-blog-export)
       (should (not (file-exists-p export-path)))
       (ox-blog--post-cache-wrap project
         (should (null (gethash export-path cache))))))))

(ert-deftest ox-blog-export/should-populate-index-from-cache ()
  (ox-blog-test--with-project
   (lambda ()
     (let* ((post-node (ox-blog--file-node project post-file))
            (export-path (plist-get post-node :export-path))
            modified-1
            modified-2)
       (ox-blog-export :force-export nil)
       (setq modified-1 (nth 5 (file-attributes export-path)))
       (ox-blog-export :force-export nil)
       (setq modified-2 (nth 5 (file-attributes export-path)))
       (should (equal modified-1 modified-2))
       (with-temp-buffer
         (insert-file-contents (expand-file-name "index.html" export-directory))
         (keep-lines "</li>")
         (let ((lis (split-string (buffer-string) "\n" t)))
           (should (equal (length lis) 2))))))))


;;;; publish

(ert-deftest ox-blog-publish/should-ensure-export-directory-is-repository-with-origin ()
  (ox-blog-test--with-project
   (lambda ()
     (let ((default-directory export-directory))
       (should
        (equal "Export directory has to be a git repository with remote origin."
               (condition-case err (ox-blog-publish) (error (cadr err)))))
       (shell-command "git init")
       (should
        (equal "Export directory has to be a git repository with remote origin."
               (condition-case err (ox-blog-publish) (error (cadr err)))))
       (shell-command "git remote add origin some-url")
       (cl-letf* ((executed nil)
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _)
                                                    (setq executed t)
                                                    nil)))
         (ox-blog-publish)
         (should executed))))))

(ert-deftest ox-blog-publish/should-push-exported-to-origin ()
  (ox-blog-test--with-project
   (lambda ()
     (cl-letf* ((default-directory export-directory)
                ((symbol-function 'yes-or-no-p)  (lambda (&rest _) t)))
       (shell-command "git init --bare ../remote")
       (shell-command "git init")
       (shell-command (format "git remote add origin %s"
                              (expand-file-name "../remote")))
       (should (cl-search
                "fatal"
                (shell-command-to-string "cd ../remote && git log")))
       (ox-blog-publish)
       (should (cl-search
                "commit"
                (shell-command-to-string "cd ../remote && git log")))))))
