.PHONY: install
install:
	emacs \
		--batch \
		--quick \
		--eval "(require 'package)" \
		--eval '(add-to-list (quote package-archives) (quote ("melpa" . "http://melpa.org/packages/")))' \
		--eval '(setq package-user-dir (concat default-directory ".emacs"))' \
		--eval '(package-initialize)' \
		--eval "(package-refresh-contents)" \
		--eval "(package-install-from-archive (cadr (assoc 'org package-archive-contents)))" \
		--eval "(package-install-from-archive (cadr (assoc 'htmlize package-archive-contents)))"

.PHONY: test
test:
	emacs \
		--batch \
		--quick \
		--eval '(setq package-user-dir (concat default-directory ".emacs"))' \
		--eval '(package-initialize)' \
		--eval '(normal-top-level-add-to-load-path (list "test" "."))' \
		--eval '(load "ox-blog-test.el")' \
		--eval '(ert-run-tests-batch-and-exit)'

.PHONY: export
export:
	emacs \
		--batch \
		--quick \
		--eval '(setq package-user-dir (concat default-directory ".emacs"))' \
		--eval '(package-initialize)' \
		--eval '(normal-top-level-add-to-load-path (list "."))' \
		--eval '(defun face-spec-set-match-display (&rest args) t)' \
		--eval "(load-theme 'wombat)" \
		--eval "(require 'ox-blog)" \
		--eval '(setq default-directory (expand-file-name "source/"))' \
		--eval '(hack-dir-local-variables-non-file-buffer)' \
		--eval '(let (org-confirm-babel-evaluate) (ox-blog-export :force-export t))'
