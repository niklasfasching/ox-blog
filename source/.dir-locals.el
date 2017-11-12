((nil . (;; export blog whenever a file in the source directory is changed
         (eval . (ox-blog-mode 1))
         ;; normally :source-directory & :export-directory must be absolute to
         ;; enable exporting from any subdirectory.
         ;; For the demo blog relative paths are possible & required to enable
         ;; exporting the blog independent of where the ox-blog repository lies.
         ;; This only works because export is only ever executed from the
         ;; ./source directory

         (ox-blog-project . (:source-directory "../source/"
							 :export-directory "../docs/"
                             :excluded-files (".dir-locals.el")
                             :server-base-url "/ox-blog"
                             :html-head "
<link rel='stylesheet' href='/ox-blog/main.css' type='text/css'/>"
                             :index-head "
<meta name='description' content='ox-blog demo project'>
<link rel='stylesheet' href='/ox-blog/main.css' type='text/css'/>
<link rel='stylesheet' href='/ox-blog/index.css' type='text/css'/>"
                             :html-preamble "
<header>
  <a class='logo' href='/ox-blog/'>home</a>
  <nav>
    <a href='https://www.github.com/niklasfasching/ox-blog'>github</a>
    <a href='about.html'>about</a>
  </nav>
</header>"
                             :html-postamble ""
                             :html-head-include-scripts nil
                             :html-head-include-default-style nil
                             :time-stamp-file nil
                             :with-sub-superscript {}
                             :with-toc nil)))))
