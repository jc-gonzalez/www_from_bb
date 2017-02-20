;;; setup.el --- Specification of JC Gonzalez Personal Web Site
;;; Code:
;;; Commentary:

(setq www-base-dir "~/Personal/www/")

(add-to-list 'load-path (concat www-base-dir (convert-standard-filename "publishing/lisp")))
;(add-to-list 'load-path (concat www-base-dir (convert-standard-filename "elisp/org-mode/lisp")))
;(add-to-list 'load-path (concat www-base-dir (convert-standard-filename "elisp/org-mode/contrib/lisp")))

(require 'org-install)
(require 'org)
(require 'ox)
(require 'ox-publish)
(require 'ox-html)

;(load-file (concat www-base-dir (convert-standard-filename "publishing/lisp/ox-rss")))
(require 'ox-rss)

;;;; For References
;;
;; `org-export-get-reference' associate a unique reference for any
;; object or element.
;;
;; `org-export-get-ordinal' associates a sequence number to any object
;; or element.

(defun org-export-get-reference (datum info)
  "Return a unique reference for DATUM, as a string.
DATUM is either an element or an object.  INFO is the current
export state, as a plist.  Returned reference consists of
alphanumeric characters only."
  (let ((type (org-element-type datum))
        (cache (or (plist-get info :internal-references)
                   (let ((h (make-hash-table :test #'eq)))
                     (plist-put info :internal-references h)
                     h))))
    (or (gethash datum cache)
        (puthash datum
                 (format "org%s%d"
                         (if type
                             (replace-regexp-in-string "-" "" (symbol-name type))
                           "secondarystring")
                         (incf (gethash type cache 0)))
                 cache))))

(defun org-export-get-ordinal (element info &optional types predicate)
  "Return ordinal number of an element or object.

ELEMENT is the element or object considered.  INFO is the plist
used as a communication channel.

Optional argument TYPES, when non-nil, is a list of element or
object types, as symbols, that should also be counted in.
Otherwise, only provided element's type is considered.

Optional argument PREDICATE is a function returning a non-nil
value if the current element or object should be counted in.  It
accepts two arguments: the element or object being considered and
the plist used as a communication channel.  This allows to count
only a certain type of objects (i.e. inline images).

Return value is a list of numbers if ELEMENT is a headline or an
item.  It is nil for keywords.  It represents the footnote number
for footnote definitions and footnote references.  If ELEMENT is
a target, return the same value as if ELEMENT was the closest
table, item or headline containing the target.  In any other
case, return the sequence number of ELEMENT among elements or
objects of the same type."
  ;; Ordinal of a target object refer to the ordinal of the closest
  ;; table, item, or headline containing the object.
  (when (eq (org-element-type element) 'target)
    (setq element
          (org-element-lineage
           element
           '(footnote-definition footnote-reference headline item table))))
  (case (org-element-type element)
    ;; Special case 1: A headline returns its number as a list.
    (headline (org-export-get-headline-number element info))
    ;; Special case 2: An item returns its number as a list.
    (item (let ((struct (org-element-property :structure element)))
            (org-list-get-item-number
             (org-element-property :begin element)
             struct
             (org-list-prevs-alist struct)
             (org-list-parents-alist struct))))
    ((footnote-definition footnote-reference)
     (org-export-get-footnote-number element info))
    (otherwise
     (let ((counter 0))
       ;; Increment counter until ELEMENT is found again.
       (org-element-map (plist-get info :parse-tree)
           (or types (org-element-type element))
         (lambda (el)
           (cond
            ((eq element el) (1+ counter))
            ((not predicate) (incf counter) nil)
            ((funcall predicate el info) (incf counter) nil)))
         info 'first-match)))))

(setf user-full-name "J C Gonzalez")
(setf user-mail-address "jcg@jcgonzalez.org")

;; At header
;; <script type="text/javascript">var switchTo5x=true;</script>
;; <script type="text/javascript" src="http://w.sharethis.com/button/buttons.js"></script>
;; <script type="text/javascript" src="http://s.sharethis.com/loader.js"></script>

;; At the end
;;     <script type="text/javascript">stLight.options({publisher: "44a394fb-c51c-44d8-a9f1-48559d120e78", doNotHash: false, doNotCopy: false, hashAddressBar: false});</script>
;;     <script>
;;     var options={ "publisher": "44a394fb-c51c-44d8-a9f1-48559d120e78", "logo": { "visible": true, "url": "http://www.jcgonzalez.org", "img": "//sd.sharethis.com/disc/images/demo_logo.png", "height": 32}, "ad": { "visible": false, "openDelay": "5", "closeDelay": "0"}, "livestream": { "domain": "", "type": "sharethis", "customColors": { "widgetBackgroundColor": "#FFFFFF", "articleLinkColor": "#006fbb"}}, "ticker": { "visible": false, "domain": "", "title": "", "type": "sharethis", "customColors": { "widgetBackgroundColor": "#a0adc7", "articleLinkColor": "#00487f"}}, "facebook": { "visible": false, "profile": "sharethis"}, "fblike": { "visible": false, "url": ""}, "twitter": { "visible": false, "user": "sharethis"}, "twfollow": { "visible": false}, "custom": [{ "visible": false, "title": "Custom 1", "url": "", "img": "", "popup": false, "popupCustom": { "width": 300, "height": 250}}, { "visible": false, "title": "Custom 2", "url": "", "img": "", "popup": false, "popupCustom": { "width": 300, "height": 250}}, { "visible": false, "title": "Custom 3", "url": "", "img": "", "popup": false, "popupCustom": { "width": 300, "height": 250}}], "chicklets": { "items": ["sharethis", "googleplus", "twitter", "linkedin", "facebook", "tumblr", "reddit", "pinterest", "google_bmarks", "evernote", "google_translate", "email"]}, "background": "#103071", "color": "#ffffff", "arrowStyle": "light", "shadow": "gloss"};
;;     var st_bar_widget = new sharethis.widgets.sharebar(options);
;;     </script>


(setq org-publish-project-alist
      '(
        ("org"
         :author "J.C.González"
         :email "jcg@jcgonzalez.org"
         :base-directory "~/Personal/www/web"
         :base-extension "orgp"
         :publishing-directory "~/Personal/www/publishing/public_html/"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc t
         :html-head "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>
<link href=\"http://fonts.googleapis.com/css?family=Martel+Sans:200\" rel=\"stylesheet\" type=\"text/css\"/>
<link rel=\"stylesheet\" href=\"css/jcg.css\" type=\"text/css\"/>
<script type=\"text/javascript\">var switchTo5x=true;</script>
<script type=\"text/javascript\" src=\"http://w.sharethis.com/button/buttons.js\"></script>
<script type=\"text/javascript\" src=\"http://s.sharethis.com/loader.js\"></script>"
         :html-head-extra "<link rel=\"alternate\" type=\"application/rss+xml\"
                href=\"http://www.jcgonzalez.org/jcg/index.xml\"
                title=\"RSS blogs feed\">"
         :html-preamble "<div class=\"header\"></div>"
         :html-postamble "<div class=\"footer\">
<table bordeer=\"0\" cellpadding=\"0\">
<tr><td><a href=\"https://es.linkedin.com/in/jcglez\"><img src=\"img/linkedin.jpg\"/></a></td>
<td><a href=\"https://twitter.com/JoseCGlezGcia\"><img src=\"img/twitter.jpg\"/></a></td>
<td><a href=\"https://www.google.com/+JCGonzalezGcia\"><img src=\"img/googleplus.jpg\"/></a></td>
<td><a href=\"http://www.jcgonzalez.org/jcg/index.xml\"><img src=\"img/rss.png\" width=\"24px\"/></a></td>
<td style=\"width:30%%\"></td>
<td>Copyright 2014-2017 %a %e - Last updated %C <br>
Built with %c - (%v HTML)</td></tr></table>
    <script type=\"text/javascript\">stLight.options({publisher: \"44a394fb-c51c-44d8-a9f1-48559d120e78\", doNotHash: false, doNotCopy: false, hashAddressBar: false});</script>
    <script>
    var options={ \"publisher\": \"44a394fb-c51c-44d8-a9f1-48559d120e78\", \"logo\": { \"visible\": true, \"url\": \"http://www.jcgonzalez.org\", \"img\": \"//sd.sharethis.com/disc/images/demo_logo.png\", \"height\": 32}, \"ad\": { \"visible\": false, \"openDelay\": \"5\", \"closeDelay\": \"0\"}, \"livestream\": { \"domain\": \"\", \"type\": \"sharethis\", \"customColors\": { \"widgetBackgroundColor\": \"#FFFFFF\", \"articleLinkColor\": \"#006fbb\"}}, \"ticker\": { \"visible\": false, \"domain\": \"\", \"title\": \"\", \"type\": \"sharethis\", \"customColors\": { \"widgetBackgroundColor\": \"#a0adc7\", \"articleLinkColor\": \"#00487f\"}}, \"facebook\": { \"visible\": false, \"profile\": \"sharethis\"}, \"fblike\": { \"visible\": false, \"url\": \"\"}, \"twitter\": { \"visible\": false, \"user\": \"sharethis\"}, \"twfollow\": { \"visible\": false}, \"custom\": [{ \"visible\": false, \"title\": \"Custom 1\", \"url\": \"\", \"img\": \"\", \"popup\": false, \"popupCustom\": { \"width\": 300, \"height\": 250}}, { \"visible\": false, \"title\": \"Custom 2\", \"url\": \"\", \"img\": \"\", \"popup\": false, \"popupCustom\": { \"width\": 300, \"height\": 250}}, { \"visible\": false, \"title\": \"Custom 3\", \"url\": \"\", \"img\": \"\", \"popup\": false, \"popupCustom\": { \"width\": 300, \"height\": 250}}], \"chicklets\": { \"items\": [\"sharethis\", \"googleplus\", \"twitter\", \"linkedin\", \"facebook\", \"tumblr\", \"reddit\", \"pinterest\", \"google_bmarks\", \"evernote\", \"google_translate\", \"email\"]}, \"background\": \"#103071\", \"color\": \"#ffffff\", \"arrowStyle\": \"light\", \"shadow\": \"gloss\"};
    var st_bar_widget = new sharethis.widgets.sharebar(options);
    </script>
</div>
"
         :auto-index t
         :auto-sitemap t                ; Generate sitemap.org automagically...
         :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
         :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
         :headline-levels 3
         :recursive t
         :auto-preamble t
         :makeindex t
         )
        ("blog"
         :author "J.C.González"
         :email "jcg@jcgonzalez.org"
         :base-directory "~/Personal/www/blog"
         :base-extension "org"
         :publishing-directory "~/Personal/www/publishing/public_html/blog"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc t
         :html-head "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>
<link href=\"http://fonts.googleapis.com/css?family=Martel+Sans:200\" rel=\"stylesheet\" type=\"text/css\"/>
<link rel=\"stylesheet\" href=\"css/jcg.css\" type=\"text/css\"/>"
         :html-preamble "<div class=\"nav\">"
         :html-postamble "<div style=\"position:absolute;top:10px;right:30px;\"><a href=\"http://www.jcgonzalez.org\"><img src=\"../../img/home.png\"/></a></div>
<div class=\"footer\">
Copyright 2014-2017 %a %e - Last updated %C <br>
Built with %c - (%v HTML)
</div>"
         :headline-levels 2
         :recursive t
         :auto-preamble t
         )
        ("notas"
         :author "J.C.González"
         :email "jcg@jcgonzalez.org"
         :base-directory "~/Personal/www/blog/notas"
         :base-extension "org"
         :publishing-directory "~/Personal/www/publishing/public_html/blog/notas"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :html-head "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>
<link href=\"http://fonts.googleapis.com/css?family=Martel+Sans:200\" rel=\"stylesheet\" type=\"text/css\"/>
<link rel=\"stylesheet\" href=\"css/jcg.css\" type=\"text/css\"/>"
         :html-head-extra "<link rel=\"alternate\" type=\"application/rss+xml\"
                href=\"http://www.jcgonzalez.org/jcg/blog/notas/notas.xml\"
                title=\"RSS blogs feed\">"
         :html-preamble "<div class=\"header\"></div>"
         :html-postamble "
<div style=\"position:absolute;top:10px;right:30px;\"><a href=\"http://www.jcgonzalez.org\"><img src=\"../../img/home.png\"/></a></div>
<div id=\"disqus_thread\"></div>
<script type=\"text/javascript\">
    /* * * CONFIGURATION VARIABLES * * */
    var disqus_shortname = 'jcgonzalezorg';

    /* * * DON'T EDIT BELOW THIS LINE * * */
    (function() {
        var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
        dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';
        (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
    })();
</script>
<noscript>Please enable JavaScript to view the <a href=\"https://disqus.com/?ref_noscript\" rel=\"nofollow\">comments powered by Disqus.</a></noscript>
<a href=\"http://disqus.com\" class=\"dsq-brlink\">comments powered by <span class=\"logo-disqus\">Disqus</span></a>
<div class=\"footer\">
<table bordeer=\"0\" cellpadding=\"0\">
<tr><td><a href=\"https://es.linkedin.com/in/jcglez\"><img src=\"../../img/linkedin.jpg\"/></a></td>
<td><a href=\"https://twitter.com/JoseCGlezGcia\"><img src=\"../../img/twitter.jpg\"/></a></td>
<td><a href=\"https://www.google.com/+JCGonzalezGcia\"><img src=\"../../img/googleplus.jpg\"/></a></td>
<td><a href=\"http://www.jcgonzalez.org/jcg/blog/notas/notas.xml\"><img src=\"../../img/rss.png\" width=\"24px\"/></a></td>
<td style=\"width:30%%\"></td>
<td>Copyright 2014-2017 %a %e - Last updated %C <br>
Built with %c - (%v HTML)</td></tr></table>
</div>"
         :auto-index t
         :auto-sitemap t                ; Generate sitemap.org automagically...
         :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
         :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
         :headline-levels 3
         :recursive t
         :auto-preamble t
         :makeindex t
         )
        ("yebsh"
         :author "J.C.González"
         :email "jcg@jcgonzalez.org"
         :base-directory "~/Personal/www/blog/yebsh"
         :base-extension "org"
         :publishing-directory "~/Personal/www/publishing/public_html/blog/yebsh"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :html-head "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>
<link href=\"http://fonts.googleapis.com/css?family=Martel+Sans:200\" rel=\"stylesheet\" type=\"text/css\"/>
<link rel=\"stylesheet\" href=\"css/jcg.css\" type=\"text/css\"/>"
         :html-head-extra "<link rel=\"alternate\" type=\"application/rss+xml\"
                href=\"http://www.jcgonzalez.org/jcg/blog/yebsh/yebsh.xml\"
                title=\"RSS blogs feed\">"
         :html-preamble "<div class=\"header\"></div>"
         :html-postamble "<div style=\"position:absolute;top:10px;right:30px;\"><a href=\"http://www.jcgonzalez.org\"><img src=\"../../img/home.png\"/></a></div>
<div id=\"disqus_thread\"></div>
<script type=\"text/javascript\">
    /* * * CONFIGURATION VARIABLES * * */
    var disqus_shortname = 'jcgonzalezorg';

    /* * * DON'T EDIT BELOW THIS LINE * * */
    (function() {
        var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
        dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';
        (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
    })();
</script>
<noscript>Please enable JavaScript to view the <a href=\"https://disqus.com/?ref_noscript\" rel=\"nofollow\">comments powered by Disqus.</a></noscript>
<a href=\"http://disqus.com\" class=\"dsq-brlink\">comments powered by <span class=\"logo-disqus\">Disqus</span></a>
<div class=\"footer\">
<table bordeer=\"0\" cellpadding=\"0\">
<tr><td><a href=\"https://es.linkedin.com/in/jcglez\"><img src=\"../../img/linkedin.jpg\"/></a></td>
<td><a href=\"https://twitter.com/JoseCGlezGcia\"><img src=\"../../img/twitter.jpg\"/></a></td>
<td><a href=\"https://www.google.com/+JCGonzalezGcia\"><img src=\"../../img/googleplus.jpg\"/></a></td>
<td><a href=\"http://www.jcgonzalez.org/jcg/blog/yebsh/yebsh.xml\"><img src=\"../../img/rss.png\" width=\"24px\"/></a></td>
<td style=\"width:30%%\"></td>
<td>Copyright 2014-2017 %a %e - Last updated %C <br>
Built with %c - (%v HTML)</td></tr></table>
</div>"
         :auto-index t
         :auto-sitemap t                ; Generate sitemap.org automagically...
         :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
         :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
         :headline-levels 3
         :recursive t
         :auto-preamble t
         :makeindex t
         )
        ("libros"
         :author "J.C.González"
         :email "jcg@jcgonzalez.org"
         :base-directory "~/Personal/www/blog/libros"
         :base-extension "org"
         :publishing-directory "~/Personal/www/publishing/public_html/blog/libros"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :html-head "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>
<link href=\"http://fonts.googleapis.com/css?family=Martel+Sans:200\" rel=\"stylesheet\" type=\"text/css\"/>
<link rel=\"stylesheet\" href=\"css/jcg.css\" type=\"text/css\"/>"
         :html-head-extra "<link rel=\"alternate\" type=\"application/rss+xml\"
                href=\"http://www.jcgonzalez.org/jcg/blog/libros/nl.xml\"
                title=\"RSS blogs feed\">"
         :html-preamble "<div class=\"header\"></div>"
         :html-postamble "<div style=\"position:absolute;top:10px;right:30px;\"><a href=\"http://www.jcgonzalez.org\"><img src=\"../../img/home.png\"/></a></div>
<div id=\"disqus_thread\"></div>
<script type=\"text/javascript\">
    /* * * CONFIGURATION VARIABLES * * */
    var disqus_shortname = 'jcgonzalezorg';

    /* * * DON'T EDIT BELOW THIS LINE * * */
    (function() {
        var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
        dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';
        (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
    })();
</script>
<noscript>Please enable JavaScript to view the <a href=\"https://disqus.com/?ref_noscript\" rel=\"nofollow\">comments powered by Disqus.</a></noscript>
<a href=\"http://disqus.com\" class=\"dsq-brlink\">comments powered by <span class=\"logo-disqus\">Disqus</span></a>
<div class=\"footer\">
<table bordeer=\"0\" cellpadding=\"0\">
<tr><td><a href=\"https://es.linkedin.com/in/jcglez\"><img src=\"../../img/linkedin.jpg\"/></a></td>
<td><a href=\"https://twitter.com/JoseCGlezGcia\"><img src=\"../../img/twitter.jpg\"/></a></td>
<td><a href=\"https://www.google.com/+JCGonzalezGcia\"><img src=\"../../img/googleplus.jpg\"/></a></td>
<td><a href=\"http://www.jcgonzalez.org/jcg/blog/libros/nl.xml\"><img src=\"../../img/rss.png\" width=\"24px\"/></a></td>
<td style=\"width:30%%\"></td>
<td>Copyright 2014-2017 %a %e - Last updated %C <br>
Built with %c - (%v HTML)</td></tr></table>
</div>"
         :auto-index t
         :auto-sitemap t                ; Generate sitemap.org automagically...
         :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
         :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
         :headline-levels 3
         :recursive t
         :auto-preamble t
         :makeindex t
         )
        ("static"
         :base-directory "~/Personal/www/publishing/resources"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Personal/www/publishing/public_html/"
         :publishing-function org-publish-attachment
         :recursive t
         :auto-preamble t
         )
        ("rss"
         :base-directory "~/Personal/www/web"
         :base-extension "orgp"
         :publishing-directory "~/Personal/www/publishing/public_html/"
         :publishing-function (org-rss-publish-to-rss)
         :html-link-home "http://www.jcgonzalez.org/"
         :html-link-use-abs-url t
         :recursive f
         :auto-preamble t
         :exclude ".*"
         :include ("index.orgp")
         )
        ("rss-notas"
         :base-directory "~/Personal/www/blog/notas"
         :base-extension "org"
         :publishing-directory "~/Personal/www/publishing/public_html/blog/notas"
         :publishing-function (org-rss-publish-to-rss)
         :html-link-home "http://www.jcgonzalez.org/"
         :html-link-use-abs-url t
         :recursive f
         :auto-preamble t
         :exclude ".*"
         :include ("notas.org")
         )
        ("rss-yebsh"
         :base-directory "~/Personal/www/blog/yebsh"
         :base-extension "org"
         :publishing-directory "~/Personal/www/publishing/public_html/blog/yebsh"
         :publishing-function (org-rss-publish-to-rss)
         :html-link-home "http://www.jcgonzalez.org/"
         :html-link-use-abs-url t
         :recursive f
         :auto-preamble t
         :exclude ".*"
         :include ("yebsh.org")
         )
        ("rss-nl"
         :base-directory "~/Personal/www/blog/libros"
         :base-extension "org"
         :publishing-directory "~/Personal/www/publishing/public_html/blog/libros"
         :publishing-function (org-rss-publish-to-rss)
         :html-link-home "http://www.jcgonzalez.org/"
         :html-link-use-abs-url t
         :recursive f
         :auto-preamble t
         :exclude ".*"
         :include ("nl.org")
         )
        ("jcg-site"
         :components ("org"
                      "blog"
                      "notas" "yebsh" "libros"
                      "rss"
                      "rss-notas" "rss-yebsh" "rss-nl"
                      "static"))
        )
      )

(defun jcg-web/export-format-drawer (name content)
  (concat "<div class=\"drawer " (downcase name) "\">\n"
	  "<h6>" (capitalize name) "</h6>\n"
	  content
	  "\n</div>"))

(setq org-export-htmlize-output-type 'css)

;; No author / date at the bottom
(setf org-html-home/up-format "")

;; Export as UTF-8
(setf org-export-html-coding-system 'utf-8-unix)

;; The defaults are just fine for mathjax and style
;; However, they do not work over TLS due to mixed content errors
(setf org-html-mathjax-options
      '((path "/etc/MathJax/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
	(scale "100") (align "center") (indent "2em")
	(mathml nil)))

(setf org-html-mathjax-template
      "<script type=\"text/javascript\" src=\"%PATH\"></script>")

(setf org-html-footnotes-section "<div id=\"footnotes\"><!--%s-->%s</div>")
;;(setf org-html-link-up "")
;;(setf org-html-link-home "")
;;(setf org-html-preamble nil)
;;(setf org-html-postamble nil)
(setf org-html-scripts "")

;;(setf org-html-postamble-format
;;      (list
;;       (list
;;	"en"
;;	(concat
;;	 "<p>By <a href=\"mailto:%e\">%a</a>.\n"
;;	 "Share it—it\"s <a href=\"http://creativecommons.org/licenses/by-sa/4.0\" rel=\"license\">CC-BY-SA licensed</a>.</p>"))))

(setq org-html-format-drawer-function 'jcg-web/export-format-drawer)

(setq org-export-html-style-include-scripts nil
      org-export-html-style-include-default nil)

(setq org-export-html-style nil)

;;  (concat
;;   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
;;   "<link rel=\"stylesheet\" type=\"text/css\" href=\"./css/jcg.css\" />"))
