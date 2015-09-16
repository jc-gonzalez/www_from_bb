;;; run.el --- Load site specification and compiles projectile
;;; Code:
;;; Commentary:

(setq debug-on-error t)
;(org-publish-remove-all-timestamps)
(load-file "setup.el")
(org-publish-project "jcg-site")

;;; run.el ends here
