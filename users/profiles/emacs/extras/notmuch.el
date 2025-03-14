(setq notmuch-show-all-tags-list t)
(setq notmuch-show-logo nil)
(setq notmuch-show-text/html-blocked-images nil)

(setq notmuch-saved-searches
      '((:name "Inbox(i)" :query "tag:Important OR tag:General" :key "i")
        (:name "Invoices(u)" :query "tag:Invoice" :key "u")
        (:name "Selfinvoices(f)" :query "tag:Selfinvoice" :key "f")
        (:name "Social(s)" :query "tag:Social" :key "s")
        (:name "Newsletter(d)" :query "tag:Newsletter" :key "d")
        (:name "Promotional(a)" :query "tag:Promotional" :key "a")
        (:name "Jobs(j)" :query "tag:Jobs" :key "j")
        (:name "Mailinglists(m)" :query "tag:Mailinglist" :key "m")
		))

; (dolist (state '((notmuch-hello-mode . motion)
;                    (notmuch-search-mode . motion)
;                    (notmuch-tree-mode . motion)
;                    (notmuch-show-mode . motion)))
;     (add-to-list 'meow-mode-state-list state))



(setq send-mail-function 'sendmail-send-it
      sendmail-program "msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

(use-package notmuch
  :ensure t
  :commands (notmuch notmuch-search notmuch-tree notmuch-show)
  :hook ((notmuch-search-mode . meow-motion-mode)
         (notmuch-show-mode . meow-motion-mode)
         (notmuch-hello-mode . meow-motion-mode)
         (notmuch-tree-mode . meow-motion-mode)
		)
  :init
  (setq notmuch-search-oldest-first nil))
