;; -*- coding: utf-8; -*-

;; javascript mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
;;(add-to-list 'auto-mode-alist '("\\.js\\'" . ecmascript-mode))

(autoload 'javascript-mode "$PRJ_ROOT/tools/javascript.el" "Mode for editing CSS files" t)
(autoload 'ecmascript-mode "$PRJ_ROOT/tools/ecmascript-mode.el" "Mode for editing CSS files" t)


;; jslint javascript lint 
(defun jslint-buffer ()
  "Runs JSLINT on current buffer."
  (interactive)
  (compile (format "$V8_SHELL $PRJ_ROOT/tools/jslint.js $PRJ_ROOT/tools/run-jslint.js -e 'RunJSLINT(\"%s\")'" (buffer-file-name))))

;; jslint javascript lint 
(defun gjslint-buffer ()
  "Runs JSLINT on current buffer."
  (interactive)
  (compile (format "gjslint --flagfile $PRJ_ROOT/tools/gjslint-flagfile.txt %s" (buffer-file-name))))


;;(add-hook 'javascript-mode-hook
;;          '(lambda () (local-set-key [f5] 'jslint-buffer)))



(defun tekiis-way ()
  "Install my build keys."
  (interactive)
  (local-set-key [f5] 'jslint-buffer)
  (local-set-key [f6] 'gjslint-buffer)
  (local-set-key [C-f7] 'first-error)
  (local-set-key [f7]   'previous-error)
  (local-set-key [f8]   'next-error)
  ;; some indent style adjust
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2)
  ;;(local-set-key [M-f9] 'compile)
  ;;(local-set-key [f12]  'compile)
  ;;(local-set-key [f11]  'recompile)
  ;;(local-set-key [f9]   'recompile)
  (lambda () ()))
  

(add-hook 'javascript-mode-hook 'tekiis-way)
(add-hook 'ecmascript-mode-hook 'tekiis-way)

