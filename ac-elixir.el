(defun elixir-mode--parse--completion-candidates (str)
  (let ((strs (split-string str "[\n\s,]+" )))
    (mapcar (lambda (x)
              (let ((tmp (split-string (substring x 1 (1- (length x))) "/")))
                (if (cadr tmp)
                    (propertize (car tmp) 'summary (format "/%s" (cadr tmp)))
                  tmp)))
            strs)))


(defun elixir-mode--parse-completions (output)
  (if (string-equal "{[], " (substring output 0 5))
      (elixir-mode--parse--completion-candidates (substring output 6 (- (length output) 3)))
    (substring output 2 (- (length output) 7))))


(defun elixir-mode-complete (str)
    (let* ((output (elixir-mode--execute-elixir-with-code-eval-string
                  (format "{:yes,to,completions}=IEx.Autocomplete.expand('%s');{to,completions}"
                          (apply #'string (reverse (string-to-list str))))))
         (completions (elixir-mode--parse-completions output)))
      (when (eql ?{ (string-to-char output))
          (if (stringp completions)
              (list (concat (substring str (1+ (or (string-match "\\.\\([^\\.]*\\)$" str) 0))) completions))
            completions))))


(defun ac-elixir--prefix ()
  (save-excursion
    (ignore-errors
      (re-search-backward "^\\|[\s\t]+\\|>")
      (re-search-forward "\\(:?[A-Za-z\\._]+\\)")
      (match-string 1))))

(defun ac-elixir--candidates ()
  (let ((prefix (ac-elixir--prefix)))
    (elixir-mode-complete prefix)))

(defun ac-elixir--prefix--function ()
  (if (re-search-backward "\\.\\(\\(?:[a-zA-Z0-9][_a-zA-Z0-9]*\\)?\\)" nil t)
      (match-beginning 1)
    (when (re-search-backward "\\(?:[ \\t]+\\|^\\):?\\(\\(?:[a-zA-Z0-9][_a-zA-Z0-9]*\\)?\\)" nil t)
      (match-beginning 1))))

(defvar ac-elixir--doc-cache (make-hash-table :test 'equal))

(defun ac-elixir--documentation (item)
  (let* ((pref (ac-elixir--prefix))
         (module   (substring pref 0 (string-match "\\.[a-zA-Z_]*$" pref)))
         (cached-doc (gethash (format "%s.%s" module item) ac-elixir--doc-cache)))
    (if (or (= ?: (string-to-char module)) cached-doc)
        cached-doc
      (let ((doc (with-temp-buffer
                   (insert (elixir-mode--execute-elixir-with-code-eval-string
                            (format "Enum.map(Enum.filter(%s.__info__(:docs),
                                           fn {{fun,_},_,_,_,_} ->
                                                fun == binary_to_atom(\"%s\") end),
                                  fn {{fun, ar},_,_,_,doc} ->
                                     IO.puts \"=== #{fun} /#{ar} ===\"
                                     IO.puts \"\n#{doc}\" end)"
                                    module item)))
                   (buffer-end 1)
                   (forward-line -1)
                   (beginning-of-line)
                   (delete-region (point) (point-max))
                   (buffer-string)
                   )))
          (puthash (format "%s.%s" module item) doc ac-elixir--doc-cache)
          doc))))


(defun ac-elixir-show-doc ()
  (interactive)
  (pos-tip-show (ac-elixir--documentation
                 (thing-at-point 'word))))

(ac-define-source elixir
  '((candidates . ac-elixir--candidates)
    (candidate-face . ac-candidate-face)
    (selection-face . ac-selection-face)
    (prefix . ac-elixir--prefix--function)
    (document . ac-elixir--documentation)
    (cache . t)
    (requires . 0)
    (symbol . "f")))

(provide 'ac-elixir)
