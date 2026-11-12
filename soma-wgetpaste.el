;;; soma-wgetpaste.el --- An interface to the wgetpaste tool  -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; An interface to the wgetpaste tool.
;;
;;    http://wgetpaste.zlin.dk/
;;
;; Usage:
;;
;;    (require 'wgetpaste)
;;
;; To send a region interactively:
;;
;;    * Select region.
;;
;;    * Type M-x wgetpaste-region
;;
;;    * If you don't have a default service or language selected,
;;      you will be prompted for them.
;;
;;    * If you do have a default service or language selected,
;;      then they will be used unless you pass wgetpaste-region
;;      a prefix argument (such as with C-u), in which case
;;      you will be prompted for them.
;;
;; To send a region non-interactively, from elisp:
;;
;;    * (wgetpaste-region BEGIN END SERVICE LANGUAGE)
;;
;; If SERVICE and LANGUAGE are omitted, the default SERVICE and/or
;; LANGUAGE will be used (if set), otherwise the user will be
;; prompted for them.
;;
;; To send a string non-interactively, from elisp:
;;
;;    * (wgetpaste-data DATA SERVICE LANGUAGE)
;;
;; If SERVICE and LANGUAGE are omitted, the default SERVICE and/or
;; LANGUAGE will be used (if set), otherwise the user will be
;; prompted for them.
;;
;;
;; Note: the URL received in reply from the service will always
;;       be sent to the kill-ring.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For (soma-utils-chomp)
;;
(require 'soma-utils)

;; For set-clipboard-contents
(require 'soma-set-clipboard-contents-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customizable variables
;;
(defgroup wgetpaste nil "wgetpaste group"
  :group 'external)
;;
(defcustom wgetpaste-default-language
  'nil
  "The language to be used by default."
  :group 'wgetpaste
  :type '(choice string (const nil)))
;;
(defcustom wgetpaste-path
  "wgetpaste"
  "The path to the wgetpaste executable."
  :group 'wgetpaste
  :type 'file)
;;
(defcustom wgetpaste-default-service
  'nil
  "The paste service to be used by default."
  :group 'wgetpaste
  :type '(choice string (const nil)))
;;
(defcustom wgetpaste-timeout
  10
  "Maximum number of seconds to wait for wgetpaste to complete."
  :group 'wgetpaste
  :type 'integer)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun wgetpaste--choose-language (service)
  "Prompt the user to make a choice of a language.  Return a string."
  (let ((prompt "Select language: ")
        (choices (wgetpaste--get-languages service))
        (predicate     't)
        (require-match 't)
        (initial-input 'nil)
        (hist          'nil)
        (default       'nil))
    (ido-completing-read
     prompt
     choices
     predicate
     require-match
     initial-input
     hist
     default)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun wgetpaste--choose-service ()
  "Prompt the user to make a choice of a pastebin.  Return a string."
  (let ((prompt "Select service: ")
        (choices (wgetpaste--get-services))
        (predicate     't)
        (require-match 't)
        (initial-input 'nil)
        (hist          'nil)
        (default       wgetpaste-default-service))
    (ido-completing-read
     prompt
     choices
     predicate
     require-match
     initial-input
     hist
     default)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun wgetpaste-data (data &optional service language)
"Send DATA to SERVICE, as LANGUAGE, return reply and kill it.

DATA is the only mandatory argument. All given arguments must be strings.

If SERVICE or LANGUAGE are not passed as arguments, the user
will be prompted for them.

Note: When SERVICE or LANGUAGE are given, they must be a literal,
      case-sensitive match for one of the supported languages
      or services."
  (cl-assert (stringp data))
  (let* ((service  (or
                    service
                    wgetpaste-default-service
                    (wgetpaste--choose-service)))
         (language (or
                    language
                    wgetpaste-default-language
                    (wgetpaste--choose-langauge)))
         (result
          (cond
            ((eq data 'nil)
             (error "You must provide some data to send."))
            ((eq service 'nil) ; Only data given as an argument
             (error "You must choose a service and language."))
            ((eq language 'nil) ; Only data and service given as arguments
             (error "You must choose a language."))
            ('t ; Every argument has been given.
             (wgetpaste--send-with-all data service language)))))
    (kill-new result)
    (message result)
    result))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The processing of wgetpaste's output gets a little hairy,
;; because it clearly wasn't designed for this sort of automated
;; processing, and contains all sorts of garbage we have to
;; hack around or ignore.
;;
(defun wgetpaste--get-languages (service)
  "Given a SERVICE, return a list of languages from wgetpaste."
  (let* ((raw-languages (wgetpaste--get-raw-languages service))
         (all-lines (split-string raw-languages "\n"))
         (first-line (first all-lines)))
    (if (string-match "not a supported service" first-line)
        (error (format first-line))
      (let* ((relevant-lines (rest all-lines))
         (lines-sans-leading-spaces
           (wgetpaste--strip-leading-spaces relevant-lines))
         (languages (wgetpaste--strip-leading-asterisk
                      lines-sans-leading-spaces)))
        (sort
          (soma-utils-uniq languages)
          'string<)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun wgetpaste--get-raw-languages (service)
  "Given a SERVICE, return string with raw list of languages."
  (let ((wgetpaste wgetpaste-path))
    ; Perform a minor sanity check:
    (if (executable-find wgetpaste)
        ; wgetpaste exists in the exec-path and is executable
        (let* ((args
                 (concat
                   "--service "
                   service
                   " --list-languages"))
               (command (format "%s %s" wgetpaste args))
               (raw-languages
                 (shell-command-to-string command)))
          (soma-utils-chomp raw-languages))
      (error "wgetpaste not found or not executable"))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun wgetpaste--get-raw-services ()
  "Return a string with the raw list of services from wgetpaste."
  (let ((wgetpaste wgetpaste-path))
    ; Perform a minor sanity check:
    (if (executable-find wgetpaste)
        ; wgetpaste exists in the exec-path and is executable
        (let* ((args "--list-services")
               (command (format "%s %s" wgetpaste args))
               (raw-services
                 (shell-command-to-string command)))
          (soma-utils-chomp raw-services))
      (error "wgetpaste not found or not executable"))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The processing of wgetpaste's output gets a little hairy,
;; because it clearly wasn't designed for this sort of automated
;; processing, and contains all sorts of garbage we have to
;; hack around or ignore.
;;
(defun wgetpaste--get-services ()
  "Return a list of services from wgetpaste."
  (let* ((raw-services (wgetpaste--get-raw-services))
         (all-lines (split-string raw-services "\n"))
         ; Skip the first three lines, as they're not relevant:
         (relevant-lines (rest (rest (rest all-lines))))
         ; Put the first column of each line in to a list:
         (lines-sans-leading-spaces
           (wgetpaste--strip-leading-spaces relevant-lines))
         (lines-sans-trailing-junk
           (wgetpaste--strip-trailing-junk lines-sans-leading-spaces))
         (services (wgetpaste--strip-leading-asterisk
                     lines-sans-trailing-junk)))
    (sort
      (soma-utils-uniq services)
      'string<)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun wgetpaste-region (begin end &optional service language)
  "Send text from BEGIN to END to SERVICE, as LANGUAGE.

When called interactively, BEGIN and END will be inferred from
the active region.

SERVICE and LANGUAGE must be strings.

The user will be prompted for SERVICE or LANGUAGE if they
are not passed as arguments and there is no default SERVICE
or LANGUAGE, or if the this function is given a prefix argument.

Note: When SERVICE or LANGUAGE are given, they must be a literal,
      case-sensitive match for one of the supported languages
      or services."
  (interactive
   (if (region-active-p)
       (if current-prefix-arg
           ; Overriding any defaults that may exist:
           (let* ((service  (wgetpaste--choose-service))
                  (language (wgetpaste--choose-language service)))
             (list (region-beginning) (region-end) service language))
         (list (region-beginning) (region-end)))
     (error "Error: No region active.  Nothing sent.")))
  (if (and ; Some sanity checks on given region
       (integerp begin)
       (integerp end)
       (>= begin (point-min))
       (<= end   (point-max)))
      (let* ((data (buffer-substring-no-properties begin end))
             (service (or
                        service
                        wgetpaste-default-service
                        (wgetpaste--choose-service)))
             (language (or
                        language
                        wgetpaste-default-language
                        (wgetpaste--choose-language service)))
             (result
              (cond
               ((eq data 'nil)
                (error "Error: You must provide some data to send."))
               ((eq service 'nil) ; Only data given as an argument
                (error "Error: You must choose a service and language."))
               ((eq language 'nil) ; Only data and service given as arguments
                (error "Error: You must choose a language."))
               ('t ; Every argument has been given.
                (wgetpaste--send-with-all data service language)))))
        (deactivate-mark)
        (kill-new result)
        (set-clipboard-contents result)
        (message result)
        result)
    (error "Error: Invalid region.")))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun wgetpaste--send-and-get-result (data service language)
"Send DATA to the SERVICE, with highlighting set to LANGUAGE.

All arguments are mandatory, and must be strings."
  (cl-assert (stringp data))
  (cl-assert (stringp service))
  (cl-assert (stringp language))
  (let ((result
         (with-temp-buffer
           (let* ((process-connection-type 'nil)
                  (wgetpaste-process-name "wgetpaste")
                  (wgetpaste-buffer (current-buffer))
                  (wgetpaste-args
                    (list
                      "--service"  service
                      "--language" language))
                  ; TODO: clear buffer here
                  (wgetpaste-process
                    (apply 'start-process
                           wgetpaste-process-name
                           wgetpaste-buffer
                           wgetpaste-path
                           wgetpaste-args)))
             (process-send-string wgetpaste-process data)
             (process-send-eof    wgetpaste-process)
             (wgetpaste--wait-until-finished wgetpaste-process)
             (when (eq (process-status wgetpaste-process-name) 'run)
               (delete-process wgetpaste-process))
             (goto-char (point-min))
             (soma-utils-chomp (thing-at-point 'line))))))
    (wgetpaste--validate-and-return-url result)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun wgetpaste--send-with-all (data service language)
  "Given DATA, SERVICE and LANGAUGE, send."
  (wgetpaste--validate-service service)
  (wgetpaste--validate-language service language)
  (let ((result (wgetpaste--send-and-get-result data service language)))
    result))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun wgetpaste--strip-leading-asterisk (lines)
"Get rid of any asterisk from the start of a service name.
The star is just an indication that the service is the
default that's used by wgetpaste, and is not really
part of the actual service name."
  (let ((leading-asterisk
         (rx (: line-start "*"))))
    (mapcar
     (lambda (service)
       (replace-regexp-in-string
        leading-asterisk
        ""
        service))
     lines)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun wgetpaste--strip-leading-spaces (lines)
  "Return a list containing LINES without the leading spaces."
  (mapcar
    (lambda (line)
      (replace-regexp-in-string
        (rx (:
              line-start
              (zero-or-more " ")))
        ""
        line))
    lines))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun wgetpaste--strip-trailing-junk (lines)
  "Return a list containing LINES without any trailing junk."
  (mapcar
    (lambda (line)
      (replace-regexp-in-string
        (rx (:
              (one-or-more " ")
              (zero-or-more anything)))
        ""
        line))
    lines))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun wgetpaste--validate-and-return-url (result)
  "Do some sanity checking on RESULT, then return a url."
  (cond
    ((string-match "Apparently nothing was received" result)
     (error
       (concat
         "Nothing was received.  "
         "Perhaps the connection failed.")))
    ((string-match
       (rx (:
             line-start
             "Your paste can be seen here: "
             (group
               (one-or-more anything))))
       result)
     (match-string 1 result))
    ('t (error "Unexpected error: '%s'" result))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun wgetpaste--validate-language (service language)
  "Find out if a given SERVICE supports the given LANGUAGE"
  (let* ((supported-languages
           (wgetpaste--get-languages service))
         (language-supported
           (member language supported-languages)))
    (if language-supported
        't
      (error "'%s' is not a supported language" language))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun wgetpaste--validate-service (service)
  "Find out if a given SERVICE is supported"
  (let* ((supported-services
           (wgetpaste--get-services))
         (service-supported
           (member service supported-services)))
    (if service-supported
        't
      (error "'%s' is not a supported service" service))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun wgetpaste--wait-until-finished (process)
  "Wait until PROCESS exits or the wgetpaste-timeout is reached"
  (cl-do
      ((elapsed-time ; var
        0            ; init
        (progn       ; step
          (accept-process-output process 1)
          (sleep-for 1)
          (+ elapsed-time 1))))
      ((or            ; end-test
         (eq (process-status process) 'exit)
         (> elapsed-time
            ; We want to reach the wgetpaste-timeout, not exceed it:
            (- wgetpaste-timeout 1))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'soma-wgetpaste)
