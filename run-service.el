;; service.el --- herp Service written in elisp.
(require 'json)
(require 'web-server)

(defvar config
  '(
    ("SERVICE_NAME" . "skeleton.herp.app")
    ("SERVICE_TITLE" . "Elisp Skeleton Service")
    ("SERVICE_DESCRIPTION" . "A skeleton service with all necessary functionality to talk with herp but without any logic. Feel free to integrate your ideas!")
    ("SERVICE_VERSION" . "1.0.0")
    ("SERVICE_HOST" . "127.0.0.1:9000")
    ("HERP_HOST" . "127.0.0.1:5050")
    )
  )

(defun node-definition()
  (let* ((nodeDefinitions `(
			    (name . "myService")
			    (label . "My Service Node")
			    (inputs . ,'(
					 ((fieldType . "string")(name . "inputField1")(label . "String input Field"))
					 ((fieldType . "string")(name . "inputField2")(label . "String input Field"))
					 ((fieldType . "string")(name . "inputField3")(label . "String input Field"))))
			    (outputs . ,'(
					  ((fieldType . "string")(name . "outputField")(label . "String output Field")))) ))
	 ;; Add further input and output Definitions here or change, or even remove the defined inputs and outputs.
	 (node `((nodeDefinitions . ,nodeDefinitions))))
    node
    ))


(defun process-data (data)
  "Business logic belongs right here. 
Process data decoded from JSON, returning data to be (not yet) encoded to JSON."

  ;; You may access data by using assoc and cdr
  (print (cdr (assoc 'inputField1 data)))
  (print (member 'inputField1 data))

  ;; You may change incoming data by using setcdr
  (setcdr (assoc 'inputField1 data) "result")

  ;; Returned value is arranged as defined in node-definition.outputs.
  (setq response '((outputField . "result")))
  
  ;; Returned data
  response
  )

(defun process-json(body)
  "Decoding, processing and endcoding json: JSON -> LISP -> JSON"
  (let ((data (json-read-from-string body)))
    (json-encode (process-data data))
    )
  )

(ert-deftest pp-test-business-logic ()
  "Business logic test"

  ;; Feed inputs to process-data
  (should (equal (process-data
		   '((inputField1 . "String input")))
		   ;; Output in Lisp style
		 '((outputField . "result"))))
  
  ;; Feed all inputs. Maybe remove this. 
  (should (equal (process-data (mapcar #' (lambda(x) `( ,(intern (cdr(assoc 'name x))) . "")) (cdr (assoc 'inputs (cdr (assoc 'nodeDefinitions (node-definition))))))) '((outputField . "result" ))))

  ;; Feed an input, that does not fit to the input definitions. Should throw a "wrong-type-argument" error. 
  (should-error (process-data
		 '((wrongFieldName . "Some String")))
  		:type 'wrong-type-argument)
  )

	   
(ert-deftest pp-test-business-logic-integration ()
    "Business logic integration test."
    (should (equal
	     (process-json "{\"inputField1\" : \"value\"}") 
	    "{\"outputField\":\"result\"}"))
    )

(defun run-server ()
  (let ((port (string-to-number (car (cdr (split-string (cdr (assoc "SERVICE_HOST" config)) ":"))))))
    (ws-start
     '(
       ((:GET . "/install.*") .
	(lambda (request)
	  (with-slots (process header) request
	    (ws-response-header process 200 '("Content-type" . "application/json"))
	    (process-send-string process (json-encode (node-definition)))
	    )
	  ))

       ((:POST . "/install.*") .
	(lambda (request)
	  (with-slots (process) request
	    (ws-response-header process 200 '("Content-type" . "application/json"))
	    (process-send-string process "Installation Sucessful")
	    )
	  ))

       ((:POST . "/do.*") .
	(lambda (request)
	  (with-slots (process headers body) request

	    (if (> (length body) 1)
		(ws-response-header process 200 '("Content-type" . "application/json"))
	      (ws-send-404 process "No valid input")
	      )
	      (process-send-string process (process-json body))
	    )
	)))
    port )
    (print (format "Service Started on port %d" port))
    )
  )

(defun print-body-kill-buffer (status)
  "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
  (if (eq (assq 'error status) nil)
      ;; possible workaround to get message body since there is no support for json-parse-buffer before emacs v 27.1
      ;;(with-current-buffer (current-buffer)
      ;;(goto-char (point-min))
      ;;(re-search-forward "^$")
      ;;(delete-region (point) (point-min))
      ;;(print (buffer-string))
      (print "Registration process complete.")
    (print "Was not able to register @herp. Please check, if hERP server is up and running under correct IP adress and port (as defined).")
    )
  )

(defun post-data (url data)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/json")))
	(url-request-data (json-encode data)
			  ))
    ;; if you want, replace `my-switch-to-url-buffer' with `my-kill-url-buffer'
    (url-retrieve url 'print-body-kill-buffer))
  )
(defun register ()
  (post-data (concat "http://" (cdr (assoc "HERP_HOST" config)) "/services/register")
	     `(
	       ,`("name" . ,(cdr (assoc "SERVICE_NAME" config)))
	       ,`("host" . ,(cdr (assoc "SERVICE_HOST" config)))
	       ,`("title" . ,(cdr (assoc "SERVICE_TITLE" config )))
	       ,`("version" . ,(cdr (assoc "SERVICE_VERSION" config)))
	       ,`("description" . ,(cdr (assoc "SERVICE_DESCRIPTION" config)))
	       )
	     ;; ` and , are used to evaluate an expression, before insertion
	     ))


;; Server for Service is started
(run-server)

;; Service is registered to hERP
(register)


