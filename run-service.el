;; service.el --- herp Service written in elisp.
(require 'json)

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

(defun process_data (data)
  "Business logic belongs right here. 
Process data decoded from JSON, returning data to be encoded to JSON."

  ;; You may access data by using assoc and cdr
  (print (cdr (assoc 'message data)))

  ;; You may change incoming data by using setcdr
  (setcdr (assoc 'message data) "result")
  (print data)

  (setq response '(("outputField" . "result")))
  
  ;; Returnin data
  response
)

(ws-start
 '(
   ((:GET . "/install.*") .
    (lambda (request)
      (with-slots (process header) request
	(ws-response-header process 200 '("Content-type" . "application/json"))
	(process-send-string process
    "nodeDefinitions: [
        // #1 a node definition with one input and one output
        {
            name: \"combineString\",
            label: \"Combine two strints to one single string\",
            inputs: [
                {
                    fieldType: \"string\",
                    name: \"inputField1\",
                    label: \"String Input 1\"
                },
                {
                    fieldType: \"string\",
                    name: \"inputField2\",
                    label: \"String Input 2\"
                },
            ],
            outputs: [
                {
                    fieldType: \"string\",
                    name: \"outputField\",
                    label: \"Combined strings\"
                },
            ]
        },
        // add fruther nodeDefinitions for workflows here
    ]"
			     )
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
     (let ((request_data (json-read-from-string body)))
       (process-send-string process
			    (json-encode(process_data request_data))
       )
     )
   ))
   ))
 9000)

(defun my-kill-url-buffer (status)
      "Kill the buffer returned by `url-retrieve'."
      (kill-buffer (current-buffer)))

(defun my-switch-to-url-buffer (status)
      "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
      (switch-to-buffer (current-buffer)))

(defun post-data (url data)
 "Send ARGS to URL as a POST request."
      (let ((url-request-method "POST")
            (url-request-extra-headers
             '(("Content-Type" . "application/json")))
            (url-request-data (json-encode data)
             ))
        ;; if you want, replace `my-switch-to-url-buffer' with `my-kill-url-buffer'
        (url-retrieve url 'my-kill-url-buffer))
      )

(post-data (concat "http://" (cdr (assoc "HERP_HOST" config)) "/services/register")
	   `(
		    ,`("name" . ,(cdr (assoc "SERVICE_NAME" config)))
		    ,`("host" . ,(cdr (assoc "SERVICE_HOST" config)))
		    ,`("title" . ,(cdr (assoc "SERVICE_TITLE" config )))
		    ,`("version" . ,(cdr (assoc "SERVICE_VERSION" config)))
		    ,`("description" . ,(cdr (assoc "SERVICE_DESCRIPTION" config)))
		    )
	   ;; ` and , are used to evaluate an expression, before insertion
	   )
