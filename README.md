<img src="https://herp.app/herp-logo.svg" width="200">

# hERP Service ELISP Skeleton

This is an empty service you can start from to create your own service for [hERP](https://herp.app). 

If you are not familiar with Services read the [documentation here](https://herp.app/docs/services/hello-world-service/) to get started.

## Features

This skeleton does already:

* Starts a webserver with necessary endpoints **/do** and **/install**

## How to use

To run a Service
1. Clone
2. Configure
3. Run Emacs
4. M-x load-file
   service-run

To stop the Service: 
	M-x load-file
	serivce-stop

## Where and how to adjust

### Where to - abstract
Basic information about your Service can be adjusted in the static variables **SERVICE_NAME** **SERVICE_TITLE** **SERVICE_DESCRIPTION** **SERVICE_VERSION** and **SERVICE_HOST**. 

Be shure to define **HERP_HOST** in the right way.

Use the function **process_data** in the main module to customize behaviour of the Service.

If you need to access the pure String, not the Lisp representation of the JSON data, you may use a string representation before ***json-read-from-string*** or bypass the ***json-encode*** method.

***nodeDefinition*** defines all the inputs and outputs of your service. 
### How to - Step by step
1. Provide basic information of your Service via capitalized variables.
2. Define inputs and outputs you need with correspondig data types via json in **nodeDefinition**.
These inputs and outputs will now be provided by hERP via JSON, when you start the service.
3. Access and use the defined inputs in the **process_data** function, according to the definition of the inputs in **nodeDefinition**. 
4. Return outputs in the **process_data** function according to the definde inputs in **nodeDefinition**.
