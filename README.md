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
2. Configure `run-service.el`
3. Run Emacs
4. Run `M-x load-file RET run-service.el RET`

To stop the Service, run `M-x load-file RET stop-serivce.el RET`

## Where and how to adjust

### Where to - abstract
Basic information about your Service can be adjusted in the static variables **SERVICE_NAME** **SERVICE_TITLE** **SERVICE_DESCRIPTION** **SERVICE_VERSION** and **SERVICE_HOST**. 

Be shure to define **HERP_HOST** in the right way.

***nodeDefinition*** defines all the inputs and outputs of your service. 

Use the function **process-data** in the main module to customize behaviour of the Service. 

You should also implement test, like the ones in ***pp-test-business-logic***, to test your implementation of the business logic. You can run these test by: `M-x ert RET RET`

If you need to access a pure string, not the Lisp representation of the JSON data, you may use a string representation before ***process-json*** or bypass the ***json-encode*** call.

### How to - Step by step
1. Provide basic information of your service via capitalized variables.
2. Define inputs and outputs you need with correspondig data types via json in **nodeDefinition**.
These inputs and outputs will now be provided by hERP via JSON, whenever you start (or restart) the service.
3. Access and use your service inputs in **process_data** function, according to the inputs, that were defined by you in **nodeDefinition**. 
4. Return outputs in the **process_data** function according to the outputs, that were defined by you in **nodeDefinition**.
