(namespace "free")
(define-keyset 'io_admin_keyset-xyzn_test9 (read-keyset "io_admin_keyset-xyzn_test9"))
(module sensor_store9 GOVERNANCE
 @doc "sensor data store."

(use coin)
(use free.cyberfly)

  (defcap GOVERNANCE ()
    (enforce-keyset 'io_admin_keyset-xyzn_test9))

 (defschema device
        @doc "Device Register"
        device_id:string
        name:string
        status:string
        account:string
        guard:guard
       )

 (defschema device-data
  @doc "Device data"
  data_id:string
  data:string
  device_id:string
 )

 (defschema device-rules
       @doc "Register Rules for device data"
        rule_id:string
        rule_name:string
        rule:string
        action:string
        status:string
        device_id:string
        )

  (defschema dashboard-schema 
    @doc "store dashboard layout and its widget properties"
    dash_id:string
    title:string
    layout:string
    account:string
    status:string
  )

 (deftable device-table:{device})
 (deftable device-data-table:{device-data})
 (deftable device-rules-table:{device-rules})
 (deftable dashboard-table:{dashboard-schema})


(defcap DEVICE_GUARD (device_id:string)
 (with-read device-table device_id{"guard":=guard, "account":=account}
   (enforce-guard guard)
   )
)

(defcap ACCOUNT_GUARD(account:string)
@doc "Verifies account meets format and belongs to caller"
(enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
(enforce-guard   
    (at "guard" (coin.details account))
)
)

(defun new-device(device_id:string
         name:string
         status:string
         account:string
         guard:keyset)
(with-capability(ACCOUNT_GUARD account)
(insert device-table device_id {
  "device_id":device_id
  ,"name":name
  ,"status":status
  ,"account":account
  ,"guard":guard
})
)
)

(defun update-device(device_id:string
                name:string
                status:string)
(with-capability (DEVICE_GUARD device_id)
 (update device-table device_id {
               "name":name
               ,"status":status
       })
))

(defun update-device-guard(device_id:string
  guard:keyset)
(with-capability (DEVICE_GUARD device_id)
(update device-table device_id {
 "guard":guard
})
))

 (defun new-device-data (data_id:string
                     data:string
                     device_id:string
                     )
 @doc "update data"
(with-read device-table device_id
  {"status":=status}
 (enforce (= status "active") "device inactive")
(with-capability (DEVICE_GUARD device_id)
 (insert device-data-table data_id
         { "data_id":data_id
         ,"data":data
         ,"device_id":device_id
         })
)))



(defun new-device-rule(rule_id:string
                device_id:string
                rule:string
                rule_name:string
                status:string
                action:string
                )
@doc "create new rules"
(with-read device-table device_id
  {"status":=device_status}
(enforce (= device_status "active") "device inactive")
(with-capability (DEVICE_GUARD device_id)
(insert device-rules-table rule_id
    {"rule_id":rule_id
    ,"rule_name":rule_name
    ,"rule": rule
    ,"status":status
    ,"device_id":device_id
    ,"action": action
    })
)))

(defun update-device-rule (rule_id:string
        device_id:string
        rule:string
        rule_name:string
        status:string
        action:string
        )
@doc "update rules"

(with-read device-table device_id
  {"status":=device_status}
  (enforce (= device_status "active") "device inactive")
(with-capability (DEVICE_GUARD device_id)
(update device-rules-table rule_id
{ "rule_name":rule_name
, "rule": rule
, "device_id":device_id
,"status":status
, "action": action
})
)
  )

)


(defun get-device(device_id:string)
 (with-read device-table device_id {
         "name":=name
         ,"status":=status
         , "guard":=guard
 }
 {"device_id":device_id , "name":name, "status":status, "guard":guard  })
)

(defun auth-device(device_id:string)
 (with-read device-table device_id {
         "name":=name
         ,"status":=status
         ,"guard":=guard
 }
 (enforce (= status "active") "device inactive")
 (with-capability (DEVICE_GUARD device_id)
 {"device_id":device_id , "name":name, "status":status, "guard":guard  }
 )
 )
)

(defun get-account-devices(account:string)
(select device-table ["device_id", "name", "status", "guard"] (where 'account (= account))
)
)

 (defun read-data (data_id:string)
  @doc "Read data by id"
  (with-read device-data-table data_id
             {
              "data":=data
             , "device_id":=device_id
             } {"data":data, "device_id": device_id}))

(defun read-rule (rule_id:string)
             @doc "Read rule by id"

             (with-read device-rules-table rule_id
                        {
                         "rule_name":=rule_name,
                         "rule":=rule,
                         "action":=action,
                         "device_id":=device_id,
                         "status":=status
                        } {"rule_name":rule_name, "rule": rule, "action": action, "device_id": device_id
                           ,"status":status})
)

  (defun read-device-data(device_id:string)
  (select device-data-table (where 'device_id (= device_id))
  ))

  (defun read-device-rules(device_id:string)
  (select device-rules-table (where 'device_id (= device_id))
  ))

  (defun create-dashboard(dash_id:string
                          title:string
                          layout:string 
                          account:string 
                          status:string)
  (with-capability(ACCOUNT_GUARD account)
  (insert dashboard-table dash_id {
    "dash_id":dash_id
    ,"title":title
    ,"layout":layout
    ,"account":account
    ,"status":status
  })
  )
  )

  (defun update-dashboard(dash_id:string title:string layout:string account:string status:string)
  
  (with-read dashboard-table dash_id {"account":=dash_account
  }
  (enforce (= account dash_account) "account not matching")
  (with-capability(ACCOUNT_GUARD account)
  (update dashboard-table dash_id {
    "dash_id":dash_id
    ,"title":title
    ,"layout":layout
    ,"account":account
    ,"status":status
  })
    )
  )
  )

  (defun get-dashboard(account:string)
(select dashboard-table (where 'account (= account)) )
)
  )

(create-table device-table)
(create-table device-data-table)
(create-table device-rules-table)
(create-table dashboard-table)