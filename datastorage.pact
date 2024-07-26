(namespace "free")
(define-keyset "free.cyberfly_team" (read-keyset "cyberfly_team"))
(module cyberfly_devices GOVERNANCE
 @doc "device data store."

(use coin)
(use free.cyberfly)

  (defcap GOVERNANCE ()
    (enforce-keyset 'cyberfly_team))

 (defschema device
        @doc "Device Register"
        device_id:string
        name:string
        status:string
        account:string
        guard:guard
       )



(defschema user-config-schema
  @doc "Store Iot platform users config"
  account:string
  dashboard_db:string
)
  
(defschema device-counter
  @doc "track each account device count"
  device_count:integer
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
    props:string
    account:string
    status:string
  )

  (defschema chat-registry-schema
    @doc "Store the account and its corresponding elliptic curve pubkey for encryption"
    account:string
    pubkey:string
    seckey:string
    )

    (defschema contacts-schema
      @doc "Store users contacts to chat"
      contact_id:string
      account:string
      label:string
      created_by:string
      )


 (deftable device-table:{device})
 (deftable device-data-table:{device-data})
 (deftable device-rules-table:{device-rules})
 (deftable dashboard-table:{dashboard-schema})
 (deftable chat-registry-table:{chat-registry-schema})
 (deftable contacts-table:{contacts-schema})
 (deftable device-counter-table:{device-counter})
 (deftable user-config-table:{user-config-schema})


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

(defun add-dashboard-address(account:string
                             dashboard_id:string
                             db_address:string
                             )
  (with-capability(ACCOUNT_GUARD account)

  (insert user-config-table dashboard_id {
    "dashboard_id":dashboard_id,
    "account":account,
    "dashboard_db":db_address
  } )
    
    )

)

(defun get-dashboard-address(account:string)
(select user-config-table ["dashboard_id", "account", "dashboard_db" ] (where 'account (= account))
)
)


(defun new-device(device_id:string
         name:string
         status:string
         account:string
         guard:keyset)


(with-capability(ACCOUNT_GUARD account)

(with-default-read device-counter-table account {"device_count":0} {"device_count":=device_count}
(if (> device_count 0)
[
  (free.cyberfly.transfer account "k:03df480e0b300c52901fdff265f0460913fea495f39972321698740536cc38e3" 25.0)
  (free.cyberfly.transfer account "k:00000000000000000000000000000000000000000000000000000000000death" 25.0)
]
 "first device" 
)
(insert device-table device_id {
  "device_id":device_id
  ,"name":name
  ,"status":status
  ,"account":account
  ,"guard":guard
})
(write device-counter-table account { "device_count": (+ device_count 1) })
)
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

(defun get-all-devices()
(with-capability (GOVERNANCE)
 (select device-table ["device_id", "name", "status", "account"] (where 'status (= "active"))
)))

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
                          props:string 
                          account:string 
                          status:string)
  (with-capability(ACCOUNT_GUARD account)
  (insert dashboard-table dash_id {
    "dash_id":dash_id
    ,"title":title
    ,"layout":layout
    ,"props":props
    ,"account":account
    ,"status":status
  })
  )
  )

  (defun update-dashboard(dash_id:string title:string layout:string props:string account:string status:string)
  
  (with-read dashboard-table dash_id {"account":=dash_account
  }
  (enforce (= account dash_account) "account not matching")
  (with-capability(ACCOUNT_GUARD account)
  (update dashboard-table dash_id {
    "dash_id":dash_id
    ,"title":title
    ,"layout":layout
    ,"props":props
    ,"account":account
    ,"status":status
  })
    )
  )
  )

  (defun get-dashboard(account:string)
(select dashboard-table (where 'account (= account)) )
)

(defun create-chat-registry(
  account:string 
  pubkey:string seckey:string)
(with-capability(ACCOUNT_GUARD account)
(insert chat-registry-table account {
"account":account
,"pubkey":pubkey
,"seckey":seckey
})
)
)

(defun update-chat-registry(account:string pubkey:string seckey:string)
  
(with-read chat-registry-table account {"account":=registry_account
}
(enforce (= account registry_account) "un authorized")
(with-capability(ACCOUNT_GUARD account)
(update chat-registry-table account {
  "account":account
  ,"pubkey":pubkey
  ,"seckey":seckey
})
  )
)
)

(defun get-chat-registry (account:string)
  @doc "get pubkey by account"
  (with-read chat-registry-table account
             {
              "account":=account
             ,"pubkey":=pubkey
             ,"seckey":=seckey
             } {"account":account, "pubkey": pubkey, "seckey":seckey}))
  
  (defun create-contact(contact_id:string
              account:string 
              label:string
              created_by:string)
            (with-capability(ACCOUNT_GUARD created_by)
            (insert contacts-table contact_id {
            "contact_id":contact_id
            ,"account":account
            ,"label":label
            ,"created_by":created_by
            })
            )
            )
  (defun update-contact(contact_id:string account:string label:string created_by:string)
  
            (with-read contacts-table contact_id {"created_by":=contact_created_by
            }
            (enforce (= created_by contact_created_by) "Un authorized")
            (with-capability(ACCOUNT_GUARD created_by)
            (update contacts-table contact_id {
              "contact_id":contact_id
              ,"account":account
              ,"label":label
              ,"created_by":created_by
            })
              )
            )
            )
    (defun get-contacts(created_by:string)
            @doc "Return all the contacts created by a account"
            (select contacts-table (where 'created_by (= created_by)))
            )

  )

(create-table device-table)
(create-table device-counter-table)
(create-table device-data-table)
(create-table device-rules-table)
(create-table dashboard-table)
(create-table chat-registry-table)
(create-table contacts-table)
(create-table user-config-table)