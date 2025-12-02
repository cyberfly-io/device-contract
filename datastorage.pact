(namespace "free")
(define-keyset "free.cyberfly_team_device" (read-keyset "cyberfly_team"))
(module cyberfly_devices GOVERNANCE
 @doc "device data store."

(use coin)
(use free.cyberfly_token)

  (defcap GOVERNANCE ()
    (enforce-keyset "free.cyberfly_team_device"))

 (defschema device
        @doc "Device Register"
        device_id:string
        name:string
        status:string
        account:string
        guard:guard
        access:[string]
       )
  
(defschema device-counter
  @doc "track each account device count"
  device_count:integer
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


 (deftable device-table:{device})
 (deftable device-rules-table:{device-rules})
 (deftable device-counter-table:{device-counter})

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

(with-default-read device-counter-table account {"device_count":0} {"device_count":=device_count}
(if (> device_count 0)
[
  (free.cyberfly_token.transfer account "k:03df480e0b300c52901fdff265f0460913fea495f39972321698740536cc38e3" 25.0)
  (free.cyberfly_token.transfer account "k:0000000000000000000000000000000000000000000000000000000000000000" 25.0)
]
 "first device" 
)
(insert device-table device_id {
  "device_id":device_id
  ,"name":name
  ,"status":status
  ,"account":account
  ,"guard":guard
  ,"access":[]
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

(defun add-device-access(device_id:string pubkey:string)
  @doc "Add a pubkey to device access list"
  (with-capability (DEVICE_GUARD device_id)
    (with-read device-table device_id {"access":=current_access}
      (enforce (not (contains pubkey current_access)) "Pubkey already has access")
      (update device-table device_id {
        "access": (+ current_access [pubkey])
      })
    )
  )
)

(defun remove-device-access(device_id:string pubkey:string)
  @doc "Remove a pubkey from device access list"
  (with-capability (DEVICE_GUARD device_id)
    (with-read device-table device_id {"access":=current_access}
      (enforce (contains pubkey current_access) "Pubkey does not have access")
      (update device-table device_id {
        "access": (filter (compose (not) (= pubkey)) current_access)
      })
    )
  )
)

(defun get-device-access(device_id:string)
  @doc "Get list of pubkeys with access to device"
  (with-read device-table device_id {"access":=access}
    access
  )
)


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
         ,"guard":=guard
         ,"access":=access
 }
 {"device_id":device_id , "name":name, "status":status, "guard":guard, "access":access  })
)


(defun get-account-devices(account:string)
(select device-table ["device_id", "name", "status", "guard"] (where 'account (= account))
)
)

(defun get-all-devices()
(with-capability (GOVERNANCE)
 (select device-table ["device_id", "name", "status", "account"] (where 'status (= "active"))
)))


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



  (defun read-device-rules(device_id:string)
  (select device-rules-table (where 'device_id (= device_id))
  ))


  )

(create-table device-table)
(create-table device-counter-table)
(create-table device-rules-table)
