(ns clj-ldap.test.client
  "Automated tests for clj-ldap"
  (:require [clj-ldap.client :as ldap]
            [clj-ldap.test.server :as server])
  (:use clojure.test)
  (:import (com.unboundid.ldap.sdk LDAPException)))


;; Tests are run over a variety of connection types (LDAP and LDAPS for now)
(def ^:dynamic *connections* nil)
(def ^:dynamic *conn* nil)
(def ^:dynamic *c* nil)

;; Tests concentrate on a single object class
(def toplevel* "dc=alienscience,dc=org,dc=uk")
(def base* (str "ou=people," toplevel*))
(def dn*  (str "cn=%s," base*))
(def object-class* #{"top" "person"})

;; Variable to catch side effects
(def ^:dynamic *side-effects* nil)

;; Result of a successful write
(def success*      {:code 0 :name "success"})

(defn read-bytes-from-file
  [filename]
  (let [f (java.io.File. filename)
        ary (byte-array (.length f))
        is (java.io.FileInputStream. f)]
    (.read is ary)
    (.close is)
    ary))

;; People to test with
(def person-a*
     {:dn (format dn* "testa")
      :object {:objectClass object-class*
               :cn "testa"
               :sn "a"
               :description "description a"
               :telephoneNumber "000000001"
               :userPassword "passa"}})

(def person-b*
     {:dn (format dn* "testb")
      :object {:objectClass object-class*
               :cn "testb"
               :sn "b"
               :description "István Orosz"
               :telephoneNumber ["000000002" "00000003"]
               :userPassword "passb"}})

(def person-c*
     {:dn (format dn* "André Marchand")
      :object {:objectClass object-class*
               :cn "André Marchand"
               :sn "Marchand"
               :description "description c"
               :telephoneNumber "000000004"
               :userPassword "passc"}})

(defn- connect-to-server
  "Opens a sequence of connection pools on the localhost server with the
   given ports"
  [port ssl-port]
  [
   (ldap/connect {:host {:port port}})
   (ldap/connect {:host {:address "localhost"
                         :port port}
                  :num-connections 4})
   (ldap/connect {:host (str "localhost:" port)})
   (ldap/connect {:ssl? true
                  :host {:port ssl-port}
                  :initial-connections 2})
   (ldap/connect {:starTLS? true
                  :host {:port port}})
   (ldap/connect {:host {:port port}
                  :connect-timeout 1000
                  :timeout 5000
                  :max-connections 2})
   (ldap/connect {:host [(str "localhost:" port)
                         {:port ssl-port}]})
   (ldap/connect {:host [(str "localhost:" ssl-port)
                         {:port ssl-port}]
                  :ssl? true
                  :num-connections 5
                  :max-connections 10})])


(defn- test-server
  "Setup server"
  [f]
  (server/start!)
  (binding [*connections* (connect-to-server (server/ldapPort) (server/ldapsPort))]
    (f))
  (server/stop!))

(defn- add-toplevel-objects!
  "Adds top level entries, needed for testing, to the ldap server"
  [connection]
  (ldap/add connection toplevel*
            {:objectClass ["top" "domain" "extensibleObject"]
             :dc "alienscience"})
  (ldap/add connection base*
            {:objectClass ["top" "organizationalUnit"]
             :ou "people"})
  (ldap/add connection
            (str "cn=Saul Hazledine," base*)
            {:objectClass ["top" "Person"]
             :cn "Saul Hazledine"
             :sn "Hazledine"
             :description "Creator of bugs"}))

(defn- test-data
  "Provide test data"
  [f]
  (doseq [connection *connections*]
    (binding [*conn* connection]
      (try
        (add-toplevel-objects! *conn*)
        (ldap/add *conn* (:dn person-a*) (:object person-a*))
        (ldap/add *conn* (:dn person-b*) (:object person-b*))
        (catch Exception e))
      (f)
      (try
        (ldap/delete *conn* toplevel* {:delete-subtree true})
        (catch Exception e)))))

(use-fixtures :each test-data)
(use-fixtures :once test-server)

(deftest test-get
  (is (= (assoc (:object person-a*) :dn (:dn person-a*))
         (ldap/get *conn* (:dn person-a*))))
  (is (= (assoc (:object person-b*) :dn (:dn person-b*))
         (ldap/get *conn* (:dn person-b*))))
  (is (= {:dn (:dn person-a*)
          :cn (-> person-a* :object :cn)
          :sn (-> person-a* :object :sn)}
         (ldap/get *conn* (:dn person-a*) [:cn :sn]))))

(deftest some-bind
  (is (= {:code 0, :name "success"}
         (ldap/bind *conn* (:dn person-a*) "passa")))
  (is (thrown? LDAPException (ldap/bind *conn* (:dn person-a*) "notthepass")))
  (is (thrown? LDAPException (ldap/bind *conn* "cn=does,ou=not,cn=exist" "password"))))

(deftest bad-bind?
  (is (= false (ldap/bind? *conn* (:dn person-a*) "not-the-password")))
  (is (= false (ldap/bind? *conn* "cn=does,ou=not,cn=exist" "password"))))

(deftest test-bind
  (if (> (-> *conn*
             (.getConnectionPoolStatistics)
             (.getMaximumAvailableConnections)) 1)
    (binding [*c* (ldap/get-connection *conn*)]
      (let [before (ldap/who-am-i *c*)
            _ (ldap/bind? *c* (:dn person-a*) "passa")
            a (ldap/who-am-i *c*)
            _ (ldap/release-connection *conn* *c*)]
        (is (= ["" (:dn person-a*)] [before a]))))))

(deftest test-add-delete
  (is (= success* (ldap/add *conn* (:dn person-c*) (:object person-c*))))
  (is (= (assoc (:object person-c*) :dn (:dn person-c*))
         (ldap/get *conn* (:dn person-c*))))
  (is (= success* (ldap/delete *conn* (:dn person-c*))))
  (is (nil? (ldap/get *conn* (:dn person-c*))))
  (is (= success*
         (ldap/add *conn* (str "changeNumber=1234," base*)
                   {:objectClass ["changeLogEntry"]
                    :changeNumber 1234
                    :targetDN base*
                    :changeType "modify"})))
  (is (= "1234" (:changeNumber (ldap/get *conn* (str "changeNumber=1234," base*)))))
  (is (= {:code 0, :name "success",
          :pre-read {:objectClass #{"top" "changeLogEntry"}}}
         (ldap/delete *conn* (str "changeNumber=1234," base*)
                      {:pre-read [:objectClass]}))))

(deftest test-delete-subtree
  (is (= success* (ldap/add *conn* (:dn person-c*) (:object person-c*))))
  (is (= success* (ldap/delete *conn* base* {:delete-subtree true})))
  (is (nil? (ldap/get *conn* base*))))

(deftest test-modify-add
  (is (= {:code 0, :name "success",
          :pre-read {:objectClass #{"top" "person"}, :cn "testa"},
          :post-read {:l "Hollywood", :cn "testa"}}
         (ldap/modify *conn* (:dn person-a*)
                      {:add {:objectClass "organizationalPerson"
                             :l "Hollywood"}
                       :pre-read #{:objectClass :l :cn}
                       :post-read #{:l :cn}})))
  (is (= success*
         (ldap/modify *conn* (:dn person-b*)
           {:add {:telephoneNumber ["0000000005" "0000000006"]}})))
  (let [new-a (ldap/get *conn* (:dn person-a*))
        new-b (ldap/get *conn* (:dn person-b*))
        obj-a (:object person-a*)
        obj-b (:object person-b*)]
    (is  (= (conj (:objectClass obj-a) "organizationalPerson")
            (:objectClass new-a)))
    (is (= "Hollywood" (:l new-a)))
    (is (= (set (concat (:telephoneNumber obj-b)
                        ["0000000005" "0000000006"]))
           (set (:telephoneNumber new-b))))))

(deftest test-modify-delete
  (let [b-phonenums (-> person-b* :object :telephoneNumber)]
    (is (= success*
           (ldap/modify *conn* (:dn person-a*)
                        {:delete {:description :all}})))
    (is (= success*
           (ldap/modify *conn* (:dn person-b*)
                        {:delete {:telephoneNumber (first b-phonenums)}})))
    (is (= (-> (:object person-a*)
               (dissoc :description)
               (assoc :dn (:dn person-a*)))
           (ldap/get *conn* (:dn person-a*))))
    (is (= (-> (:object person-b*)
               (assoc :telephoneNumber (second b-phonenums))
               (assoc :dn (:dn person-b*)))
           (ldap/get *conn* (:dn person-b*))))))

(deftest test-modify-replace
  (let [new-phonenums (-> person-b* :object :telephoneNumber)
        certificate-data (read-bytes-from-file
                           "test-resources/cert.binary")]
    (is (= success*
           (ldap/modify *conn* (:dn person-a*)
                        {:replace {:telephoneNumber new-phonenums}})))
    (is (= (-> (:object person-a*)
               (assoc :telephoneNumber new-phonenums)
               (assoc :dn (:dn person-a*)))
           (ldap/get *conn* (:dn person-a*))))

    (is (= success*
           (ldap/modify *conn* (:dn person-a*)
                        {:add {:objectclass ["inetOrgPerson"
                                             "organizationalPerson"]
                               :userCertificate certificate-data}}
                        {:proxied-auth (str "dn:" (:dn person-a*))})))
    (is (= (seq certificate-data)
           (seq (:userCertificate
                  (first (ldap/search *conn* (:dn person-a*)
                                      {:scope :base
                                       :filter "(objectclass=inetorgperson)"
                                       :attributes [:userCertificate]
                                       :byte-valued [:userCertificate]}))))))
    (is (= (seq certificate-data)
           (seq (:userCertificate
                  (first (ldap/search *conn* (:dn person-a*)
                                      {:scope :base
                                       :byte-valued [:userCertificate]}))))))
    (is (= (seq certificate-data)
           (seq (:userCertificate (ldap/get *conn* (:dn person-a*)
                                            [:userCertificate]
                                            [:userCertificate])))))))

(deftest test-modify-all
  (let [b (:object person-b*)
        b-phonenums (:telephoneNumber b)]
    (is (= success*
           (ldap/modify *conn* (:dn person-b*)
                        {:add {:telephoneNumber "0000000005"}
                         :delete {:telephoneNumber (second b-phonenums)}
                         :replace {:description "desc x"}})))
    (let [new-b (ldap/get *conn* (:dn person-b*))]
      (is (= (set [(first b-phonenums) "0000000005"])
             (set (:telephoneNumber new-b))))
      (is (= "desc x" (:description new-b))))))

(deftest test-search
  (is (= (set [nil "testa" "testb" "Saul Hazledine"])
         (set (map :cn
                   (ldap/search *conn* base* {:attributes [:cn]})))))
  (is (= (set ["testa" "testb"])
         (set (map :cn
                   (ldap/search *conn* base*
                                {:attributes [:cn]
                                 :filter     "cn=test*"
                                 :proxied-auth  (str "dn:" (:dn person-a*))})))))
  (is (= '("Saul Hazledine" "testa" "testb")
         (map :cn
              (ldap/search *conn* base*
                           {:filter "cn=*"
                            :server-sort {:is-critical true
                                          :sort-keys [:cn :ascending]}}))))

  (is (= 2 (count (map :cn
                       (ldap/search *conn* base*
                                    {:attributes [:cn] :filter "cn=*"
                                     :size-limit 2})))))
  (is (= nil (:description (map :cn
                                (ldap/search *conn* base*
                                            {:attributes [:cn]
                                             :filter "cn=István Orosz"
                                             :types-only true})))))
  (is (= (set ["István Orosz"])
         (set (map :description
                   (ldap/search *conn* base*
                                {:filter "cn=testb" :types-only false})))))
  (binding [*side-effects* #{}]
    (ldap/search! *conn* base* {:attributes [:cn :sn] :filter "cn=test*"}
                  (fn [x]
                    (set! *side-effects*
                          (conj *side-effects* (dissoc x :dn)))))
    (is (= (set [{:cn "testa" :sn "a"}
                 {:cn "testb" :sn "b"}])
           *side-effects*))))


(deftest test-search-all
  (let [options {:attributes [:cn] :page-size 2}
        lazy-results (ldap/search-all *conn* base* options)
        eager-results (ldap/search *conn* base* options)
        ->cn-set (comp set (partial map :cn))]
    (testing "Since search-all is lazy, not all results are fetched"
      (is (not (realized? lazy-results))))
    (testing "Lazy and eager search eventually produce the same results"
      (is (->cn-set lazy-results)
          (->cn-set eager-results)))))

(deftest test-compare?
  (is (= true (ldap/compare? *conn* (:dn person-b*)
                             :description "István Orosz")))
  (is (= false (ldap/compare? *conn* (:dn person-a*)
                              :description "István Orosz"
                              {:proxied-auth (str "dn:" (:dn person-b*))}))))
