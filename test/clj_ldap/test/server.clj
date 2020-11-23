(ns clj-ldap.test.server
  "An embedded ldap server for unit testing"
  (:require [clj-ldap.client :as ldap])
  (:import [com.unboundid.ldap.listener
            InMemoryDirectoryServerConfig
            InMemoryDirectoryServer
            InMemoryListenerConfig])
  (:import [com.unboundid.ldap.sdk.schema
            Schema])
  (:import [com.unboundid.util.ssl
            KeyStoreKeyManager
            SSLUtil
            TrustAllTrustManager])
  (:import (java.io File)
           (java.util.logging FileHandler Level)
           (com.unboundid.util MinimalLogFormatter)))

;; server will hold an InMemoryDirectoryServer instance
(defonce server (atom nil))

(defn- createAccessLogger
  "Let the server write protocol information to test-resources/access"
  []
  (let [logFile (File. "test-resources/access")
        fileHandler (FileHandler. (.getAbsolutePath logFile) true)]
    (.setLevel fileHandler Level/INFO)
    (.setFormatter fileHandler (MinimalLogFormatter. nil false false true))
    fileHandler))

;; Server's certificate, with alias "server-cert" generated using keytool like so:
;; keytool -genkey -keyalg RSA -alias server-cert -keystore selfsigned.jks -validity 3650 -keysize 2048

(defn- start-ldap-server
  "Setup a server listening on available LDAP and LDAPS ports chosen at random"
  []
  (let [cfg (InMemoryDirectoryServerConfig. (into-array String ["dc=alienscience,dc=org,dc=uk"]))
        _ (.addAdditionalBindCredentials cfg "cn=Directory Manager" "password")
        _ (.setSchema cfg (Schema/getDefaultStandardSchema))
        _ (.setAccessLogHandler cfg (createAccessLogger))
        keystore (KeyStoreKeyManager. "test-resources/server.keystore"
                                      (char-array "password") "JKS" "server-cert")
        serverSSLUtil (SSLUtil. keystore (TrustAllTrustManager.))
        clientSSLUtil (SSLUtil. (TrustAllTrustManager.))
        _ (.setListenerConfigs cfg
                               [(InMemoryListenerConfig/createLDAPConfig
                                  "LDAP" nil 0
                                  (.createSSLSocketFactory serverSSLUtil))
                                (InMemoryListenerConfig/createLDAPSConfig
                                  "LDAPS" nil 0
                                  (.createSSLServerSocketFactory serverSSLUtil)
                                  (.createSSLSocketFactory clientSSLUtil))])
        ds (InMemoryDirectoryServer. cfg)]
    (.startListening ds)
    ds))

(defn stop!
  "Stops the embedded ldap server (listening on LDAP and LDAPS ports)"
  []
  (if @server
    (do
      (.shutDown @server true)
      (reset! server nil))))

(defn ldapPort
  []
  (.getListenPort @server "LDAP"))

(defn ldapsPort
  []
  (.getListenPort @server "LDAPS"))

(defn start!
  "Starts an embedded ldap server on the given port and SSL"
  []
  (stop!)
  (reset! server (start-ldap-server)))
