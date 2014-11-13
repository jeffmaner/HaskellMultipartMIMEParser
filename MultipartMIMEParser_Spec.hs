module MultipartMIMEParser_Spec where

import MultipartMIMEParser
import Test.Hspec

main :: IO ()
main = hspec $ do
  --let p = readFile "C:/Users/jeff.maner/Documents/Projects/RosettaNet/sampleRNIFReceiptAck.txt"

  describe "Multipart MIME Parser" $ do
    it "Parses a header into a name and value and recognizes the name." $
       (hName $ parseHeader "Mime-Version: 1.0") `shouldBe` "Mime-Version"

    it "Parses a header into a name and value and recognizes the value." $
       (hValue $ parseHeader "Mime-Version: 1.0") `shouldBe` "1.0"

    it "Recognizes and parses additional header information." $
       (hAddl $ parseHeader "content-type: Multipart/related; boundary=\"RN-Http-Body-Boundary\"; type=\"multipart/related\"")
       `shouldBe`
       [("boundary","RN-Http-Body-Boundary"),("type","multipart/related")]

    it "Handles a single header up to a blank field." $
       (parseHeaders $ unlines ["content-type: Multipart/related; boundary=\"RN-Http-Body-Boundary\"; type=\"multipart/related\"",""])
       `shouldBe`
       [Header { hName="content-type", hValue="Multipart/related", hAddl=[("boundary","RN-Http-Body-Boundary"),("type","multipart/related")] }]

    it "Handles multiple headers up to a blank field." $
       (parseHeaders $ unlines [ "Content-Type: Application/XML"
                               , "Content-Transfer-Encoding: binary"
                               , "Content-Location: RN-Preamble"
                               , "Content-ID: <1430586.1160080657050.JavaMail.webmethods@exshaw>"
                               , ""
                               , "Content."])
       `shouldBe`
       [ Header { hName="Content-Type", hValue="Application/XML", hAddl=[] }
       , Header { hName="Content-Transfer-Encoding", hValue="binary", hAddl=[] }
       , Header { hName="Content-Location", hValue="RN-Preamble", hAddl=[] }
       , Header { hName="Content-ID", hValue="<1430586.1160080657050.JavaMail.webmethods@exshaw>", hAddl=[] } ]

    it "Handles multiple headers, including additional information wrapped to a subsequent line." $
       (parseHeaders $ unlines [ "Message-ID: <25845033.1160080657073.JavaMail.webmethods@exshaw>"
                               , "Mime-Version: 1.0"
                               , "Content-Type: multipart/related; type=\"application/xml\";"
                               , "  boundary=\"----=_Part_235_11184805.1160080657052\""
                               , ""
                               , "Content." ])
       `shouldBe`
       [ Header { hName="Message-ID", hValue="<25845033.1160080657073.JavaMail.webmethods@exshaw>", hAddl=[] }
       , Header { hName="Mime-Version", hValue="1.0", hAddl=[]}
       , Header { hName="Content-Type", hValue="multipart/related", hAddl=[("type", "application/xml"),("boundary","----=_Part_235_11184805.1160080657052")]} ]

    it "Handles simple content." $
       (parseContent $ unlines [ "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
                               , "<!DOCTYPE Preamble SYSTEM \"Preamble_MS_V02_00.dtd\">"
                               , "<Preamble>"
                               , "  <standardName>"
                               , "    <GlobalAdministeringAuthorityCode>RosettaNet</GlobalAdministeringAuthorityCode>"
                               , "  </standardName>"
                               , "  <standardVersion>"
                               , "    <VersionIdentifier>V02.00</VersionIdentifier>"
                               , "  </standardVersion>"
                               , "</Preamble>"
                               , "" ])
       `shouldBe`
       (unlines [ "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
                , "<!DOCTYPE Preamble SYSTEM \"Preamble_MS_V02_00.dtd\">"
                , "<Preamble>"
                , "  <standardName>"
                , "    <GlobalAdministeringAuthorityCode>RosettaNet</GlobalAdministeringAuthorityCode>"
                , "  </standardName>"
                , "  <standardVersion>"
                , "    <VersionIdentifier>V02.00</VersionIdentifier>"
                , "  </standardVersion>"
                , "</Preamble>" ])

    it "Handles a simple post." $
       (parsePost $ unlines [ "Content-Type: Application/XML"
                            , "Content-Transfer-Encoding: binary"
                            , "Content-Location: RN-Preamble"
                            , "Content-ID: <1430586.1160080657050.JavaMail.webmethods@exshaw>"
                            , ""
                            , "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
                            , "<!DOCTYPE Preamble SYSTEM \"Preamble_MS_V02_00.dtd\">"
                            , "<Preamble>"
                            , "  <standardName>"
                            , "    <GlobalAdministeringAuthorityCode>RosettaNet</GlobalAdministeringAuthorityCode>"
                            , "  </standardName>"
                            , "  <standardVersion>"
                            , "    <VersionIdentifier>V02.00</VersionIdentifier>"
                            , "  </standardVersion>"
                            , "</Preamble>"
                            , "" ])
       `shouldBe`
       Post { pHeaders = [ Header { hName="Content-Type", hValue="Application/XML", hAddl=[] }
                         , Header { hName="Content-Transfer-Encoding", hValue="binary", hAddl=[] }
                         , Header { hName="Content-Location", hValue="RN-Preamble", hAddl=[] }
                         , Header { hName="Content-ID", hValue="<1430586.1160080657050.JavaMail.webmethods@exshaw>", hAddl=[] } ]
            , pContent = unlines [ "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
                                 , "<!DOCTYPE Preamble SYSTEM \"Preamble_MS_V02_00.dtd\">"
                                 , "<Preamble>"
                                 , "  <standardName>"
                                 , "    <GlobalAdministeringAuthorityCode>RosettaNet</GlobalAdministeringAuthorityCode>"
                                 , "  </standardName>"
                                 , "  <standardVersion>"
                                 , "    <VersionIdentifier>V02.00</VersionIdentifier>"
                                 , "  </standardVersion>"
                                 , "</Preamble>" ] }

    it "Handles a complex post." $
       (parsePost $ unlines [ "content-type: Multipart/related; boundary=\"RN-Http-Body-Boundary\"; type=\"multipart/related\""
                            , ""
                            , "--RN-Http-Body-Boundary"
                            , "Message-ID: <25845033.1160080657073.JavaMail.webmethods@exshaw>"
                            , "Mime-Version: 1.0"
                            , "Content-Type: multipart/related; type=\"application/xml\";"
                            , "  boundary=\"----=_Part_235_11184805.1160080657052\""
                            , ""
                            , "------=_Part_235_11184805.1160080657052"
                            , "Content-Type: Application/XML"
                            , "Content-Transfer-Encoding: binary"
                            , "Content-Location: RN-Preamble"
                            , "Content-ID: <1430586.1160080657050.JavaMail.webmethods@exshaw>"
                            , ""
                            , "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
                            , "<!DOCTYPE Preamble SYSTEM \"Preamble_MS_V02_00.dtd\">"
                            , "<Preamble>"
                            , "  <standardName>"
                            , "    <GlobalAdministeringAuthorityCode>RosettaNet</GlobalAdministeringAuthorityCode>"
                            , "  </standardName>"
                            , "  <standardVersion>"
                            , "    <VersionIdentifier>V02.00</VersionIdentifier>"
                            , "  </standardVersion>"
                            , "</Preamble>"
                            , ""
                            , "------=_Part_235_11184805.1160080657052"
                            , "Content-Type: Application/XML"
                            , "Content-Transfer-Encoding: binary"
                            , "Content-Location: RN-Delivery-Header"
                            , "Content-ID: <18888489.1160080657050.JavaMail.webmethods@exshaw>"
                            , ""
                            , "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
                            , "<!DOCTYPE DeliveryHeader SYSTEM \"DeliveryHeader_MS_V02_00.dtd\">"
                            , "<DeliveryHeader>"
                            , "  <isSecureTransportRequired>"
                            , "    <AffirmationIndicator>Yes</AffirmationIndicator>"
                            , "  </isSecureTransportRequired>"
                            , "  <messageDateTime>"
                            , "    <DateTimeStamp>20061005T143737.010Z</DateTimeStamp>"
                            , "  </messageDateTime>"
                            , "  <messageReceiverIdentification>"
                            , "    <PartnerIdentification>"
                            , "      <domain>"
                            , "        <FreeFormText xml:lang=\"EN\">DUNS</FreeFormText>"
                            , "      </domain>"
                            , "      <GlobalBusinessIdentifier>200103377</GlobalBusinessIdentifier>"
                            , "    </PartnerIdentification>"
                            , "  </messageReceiverIdentification>"
                            , "  <messageSenderIdentification>"
                            , "    <PartnerIdentification>"
                            , "      <domain>"
                            , "        <FreeFormText xml:lang=\"EN\">DUNS</FreeFormText>"
                            , "      </domain>"
                            , "      <GlobalBusinessIdentifier>123456789</GlobalBusinessIdentifier>"
                            , "    </PartnerIdentification>"
                            , "  </messageSenderIdentification>"
                            , "  <messageTrackingID>"
                            , "    <InstanceIdentifier>c0a878d3f70d30fa0000478b</InstanceIdentifier>"
                            , "  </messageTrackingID>"
                            , "</DeliveryHeader>"
                            , "" ])
       `shouldBe`
       Post { pHeaders=[ Header { hName="content-type", hValue="Multipart/related", hAddl=[("boundary", "RN-Http-Body-Boundary"), ("type", "multipart/related")] }]
            , pContent=undefined}
