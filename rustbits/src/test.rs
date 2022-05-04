#[cfg(test)]
mod tests {
  use yubihsm::{object, Capability, Client, Connector};
  use crate::create_client;
  use crate::sign_with_ed_key_internal;
  const TEST_KEY_ID: u16 = 200;
  const TEST_SIGNING_KEY_DOMAINS: yubihsm::Domain = yubihsm::Domain::DOM1;

  const TEST_DOMAIN: u16 = 0x0001;
  
  const TEST_SIGNING_KEY_LABEL: &str = "Signatory test key";
  const MESSAGE: &[u8] = b"";
  const SECRETKEY: &[u8; 32] = b"\x9D\x61\xB1\x9D\xEF\xFD\x5A\x60\xBA\x84\x4A\xF4\x92\xEC\x2C\xC4\x44\x49\xC5\x69\x7B\x32\x69\x19\x70\x3B\xAC\x03\x1C\xAE\x7F\x60";
  const PUBLICKEY: &[u8; 32] = b"\xD7\x5A\x98\x01\x82\xB1\x0A\xB7\xD5\x4B\xFE\xD3\xC9\x64\x07\x3A\x0E\xE1\x72\xF3\xDA\xA6\x23\x25\xAF\x02\x1A\x68\xF7\x07\x51\x1A";
  const SIGNATURE: &[u8; 64] = b"\xE5\x56\x43\x00\xC3\x60\xAC\x72\x90\x86\xE2\xCC\x80\x6E\x82\x8A\x84\x87\x7F\x1E\xB8\xE5\xD9\x74\xD8\x73\xE0\x65\x22\x49\x01\x55\x5F\xB8\x82\x15\x90\xA3\x3B\xAC\xC6\x1E\x39\x70\x1C\xF9\xB4\x6B\xD2\x5B\xF5\xF0\x59\x5B\xBE\x24\x65\x51\x41\x43\x8E\x7A\x10\x0B";
  
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
 
    #[test]
    fn test_put_ed_key() {
    
      use crate::put_ed_key_internal;
      
      let l:&[u8; 40] = &[0u8;40];
      
      let connector: Connector = Connector::mockhsm();
      let client: Client = create_client(connector).expect("could not connect to YubiHSM");
      client.delete_object(TEST_KEY_ID, object::Type::AsymmetricKey);
      put_ed_key_internal(
        &client,
        TEST_KEY_ID,
        l,
        TEST_DOMAIN,
       SECRETKEY
        );
      //  
      match client.get_public_key(TEST_KEY_ID) {
        Ok(key) => {
          client.reset_device(); // Probably unnessecary 
          assert_eq!(key.bytes, PUBLICKEY)
        },
        Err(e) => panic!("Error during asymmetric key test {}", e),
      }
      
    }
     #[test]
    fn test_sign_with_ed_key() {
      use yubihsm::asymmetric;
      
      let mut res = [0u8; 64]; 
      
      
      let connector: Connector = Connector::mockhsm();
      let client: Client = create_client(connector).expect("could not connect to YubiHSM");
      client.delete_object(TEST_KEY_ID, object::Type::AsymmetricKey);
      client.put_asymmetric_key(
        TEST_KEY_ID,
        TEST_SIGNING_KEY_LABEL.into(),
        TEST_SIGNING_KEY_DOMAINS,
        Capability::SIGN_EDDSA,
        asymmetric::Algorithm::Ed25519,
        SECRETKEY.to_vec()
      ).unwrap();

      unsafe {sign_with_ed_key_internal(&client,TEST_KEY_ID, MESSAGE,res.as_mut_ptr())};
      client.reset_device(); // probably unessecary
      assert_eq!(SIGNATURE,&res);
     
   }



  }








