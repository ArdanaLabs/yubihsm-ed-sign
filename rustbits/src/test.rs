
#[cfg(test)]
mod tests {
  use yubihsm::{object, Capability, Client, Connector, Domain};
  use yubihsm::{asymmetric::signature::Signer as _, ed25519};
  
  extern crate libc;
  use core::mem;
  use crate::create_client;
  use crate::sign_with_ed_key_internal;
  const TEST_KEY_ID: u16 = 200;
  const TEST_SIGNING_KEY_ID: yubihsm::object::Id = 200;
  const TEST_SIGNING_KEY_DOMAINS: yubihsm::Domain = yubihsm::Domain::DOM1;
  const TEST_SIGNING_KEY_CAPABILITIES: yubihsm::Capability = yubihsm::Capability::SIGN_EDDSA;
  const TEST_MESSAGE: &[u8] =
    b"The Edwards-curve Digital Signature yubihsm::asymmetric::Algorithm  (EdDSA) is a \
        variant of Schnorr's signature system with (possibly twisted) Edwards curves.";
  const TEST_DOMAIN: u16 = 0x0001;
  const LABEL_SIZE: usize = 40;
  const TEST_SIGNING_KEY_LABEL: &str = "Signatory test key";
  const MESSAGE: &[u8] = b"";
  const SecretKey: &[u8; 32] = b"\x9D\x61\xB1\x9D\xEF\xFD\x5A\x60\xBA\x84\x4A\xF4\x92\xEC\x2C\xC4\x44\x49\xC5\x69\x7B\x32\x69\x19\x70\x3B\xAC\x03\x1C\xAE\x7F\x60";
  const PublicKey: &[u8; 32] = b"\xD7\x5A\x98\x01\x82\xB1\x0A\xB7\xD5\x4B\xFE\xD3\xC9\x64\x07\x3A\x0E\xE1\x72\xF3\xDA\xA6\x23\x25\xAF\x02\x1A\x68\xF7\x07\x51\x1A";
  
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
 //   #[test]
 //   fn test_create_client() {
 //    let connector: Connector = Connector::usb(&Default::default());
     // let connector: Connector = Connector::mockhsm;
 //    let client = create_client(connector).expect("fuck");
      
 //    let isgood = match client {
  //      Ok(_) => true,
  //      Err(_) => false,
   //   };
    
 //     assert_eq!(true,true);
      
 //   }
    
    #[test]
    fn test_sign_with_ed_key() {
      use yubihsm::{asymmetric,Domain};
    
      use crate::Signature;
      let mut placeholder: u8 = 0x1Au8;
      let res = &mut placeholder as *mut u8; 
      
      let connector: Connector = Connector::usb(&Default::default());
      let label = b"\x73\x74\x75\x70\x69\x64\x73\x74\x75\x70\x69\x64\x73\x74\x75\x70\x69\x64\x73\x74\x75\x70\x69\x64\x73\x74\x75\x70\x69\x64\x73\x74\x75\x70\x69\x64\x73\x74\x75\x70";
      
      let client: Client = create_client(connector).expect("could not connect to YubiHSM");
      client.generate_asymmetric_key(
        TEST_KEY_ID,
        TEST_SIGNING_KEY_LABEL.into(),
        Domain::DOM1,
        TEST_SIGNING_KEY_CAPABILITIES,
        asymmetric::Algorithm::Ed25519
      ).unwrap();


      
      
      sign_with_ed_key_internal(client,TEST_KEY_ID, MESSAGE,res);
      let usafe = unsafe {*res};
      
     // let signature: Signature = res as Signature;
      print!("res {}", usafe);
//      let sig = sign_with_ed_key_internal(client,TEST_KEY_ID, Message);
     // client.
   //  unsafe {libc::free(res as *mut libc::c_void)};
   

    }

    fn create_yubihsm_key(client: &Client) {
      // Delete the key in TEST_KEY_ID slot it exists
      // Ignore errors since the object may not exist yet
      let _ = client.delete_object(TEST_SIGNING_KEY_ID, yubihsm::object::Type::AsymmetricKey);
    
      // Create a new key for testing
      client
          .generate_asymmetric_key(
              TEST_SIGNING_KEY_ID,
              TEST_SIGNING_KEY_LABEL.into(),
              TEST_SIGNING_KEY_DOMAINS,
              TEST_SIGNING_KEY_CAPABILITIES,
              yubihsm::asymmetric::Algorithm::Ed25519,
          )
          .unwrap();
    }
  }








