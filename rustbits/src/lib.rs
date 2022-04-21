use yubihsm::client::Error;
use yubihsm::{object, Capability, Client, Connector, Domain};
use yubihsm::asymmetric::Algorithm;
use yubihsm::ed25519::Signature;
use std::slice;

const LABEL_SIZE: usize = 40;
const KEY_SIZE: usize = 32;
const SIGNATURE_SIZE: usize = 64;

#[cfg(test)]
mod tests {
  use crate::create_client;
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
    #[test]
    fn test_create_client() {
      let res = create_client();
      let isgood = match res {
        Ok(_) => true,
        Err(_) => false,
      };
      assert_eq!(isgood,true);
    }
    // Client is not ffi safe
    #[test]
    fn test_put_ed_key() {
      use yubihsm::{object, Capability};
    //  use crate::put_ed_key;
      use crate::put_ed_key_internal;
      
      let _client = create_client();

      let client = match _client {
        Ok(c) => c,
        Err(_)     => panic!("failed to create client"),
      };
      let label = b"\x73\x74\x75\x70\x69\x64\x73\x74\x75\x70\x69\x64\x73\x74\x75\x70\x69\x64\x73\x74\x75\x70\x69\x64\x73\x74\x75\x70\x69\x64\x73\x74\x75\x70\x69\x64\x73\x74\x75\x70";
      let key = b"\x9D\x61\xB1\x9D\xEF\xFD\x5A\x60\xBA\x84\x4A\xF4\x92\xEC\x2C\xC4\x44\x49\xC5\x69\x7B\x32\x69\x19\x70\x3B\xAC\x03\x1C\xAE\x7F\x60";
      let capabilities = Capability::SIGN_EDDSA;
      put_ed_key_internal(&client,4, label, 0, key );
      let object_info = client.get_object_info(4, object::Type::AsymmetricKey).unwrap_or_else(|err| panic!("error getting object info: {}", err));
        assert_eq!(object_info.capabilities, capabilities);
   }


}

pub fn create_client() -> Result<Client, Error> {
  let connector: Connector = Connector::http(&Default::default());
  Client::open(connector, Default::default(), true)
}

pub extern fn put_ed_key(id: u16, label: &[u8; LABEL_SIZE], domains: u16, key: &[u8; KEY_SIZE]) -> () {
  let client: Client = create_client().expect("could not connect to YubiHSM");
    put_ed_key_internal(&client,id,label,domains,key);
}
#[no_mangle]
fn put_ed_key_internal(client: &Client, id: u16, label: &[u8; LABEL_SIZE], domains: u16, key: &[u8; KEY_SIZE]) -> () {
  let _ = client.delete_object(id, object::Type::AsymmetricKey);
  client.put_asymmetric_key(
    id,
    object::Label::from_bytes(label).expect("failed to construct Label from byte array"),
    Domain::from_bits_truncate(domains),
    Capability::SIGN_EDDSA,
    Algorithm::Ed25519,
    &key[..]
  ).expect("could not put the key");
  ()
}

#[no_mangle]
pub extern fn sign_with_ed_key(id: u16, msgptr: *const u8, msglen: usize, result: *mut u8) -> () {
  let msg: &[u8] = unsafe { slice::from_raw_parts(msgptr, msglen) };
  let client: Client = create_client().expect("could not connect to YubiHSM");
  let sig: Signature = client.sign_ed25519(id, msg).expect("could not get the signature");
  let sigbytes: [u8; SIGNATURE_SIZE] = sig.to_bytes();
  unsafe { sigbytes.as_ptr().copy_to(result, SIGNATURE_SIZE) }
}
