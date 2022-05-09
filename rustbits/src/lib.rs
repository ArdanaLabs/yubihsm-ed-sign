use yubihsm::client::Error;
use yubihsm::{object, Capability, Client, Connector, Domain};
use yubihsm::asymmetric::Algorithm;
use yubihsm::ed25519::Signature;
use std::slice;

const LABEL_SIZE: usize = 40;
const KEY_SIZE: usize = 32;
const SIGNATURE_SIZE: usize = 64;

mod test;

pub fn create_client(connector: Connector) -> Result<Client, Error> {
  Client::open(connector, Default::default(), true)
}

#[no_mangle]
pub extern fn hello_world() {
  print!("Hello world");
}

#[cfg(debug_assertions)]
fn mock_connector() -> Connector {
  Connector::mockhsm()
}
 
#[cfg(not(debug_assertions))]
fn mock_connector() -> Connector {
  panic!("Error: Attempted to use the testing mock connector with debug_assertions.")
}

fn make_connector(testing_mock: bool) -> Connector {
  if testing_mock { mock_connector() } else {
    panic!("wrong")
    // Connector::usb(&Default::default())
  }
}

#[no_mangle]
pub unsafe extern fn sign_with_ed_key(id: u16, msgptr: *const u8, msglen: usize, result: *mut u8, testing_mock: bool) {
  let connector = make_connector(testing_mock);
  let client: Client = create_client(connector).expect("could not connect to YubiHSM");
  let msg: &[u8] = slice::from_raw_parts(msgptr, msglen);
   sign_with_ed_key_internal(&client,id,msg,result);
}

pub unsafe fn sign_with_ed_key_internal(client: &Client,id: u16, msg: &[u8], result: *mut u8) -> () {
 
  let sig: Signature = client.sign_ed25519(id, msg).expect("could not get the signature");
  let sigbytes: [u8; SIGNATURE_SIZE] = sig.to_bytes();
 
  sigbytes.as_ptr().copy_to(result, SIGNATURE_SIZE);
}

#[no_mangle]
pub extern fn put_ed_key(id: u16, label: &[u8; LABEL_SIZE], domain: u16, key: &[u8; KEY_SIZE], testing_mock: bool) -> () {
 
  let connector = make_connector(testing_mock); 
  let client: Client = create_client(connector).expect("could not connect to YubiHSM");
  put_ed_key_internal(&client,id,label,domain,key);
}

pub fn put_ed_key_internal(client: &Client, id: u16, label: &[u8; LABEL_SIZE], domain: u16, key: &[u8; KEY_SIZE]) -> () {
  let _ = client.delete_object(id, object::Type::AsymmetricKey);
  let t_domain: Domain =  Domain::from_bits_truncate(domain);

  client.put_asymmetric_key(
    id,
    object::Label::from_bytes(label).expect("failed to construct Label from byte array"),
    t_domain,
    Capability::SIGN_EDDSA,
    Algorithm::Ed25519,
    &key[..]
  ).expect("could not put the key");
}
