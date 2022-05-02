use yubihsm::client::Error;
use yubihsm::{object, Capability, Client, Connector, Domain};
use yubihsm::object::Id;
use yubihsm::{asymmetric::signature::Signer as _};
use yubihsm::asymmetric::Algorithm;
use yubihsm::ed25519::Signature;
use std::slice;

const LABEL_SIZE: usize = 40;
const KEY_SIZE: usize = 32;
const SIGNATURE_SIZE: usize = 64;

mod test;

pub fn create_client(connector: Connector) -> Result<Client, Error> {
 // let connector: Connector = Connector::usb(&Default::default());
  Client::open(connector, Default::default(), true)
}


#[no_mangle]
pub extern fn sign_with_ed_key(id: u16, msgptr: *const u8, msglen: usize, result: *mut u8) -> () {
  let connector: Connector = Connector::usb(&Default::default());
  //let _: Connector = mockhsm();
  let client: Client = create_client(connector).expect("could not connect to YubiHSM");
  let msg: &[u8] = unsafe { slice::from_raw_parts(msgptr, msglen) };
  sign_with_ed_key_internal(client,id,msg,result);

}

pub fn sign_with_ed_key_internal(client: Client,id: u16, msg: &[u8], result: *mut u8) -> () {
 // let client: Client = create_client().expect("could not connect to YubiHSM");
  let sig: Signature = client.sign_ed25519(id, msg).expect("could not get the signature");
  let sigbytes: [u8; SIGNATURE_SIZE] = sig.to_bytes();
 // return sigbytes;
  unsafe { sigbytes.as_ptr().copy_to(result, SIGNATURE_SIZE) }
}

#[no_mangle]
pub extern fn put_ed_key(id: u16, label: &[u8; LABEL_SIZE], domain: u16, key: &[u8; KEY_SIZE]) -> () {
 // let _ = Domain[0];
  let connector: Connector = Connector::usb(&Default::default());
  let client: Client = create_client(connector).expect("could not connect to YubiHSM");
  put_ed_key_internal(&client,id,label,domain,key);
}

pub fn put_ed_key_internal(client: &Client, id: u16, label: &[u8; LABEL_SIZE], domain: u16, key: &[u8; KEY_SIZE]) -> () {
  let _ = client.delete_object(id, object::Type::AsymmetricKey);
  let t_domain: Domain =  Domain::from_bits_truncate(domain);
 // print!("domain is {}", tDomain);
  client.put_asymmetric_key(
    id,
    object::Label::from_bytes(label).expect("failed to construct Label from byte array"),
    t_domain,
    Capability::SIGN_EDDSA,
    Algorithm::Ed25519,
    &key[..]
  ).expect("could not put the key");
  ()
}


fn make_asymmetric_key(label: &[u8; LABEL_SIZE],domain: Domain, key_id: u16) -> Result<object::Id,Error>{
  let connector: Connector = Connector::usb(&Default::default());
  let client: Client = create_client(connector).expect("could not connect to YubiHSM");
  return client.generate_asymmetric_key(
    key_id,
    object::Label::from_bytes(label).expect("failed to construct Label from byte array"),
    domain,
    Capability::SIGN_EDDSA,
    Algorithm::Ed25519 );
 }