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
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

pub fn create_client() -> Result<Client, Error> {
  let connector: Connector = Connector::usb(&Default::default());
  Client::open(connector, Default::default(), true)
}

#[no_mangle]
pub extern fn put_ed_key(id: u16, label: &[u8; LABEL_SIZE], domains: u16, key: &[u8; KEY_SIZE]) -> () {
  let client: Client = create_client().expect("could not connect to YubiHSM");
  client.delete_object(id, object::Type::AsymmetricKey);
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
