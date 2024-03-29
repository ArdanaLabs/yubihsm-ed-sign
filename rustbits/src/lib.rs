use std::slice;

use yubihsm::asymmetric::Algorithm;
use yubihsm::client::Error;
use yubihsm::ed25519::Signature;
use yubihsm::{object, Capability, Client, Connector, Domain}; // for testing only                                                         // use std::str::{from_utf8}; // testing only
                                                              // use std::alloc::Global; // testing only

 // These constants are needed for haskell-side testing                                                             
const LABEL_SIZE: usize = 40;
const KEY_SIZE: usize = 32;
const SIGNATURE_SIZE: usize = 64;
const TEST_KEY_ID: u16 = 200;
const TEST_SIGNING_KEY_LABEL: &str = "Signatory test key";
const TEST_SIGNING_KEY_DOMAINS: yubihsm::Domain = yubihsm::Domain::DOM1;
const SECRETKEY: &[u8; 32] = b"\x9D\x61\xB1\x9D\xEF\xFD\x5A\x60\xBA\x84\x4A\xF4\x92\xEC\x2C\xC4\x44\x49\xC5\x69\x7B\x32\x69\x19\x70\x3B\xAC\x03\x1C\xAE\x7F\x60";

mod test;

pub fn create_client(connector: Connector) -> Result<Client, Error> {
    Client::open(connector, Default::default(), true)
}

#[no_mangle]
pub extern "C" fn hello_world() {
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
    if testing_mock {
        mock_connector()
    } else {
        Connector::usb(&Default::default())
    }
}

#[no_mangle]
/// # Safety
///
/// To Do -mlitchard
pub unsafe extern "C" fn sign_with_ed_key(
    id: u16,
    msgptr: *const u8,
    msglen: usize,
    result: *mut u8,
    testing_mock: bool,
) {
    let connector = make_connector(testing_mock);
    let client: Client = create_client(connector).expect("could not connect to YubiHSM");
    let msg: &[u8] = slice::from_raw_parts(msgptr, msglen);

    sign_with_ed_key_internal(&client, id, msg, result, testing_mock);
}
/// # Safety
///
/// To Do -mlitchard
pub unsafe fn sign_with_ed_key_internal(
    client: &Client,
    id: u16,
    msg: &[u8],
    result: *mut u8,
    testing_mock: bool,
) {
    // testing_mock is a boolean passed in from the Haskell side to faciliate 
    // haskell-side tests
    let sig = if testing_mock {
        use yubihsm::asymmetric;
        client
            .put_asymmetric_key(
                TEST_KEY_ID,
                TEST_SIGNING_KEY_LABEL.into(),
                TEST_SIGNING_KEY_DOMAINS,
                Capability::SIGN_EDDSA,
                asymmetric::Algorithm::Ed25519,
                SECRETKEY.to_vec(),
            )
            .unwrap();
        let sig: Signature = client
            .sign_ed25519(id, msg)
            .expect("could not get the signature");

        sig
    } else {
        let sig: Signature = client
            .sign_ed25519(id, msg)
            .expect("could not get the signature");

        sig
    };

    let sigbytes: [u8; SIGNATURE_SIZE] = sig.to_bytes();

    sigbytes.as_ptr().copy_to(result, SIGNATURE_SIZE);
}

#[no_mangle]
pub extern "C" fn put_ed_key(
    id: u16,
    label: &[u8; LABEL_SIZE],
    domain: u16,
    key_: &[u8; KEY_SIZE],
    testing_mock: bool,
) -> bool {
    let connector = make_connector(testing_mock);
    let client: Client = create_client(connector).expect("could not connect to YubiHSM");
    put_ed_key_internal(&client, id, label, domain, key_);
    // testing_mock is a boolean passed in from the Haskell side to faciliate 
    // haskell-side tests
    if testing_mock {
        match client.get_object_info(id, object::Type::AsymmetricKey) {
            Ok(object_info) => {
                object_info.object_id == id
                    && object_info.object_type == object::Type::AsymmetricKey
            }

            Err(_) => panic!("put asymmetric key failed in testing"),
        }
    } else {
        true
    }
}

pub fn put_ed_key_internal(
    client: &Client,
    id: u16,
    label: &[u8; LABEL_SIZE],
    domain: u16,
    key: &[u8; KEY_SIZE],
) {
    let _ = client.delete_object(id, object::Type::AsymmetricKey);
    let t_domain: Domain = Domain::from_bits_truncate(domain);

    client
        .put_asymmetric_key(
            id,
            object::Label::from_bytes(label).expect("failed to construct Label from byte array"),
            t_domain,
            Capability::SIGN_EDDSA,
            Algorithm::Ed25519,
            &key[..],
        )
        .expect("could not put the key");
}
/// # Safety
///
/// To Do -mlitchard
#[no_mangle]
pub unsafe extern "C" fn get_public_key(kid: u16,result: *mut u8,testing_mock: bool) {
    use yubihsm::asymmetric;
    let connector = make_connector(testing_mock);
    let client: Client = create_client(connector).expect("could not connect to YubiHSM");

    if testing_mock {
        client
        .put_asymmetric_key(
            TEST_KEY_ID,
            TEST_SIGNING_KEY_LABEL.into(),
            TEST_SIGNING_KEY_DOMAINS,
            Capability::SIGN_EDDSA,
            asymmetric::Algorithm::Ed25519,
            SECRETKEY.to_vec(),
        )
        .unwrap();
        get_public_key_internal(&client, kid, result)
    } else {
    get_public_key_internal(&client, kid, result)
    }
}
/// # Safety
///
/// To Do -mlitchard
pub unsafe fn get_public_key_internal(client: &Client,kid: u16,result: *mut u8 ) {
    // let connector = make_connector(testing_mock);
    // let client: Client = create_client(connector).expect("could not connect to YubiHSM");
    let mut err_result: Vec<u8> = [0; KEY_SIZE].into();
        
    match client.get_public_key(kid) {
        Ok(key) => {
            let mut bkey = key.bytes;
            
            bkey.as_mut_ptr().copy_to(result, KEY_SIZE);
        },
        Err(_) => err_result.as_mut_ptr().copy_to(result, KEY_SIZE),
    }
}

fn zeroes(size: usize) -> Vec<u8> {
    let mut zero_vec: Vec<u8> = Vec::with_capacity(size as usize);
    for _i in 0..size {
        zero_vec.push(0);
    } 
    zero_vec
}
