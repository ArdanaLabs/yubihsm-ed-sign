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

}