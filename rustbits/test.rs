#[cfg(test)]
mod tests {
    use crate::create_client;
    fn test_create_client() {
        let res = create_client();
        let isgood = match res {
          Ok(_) => true,
          Err(_) => false,
        };
      
        assert_eq!(isgood,true);
      }
}