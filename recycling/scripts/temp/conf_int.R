get_conf_int_lower <- 
  function(.x) {
    t.test(.x) %>% 
      pluck("conf.int") %>% 
      .[[1]]
  }

get_conf_int_lower <- 
  function(.x) {
    t.test(.x) %>% 
      pluck("conf.int") %>% 
      .[[2]]
  }

get_conf_int <- 
  function(
    .x,
    boundary = "lower"
  ) {
    test_out <-
      t.test(.x) %>% 
      pluck("conf.int")
    
    if(boundary == "lower") {
      test_out[[1]]
    } else {
      test_out[[2]]
    }
  }
