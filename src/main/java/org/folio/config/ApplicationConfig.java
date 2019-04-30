package org.folio.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({
  "org.folio.rest.impl",
  "org.folio.orders"})
public class ApplicationConfig {}
