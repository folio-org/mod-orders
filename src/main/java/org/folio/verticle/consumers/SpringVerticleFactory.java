package org.folio.verticle.consumers;

import io.vertx.core.Promise;
import io.vertx.core.Verticle;
import io.vertx.core.spi.VerticleFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component;

import java.util.concurrent.Callable;

/**
 * Factory, that allows to implement verticles as Spring beans.
 *
 * @see VerticleFactory for creating verticles
 * @see ApplicationContextAware to be notified of the ApplicationContext that it runs in
 */
@Component
public class SpringVerticleFactory implements VerticleFactory, ApplicationContextAware {
  private ApplicationContext applicationContext;

  @Override
  public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
    this.applicationContext = applicationContext;
  }

  @Override
  public String prefix() {
    return "orders";
  }

  @Override
  public void createVerticle(String verticleName, ClassLoader classLoader, Promise<Callable<Verticle>> promise) {
    String clazz = VerticleFactory.removePrefix(verticleName);
    promise.complete(() -> (Verticle) applicationContext.getBean(Class.forName(clazz)));
  }
}
