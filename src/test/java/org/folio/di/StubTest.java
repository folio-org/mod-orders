package org.folio.di;

import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.io.IOException;
import java.util.UUID;

import static org.junit.Assert.assertTrue;

@RunWith(VertxUnitRunner.class)
public class StubTest extends  DiAbstractRestTest {
  private String handlerId;
  private String eventId;
  @Before
  public void setUp(TestContext context) throws IOException {
    super.setUp(context);
    handlerId = UUID.randomUUID().toString();
    eventId = UUID.randomUUID().toString();
  }

  @Test
  public void test(){
    assertTrue(true);
  }
}
