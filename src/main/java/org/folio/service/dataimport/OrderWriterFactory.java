package org.folio.service.dataimport;

import org.folio.processing.mapping.mapper.writer.Writer;
import org.folio.processing.mapping.mapper.writer.WriterFactory;
import org.folio.processing.mapping.mapper.writer.common.JsonBasedWriter;
import org.folio.rest.jaxrs.model.EntityType;

public class OrderWriterFactory implements WriterFactory {

  @Override
  public Writer createWriter() {
    return new JsonBasedWriter(EntityType.ORDER);
  }

  @Override
  public boolean isEligibleForEntityType(EntityType entityType) {
    return EntityType.ORDER == entityType;
  }
}
