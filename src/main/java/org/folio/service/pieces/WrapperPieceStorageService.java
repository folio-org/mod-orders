package org.folio.service.pieces;

import io.vertx.core.Future;
import lombok.extern.log4j.Log4j2;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.WrapperPiece;
import org.folio.rest.jaxrs.model.WrapperPieceCollection;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.consortium.ConsortiumUserTenantsRetriever;
import org.folio.service.settings.SettingsRetriever;

import static org.folio.orders.utils.ResourcePathResolver.WRAPPER_PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

@Log4j2
public class WrapperPieceStorageService extends PieceStorageService {

  private static final String WRAPPER_PIECES_STORAGE_ENDPOINT = resourcesPath(WRAPPER_PIECES_STORAGE);
  private static final String WRAPPER_PIECES_STORAGE_BY_ID_ENDPOINT = WRAPPER_PIECES_STORAGE_ENDPOINT + "/{id}";

  public WrapperPieceStorageService(ConsortiumConfigurationService consortiumConfigurationService,
                                    ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever,
                                    SettingsRetriever settingsRetriever, RestClient restClient) {
    super(consortiumConfigurationService, consortiumUserTenantsRetriever, settingsRetriever, restClient);
  }

  public Future<WrapperPieceCollection> getWrapperPieces(int limit, int offset, String query, RequestContext requestContext) {
    return getUserTenantsIfNeeded(requestContext)
      .map(userTenants -> getQueryForUserTenants(userTenants, query))
      .compose(cql -> getAllWrapperPieces(limit, offset, cql, requestContext));
  }

  public Future<WrapperPieceCollection> getAllWrapperPieces(int limit, int offset, String query, RequestContext requestContext) {
    log.debug("getAllWrapperPieces:: limit: {}, offset: {}, query: {}", limit, offset, query);
    var requestEntry = new RequestEntry(WRAPPER_PIECES_STORAGE_ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return super.restClient.get(requestEntry, WrapperPieceCollection.class, requestContext);
  }

  public Future<WrapperPiece> getWrapperPieceById(String pieceId, RequestContext requestContext) {
    var requestEntry = new RequestEntry(WRAPPER_PIECES_STORAGE_BY_ID_ENDPOINT).withId(pieceId);
    return super.restClient.get(requestEntry, WrapperPiece.class, requestContext);
  }
}
