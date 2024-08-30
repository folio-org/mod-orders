package org.folio.helper;

import static org.folio.rest.impl.MockServer.getCreatedEncumbrances;
import static org.folio.rest.impl.MockServer.getPoLineUpdates;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.PoLine;

public final class FinanceInteractionsTestHelper {

  public static void verifyEncumbrancesOnPoUpdate(CompositePurchaseOrder rqPo) {
    verifyEncumbrancesOnPoUpdate(rqPo, getFundDistributionCount(rqPo));
  }

  static void verifyEncumbrancesOnPoUpdate(CompositePurchaseOrder rqPo, int expectedCreated) {
    // Check that number of linked encumbrances corresponds to FundDistribution count
    List<Transaction> createdEncumbrances = getCreatedEncumbrances();

    Map<String, List<PoLine>> polUpdates = getPoLineUpdates().stream()
      .map(json -> json.mapTo(PoLine.class))
      .filter(poLine -> poLine.getFundDistribution()
        .stream()
        .allMatch(distr -> distr.getEncumbrance() != null))
      .collect(Collectors.groupingBy(PoLine::getId));

    assertThat(createdEncumbrances.size(), equalTo(expectedCreated));
    // Make sure that each fund distribution has now links to encumbrances
    assertThat(polUpdates.size(), equalTo(rqPo.getCompositePoLines().size()));

    // Verify created encumbrances
    if (!createdEncumbrances.isEmpty()) {
      for (Map.Entry<String, List<PoLine>> poLines : polUpdates.entrySet()) {
        PoLine poLine = poLines.getValue().get(0);
        for (FundDistribution fundDistr : poLine.getFundDistribution()) {
          Transaction encumbrance = getEncumbranceById(createdEncumbrances, fundDistr.getEncumbrance());
          if (encumbrance != null) {
            assertThat(fundDistr.getFundId(), equalTo(encumbrance.getFromFundId()));
          }
        }
      }
    }
  }

  private static Transaction getEncumbranceById(List<Transaction> createdEncumbrances, String id) {
    return createdEncumbrances.stream()
      .filter(encumbrance -> encumbrance.getId().equals(id))
      .findFirst()
      .orElse(null);
  }

  private static int getFundDistributionCount(CompositePurchaseOrder rsPo) {
    return rsPo.getCompositePoLines()
      .stream()
      .mapToInt(pol -> pol.getFundDistribution().size())
      .sum();
  }
}
