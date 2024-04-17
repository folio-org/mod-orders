package org.folio.models;

import java.util.List;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UserCollection {
  @JsonProperty
  private List<User> users;
  @JsonProperty
  private int totalRecords;

  public List<User> getUsers() {
    return users;
  }
  public int getTotalRecords() {
    return totalRecords;
  }

  public UserCollection withUsers(List<User> users) {
    this.users = users;
    return this;
  }

  @JsonIgnoreProperties(ignoreUnknown = true)
  public static class User {
    @JsonProperty
    private UUID id;
    @JsonProperty
    private Personal personal;

    public UUID getId () {
      return id;
    }
    public Personal getPersonal () {
      return personal;
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class Personal {
      @JsonProperty
      private String firstName;
      @JsonProperty
      private String lastName;
      @JsonProperty
      private List<Address> addresses;

      public String getFirstName() {
        return firstName;
      }
      public String getLastName() {
        return lastName;
      }
      public List<Address> getAddresses() {
        return addresses;
      }
      @JsonIgnoreProperties(ignoreUnknown = true)
      public static class Address {
        @JsonProperty
        private String addressLine1;
        @JsonProperty
        private String addressTypeId;

        public String getAddressLine1() {
          return addressLine1;
        }
        public String getAddressTypeId() {
          return addressTypeId;
        }
      }
    }
  }
}
