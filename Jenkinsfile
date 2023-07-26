buildMvn {
  publishModDescriptor = 'yes'
  mvnDeploy = 'yes'
  buildNode = 'jenkins-agent-java17'

  doDocker = {
    buildJavaDocker {
      publishMaster = 'yes'
      //healthChk for /admin/health in InstallUpgradeIT.java
    }
  }
}
