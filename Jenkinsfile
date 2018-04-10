pipeline {
  agent any
  stages {
    stage('Build') {
      agent any
      steps {
        git 'https://github.com/ostahluio/testMaster.git'
        sh 'ant -buildfile /var/jenkins_home/workspace/ABS_Tools/frontend/build.xml dist'
        archiveArtifacts 'frontend/dist/absfrontend.jar'
      }
    }
  }
}