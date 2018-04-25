pipeline {
  agent any
  options { 
  	buildDiscarder(logRotator(numToKeepStr: '5'))
	disableConcurrentBuilds() 
  }
  stages {
    stage('Build') {
      agent any
      steps {
        //git 'https://github.com/ostahluio/testMaster.git'
        //sh 'ant -buildfile ./frontend/build.xml dist'
        //archiveArtifacts 'frontend/dist/absfrontend.jar'
      }
    }
	stage('Test') {
		steps {
			sh 'docker run hello-world'
		}
	}
  }
}