pipeline {
  agent any
  options { 
  	buildDiscarder(logRotator(numToKeepStr: '5'))
	disableConcurrentBuilds() 
  }
  stages {
    //stage('Build') {
    //  agent any
    //  steps {
    //    git 'https://github.com/ostahluio/testMaster.git'
    //    sh 'ant -buildfile ./frontend/build.xml dist'
    //    archiveArtifacts 'frontend/dist/absfrontend.jar'
    //  }
    //}
	stage('Test') {
		steps {
			sh 'docker pull jacopomauro/abs_optimizer:latest'
			sh 'docker run -d -p 9001:9001 --name worker_container jacopomauro/abs_optimizer:latest'
			sh 'curl http://localhost:9001/health'
		}
	}
  }
}