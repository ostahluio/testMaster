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
			//sh 'curl http://localhost:9001/health'
			sh 'docker pull jacopomauro/abs_optimizer:main'
			//sh 'docker run --net="host" -d --name controller_container jacopomauro/abs_optimizer:main /bin/bash -c "while true; do sleep 60; done"'
			//sh 'docker exec -i -t controller_container /bin/sh'
			sh 'python /abs_optimizer/abs_opt.py run --param-file /abs_optimizer/examples/new_years_eve/param_spec.json --abs-file /abs_optimizer/examples/new_years_eve/NewYearsEve.abs --output-log-parser /abs_optimizer/examples/new_years_eve/solution_quality.py --global-simulation-limit 5 --global-timeout 60 --abs-option-l 310'
		}
	}
  }
}