#!groovy

pipeline {
  agent {
    label 'docker-runner'
  }
  stages{
    stage('Run docker container') {
      environment {
        AWS_ACCESS_KEY_ID = credentials('AWS_ACCESS_KEY_ID')
        AWS_SECRET_ACCESS_KEY = credentials('AWS_SECRET_ACCESS_KEY')
      }
      steps {
        echo "Building Docker image"
        sh """
        \$(aws ecr get-login --no-include-email --region us-west-2 &> /dev/null)
        docker build -t pkpdsim .
        """
      }

    }
    stage('Build & test PKPDsim') {
      steps {
        echo 'Installing and checking PKPDsim'
        /*
        The command to Sys.setlocale('LC_ALL','C') is required due to a bug in processx, which has
        already been fixed in the dev version as of May 26th 2021, but is not yet in the CRAN version.
        This line can be removed when processx (called by rcmdcheck) is updated.
        */
        sh """
        docker run -d -t --name ${BUILD_TAG} pkpdsim:latest
        docker exec -i ${BUILD_TAG} Rscript -e "Sys.setlocale('LC_ALL','C'); devtools::check()"
        """
      }
    }
  }
  post {
    always {
      sh """
      docker rm -f ${BUILD_TAG} &>/dev/null && echo 'Removed container'
      """
    }
    failure {
      sh "chmod +x slack_notification.sh"
      sh "./slack_notification.sh"
    }
  }
  environment {
    KHALEESI_SLACK_TOKEN = credentials('KHALEESI_SLACK_TOKEN')
    JENKINS_SLACKBOT = credentials('JENKINS_SLACKBOT')
  }
}
