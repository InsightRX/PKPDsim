#!groovy

  pipeline {
    agent {
      label 'r-slave'
    }
    stages{
      stage('Build - PKPDsim') {
        steps {
          echo 'building PKPDsim'
          sh """
          if [ -d "PKPDsim2" ]; then
            sudo rm -R PKPDsim2
          fi
          git clone git@github.com:InsightRX/PKPDsim2.git
          cd PKPDsim2
          git checkout $GIT_BRANCH
          git pull origin $GIT_BRANCH

          chmod +x slack_notification.sh
          R CMD INSTALL . --library=/usr/lib/R/site-library || { export STATUS=failed
          ./slack_notification.sh
          exit 1
          }
          R CMD check . --no-manual || { export STATUS=failed
          ./slack_notification.sh
          exit 1
          }
          """
        }
      }
    }
  }
