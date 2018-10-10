#!groovy

  pipeline {
    agent {
      label 'r-slave'
    }
    stages{
      stage('Dependencies - build json2test') {
        steps {
          echo 'building json2test'
          sh """
            #!/bin/bash
            set -ex
            pwd
            sudo chmod 777 ~/workspace
            cd ~/workspace
            git clone git@github.com:InsightRX/json2test.git
            R CMD INSTALL . --library=/usr/lib/R/site-library || { export STATUS=failed
            ./slack_notification.sh
            exit 1
            }
          """
        }
      }
      stage('Dependencies - build clinPK') {
        steps {
          echo 'building clinPK'
          sh """
            cd /$workspace
            git clone git@github.com:InsightRX/clinPK2.git
            R CMD INSTALL . --library=/usr/lib/R/site-library || { export STATUS=failed
            ./slack_notification.sh
            exit 1
            }
            """
        }
      }
      stage('Build - PKPDsim') {
        steps {
          echo 'building PKPDsim'
          sh """
          R CMD INSTALL . --library=/usr/lib/R/site-library ||  || { export STATUS=failed
          ./slack_notification.sh
          exit 1
          }
          R CMD check . --no-manual ||  || { export STATUS=failed
          ./slack_notification.sh
          exit 1
          }
          """
        }
      }
    }
  }
