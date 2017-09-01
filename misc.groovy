
// Jenkins configuration for generating ssh keys?

stage('ssk keygen') {
  sshagent(['...']) {
    sh 'ssh-keygen -R github.com -f ~/.ssh/known_hosts'
    sh 'ssh-keyscan -t rsa github.com >> ~/.ssh/known_hosts'
    run 'bundle install --without development --deployment'
  }
}

