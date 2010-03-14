# Generated by Buildr 1.3.5, change to your liking
# Version number for this release
VERSION_NUMBER = "0.0.1"
# Group identifier for your projects
GROUP = "probility"
COPYRIGHT = "Steffen Siering"

require 'buildr/scala'

desc "Probability Monad for Scala"
define "probability" do

  project.version = VERSION_NUMBER
  project.group = GROUP
  manifest["Implementation-Vendor"] = COPYRIGHT

  package :jar

  task :api do
    system 'mkdir api; scaladoc -d api src/main/scala/probability/*.scala'
  end

  clean do 
    rm_rf('api')
  end
  

end
