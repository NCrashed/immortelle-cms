let
  region = "us-west-2";
  accessKeyId = "immortelle-production"; # symbolic name looked up in ~/.ec2-keys or a ~/.aws/credentials profile name

  makeAws = { ip, resources, ... }:
    { deployment.targetEnv = "ec2";
      deployment.ec2.accessKeyId = accessKeyId;
      deployment.ec2.region = region;
      deployment.ec2.instanceType = "t2.micro";
      deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
      deployment.ec2.securityGroups = [ "allow-ssh" "allow-http" ];
      deployment.ec2.ebsInitialRootDiskSize = 20;
      deployment.ec2.elasticIPv4 = ip;
    };
in {
  immortelleServer = {resources, ...}: makeAws { ip = "35.161.43.250"; inherit resources; };

  resources.ec2KeyPairs.my-key-pair = { inherit region accessKeyId; };
}
