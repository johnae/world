let
  eris = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIO1+nbifIWVNJJu/5zDga9cX1qVVMYyg0XZYyRos4Ue";
  johnae = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKfOiIu2X42aHpqAlzF6dLfI6PCTeVw0z1qb/9lroIHp";
in
{
  "eris.age".publicKeys = [ eris johnae ];
}
