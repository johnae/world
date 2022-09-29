{
  self,
  inputs,
}: (
  final: prev: let
    l = prev.lib // builtins;
    inherit
      (l)
      baseNameOf
      concatStringsSep
      listToAttrs
      mapAttrs'
      mapAttrsToList
      readDir
      recursiveUpdate
      removeSuffix
      ;
    pushArchive = attr: image: let
      inherit (image) imageTag imageName;
    in
      final.writeShellApplication {
        name = "build-and-push";
        runtimeInputs = [final.skopeo];
        text = ''
          EXTRA_TAG="''${1:-}"
          if ! skopeo list-tags docker://${imageName} | grep -q "${imageTag}" >/dev/null; then
            echo building image ${imageName}:${imageTag}
            outname="image-$(basename ${attr})"
            trap 'rm -f $outname' EXIT
            nix build -L ${self}#${attr} -o "$outname"
            echo pushing ${imageName}:${imageTag}
            skopeo --insecure-policy copy \
              docker-archive:./"$outname" \
              docker://${imageName}:${imageTag}
            if [ "$EXTRA_TAG" != "" ]; then
              echo pushing ${imageName}:"$EXTRA_TAG"
              skopeo --insecure-policy copy \
                docker-archive:./"$outname" \
                docker://${imageName}:"$EXTRA_TAG"
            fi
            echo pushing ${imageName}:latest
            skopeo --insecure-policy copy \
              docker-archive:./"$outname" \
              docker://${imageName}:latest
            echo pushed to ${imageName}:${imageTag}
            if [ "$EXTRA_TAG" != "" ]; then
              echo pushed to ${imageName}:"$EXTRA_TAG"
            fi
            echo pushed to ${imageName}:latest
          else
            echo ${imageName}:${imageTag} already exists at remote
            echo skip build and push
          fi
        '';
      };
    imagesInDirectory = dir: (mapAttrs' (
      file: type: let
        name = removeSuffix ".nix" (baseNameOf file);
        req =
          if type == "directory"
          then name
          else file;
      in {
        name = "images/archives/${name}";
        value = final.callPackage (./images + "/${req}") {};
      }
    ) (readDir dir));
    pushersOfImages = images: (mapAttrs' (
        name: value: {
          name = "images/push/${baseNameOf name}";
          value = pushArchive name value;
        }
      )
      images);
    images = imagesInDirectory ./images;
    pushers = pushersOfImages images;
    pushAll = final.writeShellApplication {
      name = "push-all-images";
      text = ''
        ${concatStringsSep "\n" (mapAttrsToList (
            name: value: ''
              echo building and pushing ${name}
              ${value}/bin/build-and-push "$@"
            ''
          )
          pushers)}
      '';
    };
  in
    recursiveUpdate images (recursiveUpdate pushers {"images/push/all" = pushAll;})
)
