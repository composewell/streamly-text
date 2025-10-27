{nixpack}:
with nixpack.mkSources;
{
layers = [
{
  streamly-text = local ./.;
}
];
}
