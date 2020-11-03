defmodule Poolgirl.Mixfile do
  use Mix.Project

  @version File.read!("VERSION") |> String.strip

  def project do
    [app: :poolgirl,
     version: @version,
     description: "A sexy Erlang worker pool factory",
     package: package]
  end

  defp package do
    [files: ~w(src rebar.config README.md CHANGELOG.md LICENSE VERSION),
     contributors: ["Luis Rascão"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/miniclip/poolgirl"}]
  end
end
