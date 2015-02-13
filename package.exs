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
    [files: ~w(src rebar.config README.md LICENSE UNLICENSE VERSION),
     contributors: ["Luis Rascao"],
     licenses: ["Unlicense", "Apache 2.0"],
     links: %{"GitHub" => "https://github.com/lrascao/poolgirl"}]
  end
end
