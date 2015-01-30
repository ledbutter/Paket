module Paket.DependencySetSpecs

open Paket
open Paket.BindingRedirects
open NUnit.Framework
open System.Xml.Linq
open FsUnit
open Paket.PackageResolver
open Paket.Requirements
open Paket.Domain

let isIncluded (restriction:FrameworkRestriction) (dependency:PackageName * VersionRequirement * FrameworkRestrictions) =
    let _,_,restrictions = dependency
    if Seq.isEmpty restrictions then true else
    match restriction with
    | FrameworkRestriction.Exactly v1 -> restrictions |> Seq.exists ((=) (FrameworkRestriction.Exactly v1))
    | _ -> true

let filterByRestrictions (x:FrameworkRestriction seq) (dependencies:DependencySet) = 
    x
    |> Seq.fold (fun currentSet restriction -> isIncluded restriction currentSet) dependencies


[<Test>]
let ``empty set filtered with empty restrictions should give empty set``() = 
    Set.empty
    |> filterByRestrictions []
    |> shouldEqual Set.empty

[<Test>]
let ``filtered with empty restrictions should give full set``() = 
    let set = 
      [PackageName("P1"), VersionRequirement.AllReleases, []
       PackageName("P2"), VersionRequirement.AllReleases, [FrameworkRestriction.AtLeast (DotNetFramework(FrameworkVersion.V4))]
       PackageName("P3"), VersionRequirement.AllReleases, [FrameworkRestriction.Exactly (DotNetFramework(FrameworkVersion.V4_5))]]
      |> Set.ofList

    set
    |> filterByRestrictions []
    |> shouldEqual set


[<Test>]
let ``filtered with concrete restriction should filter non-matching``() = 
    let original = 
      [PackageName("P1"), VersionRequirement.AllReleases, []
       PackageName("P2"), VersionRequirement.AllReleases, [FrameworkRestriction.AtLeast (DotNetFramework(FrameworkVersion.V4))]
       PackageName("P3"), VersionRequirement.AllReleases, [FrameworkRestriction.Exactly (DotNetFramework(FrameworkVersion.V4_5))]]
      |> Set.ofList

    let expected = 
      [PackageName("P1"), VersionRequirement.AllReleases, []
       PackageName("P2"), VersionRequirement.AllReleases, [FrameworkRestriction.AtLeast (DotNetFramework(FrameworkVersion.V4))]]
      |> Set.ofList


    original
    |> filterByRestrictions [FrameworkRestriction.Exactly (DotNetFramework(FrameworkVersion.V4))]
    |> shouldEqual expected