import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Version

-- This checks the direct dependency of the bytestring package being
-- compiled against to set the bytestring_has_itoa_c and
-- bytestring_has_builder flags accordingly.

main = defaultMainWithHooks simpleUserHooks {
    confHook = \pkg flags -> do
        lbi <- confHook simpleUserHooks pkg flags
        let bytestring_version =
                case [ versionBranch v
                     | (Dependency pkg ver)  <- configConstraints flags
                     , pkg == PackageName "bytestring"
                     , (Just v) <- [isSpecificVersion ver] ]
                  of
                     [v] -> v
                     vs   -> error ("error detecting bytestring version  "  ++ show vs)

        let has_itoa_c  = ( FlagName "bytestring_has_itoa_c"
                          , bytestring_version >= [0,10]      )

        let has_builder = ( FlagName "bytestring_has_builder"
                          , bytestring_version >= [0,10,4]   )

        let update fs gs =
                 fs ++ [ g | g <- gs, not $ any (\f -> fst f == fst g) fs]

        let flags  = configFlags lbi

        let flags' = flags { configConfigurationsFlags =
                                update [has_itoa_c, has_builder]
                                       (configConfigurationsFlags flags) }

        let lbi' = lbi { configFlags = flags' }
        
        return lbi'
  }
