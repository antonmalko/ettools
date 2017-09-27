context("Naming schemas")



test_that("mal-formed schemas are throwing errors",{

  # malformed schema with non-character values
  nonchar_schema <- list(markup = "mk",
                 preproc = 2)

  # malformed schema with multiple values per component
  dups_schema <- list(markup = c("A","B"),
                      preproc = "a")

  expect_error(check_schema_well_formedness("a"),
               "A naming components schema should be a list!")

  expect_error(check_schema_well_formedness(nonchar_schema),
               "Cannot use the naming components schema! ",
               "Some of the components are not of type `character`")

  expect_error(check_schema_well_formedness(dups_schema),
               paste0("Cannot use the naming components schema! ",
                      "Some of the components have more or fewer than one corresponding tag"))
})

test_that("duplicates are detected in a naming schema", {

  # names of the elements of the schema are duplicated
  names_dup_schema <- list(markup = "A",
                           markup = "B")

  # values of the elements of the schema are duplicated
  values_dup_schema <- list(markup = "A",
                           preproc = "A")

  expect_error(check_schema_for_duplicates(names_dup_schema,
                                           check.values.for.dups = FALSE),
               "Duplicates found in names_dup_schema (schema tags): markup",
               fixed = TRUE)

  expect_error(check_schema_for_duplicates(values_dup_schema,
                                           check.values.for.dups = TRUE),
               "Duplicates found in values_dup_schema (schema values): A",
               fixed = TRUE)

  # here no error should be thrown, even though there are duplicated values,
  # since we don't check the values by setting the `check.values.for.dups`
  # to FALSE

  check_schema_for_duplicates(values_dup_schema,
                              check.values.for.dups = FALSE)

})

test_that("delimiters are detected in a naming schema",{
  component.delim = "_"
  tag.delim = "."

  # tag delim in names
  schema1 <- list(markup.1 = "a",
                  preproc = "b")
  # tag delim in values
  schema2 <- list(markup = "a.2",
                  preproc = "b")
  # component delim in names
  schema3 <- list(markup_1 = "a",
                  preproc = "b")
  # component delim in values
  schema4 <- list(markup = "a_2",
                  preproc = "b")

  expect_error(check_schema_for_delims(schema1, tag.delim = tag.delim,
                                       component.delim = component.delim),
               paste0("The following items in schema names contain(s) the character used as",
                      " tag delimiter ('.'): markup.1"),
               fixed = TRUE)

  expect_error(check_schema_for_delims(schema2, tag.delim = tag.delim,
                                       component.delim = component.delim),
               paste0("The following items in schema values contain(s) the character used as",
                      " tag delimiter ('.'): a.2"),
               fixed = TRUE)

  expect_error(check_schema_for_delims(schema3, tag.delim = tag.delim,
                                       component.delim = component.delim),
               paste0("The following items in schema names contain(s) the character used as",
                      " component delimiter ('_'): markup_1"),
               fixed = TRUE)

  expect_error(check_schema_for_delims(schema4, tag.delim = tag.delim,
                                       component.delim = component.delim),
               paste0("The following items in schema values contain(s) the character used as",
                      " component delimiter ('_'): a_2"),
               fixed = TRUE)


})

test_that("'make_filename_postfix' works as expected", {
  tags <- list(markup = "mk",
              preproc = "prep")
  values <- list(markup = "markup",
                preproc = "preprocessing")

  # "marlup" instead of "markup" - doesn't correspond to any tag in tagging schema
  garbled_values <- list(marlup = "markup",
                         preproc = "preprocessing")

  # normal case
  expect_equal(make_filename_postfix(tags = tags, values = values,
                                     tag.delim = ".", component.delim = "_"),
               "_mk.markup_prep.preprocessing")

  # throw an error if a tag in values schema doesn't correspond to any tag in
  # the tagging schema
  expect_error(make_filename_postfix(tags = tags, values = garbled_values,
                                     tag.delim = ".", component.delim = "_"),
               "No tag found for name component `marlup` !")

})

test_that("'make_filename' works as expected",{
  tags <- list(markup = "mk",
               preproc = "prep")
  values <- list(markup = "markup",
                 preproc = "preprocessing")
  proj_name = "testproj"

  # contains a tag delimiter
  bad_proj_name <- "test.proj"

  expect_equal(make_filename(proj.name = proj_name, tags = tags, values = values,
                             tag.delim = ".", component.delim = "_"),
               "testproj_mk.markup_prep.preprocessing")

  expect_error(make_filename(proj.name = bad_proj_name, tags = tags, values = values,
                             tag.delim = ".", component.delim = "_"),
               paste0("The following items in project name contain(s) the character used as",
                      " tag delimiter ('.'): test.proj"),
               fixed = TRUE)
})
