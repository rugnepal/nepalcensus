# Roxygen2 documentation for all Individual (Indv) datasets
# Nepal Population and Housing Census 2021 (NPHC 2078 BS)
# Source: National Statistics Office (NSO) of Nepal

# ---------------------------------------------------------------------------
# Common column descriptions
# ---------------------------------------------------------------------------
# prov          : Province code (0 = Nepal total)
# dist          : District code (0 = province total)
# gapa          : Gaunpalika/Nagarpalika code (0 = district total)
# provname      : Province name
# dname         : District name
# gapaname      : Gaunpalika/Nagarpalika name
# sex           : Sex code (0 = total, 1 = male, 2 = female)
# sexname       : Sex label ("Total", "Male", "Female")
# agegrp        : Age group code
# agegrpname    : Age group label
# litsts        : Literacy status code
# edulvl        : Education level code
# educationlevel: Education level label
# occ1          : Major occupation code (1-digit ISCO)
# occname       : Occupation name
# ind1          : Major industry code (1-digit ISIC)
# indname       : Industry name
# category      : Classification category label
# number        : Count (character; includes totals)
# ---------------------------------------------------------------------------


#' Table 01: Households and population by sex
#'
#' Number of households and population by sex, NPHC 2021.
#'
#' @format A tibble with 9 variables:
#' \describe{
#'   \item{prov}{Character. Province code (0 = Nepal total).}
#'   \item{dist}{Character. District code (0 = province total).}
#'   \item{gapa}{Character. Gaunpalika/Nagarpalika code.}
#'   \item{provname}{Character. Province name.}
#'   \item{dname}{Character. District name.}
#'   \item{gapaname}{Character. Gaunpalika/Nagarpalika name.}
#'   \item{n_hhld}{Character. Number of households.}
#'   \item{category}{Character. Sex category ("Total", "Male", "Female").}
#'   \item{number}{Character. Population count.}
#' }
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv01)
#' head(Indv01)
"Indv01"


#' Table 02: Households by household size
#'
#' Number of households by household size, NPHC 2021.
#'
#' @format A tibble with 8 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{provname}, \code{dname}, \code{gapaname},
#'   \code{category} (household size), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv02)
#' head(Indv02)
"Indv02"


#' Table 03: Population by single year of age and sex
#'
#' Population by single year of age and sex, NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{agegrp} (single year code), \code{provname},
#'   \code{dname}, \code{gapaname}, \code{agegrpname} (age in years),
#'   \code{category} (sex), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv03)
#' head(Indv03)
"Indv03"


#' Table 04: Population by five-year age group and sex
#'
#' Population by five-year age group and sex, NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{agegrpname}, \code{category} (sex),
#'   \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv04)
#' head(Indv04)
"Indv04"


#' Table 05: Local levels, households and population by locality size
#'
#' Number of local levels, households and population by size of locality,
#' NPHC 2021.
#'
#' @format A tibble with 10 variables:
#' \describe{
#'   \item{prov}{Character. Province code (0 = Nepal total).}
#'   \item{dist}{Character. District code (0 = province total).}
#'   \item{sizeclass}{Character. Locality size class code.}
#'   \item{provname}{Character. Province name.}
#'   \item{dname}{Character. District name.}
#'   \item{sizeclassname}{Character. Locality size class label.}
#'   \item{num_locality}{Character. Number of localities.}
#'   \item{no_hhld}{Character. Number of households.}
#'   \item{category}{Character. Sex category ("Total", "Male", "Female").}
#'   \item{number}{Character. Population count.}
#' }
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv05)
#' head(Indv05)
"Indv05"


#' Table 06: Household heads by age and sex
#'
#' Number of household heads by age group and sex of the head, NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{category} (age group),
#'   \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv06)
#' head(Indv06)
"Indv06"


#' Table 07: Population by relationship to household head
#'
#' Number of household members by relationship to household head and sex,
#' NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{category} (relationship type),
#'   \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv07)
#' head(Indv07)
"Indv07"


#' Table 09: Population by age group and nationality
#'
#' Population by 5-year age group, sex, and country/region of nationality,
#' NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (nationality), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv09)
#' head(Indv09)
"Indv09"


#' Table 10: Population aged 10+ by marital status
#'
#' Population aged 10 years and above by 5-year age group, sex, and
#' marital status, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (marital status), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv10)
#' head(Indv10)
"Indv10"


#' Table 11: Married population aged 10+ by age at first marriage
#'
#' Married population aged 10 years and above by 5-year age group, sex,
#' and age at first marriage, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (age at first marriage), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv11)
#' head(Indv11)
"Indv11"


#' Table 16: Population by age group and disability type
#'
#' Population by 5-year age group, sex, and type of disability, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (disability type), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv16)
#' head(Indv16)
"Indv16"


#' Table 17: Population aged 5+ by literacy status
#'
#' Population aged 5 years and above by literacy status and sex,
#' NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (literacy status), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv17)
#' head(Indv17)
"Indv17"


#' Table 18: Population aged 5+ by educational level
#'
#' Population aged 5 years and above by educational level attained and
#' sex, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (education level), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv18)
#' head(Indv18)
"Indv18"


#' Table 19: Population aged 5-25 by school attendance (completed level)
#'
#' Population aged 5 to 25 years who have completed a level of education
#' by school/college attendance status, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (attendance status), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv19)
#' head(Indv19)
"Indv19"


#' Table 20: Population aged 5-25 by current school attendance status
#'
#' Population aged 5-25 years by status of school/college attendance,
#' NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{agegrpname}, \code{category} (attendance
#'   status), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv20)
#' head(Indv20)
"Indv20"


#' Table 21: Population aged 15+ with 10+2 or higher by field of education
#'
#' Population aged 15 years and above with 10+2 or above educational
#' level by field of education, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (field of education), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv21)
#' head(Indv21)
"Indv21"


#' Table 22: Population by place of birth
#'
#' Population by place of birth, sex, and 5-year age group, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (place of birth), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv22)
#' head(Indv22)
"Indv22"


#' Table 23: Native-born population by province of birth
#'
#' Native-born population by province of birth, sex, and 5-year age
#' group, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (province of birth), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv23)
#' head(Indv23)
"Indv23"


#' Table 24: Foreign-born population by country of birth
#'
#' Foreign-born population by broad age group and country of birth,
#' NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (country of birth), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv24)
#' head(Indv24)
"Indv24"


#' Table 25: Population by former place of residence
#'
#' Population residing in enumerated area by former place of residence
#' (internal migration), NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (former place of residence), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv25)
#' head(Indv25)
"Indv25"


#' Table 26: Migrated population by ecological belt
#'
#' Population residing in enumerated area by former ecological belt of
#' residence, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (ecological belt), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv26)
#' head(Indv26)
"Indv26"


#' Table 27: Migrated population by former province of residence
#'
#' Population residing in enumerated area by former province of
#' residence, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (former province), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv27)
#' head(Indv27)
"Indv27"


#' Table 28: Currently migrated population by length of stay
#'
#' Currently migrated population by length of stay and 5-year age group,
#' NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (length of stay), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv28)
#' head(Indv28)
"Indv28"


#' Table 29: Population by reason for migration (preceding census)
#'
#' Population whose former place of residence preceding the census is
#' different from current, by reason for migration, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (reason for migration), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv29)
#' head(Indv29)
"Indv29"


#' Table 30: Population by former country of stay
#'
#' Population whose former place of residence is a foreign country, by
#' country of stay, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (country of stay), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv30)
#' head(Indv30)
"Indv30"


#' Table 31: Migrated population by length of stay (5-year)
#'
#' Population whose former place of residence preceding the census is
#' different, by length of stay, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (length of stay), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv31)
#' head(Indv31)
"Indv31"


#' Table 32: Migrated population by reason of stay
#'
#' Population whose former place of residence preceding the census is
#' different, by reason of stay, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (reason of stay), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv32)
#' head(Indv32)
"Indv32"


#' Table 33: Number of children ever born
#'
#' Ever-married female population 15-49 years of age by number of
#' children ever born, NPHC 2021.
#'
#' @format A tibble with 10 variables:
#' \describe{
#'   \item{prov}{Character. Province code (0 = Nepal total).}
#'   \item{dist}{Character. District code (0 = province total).}
#'   \item{gapa}{Character. Gaunpalika/Nagarpalika code.}
#'   \item{nceb}{Character. Number of children ever born code.}
#'   \item{provname}{Character. Province name.}
#'   \item{dname}{Character. District name.}
#'   \item{gapaname}{Character. Gaunpalika/Nagarpalika name.}
#'   \item{childreneverborn}{Character. Number of children ever born label.}
#'   \item{category}{Character. Age group or other category.}
#'   \item{number}{Character. Count of women.}
#' }
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv33)
#' head(Indv33)
"Indv33"


#' Table 34: Number of children dead
#'
#' Ever-married female population 15-49 years of age by number of
#' children who have died, NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{ndead} (children dead code), \code{provname},
#'   \code{dname}, \code{gapaname}, \code{childrendead} (label),
#'   \code{category}, \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv34)
#' head(Indv34)
"Indv34"


#' Table 35: Number of children born alive in the last 12 months
#'
#' Ever-married female population 15-49 years of age by number of
#' children born alive in the last 12 months, NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{code} (children born code), \code{provname},
#'   \code{dname}, \code{gapaname}, \code{codename} (label),
#'   \code{category}, \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv35)
#' head(Indv35)
"Indv35"


#' Table 36: Children ever born, still alive, and died
#'
#' Number of children ever born, still alive and died by sex of child,
#' NPHC 2021.
#'
#' @format A tibble with 12 variables:
#' \describe{
#'   \item{prov}{Character. Province code.}
#'   \item{dist}{Character. District code.}
#'   \item{gapa}{Character. Gaunpalika/Nagarpalika code.}
#'   \item{chldtype}{Character. Child count type code (born/alive/dead).}
#'   \item{chldsex}{Character. Child sex code.}
#'   \item{provname}{Character. Province name.}
#'   \item{dname}{Character. District name.}
#'   \item{gapaname}{Character. Gaunpalika/Nagarpalika name.}
#'   \item{typename}{Character. Child count type label.}
#'   \item{sexname}{Character. Sex label.}
#'   \item{category}{Character. Age group of mother.}
#'   \item{number}{Character. Count.}
#' }
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv36)
#' head(Indv36)
"Indv36"


#' Table 37: Children ever born by literacy and education of mother
#'
#' Number of children ever born by literacy/educational status of mother,
#' NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{litsts} (literacy status code), \code{edulvl}
#'   (education level code), \code{provname}, \code{dname}, \code{gapaname},
#'   \code{educationlevel} (label), \code{category}, \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv37)
#' head(Indv37)
"Indv37"


#' Table 38: Children died by literacy and education of mother
#'
#' Number of children died by literacy/educational status of mother,
#' NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{litsts}, \code{edulvl}, \code{provname},
#'   \code{dname}, \code{gapaname}, \code{educationlevel},
#'   \code{category}, \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv38)
#' head(Indv38)
"Indv38"


#' Table 39: Children born alive (last 12 months) by education of mother
#'
#' Number of children born alive in the last 12 months by literacy/
#' educational status of mother, NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{litsts}, \code{edulvl}, \code{provname},
#'   \code{dname}, \code{gapaname}, \code{educationlevel},
#'   \code{category}, \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv39)
#' head(Indv39)
"Indv39"


#' Table 40: Children ever born by occupation of mother
#'
#' Ever-married female population 15-49 years of age by major occupation
#' and number of children ever born, NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{occ1} (1-digit occupation code), \code{provname},
#'   \code{dname}, \code{gapaname}, \code{occname} (occupation label),
#'   \code{category}, \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv40)
#' head(Indv40)
"Indv40"


#' Table 41: Children died by occupation of mother
#'
#' Number of children died by major occupation of mother, NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{occ1}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{occname}, \code{category}, \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv41)
#' head(Indv41)
"Indv41"


#' Table 42: Children born alive (last 12 months) by occupation of mother
#'
#' Number of children born alive in last 12 months by major occupation of
#' mother, NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{occ1}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{occname}, \code{category}, \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv42)
#' head(Indv42)
"Indv42"


#' Table 43: Children ever born by industry of mother
#'
#' Number of children ever born by major industry of mother, NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{ind1} (1-digit industry code), \code{provname},
#'   \code{dname}, \code{gapaname}, \code{indname} (industry label),
#'   \code{category}, \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv43)
#' head(Indv43)
"Indv43"


#' Table 44: Children died by industry of mother
#'
#' Number of children died by major industry of mother, NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{ind1}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{indname}, \code{category}, \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv44)
#' head(Indv44)
"Indv44"


#' Table 45: Children born alive (last 12 months) by industry of mother
#'
#' Number of children born alive in last 12 months by major industry of
#' mother, NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{ind1}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{indname}, \code{category}, \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv45)
#' head(Indv45)
"Indv45"


#' Table 46: Children ever born by reason mother is not economically active
#'
#' Number of children born alive in last 12 months by reason of economic
#' inactivity of mother, NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{q36rsn} (reason code), \code{provname},
#'   \code{dname}, \code{gapaname}, \code{reasonnotactive} (label),
#'   \code{category}, \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv46)
#' head(Indv46)
"Indv46"


#' Table 47: Children dead by reason mother is not economically active
#'
#' Number of children dead by reason of economic inactivity of mother,
#' NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{q36rsn}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{reasonnotactive}, \code{category}, \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv47)
#' head(Indv47)
"Indv47"


#' Table 48: Children born alive (last 12 months) by reason mother not active
#'
#' Number of children born alive in last 12 months by reason of economic
#' inactivity of mother, NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{q36rsn}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{reasonnotactive}, \code{category}, \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv48)
#' head(Indv48)
"Indv48"


#' Table 49: Population aged 10+ by number of work months
#'
#' Population 10 years of age and above by number of months worked in
#' the last 12 months, NPHC 2021.
#'
#' @format A tibble with 12 variables:
#' \describe{
#'   \item{prov}{Character. Province code (0 = Nepal total).}
#'   \item{dist}{Character. District code (0 = province total).}
#'   \item{gapa}{Character. Gaunpalika/Nagarpalika code.}
#'   \item{sex}{Character. Sex code.}
#'   \item{wrkmon}{Character. Work months code.}
#'   \item{provname}{Character. Province name.}
#'   \item{dname}{Character. District name.}
#'   \item{gapaname}{Character. Gaunpalika/Nagarpalika name.}
#'   \item{sexname}{Character. Sex label.}
#'   \item{workmonth}{Character. Work months label.}
#'   \item{category}{Character. Economic activity category.}
#'   \item{number}{Character. Population count.}
#' }
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv49)
#' head(Indv49)
"Indv49"


#' Table 50: Population aged 10+ by work months and occupation
#'
#' Population 10 years of age and above by number of months worked and
#' major occupation, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{wrkmon}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{workmonth},
#'   \code{category} (occupation), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv50)
#' head(Indv50)
"Indv50"


#' Table 51: Population aged 10+ by work months and industry
#'
#' Population 10 years of age and above by number of months worked and
#' major industry, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{wrkmon}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{workmonth},
#'   \code{category} (industry), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv51)
#' head(Indv51)
"Indv51"


#' Table 52: Population aged 10+ by work months and job search status
#'
#' Population 10 years of age and above by number of months worked and
#' job search status, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (job search/work status), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv52)
#' head(Indv52)
"Indv52"


#' Table 53: Economically active and inactive population aged 10+
#'
#' Economically active and not active population 10 years of age and
#' above by age group, NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{agegrpname},
#'   \code{category} (economic activity status), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv53)
#' head(Indv53)
"Indv53"


#' Table 54: Population aged 10+ by literacy and economic activity
#'
#' Population aged 10 years and above by literacy status and economic
#' activity, NPHC 2021.
#'
#' @format A tibble with 10 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{litsts}, \code{edulvl}, \code{provname},
#'   \code{dname}, \code{gapaname}, \code{educationlevel},
#'   \code{category}, \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv54)
#' head(Indv54)
"Indv54"


#' Table 55: Economically active population aged 10+ by occupation
#'
#' Economically active population 10 years of age and above by sex and
#' major occupation, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{occ1}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{occname},
#'   \code{category} (age group), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv55)
#' head(Indv55)
"Indv55"


#' Table 56: Economically active population aged 10+ by industry
#'
#' Economically active population 10 years of age and above by sex and
#' major industry, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{ind1}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{indname},
#'   \code{category} (age group), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv56)
#' head(Indv56)
"Indv56"


#' Table 57: Economically active population aged 10+ by occupation and industry
#'
#' Economically active population 10 years of age and above by sex,
#' major occupation, and major industry, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{ind1}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{indname},
#'   \code{category} (occupation), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv57)
#' head(Indv57)
"Indv57"


#' Table 58: Economically active population by literacy status and occupation
#'
#' Economically active population 10 years of age and above by sex,
#' literacy status, and major occupation, NPHC 2021.
#'
#' @format A tibble with 13 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{litsts}, \code{edulvl}, \code{provname},
#'   \code{dname}, \code{gapaname}, \code{sexname}, \code{educationlevel},
#'   \code{category} (occupation), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv58)
#' head(Indv58)
"Indv58"


#' Table 59: Economically active population by literacy status and industry
#'
#' Economically active population 10 years of age and above by sex,
#' literacy status, and major industry, NPHC 2021.
#'
#' @format A tibble with 13 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{litsts}, \code{edulvl}, \code{provname},
#'   \code{dname}, \code{gapaname}, \code{sexname}, \code{educationlevel},
#'   \code{category} (industry), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv59)
#' head(Indv59)
"Indv59"


#' Table 60: Economically active population by employment status
#'
#' Economically active population 10 years of age and above by sex and
#' employment status, NPHC 2021.
#'
#' @format A tibble with 12 variables:
#' \describe{
#'   \item{prov}{Character. Province code (0 = Nepal total).}
#'   \item{dist}{Character. District code (0 = province total).}
#'   \item{gapa}{Character. Gaunpalika/Nagarpalika code.}
#'   \item{sex}{Character. Sex code.}
#'   \item{empsts}{Character. Employment status code.}
#'   \item{provname}{Character. Province name.}
#'   \item{dname}{Character. District name.}
#'   \item{gapaname}{Character. Gaunpalika/Nagarpalika name.}
#'   \item{sexname}{Character. Sex label.}
#'   \item{empstatus}{Character. Employment status label (e.g., "Employee",
#'     "Self-employed", "Employer", "Unpaid family worker").}
#'   \item{category}{Character. Occupation or age group.}
#'   \item{number}{Character. Population count.}
#' }
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv60)
#' head(Indv60)
"Indv60"


#' Table 61: Economically active population by occupation and employment status
#'
#' Economically active population 10 years of age and above by sex,
#' major occupation, and employment status, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{occ1}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{occname},
#'   \code{category} (employment status), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv61)
#' head(Indv61)
"Indv61"


#' Table 62: Economically active population by sector
#'
#' Economically active population 10 years of age and above by sex, age
#' group, and sector of employment, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (sector), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv62)
#' head(Indv62)
"Indv62"


#' Table 63: Economically active population by literacy status and sector
#'
#' Economically active population 10 years of age and above by sex,
#' literacy status, and sector of employment, NPHC 2021.
#'
#' @format A tibble with 13 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{litsts}, \code{edulvl}, \code{provname},
#'   \code{dname}, \code{gapaname}, \code{sexname}, \code{educationlevel},
#'   \code{category} (sector), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv63)
#' head(Indv63)
"Indv63"


#' Table 64: Economically active population by occupation and sector
#'
#' Economically active population 10 years of age and above by sex,
#' major occupation, and sector, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{occ1}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{occname},
#'   \code{category} (sector), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv64)
#' head(Indv64)
"Indv64"


#' Table 65: Economically active population by industry and sector
#'
#' Economically active population 10 years of age and above by sex,
#' major industry, and sector, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{ind1}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{indname},
#'   \code{category} (sector), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv65)
#' head(Indv65)
"Indv65"


#' Table 66: Population aged 10+ who did not work, by reason and age group
#'
#' Population aged 10 years and above who did not work in the last 12
#' months by reason and age group, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (reason for not working), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv66)
#' head(Indv66)
"Indv66"


#' Table 67: Population aged 10+ who didn't work, by literacy and reason
#'
#' Population aged 10 years and above who didn't work in the last 12
#' months by sex, literacy status, and reason, NPHC 2021.
#'
#' @format A tibble with 13 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{litsts}, \code{edulvl}, \code{provname},
#'   \code{dname}, \code{gapaname}, \code{sexname}, \code{educationlevel},
#'   \code{category} (reason), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv67)
#' head(Indv67)
"Indv67"


#' Table 68: Population aged 10+ who did not work, by work months and reason
#'
#' Population 10 years of age and over who did not work or worked less
#' than 12 months, by number of work months and reason, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{wrkmon} (work months code),
#'   \code{provname}, \code{dname}, \code{gapaname}, \code{sexname},
#'   \code{workmonth} (work months label), \code{category} (reason),
#'   \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv68)
#' head(Indv68)
"Indv68"


#' Table 69: Population under 18 by living arrangement (sex and age)
#'
#' Population under 18 years of age by status of living arrangement,
#' sex, and age group, NPHC 2021.
#'
#' @format A tibble with 12 variables: \code{prov}, \code{dist},
#'   \code{gapa}, \code{sex}, \code{agegrp}, \code{provname}, \code{dname},
#'   \code{gapaname}, \code{sexname}, \code{agegrpname},
#'   \code{category} (living arrangement), \code{number}.
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv69)
#' head(Indv69)
"Indv69"


#' Table 70: Population under 18 by living arrangement and literacy status
#'
#' Population under 18 years of age by status of living arrangement and
#' literacy status, NPHC 2021.
#'
#' @format A tibble with 12 variables:
#' \describe{
#'   \item{prov}{Character. Province code (0 = Nepal total).}
#'   \item{dist}{Character. District code (0 = province total).}
#'   \item{gapa}{Character. Gaunpalika/Nagarpalika code.}
#'   \item{sex}{Character. Sex code.}
#'   \item{litsts}{Character. Literacy status code.}
#'   \item{provname}{Character. Province name.}
#'   \item{dname}{Character. District name.}
#'   \item{gapaname}{Character. Gaunpalika/Nagarpalika name.}
#'   \item{sexname}{Character. Sex label.}
#'   \item{literacystatus}{Character. Literacy status label.}
#'   \item{category}{Character. Living arrangement category.}
#'   \item{number}{Character. Population count.}
#' }
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv70)
#' head(Indv70)
"Indv70"


#' Table 71: Children aged 5 and below by birth registration status
#'
#' Number of children 5 years of age and below by sex and birth
#' registration status, NPHC 2021.
#'
#' @format A tibble with 12 variables:
#' \describe{
#'   \item{prov}{Character. Province code (0 = Nepal total).}
#'   \item{dist}{Character. District code (0 = province total).}
#'   \item{gapa}{Character. Gaunpalika/Nagarpalika code.}
#'   \item{sex}{Character. Sex code.}
#'   \item{brthreg}{Character. Birth registration status code.}
#'   \item{provname}{Character. Province name.}
#'   \item{dname}{Character. District name.}
#'   \item{gapaname}{Character. Gaunpalika/Nagarpalika name.}
#'   \item{sexname}{Character. Sex label.}
#'   \item{brthregname}{Character. Birth registration status label
#'     (e.g., "Registered", "Not registered").}
#'   \item{category}{Character. Age group category.}
#'   \item{number}{Character. Population count.}
#' }
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#' @examples
#' data(Indv71)
#' head(Indv71)
"Indv71"
