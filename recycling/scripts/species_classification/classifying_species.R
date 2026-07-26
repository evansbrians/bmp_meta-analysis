
# Tara's batshit crazy species classification code

# Do not run this! It uses objects from the script file
# generate_species_search.R because that stuff takes so dang long.

# obligates ---------------------------------------------------------------

# I think this is a combined list from a few different sources.

obligates <- 
  "Northern Harrier;Swainson's Hawk;Ferruginous Hawk;Rough-legged Hawk;Aplomado Falcon;Rock Ptarmigan;White-tailed Ptarmigan;Sharp-tailed Grouse;Greater Prairie-Chicken;Lesser Prairie-Chicken;Montezuma Quail;Ocellated Quail;Double-striped Thick-knee;American Golden-Plover;Pacific Golden-Plover;Mountain Plover;Upland Sandpiper;Eskimo Curlew;Bristle-thighed Curlew;Long-billed Curlew;Marbled Godwit;Baird's Sandpiper;Buff-breasted Sandpiper;Pomarine Jaeger;Parasitic Jaeger;Long-tailed Jaeger;Snowy Owl;Burrowing Owl;Long-eared Owl;Short-eared Owl;Horned Lark;Sedge Wren;American Pipit;Sprague's Pipit;Ruddy-breasted Seedeater;Saffron Finch;Grassland Yellow-Finch;Cassin's Sparrow;Bachman's Sparrow;Botteri's Sparrow;Striped Sparrow;Vesper Sparrow;Lark Bunting;Savannah Sparrow;Grasshopper Sparrow;Baird's Sparrow;Henslow's Sparrow;Le Conte's Sparrow;Sierra Madre Sparrow;McCown's Longspur;Lapland Longspur;Smith's Longspur;Chestnut-collared Longspur;Snow Bunting;McKay's Bunting;Dickcissel;Bobolink;Eastern Meadowlark;Western Meadowlark" %>% 
  str_split_1(
    ";"
  ) %>% 
  tolower() %>% 
  str_replace_all("-", " ") %>% 
  str_remove_all("'") %>% 
  as_tibble() %>% 
  mutate(
    common_name = value,
    classification = "grassland_obligate",
    classification_source = "combined",
    .keep = "none"
  )

# facultative -------------------------------------------------------------

# Remove obligates (as classified by Vickery) from our big species search to get
# a broad sense of facultatives:

facultatives <- 
  read_lines("searches/species_search.txt") %>% 
  str_split_1(
    "\" OR \""
  ) %>% 
  as_tibble() %>% 
  mutate(
    common_name = 
      value %>% 
      str_replace_all("-", " ") %>% 
      str_remove_all("\\(TS = \\(|\\)\\)|[:punct:]") %>% 
      tolower(),
    .keep = "none"
  ) %>% 
  filter(!common_name %in% obligates) %>% 
  pull() %>% 
  str_c(facultatives, collapse = ";")

# From Vickery et al. 1999:

vickery_facultative <- 
  "American Bittern;Cattle Egret;Jabiru;Turkey Vulture;Lesser Yellow-headed Vulture;Greater White-fronted Goose;Emperor Goose;Snow Goose;Ross's Goose;Canada Goose;Brant Gadwall;American Wigeon;Mallard;Blue-winged Teal;Northern Shoveler;Northern Pintail;Green-winged Teal;Crested Caracara;American Kestrel;Merlin;Gyrfalcon;Peregrine Falcon;Prairie Falcon;Gray Partridge;Ring-necked Pheasant;Willow Ptarmigan;Scaled Quail;Elegant Quail;Northern Bobwhite;Black-throated Bobwhite;Crested Bobwhite;Yellow Rail;Sandhill Crane;Whooping Crane;Black-bellied Plover;Killdeer;Lesser Yellowlegs;Willet;Whimbrel;Hudsonian Godwit;Surfbird;Red Knot;Sanderling;Semipalmated Sandpiper;Western Sandpiper;Least Sandpiper;White-rumped Sandpiper;Pectoral Sandpiper;Purple Sandpiper;Rock Sandpiper;Dunlin;Short-billed Dowitcher;Long-billed Dowitcher;Common Snipe Wilson's Phalarope;Franklin's Gull;Mourning Dove;Common Ground-Dove;Barn Owl;Striped Owl;Lesser Nighthawk;Common Nighthawk;Common Poorwill;Say's Phoebe;Ash-throated Flycatcher;Cassin's Kingbird;Western Kingbird;Eastern Kingbird;Scissor-tailed Flycatcher;Fork-tailed Flycatcher;Loggerhead Shrike;Northern Shrike;Chihuahuan Raven;Eastern Bluebird;Western Bluebird;Mountain Bluebird;Bendire's Thrasher;Common Yellowthroat;Blue-black Grassquit;Yellow-bellied Seedeater;Yellow-faced Grassquit;Canyon Towhee;Rufous-winged Sparrow;Rufous-crowned Sparrow;Oaxaca Sparrow;Clay-colored Sparrow;Worthen's Sparrow;Lark Sparrow;Red-winged Blackbird;Brewer's Blackbird;Shiny Cowbird;Bronzed Cowbird;Brown-headed Cowbird;Gray-crowned Rosy-Finch;Black Rosy-Finch;Brown-capped Rosy-Finch" %>% 
  str_split_1(
    ";"
  ) %>% 
  tolower() %>% 
  str_replace_all("-", " ") %>% 
  str_remove_all("'") %>% 
  as_tibble() %>% 
  mutate(
    common_name = value,
    classification = "facultative grassland",
    classification_source = "vickery",
    .keep = "none"
  )

# From VGBI website

vgbi_facultative <-
  tibble(
    facultative_nesting = 
      str_split_1(
        "Northern Bobwhite;Song Sparrow;Wild Turkey;Loggerhead Shrike;Field Sparrow;Red-winged Blackbird;Common Yellowthroat;Blue Grosbeak",
        ";"
      ),
    overwinter_and_migration = 
      str_split_1(
        "American Tree Sparrow;Short-eared Owl;American Pipit;Lapland Longspur;Dark-eyed Junco;Fox Sparrow;White-crowned Sparrow;White-throated Sparrow",
        ";"
      )
  ) %>% 
  pivot_longer(
    everything(),
    values_to = "common_name",
    names_to = "classification"
  ) %>% 
  bind_rows(
    tibble(
      grassland_foragers =
        str_split_1(
          "Canada Goose;Eastern Kingbird;Mourning Dove;Yellow-billed Cuckoo;Black-billed Cuckoo;Common Nighthawk;Chimney Swift;Ruby-throated Hummingbird;Killdeer;American Woodcock;Cooper's Hawk;Red-shouldered Hawk;Red-tailed Hawk;Barn Owl;Downy Woodpecker;Northern Flicker;American Kestrel;Eastern Phoebe;American Crow;Purple Martin;Tree Swallow;Barn Swallow;Brown Thrasher;Northern Mockingbird;Eastern Bluebird;American Robin;House Finch;American Goldfinch;Chipping Sparrow;Eastern Towhee;Orchard Oriole;Brown-headed Cowbird;Yellow-breasted Chat;Prairie Warbler;Northern Cardinal;Indigo Bunting",
          ";"
        )
    ) %>% 
      pivot_longer(
        everything(),
        values_to = "common_name",
        names_to = "classification"
      )
  ) %>% 
  mutate(
    common_name =
      common_name %>% 
      tolower() %>% 
      str_replace_all("-", " ") %>% 
      str_remove_all("'"),
    classification_source = "vgbi"
  )

# From partners in flight (read in database):

pif <- 
  googlesheets4::read_sheet(
    file.path(
      "https://docs.google.com/spreadsheets/d",
      "1UhSvMqwGYMTpQFYyjrDI2xKxvZQVMSVRXHtaH10y_ss",
      "edit?gid=955867080#gid=955867080"
    )
  ) %>% 
  janitor::clean_names() 

pif_classified <-
  pif %>% 
  filter(
    if_any(
      contains("habitat"),
      ~ str_detect(.x, "Open|Scrub|Chaparral")
    ),
    order %in% bird_orders
  ) %>% 
  select(
    common_name,
    contains("habitat")
  ) %>% 
  pivot_longer(
    contains("habitat"),
    names_to = "habitat_rank",
    values_to = "classification"
  ) %>% 
  mutate(
    common_name =
      common_name %>% 
      tolower() %>% 
      str_replace_all("-", " ") %>% 
      str_remove_all("'"),
    classification_source = "Partners in Flight",
  ) %>% 
  filter(
    str_detect(classification, "Open|Scrub|Chaparral")
  )

# All about birds:

aab <- 
  all_about_birds_eastern_forest %>% 
  filter(
    !str_detect(habitat, "Forests|River|Grassland"),
  ) %>% 
  mutate(
    common_name =
      common_name %>% 
      tolower() %>% 
      str_replace_all("-", " ") %>% 
      str_remove_all("'"),
    classification = habitat,
    classification_source = "All About Birds",
    .keep = "none"
  )

# From Peterjohn:

pj <- 
  "Northern bobwhite;Black-billed cuckoo;Yellow-billed cuckoo;Alder flycatcher;Willow flycatcher;White-eyed vireo;Carolina wren;Gray catbird;Northern mockingbird;Brown thrasher;Blue-winged warbler;Golden-winged warbler;Nashville warbler;Yellow warbler;Chestnut-sided warbler;Prairie warbler;Common yellowthroat;Yellow-breasted chat;Eastern towhee;Field sparrow;Song sparrow;Northern cardinal;Blue grosbeak;Indigo bunting" %>% 
  str_split_1(
    ";"
  ) %>% 
  tolower() %>% 
  str_replace_all("-", " ") %>% 
  str_remove_all("'") %>% 
  as_tibble() %>% 
  mutate(
    common_name = value,
    classification = "shrubland",
    classification_source = "Peterjohn 2006",
    .keep = "none"
  )

# combined ----------------------------------------------------------------

bind_rows(
  obligates,
  vickery_facultative,
  vgbi_facultative,
  pif_classified,
  aab,
  pj
) %>% 
  write_rds(
    "data/processed/species_classification.rds"
  )
