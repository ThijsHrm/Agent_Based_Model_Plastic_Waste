; Variable naming conventions:
; G_xxx  => Global variables
; HH_xxx => Household breed variables
; MP_xxx => Municipality breed variables
; RC_xxx => Recycling company variables
; CT_xxx => Contract variables
; _xxx   => Local variable carrying parameters in function
; xxx    => Local variable in function

; Function naming conventions:
; with the exception of the "go" and "setup" function (for the sake of conformity),
; function names have capitalized words which are seperated by a dash (-)

; Comment key conventions:
; REF-N  => Reference needed
; REF-## => Reference provided. ## relates to the description provided in the Info --> References
; ASS-N  => Formal assumption needed
; ASS-## => Formal assumption provided. ## relates to the description provided in Info --> Assumptions

;=================================================================================
;    Part 1: Global variables
;=================================================================================

globals [
  ;----------------------- Household related ------------------
  G_occupancy_qty_list             ;; A list in which each number represents the occupancy quantity matching that given index
  G_waste_household_type_multiplier;; Base waste multiplier for every household type
  G_household_total_plastic_pct    ;; Percent of total household waste that is plastic [ASS-N] Assume this is fixed over time and by household type
  G_initial_perception_avg         ;; An indicator in range 0.00 - 1.00 for the average initial perception
  G_initial_knowledge_avg          ;; An indicator in range 0.00 - 1.00 for the average initial knowledge
  ;------------------- Recycling Company related --------------
  G_recycled_plastic_price         ;; Market price for recycled plastic
  G_tech_capacity_avg              ;; Average processing capacity at current time
  G_tech_recyc_rate_avg            ;; Average recycling rate at current time
  ;---------------------- Municipality related ----------------
  G_centralized_collection_impact  ;; % of separated waste that can be collected when using centralized infrastructure vs decentralized
  G_budget_per_person              ;; The recycling budget per person
  G_target                         ;; Monthly recycling target for municipalities set by national government
  G_pct_budget_edu_spending        ;; The percentage of the budget that will be spent on education (knowledge & perception increases)
  G_pct_education_split            ;; The distribution of education budget on knowledge vs. perception (0 is 100% knowledge, 0.5 is equally distributed, 1 is 100% perception)
]

;=================================================================================
;    Part 2: Breed declarations and specifications
;=================================================================================
breed [HHs HH]                   ;; Households
breed [MPs MP]                   ;; Municipalities
breed [RCs RC]                   ;; Recycling Companies
breed [Contracts Contract]       ;; Contracts

HHs-own [
  HH_owner                       ;; The municipality that the household belongs to
  HH_occupancy                   ;; Type of occupancy (0 = family [4 people], 1 = couple [2 people], 2 = single [1 person], 3 = senior [2 people])
  HH_perception                  ;; Perception of importance of recycling (%)
  HH_knowledge                   ;; Knowledge of proper recycling (%)
  HH_total_waste                 ;; Refer to Info Tab (tonne)
  HH_plastic_waste               ;; Refer to Info Tab (tonne)
  HH_separated_waste             ;; Refer to Info Tab (tonne)
  HH_separated_recyc_frac        ;; Refer to Info Tab (%)
]

MPs-own [
  MP_list_of_HHs                 ;; List of households in the municipality
  MP_budget                      ;; Monthly recycling budget of the municipality
  MP_infrastructure              ;; Type of infrastructure (0 = centralized, 1 = decentralized)
  MP_months_overbudget           ;; How many months the municipality has gone over their recycling budget
  MP_my_contracts                ;; Link to the current recycling contract held by the municipality
  MP_total_waste                 ;; Refer to Info Tab (tonne)
  MP_plastic_waste               ;; Refer to Info Tab (tonne)
  MP_separated_waste             ;; Refer to Info Tab (tonne)
  MP_separated_recyc_frac        ;; Refer to Info Tab (%)
  MP_recycled                    ;; The amount of plastic successfully recycled (tonne)
  MP_recyc_rate                  ;; Recycling rate (%)
  MP_knowledge_spending          ;; How many months worth of budget were spent on knowledge
  MP_perception_spending         ;; How many months worth of budget were spent on perception of importance
]

RCs-own [
  RC_list_of_techs               ;; List of technologies operated by the recycling company
  RC_list_of_contracts           ;; List of contracts held by the recycling company
  RC_account_balance             ;; Account balance of the recycling company
]

Contracts-own [
  CT_MP_own                      ;; MP that owns the contract
  CT_RC_own                      ;; RC that owns the contract
  CT_min                         ;; The minimum amount required by the RC
  CT_max                         ;; The maximum amount allowed by the RC
  CT_mid                         ;; The amount of separated waste expected by the RC
  CT_recyc_rate                  ;; Recycle rate to be achieved
  CT_recyc_rate_gap              ;; Difference between the MP's target and the RC's capability (used to reverse sort contracts by highest recycling rate)
  CT_fee                         ;; Monthly fee paid to RC
  CT_fine                        ;; Fine to be levied should the MP not meet the min_waste
  CT_fine_levied                 ;; This variable carries either 0 or CT_fine depending on wether recycling targets were met
  CT_overallrank                 ;; Used to rank contracts against each other
  CT_active                      ;; Whether or not the contract has been accepted by the MP and activated
]

;=================================================================================
;    Part 3.1: Function definitions - Program flow
;=================================================================================

; The setup function has to be run before the go button is pushed,
; and executes only ONCE per simulation
to setup
  ; Create an empty space for the model to run in
  clear-all
  reset-ticks

  ; Give values to the global variables [some based on slider input]
  ;----------------------- Household related ------------------
  set G_household_total_plastic_pct (S_pct_waste_is_plastic / 100)
  set G_occupancy_qty_list (list 4 2 1 2)
  set G_waste_household_type_multiplier (list 3.5 2 1 0.8)
  ;---------------------- Municipality related ----------------
  set G_pct_budget_edu_spending (S_pct_budget_education_spending / 100)
  set G_pct_education_split (S_pct_edu_budget_split / 100)
  set G_centralized_collection_impact (S_centralized_collection_ratio / 100)
  set G_budget_per_person (S_budget_per_person / 100)
  ;------------------- Recycling company related --------------
  set G_tech_capacity_avg S_tech_avg_capacity
  set G_tech_recyc_rate_avg (S_tech_avg_recycling_rate / 100)

  ; Create the households and municipalities
  foreach range S_#_of_municipalities [
    create-MPs 1 [
      let number random (S_max_#_households - S_min_#_households) + S_min_#_households
      hatch-HHs number [
        set HH_owner myself
        set HH_occupancy random 4
      ]
      set MP_list_of_HHs HHs with [HH_owner = myself]
      if S_infrastructure = "Random" [set MP_infrastructure random 2 ]
      if S_infrastructure = "Centralized" [set MP_infrastructure 0 ]
      if S_infrastructure = "Decentralized" [set MP_infrastructure 1 ]
      set MP_months_overbudget 0
      set MP_knowledge_spending 0
      set MP_perception_spending 0
      output-print (word self ",  " MP-population " people,  " (count MP_list_of_HHs) " households,  infrastructure type: " MP_infrastructure)
    ]
  ]
  ; Create the recycling companies
  create-RCs S_#_of_recycling_companies [
    ; New RCs get assigned one random tech
    let capacity G_tech_capacity_avg * (1 + (random 50 - 25) / 100)         ;; Capacity of new tech is the global average +/- 25%
    let recyc_rate G_tech_recyc_rate_avg * (1 + (random 20 - 10) / 100)     ;; Recycling rate of new tech is the global average +/- 10%
    let tech (list capacity recyc_rate)
    set RC_list_of_techs (list tech)
  ]
  output-print ""
end

; This is the main loop of the simulation, and executes on every tick (representing 1 month)
to go
  ask MPs [
    set MP_budget G_budget_per_person * MP-Population
    MP-Educate
  ]
  ask HHs [ HH-Generate-Waste ]
  ask MPs [ MP-Collect-Waste ]

  if ticks mod 36 = 0 [
    ; Kill RCs with no contracts and create new ones
    ask RCs [ set RC_list_of_contracts Contracts with [CT_RC_own = myself]]
    let #_bankrupt_RCs count RCs with [count RC_list_of_contracts = 0]
    ask RCs with [count RC_list_of_contracts = 0] [die]
    output-print G_tech_recyc_rate_avg
    create-RCs #_bankrupt_RCs [
    ; New RCs get assigned one random tech
      let capacity G_tech_capacity_avg * (1 + (random 50 - 25) / 100)        ;; Capacity of new tech is the global average +/- 25%
      let recyc_rate G_tech_recyc_rate_avg * (1 + (random 20 - 10) / 100)    ;; Recycling rate of new tech is the global average +/- 10%
      let tech (list capacity recyc_rate)
      set RC_list_of_techs (list tech)
    ]
    ask MPs [
      MP-Select-Bid
      output-print (word "Municipality: " self)
      output-print (word "Bidding for year: " round (ticks / 12))
      ask MP_my_contracts [output-print (word CT_RC_own "  " CT_min)]
      if count MP_my_contracts = 0 [output-print "No match between supply and demand! No contracts present"]
      output-print ""
    ]

  ]

  ask MPs [
    MP-Transfer-Separated-Waste                                              ;; Separated recycling waste is transferred to RCs. Remaining plastic waste is incinerated
    if MP_budget < 0 [set MP_months_overbudget MP_months_overbudget + 1]
  ]

  set G_tech_capacity_avg min list (G_tech_capacity_avg + 1) 2000
  set G_tech_recyc_rate_avg min list (G_tech_recyc_rate_avg + 0.001) 1

  ifelse ticks = 240 [stop][tick]

end

; This resets the slider values to their defaults
to default-sliders
  set S_#_of_municipalities 3
  set S_pct_waste_is_plastic 14
  set S_min_#_households 1000
  set S_max_#_households 5000
  set S_#_of_recycling_companies 10
  set S_tech_avg_recycling_rate 65
  set S_tech_avg_capacity 50
  set S_centralized_collection_ratio 80
  set S_budget_per_person 0.50
  set S_pct_budget_education_spending 50
  set S_pct_edu_budget_split 50
end
;=================================================================================
;    Part 3.2: Function definitions - Households
;=================================================================================

; Calculate total waste generated by each HH [DONE]
to HH-Generate-Waste
  let _waste (40 - 0.04 * ticks - exp(-0.01 * ticks) * sin(0.3 * ticks * 180 / pi)) / 1000
  set HH_total_waste _waste * item HH_occupancy G_waste_household_type_multiplier   ;; Household type multiplier for waste generation
  set HH_plastic_waste HH_total_waste * G_household_total_plastic_pct
  set HH_separated_waste HH_plastic_waste * HH_perception
  set HH_separated_recyc_frac HH_knowledge
end

;;---------------------------------------------------------------------------
;;    Reporters for analysis
;;---------------------------------------------------------------------------
to-report HH-Knowledge
  report mean [HH_knowledge] of HHs
end

to-report HH-Perception
  report mean [HH_perception] of HHs
end
;;---------------------------------------------------------------------------

;=================================================================================
;    Part 3.2: Function definitions - Municipalities
;=================================================================================

; This function clears the current contracts, asks companies to generate new ones and picks the best options [DONE]
to MP-Select-Bid
  ; Clear the previous contracts for the current MP
  ask Contracts with [CT_MP_own = myself] [die]

  ; Generate a list of bids from RCs
  let bids []
  ask RCs [
    let bid RC-Prepare-Bid [MP_separated_waste] of myself [MP_separated_recyc_frac] of myself
    if item 2 bid != 0 [set bids lput (bid) bids]                                  ;; If the min capacity of the bid is 0, means that the RC is 100% busy with other contracts.
                                                                                   ;; Don't add these to the pool of bids.
  ]
  ; Create a contract agent for each bid to be assessed
  foreach bids [ [?1] ->
    hatch-Contracts 1 [
      set CT_MP_own myself
      set CT_RC_own item 1 ?1
      set CT_min item 2 ?1
      set CT_max item 3 ?1
      set CT_mid CT_min + (CT_max - CT_min) / 2
      set CT_recyc_rate item 4 ?1
      set CT_recyc_rate_gap G_target - CT_recyc_rate
      set CT_fee item 5 ?1
      set CT_fine item 6 ?1
      set CT_active 0
    ]
  ]
  let winners MP-Pick-Winners
  ask contracts with [CT_active = 0] [die]
  set MP_my_contracts Contracts with [CT_MP_own = myself]
end

; This function ranks the available contracts based on fine and capacity and picks the most ideal one [DONE]
to-report MP-Pick-Winners
  let contracts_total_capacity 0
  let contract_list Contracts with [CT_MP_own = myself]
  let contracts_picked []

  ; Create rankings based on different criteria
  let lowest_fee sort-on [CT_fee] contract_list
  let lowest_fine sort-on [CT_fine] contract_list
  let highest_recyc_rate sort-on [CT_recyc_rate_gap] contract_list

  ; Average the scores based on the different rankings and sort
  ask contract_list [set CT_overallrank mean (list position self lowest_fine position self lowest_fee position self highest_recyc_rate)]
  let contract_ranking sort-on [CT_overallrank] contract_list

  ; Keep adding contracts until the municipality's requirements are met
  foreach contract_ranking [ [?1]->
    if MP_separated_waste - contracts_total_capacity > 0 [                       ;; If all of the MPs separated waste isn't accounted for, add more contracts
      set contracts_picked lput ?1 contracts_picked
      ask ?1 [
        set contracts_total_capacity contracts_total_capacity + CT_max
        set CT_active 1                                                          ;; Once a contract is selected, activate it
      ]
    ]
  ]
  report contracts_picked
end

; Count population [DONE]
to-report MP-Population
    let families count MP_list_of_HHs with [HH_occupancy = 0]
    let couples count MP_list_of_HHs with [HH_occupancy = 1]
    let singles count MP_list_of_HHs with [HH_occupancy = 2]
    let senors count MP_list_of_HHs with [HH_occupancy = 3]
    let total families * 4 + couples * 2 + singles * 1 + senors * 2
    report total
end

; Collect the waste generated by HHs [DONE]
to MP-Collect-Waste
  let total_waste 0
  let plastic_waste 0
  let separated_waste 0
  let separated_recyc_frac 0

  ; Collect waste from HHs
  ask MP_list_of_HHs [
    set total_waste total_waste + HH_total_waste
    set plastic_waste plastic_waste + HH_plastic_waste
    set separated_waste separated_waste + HH_separated_waste
    ; Weighted average
    set separated_recyc_frac separated_recyc_frac + HH_separated_waste * HH_separated_recyc_frac
  ]
  ; Weighted average continued
  set separated_recyc_frac separated_recyc_frac / separated_waste

  ; If collection is decentralized, all the plastic set aside for recycling is collected.
  ; If collection is centralized, only a certain percentage of plastic set aside for recycling is collected.
  ; The assumption here is that even though people might know of the importance of recycling, some may be
  ; too lazy/busy to take their recycling to the collection point.
  if MP_infrastructure = 0 [set separated_waste separated_waste * G_centralized_collection_impact]

  ; Transfer this waste to the MP
  set MP_total_waste total_waste
  set MP_plastic_waste plastic_waste
  set MP_separated_waste separated_waste
  set MP_separated_recyc_frac separated_recyc_frac
end

; Transfer recycled waste to RCs [DONE]
to MP-Transfer-Separated-Waste
  set MP_recycled 0

  ; First step is to meet all the minimum requirements for each contract so that the MP doesn't get fined
  ask MP_my_contracts [
    let fine 0
    let fee CT_fee
    let transfer_qnty min list CT_min [MP_separated_waste] of myself                                                 ;; Define amount to be transfered
    let transfer_qlty [MP_separated_recyc_frac] of myself
    ifelse transfer_qnty < CT_min [set fine CT_fine * (CT_min - transfer_qnty)][set fine 0]                          ;; If the transfered amount is less than what the contract specifies, a fine is applied
    ask CT_MP_own [set MP_budget MP_budget - fee - fine]                                                             ;; Monthly fee and the fine are paid by the MP
    ; Waste of specific quantity and quality is sent to RC for separation and sale
    let sellable_qnty 0
    ask CT_RC_own [
      set sellable_qnty transfer_qnty * transfer_qlty * [CT_recyc_rate] of myself                                    ;; RC separates the plastic into a sellable quantity
      set RC_account_balance RC_account_balance + fee + fine + sellable_qnty * G_recycled_plastic_price              ;; Monthly fee and fines are paid to the RC. Sellable plastic is also sold
      set RC_account_balance RC_account_balance - transfer_qnty * Operating-Cost [CT_min] of myself RC-Recyc-Rate    ;; Deduct operating costs
    ]
    ask CT_MP_own [
      set MP_separated_waste MP_separated_waste - transfer_qnty                                                      ;; Transfered amount is removed from the MPs inventory
      set MP_recycled MP_recycled + sellable_qnty                                                                    ;; Account for how much was successfully recycled
    ]
  ]

  ; Now that all the contractual minimums have been met, any remaining separated waste is transfered to the RCs
  ask MP_my_contracts [
    if [MP_separated_waste] of myself > 0 [
      let transfer_qnty min list (CT_max - CT_min) ([MP_separated_waste] of myself)
      let transfer_qlty [MP_separated_recyc_frac] of myself
      let sellable_qnty 0
      ask CT_RC_own [
        set sellable_qnty transfer_qnty * transfer_qlty * [CT_recyc_rate] of myself                                  ;; RC separates the plastic into a sellable quantity
        set RC_account_balance RC_account_balance + sellable_qnty * G_recycled_plastic_price                         ;; Sellable plastic is sold
        set RC_account_balance RC_account_balance - transfer_qnty * Operating-Cost [CT_min] of myself RC-Recyc-Rate  ;; Deduct operating costs
      ]
      ask CT_MP_own [
        set MP_separated_waste MP_separated_waste - transfer_qnty                                                    ;; Transfered amount is removed from the MPs inventory
        set MP_recycled MP_recycled + sellable_qnty                                                                  ;; Account for how much was successfully recycled
      ]
    ]
  ]
  set MP_recyc_rate MP_recycled / MP_plastic_waste                                                                   ;; Calculate recycling rate
end

; Educate households
to MP-Educate
  ; Figure out the investment in education. A certain amount is removed every month to account for people losing knowledge if it is not trained
  set MP_knowledge_spending max list (MP_knowledge_spending + G_pct_budget_edu_spending * G_budget_per_person * (1 - G_pct_education_split) - 0.2) 0
  set MP_perception_spending max list (MP_perception_spending + G_pct_budget_edu_spending * G_budget_per_person * G_pct_education_split - 0.2) 0
  ; Sigmoid functions
  ask MP_list_of_HHS [
    set HH_perception 0.85 / (1 + exp(-2 * [MP_perception_spending] of myself + 2)) + 0.1
    set HH_knowledge  0.85 / (1 + exp(-1 * [MP_knowledge_spending] of myself + 2.5)) + 0.05
  ]
 set MP_budget MP_budget * (1 - G_pct_budget_edu_spending)
end

to-report MP-Months-Overbudget
  let val 0
  ask MP 0 [set val MP_months_overbudget]
  report val
end

;;---------------------------------------------------------------------------
;;    Reporters for analysis
;;---------------------------------------------------------------------------
to-report MP-Recycling-Rates
  report mean [MP_recyc_rate] of MPs
end

;=================================================================================
;    Part 3.2: Function definitions - Recycling Companies
;=================================================================================

to-report RC-Prepare-Bid [_qnty _qlty]
  let total_capacity 0
  let total_separated 0
  ; RCs sum up their tech
  foreach RC_list_of_techs [ [?1] ->
    set total_capacity total_capacity + item 0 ?1
    set total_separated total_separated + item 0 ?1 * item 1 ?1
    ]
  let total_recyc_rate total_separated / total_capacity
  ask Contracts with [CT_RC_own = myself] [set total_capacity total_capacity - CT_max]                            ;; Subtract processing capacities allocated to other contracts

  let _min _qnty * 0.9
  let _max _qnty * 1.1
  if _max > total_capacity [report (list 0 0 0)]                                                                  ;; If the RC can't allocate enough capacity, they do not submit a bid
  let fine (G_recycled_plastic_price - (Not-Operating-Cost _min total_recyc_rate)) * total_recyc_rate * _qlty     ;; Refer to "Bidding Model.xlsx"
  let fee _min * Operating-Cost _min total_recyc_rate                                                             ;; Refer to "Bidding Model.xlsx"
  let bid (list 0 self _min _max total_recyc_rate fee fine)
  report bid
end

to-report RC-Capacity
  let total 0
  foreach RC_list_of_techs [ [?1] ->
    set total total + item 0 ?1
  ]
  report total
end

to-report RC-Recyc-Rate
  let total 0
  let separated_total 0
  foreach RC_list_of_techs [ [?1] ->
    set total total + item 0 ?1
    set separated_total separated_total + item 0 ?1 * item 1 ?1
  ]
  report separated_total / total
end

; Calculate operating cost of running tech
to-report Operating-Cost [_capacity _recyc_rate]
  report -135 / 40000 * _capacity / (1 + (_recyc_rate - G_tech_recyc_rate_avg)) + 215 * (1 + (_recyc_rate - G_tech_recyc_rate_avg))    ;; Empirically modelled based on data
end

to-report Not-Operating-Cost [_capacity _recyc_rate]
  report (Operating-Cost _capacity _recyc_rate) / 2
end
;;---------------------------------------------------------------------------

;=================================================================================
;    Part 4: Validation Functions
;=================================================================================
; These functions can be run to validate the operation of the functions. The
; inputs and outputs are provided so that the results can be compared with
; hand calculations.

; Validation of bidding process functions:
; MP-Select-Bid, RC-Prepare-Bid, MP-Pick-Winners
to setup-bid-validation
  ; Create an empty space for the model to run in
  clear-all
  reset-ticks

  ; Give values to the global variables
  ;------------------- Recycling company related --------------
  set G_tech_capacity_avg 6000
  set G_tech_recyc_rate_avg 0.85
  set G_recycled_plastic_price 1000
  set G_target 0.8
  ; Create the households and municipalities
  create-MPs 2 [
    set MP_total_waste 20000
    set MP_plastic_waste 4000
    set MP_separated_waste 3000
    set MP_separated_recyc_frac 1
  ]

  ; Create the recycling companies
  create-RCs 10 [
    let capacity round G_tech_capacity_avg * (1 + (random 50 - 25) / 100)
    let recyc_rate min list (G_tech_recyc_rate_avg * (1 + (random 10 - 5) / 100)) 1
    let tech (list capacity recyc_rate 0 0)
    set RC_list_of_techs (list tech)
  ]

  ask MPs [

    let bids []
    ask RCs [
      let bid RC-Prepare-Bid [MP_separated_waste] of myself [MP_separated_recyc_frac] of myself
      if item 2 bid != 0 [set bids lput (bid) bids]
    ]

    output-print (word "Municipality: " self)
    output-print (word "Separated waste generation rate (/month): " MP_separated_waste)
    output-print "-------------------------"
    output-print "Bids presented by Recycling Companies"
    output-print (word "RC-own/min/max/recyc rate/fee/fine")
    foreach bids [ [?1] ->
      output-print (word item 1 ?1 "    " round item 2 ?1 "    " round item 3 ?1 "    " ((round (item 4 ?1 * 100)) / 100) "    " round item 5 ?1 "    " round item 6 ?1)
    ]
    output-print "-------------------------"
    MP-Select-Bid
    output-print "Winning Bids:"
    output-print "RC-own/min/max/Overall Rank/Full Capacity"
    ask MP_my_contracts [
      let full_capacity 0
      ask CT_RC_own [set full_capacity RC-Capacity]
      output-print (word CT_RC_own "  " round CT_min "  " round CT_max "  " ((round (CT_overallrank * 10)) / 10) "  " round full_capacity)
    output-print ""
    ]
  ]

end

; Validation of waste generation and collection functions:
; HH-Generate-Waste and MP-Collect-Waste
to setup-waste-validation

  ; Create an empty space for the model to run in
  clear-all
  reset-ticks

  ; Give values to the global variables
  ;----------------------- Household related ------------------
  set G_initial_perception_avg 1
  set G_initial_knowledge_avg 0.5
  set G_household_total_plastic_pct 0.5
  set G_occupancy_qty_list (list 4 2 1 2)
  set G_waste_household_type_multiplier (list 3.5 2 1 0.8)      ; [ASS-N]
  ;---------------------- Municipality related ----------------
  set G_centralized_collection_impact 1

  ; Create the households and municipalities
  create-MPs 1
  create-HHs 5 [
    set HH_owner one-of MPs                                               ;; households get assigned to a municipality randomly
    set HH_occupancy random 4                                             ;; sets occupancy type randomly
    set HH_perception G_initial_perception_avg                            ;;* (1 + (random 50 - 25) / 100)
    set HH_knowledge G_initial_knowledge_avg                              ;;* (1 + (random 50 - 25) / 100)
  ]
  ask MPs [
    set MP_list_of_HHs HHs with [HH_owner = myself]                       ;; municipalities gather their households
    set MP_budget 10 * count MP_list_of_HHs                               ;; budget based on number of households
    set MP_infrastructure random 2                                        ;; infrastruction (centralized/de-centralized) assigned randomly
    set MP_months_overbudget 0
  ]
  ask HHs [ HH-Generate-Waste ]

  ask MPs [
    output-print (word "Households (HH) created: " count MP_list_of_HHs)
    let families count MP_list_of_HHs with [HH_occupancy = 0]
    let couples count MP_list_of_HHs with [HH_occupancy = 1]
    let singles count MP_list_of_HHs with [HH_occupancy = 2]
    let senors count MP_list_of_HHs with [HH_occupancy = 3]

    output-print (word "# of Family HHs: " families)
    output-print (word "# of Couple HHs: " couples)
    output-print (word "# of Single HHs: " singles)
    output-print (word "# of Senor HHs:  " senors)
    output-print (word "Total population: " MP-Population)
    output-print "-------------------------"
    output-print "(1) Executing function 'HH-Gather-Waste'"
    hatch-HHs 1 [
      set HH_occupancy 0                                                 ;; sets occupancy type randomly
      set HH_perception G_initial_perception_avg                         ;;* (1 + (random 50 - 25) / 100)
      set HH_knowledge G_initial_knowledge_avg                           ;;* (1 + (random 50 - 25) / 100)
      HH-Generate-Waste
      output-print (word "Waste from Family HHs: " HH_total_waste)
      die
    ]

    hatch-HHs 1 [
      set HH_occupancy 1                                                 ;; sets occupancy type randomly
      set HH_perception G_initial_perception_avg                         ;;* (1 + (random 50 - 25) / 100)
      set HH_knowledge G_initial_knowledge_avg                           ;;* (1 + (random 50 - 25) / 100)
      HH-Generate-Waste
      output-print (word "Waste from Couple HHs: " HH_total_waste)
      die
    ]

    hatch-HHs 1 [
      set HH_occupancy 2                                                 ;; sets occupancy type randomly
      set HH_perception G_initial_perception_avg                         ;;* (1 + (random 50 - 25) / 100)
      set HH_knowledge G_initial_knowledge_avg                           ;;* (1 + (random 50 - 25) / 100)
      HH-Generate-Waste
      output-print (word "Waste from Single HHs: " HH_total_waste)
      die
    ]

    hatch-HHs 1 [
      set HH_occupancy 3                                                 ;; sets occupancy type randomly
      set HH_perception G_initial_perception_avg                         ;;* (1 + (random 50 - 25) / 100)
      set HH_knowledge G_initial_knowledge_avg                           ;;* (1 + (random 50 - 25) / 100)
      HH-Generate-Waste
      output-print (word "Waste from Senor HHs: " HH_total_waste)
      die
    ]

    output-print "-------------------------"
    output-print "(2) Executing function 'MP-Collect-Waste'"

    output-print (word "% of Total waste that is Plastic): " (G_household_total_plastic_pct * 100) "%")
    output-print (word "HH Perception of Importance: " (G_initial_perception_avg * 100) "%")
    output-print (word "HH Knowledge: " (G_initial_knowledge_avg * 100) "%")
    ifelse MP_infrastructure = 0 [output-print (word "Infrastructure: Centralized")][output-print (word "Infrastructure: De-centralized")]
    output-print (word "% diff of central vs de-central " (G_centralized_collection_impact * 100) "%")
    output-print ""
    MP-Collect-Waste
    output-print (word "Municipality total waste: " MP_total_waste)
    output-print (word "Municipality plastic waste: " MP_plastic_waste)
    output-print (word "Municipality recyclable plastic waste: " MP_separated_waste)
    output-print (word "Municipality separated recyclable fraction: " (MP_separated_recyc_frac * 100) "%")
  ]
end

to setup-transfer-validation
  ; Create an empty space for the model to run in
  clear-all
  reset-ticks

  ; Give values to the global variables
  ;----------------------- Household related ------------------
  set G_initial_perception_avg 1
  set G_initial_knowledge_avg 1
  set G_household_total_plastic_pct 1
  set G_occupancy_qty_list (list 4 2 1 2)
  set G_waste_household_type_multiplier (list 3.5 2 1 0.8)
  ;---------------------- Municipality related ----------------
  set G_pct_budget_edu_spending 1
  set G_pct_education_split 0.5
  set G_centralized_collection_impact 1
  set G_budget_per_person 100
  ;------------------- Recycling company related --------------
  set G_tech_capacity_avg 5000
  set G_tech_recyc_rate_avg 1

  ; Create the households and municipalities
  create-MPs 1

  create-HHs 1000 [
    set HH_owner one-of MPs                                               ;; households get assigned to a municipality randomly
    set HH_occupancy random 4                                             ;; sets occupancy type randomly
    set HH_perception G_initial_perception_avg                            ;;* (1 + (random 50 - 25) / 100)
    set HH_knowledge G_initial_knowledge_avg                              ;;* (1 + (random 50 - 25) / 100)
  ]

  ask MPs [
    set MP_list_of_HHs HHs with [HH_owner = myself]                       ;; municipalities gather their households
    set MP_budget G_budget_per_person * MP-Population                     ;; budget based on number of households
    set MP_infrastructure random 2                                        ;; infrastruction (centralized/de-centralized) assigned randomly
    set MP_months_overbudget 0
  ]

  ; Create the recycling companies
  create-RCs 3 [
    let capacity round G_tech_capacity_avg
    let recyc_rate G_tech_recyc_rate_avg
    let tech (list capacity recyc_rate 0 0)
    set RC_list_of_techs (list tech)
  ]

  ask HHs [ HH-Generate-Waste ]

  ask MPs [
    output-print (word "Municipality: " self)
    MP-Collect-Waste
    output-print "\n---------Before Transfer---------\n"
    MP-Select-Bid
    output-print (word "Municipality waste\n(total/separated waste/budget)\n" round MP_total_waste "  " round MP_separated_waste "  " round MP_budget "\n")
    output-print (word "Recycling Company/Account Balance")
    ask RCs [output-print (word self "  " round RC_account_balance)]
    MP-Transfer-Separated-Waste
    output-print "\n---------After Transfer---------\n"
    output-print (word "Municipality waste\n(total/separated waste/budget)\n" round MP_total_waste "  " round MP_separated_waste "  " round MP_budget "\n")
    output-print (word "Recycling Company/Account Balance")
    ask RCs [output-print (word self "  " round RC_account_balance)]
    output-print "-------------------------"
    let capacity_sum 0
    ask MP_my_contracts [set capacity_sum capacity_sum + CT_max]
    output-print (word "Total capacity of contracts: " round capacity_sum)
    output-print (word "Recycling rate: " ((round (MP_recyc_rate * 100)) / 100))
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
1190
417
1231
459
-1
-1
1.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
933
145
1237
178
Validate Waste Generation and Collection Routines
setup-waste-validation
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
561
10
918
459
11

BUTTON
933
104
1236
137
Validate Contract Bidding Routines
setup-bid-validation
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
933
188
1237
221
Validate Transferring and Accounting Routines
setup-transfer-validation
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
934
10
1236
98
Validation Functions:\nThe following validations test most of the functions except for the 'setup' or 'go' routines. The validations ignore the variables defined by sliders. The inputs and outputs of the functions are provided in the output box so that the results can be compared to hand calculations.
11
0.0
1

BUTTON
10
10
103
43
SETUP
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
11
50
103
83
GO
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
112
10
536
181
Household Education
Months
%
0.0
240.0
0.0
1.0
true
true
"" ""
PENS
"Knowledge" 1.0 0 -2674135 true "" "ask one-of HHs [plot HH_knowledge] "
"Perception" 1.0 0 -13345367 true "" "ask one-of HHs [plot HH_perception] "

PLOT
112
184
536
334
Plastic Recycling Rate
Month
%
0.0
240.0
0.0
1.0
true
false
"" ""
PENS
"Recycling Rate" 1.0 0 -16777216 true "" "plot mean [MP_recyc_rate] of MPs"

SLIDER
318
337
536
370
S_pct_waste_is_plastic
S_pct_waste_is_plastic
0
100
14.0
1
1
%
HORIZONTAL

SLIDER
324
543
542
576
S_pct_budget_education_spending
S_pct_budget_education_spending
0
100
80.0
1
1
%
HORIZONTAL

SLIDER
111
578
323
611
S_pct_edu_budget_split
S_pct_edu_budget_split
0
100
80.0
1
1
%
HORIZONTAL

SLIDER
318
442
536
475
S_centralized_collection_ratio
S_centralized_collection_ratio
1
100
80.0
1
1
%
HORIZONTAL

SLIDER
111
543
322
576
S_budget_per_person
S_budget_per_person
0
125
80.0
1
1
cents
HORIZONTAL

SLIDER
111
442
315
475
S_tech_avg_capacity
S_tech_avg_capacity
10
500
50.0
10
1
Tonnes
HORIZONTAL

SLIDER
318
407
536
440
S_tech_avg_recycling_rate
S_tech_avg_recycling_rate
1
100
65.0
1
1
%
HORIZONTAL

SLIDER
112
337
315
370
S_#_of_municipalities
S_#_of_municipalities
1
10
3.0
1
1
NIL
HORIZONTAL

SLIDER
111
407
315
440
S_#_of_recycling_companies
S_#_of_recycling_companies
1
50
10.0
1
1
RCs
HORIZONTAL

SLIDER
112
372
315
405
S_min_#_households
S_min_#_households
500
2000
1000.0
100
1
NIL
HORIZONTAL

BUTTON
11
89
104
122
Default Sliders
default-sliders
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
115
479
532
497
-------------------------------------Municipal Policy Variables-------------------------------------
11
0.0
1

SLIDER
318
372
536
405
S_max_#_households
S_max_#_households
2000
10000
2000.0
100
1
NIL
HORIZONTAL

MONITOR
1
129
109
174
Months Overbudget
MP-Months-Overbudget
0
1
11

CHOOSER
111
496
537
541
S_Infrastructure
S_Infrastructure
"Random" "Centralized" "Decentralized"
2

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

**Waste Generation and Collection:**
The waste is broken down into three types:

_Total Waste_ - This represents the base waste generated by a household and is inclusive of all types of waste (plastic, paper, organic, etc.). Households generate waste based on their occupancy type and the waste generation equation (waste generated per person per month = 40 - 0.04*x – exp(-0.01*x)*sin(0.3*x) where x is the month). Depending on the occupancy type, the following multipliers are applied: [senior 0.80; single 1; couple 2; family 4]. This ensures that the base waste generation follows the trend senior<single<couple<family.

_Total Plastic Waste_ - This represents the amount of plastic waste in the total waste

_Separated Plastic Waste_ - This represents the amount of plastic waste separated by households for recycling. This is impacted by the perception of importance of recycling.

_Recyclable Fraction_ - This represents the fraction of the separated plastic waste that is recyclable.

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="4" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>MP-Months-Overbudget</metric>
    <metric>HH-Knowledge</metric>
    <metric>HH-Perception</metric>
    <metric>MP-Recycling-Rates</metric>
    <enumeratedValueSet variable="S_tech_avg_recycling_rate">
      <value value="65"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S_Infrastructure">
      <value value="&quot;Centralized&quot;"/>
      <value value="&quot;Decentralized&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S_#_of_recycling_companies">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S_max_#_households">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S_pct_waste_is_plastic">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S_tech_avg_capacity">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S_budget_per_person">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S_pct_edu_budget_split">
      <value value="20"/>
      <value value="40"/>
      <value value="60"/>
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S_pct_budget_education_spending">
      <value value="20"/>
      <value value="40"/>
      <value value="60"/>
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S_min_#_households">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S_centralized_collection_ratio">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S_#_of_municipalities">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
