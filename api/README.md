---
address: 'King''s College London'
author:
- Martin Chapman
bibliography:
- 'bib.bib'
title: 'CONSULT guideline server: TMRweb'
---

Introduction
============

This document summarises how to interact with the CONSULT guidelines
service, TMRweb, which packages TMR [@Zamborlini2016] as a web service,
allowing for the representation of, and identification of interactions
between, clinical guidelines, specifically drug administration, in an
interoperable manner.

Scenario
========

![Guideline set *HT* (hypertension), consisting of *Diueretic2* and
*Diuretic*.[]{label="HT"}](CIG-HT.png){#HT width="0.8\linewidth"}

Figure [1](#HT){reference-type="ref" reference="HT"} shows two
hypertension guidelines that are *alternatives*, in that they express
the same information. This is also a type of alternative known as a
*repairable transition*, because the actions undertaken by one guideline
can be reversed by performing the actions associated with the other. In
text form, these guidelines might read '*To reduce a patient's blood
pressure, administer Thiazide. Similarly, to avoid exacerbating a
patient's blood pressure, avoid the administration of Ibuprofen.*'

In the example detailed in this document, our aim is to represent these
guidelines using the semantic format used by TMR, and to then use a
computational implementation of TMR to identify the repairable
transition shown, features which are both offered by TMRweb. This then
serves as an example for how to use TMRweb to create new guideline sets,
and identify interactions between the constituent guidelines.

In Figure [1](#HT){reference-type="ref" reference="HT"}, the second
guideline, Reduce Blood Pressure, will be referred to using the ID
*Diuretic*; the first, Avoid High Blood Pressure, using the ID
*Diuretic2*; and the guidelines as a collection using the ID *HT*.

Implementation
==============

TMRweb is a RESTful web service, which accepts HTTP POST requests.
Therefore, to construct our semantic representation, and to then
interrogate this implementation, requests will be issued to this
service, using the *CURL* command[^1]. Examples are given throughout the
document, and should always be accompanied by the following `header`
information:

      --header `Content-Type: application/x-www-form-urlencoded'

Example representation process {#example}
==============================

To represent the set of guidelines shown in Figure
[1](#HT){reference-type="ref" reference="HT"} and have TMRweb
successfully leverage TMR to identify the interactions between them, we
first need to define the world knowledge that supports their definition
and allows TMR to operate: the existence of drugs; the concept of a
patient having a certain medical state, such as a blood pressure level;
the concept of altering a person's medical state, such as their blood
pressure; and the effects of taking a drug.

Drugs
-----

To define a drug, we might start by representing a general category of
drugs. Here, we create a dummy category *Thiazide* for *Diuretic*:

    curl --request POST \
      --url https://kclhi.org/tmrweb/drug/category/add \
      --header `Content-Type: application/x-www-form-urlencoded'\
      --data `drug_category_id=Thiazide`

When we define a category, we can also specify which drugs are in this
category (in this case only Thiazide), and general properties of the
drugs in this category, but this falls outside the scope of this
example.

     subsumed_drug_ids=Aspirin%2C%20Ibuprofren&grouping_criteria_ids=SitNonSteroidalDrug%2C%20TrAntinflammatory`

Given this category, we can now define the actual *Diuretic* drug, and
state that it is part of this category. We can also state individual
relationship to other drugs (in this case not one we have priorly
represented):

    curl --request POST \
      --url https://kclhi.org/tmrweb/drug/individual/add \
      --data`drug_id=Thiazide&drug_category_id=Thiazide&subsumed_drug_id=Bendroflumethiazide'

We follow a similar approach for *Diuretic2*, which pertains to the drug
Ibuprofen and in this instance does not have a category:

    curl --request POST \
      --url https://kclhi.org/tmrweb/drug/individual/add \
      --data drug_id=Ibuprofen

We have now defined the drugs shown in Figure
[1](#HT){reference-type="ref" reference="HT"}.

Situation
---------

We next define the situations a patient might find themselves in, in
respect of their vitals, which in the case of *HT* is varying levels of
blood pressure (Figure [1](#HT){reference-type="ref" reference="HT"}).
Specifically we define the state in which a patient has normal blood
pressure, and the state in which a patient has high blood pressure. We
can also add additional information such as the clinical codes that
might be used to reference such states in an EHR:

    curl --request POST \
      --url https://kclhi.org/tmrweb/transition/situation/add \
      --data`situation_id=NormalBP&situation_label=Blood%20pressure%20is%20normal'

    curl --request POST \
      --url https://kclhi.org/tmrweb/transition/situation/add \
      --data`situation_id=HighBP&situation_label=Blood%20pressure%20is%20high&umlsCodes=C0020538%2C%20C3843080'

Transition
----------

We next model transitions between the situations specified previously,
specifically moving between different blood pressure levels, as shown in
Figure [1](#HT){reference-type="ref" reference="HT"}.

    curl --request POST \
      --url https://kclhi.org/tmrweb/transition/add \
      --data`transition_id=IncreaseBP&prior_situation_id=NormalBP&post_situation_id=HighBP'

    curl --request POST \
      --url https://kclhi.org/tmrweb/transition/add \
      --data`transition_id=DecreaseBP&prior_situation_id=HighBP&post_situation_id=NormalBP'

Beliefs
-------

Finally, we combine our drug information (Section
[4.1](#drugs){reference-type="ref" reference="drugs"}) and transition
information (Section [4.3](#transition){reference-type="ref"
reference="transition"}) to construct beliefs about the effects of
administering a drug, for *Diuretic* and *Diuretic2* (Figure
[1](#HT){reference-type="ref" reference="HT"}):

    curl --request POST \
      --url https://kclhi.org/tmrweb/belief/add \
      --data`belief_id=ThiazideBP&drug_cause_id=Thiazide&transition_effect_id=DecreaseBP&strength=L1&frequency=always&author=martin'

    curl --request POST \
      --url https://kclhi.org/tmrweb/belief/add \
      --data`belief_id=IbuprofenBP&drug_cause_id=Ibuprofen&transition_effect_id=IncreaseBP&strength=L1&frequency=always&author=martin'

We can add additional information, such as a code to indicate the
strength of the belief, and how often this belief applies to the
referenced transition.

Guidelines
----------

With all of our background information specified, we can now construct
our actual guidelines, combined in a guideline group, which brings all
of this information together.

First, we supply some details for our guideline group[^2]:

    curl --request POST \
      --url https://kclhi.org/tmrweb/guideline/create \
      --data`guideline_group_id=HT&description=CIG%20for%20hypertension'

Then, we specify information for *Diuretic*, including the remaining
information from Figure [1](#HT){reference-type="ref" reference="HT"},
such as the guideline name, and the nature of the associated
recommendation (should or should not):

    curl --request POST \
      --url https://kclhi.org/tmrweb/guideline/add \
      --data`guideline_group_id=HT&guideline_id=Diuretic&drug_id=Thiazide&belief_id=ThiazideBP&label=Reduce%20blood%20pressure&should_or_shouldnot=should&author=martin'

*Diuretic2* is also defined accordingly:

    curl --request POST \
      --url https://kclhi.org/tmrweb/guideline/add \
      --data`guideline_group_id=HT&guideline_id=Diuretic2&drug_id=Ibuprofen&belief_id=IbuprofenBP&label=Avoid%20high%20blood%20pressure&should_or_shouldnot=should-not&author=martin'

Example interrogation process (single interaction)
==================================================

Now that we have our guideline set represented, we can interrogate it in
order to find any interactions in the following way:

    curl --request POST \
      --url https://kclhi.org/tmrweb/guidelines/interactions \
      --data guideline_group_id=HT

This gives us the following response:

``` {frame="none"}
[interaction(http://anonymous.org/data/ReparableTransitionRecHT-Diuretic2RecHT-Diuretic,Reparable Transition,[http://anonymous.org/data/RecHT-Diuretic,http://anonymous.org/data/RecHT-Diuretic2],[])]
```

The first element of this interaction object is a unique name for the
identified interaction, the next the type of interaction, the third the
fully qualified IDs, and finally any additional external information. We
can see that, as expected, the TMR reasoning engine within TMRweb has
identified the interaction shown within Figure
[1](#HT){reference-type="ref" reference="HT"}.

Using this information, we can learn more about the guidelines involved
in the interaction, and thus re-acquire the information shown in Figure
[1](#HT){reference-type="ref" reference="HT"}, from TMRweb. For example,
we can first ask which drugs each guideline in the interaction relates
to:

    curl --request POST \
      --url https://kclhi.org/tmrweb/guidelines/drug \
      --data `guideline_id=http%3A%2F%2Fanonymous.org%2Fdata%2FRecHT-Diuretic&guideline_group_id=HT'

``` {frame="none"}
http://anonymous.org/data/DrugCatThiazide
```

    curl --request POST \
      --url https://kclhi.org/tmrweb/guidelines/drug \
      --data `guideline_id=http%3A%2F%2Fanonymous.org%2Fdata%2FRecHT-Diuretic2&guideline_group_id=HT'

``` {frame="none"}
http://anonymous.org/data/DrugTIbuprofen
```

We can then ask what the effects of these drugs are, and thus learn why
they are considered alternative actions:

    curl --request POST \
      --url https://kclhi.org/tmrweb/drugs/effects \
      --data drug_full_id=http%3A%2F%2Fanonymous.org%2Fdata%2FDrugCatThiazide

``` {frame="none"}
http://anonymous.org/data/ActAdministerThiazide causes http://anonymous.org/data/TrDecreaseBP
```

    curl --request POST \
      --url https://consultin.hscr.kcl.ac.uk/tmrweb/drugs/effects \
      --data drug_full_id=http%3A%2F%2Fanonymous.org%2Fdata%2FDrugTIbuprofen

``` {frame="none"}
http://anonymous.org/data/ActAdministerIbuprofen causes http://anonymous.org/data/TrIncreaseBP
```

Example interrogation process (multiple interactions)
=====================================================

![Guideline set *HT-OA*, consisting of *Diuretic*, *Diuretic2* and
*Painkiller*.[]{label="HT-OA"}](CIG-HT-OA.png){#HT-OA
width="0.8\linewidth"}

In order to illustrate the identification of multiple guideline
interactions, we add a new guideline, *Painkiller*, shown in Figure
[2](#HT-OA){reference-type="ref" reference="HT-OA"} along with
*Diuretic* and *Diuretic2*. With the addition of this drug, we now also
have a *contradiction* interaction: two guidelines in the same set that
recommend, and do not recommend, the administration of the same drug,
respectively. To model and identify this through TMRweb, first we model
the new transitions and beliefs associated with this guideline
(Ibuprofen is already modelled):

    curl --request POST \
      --url https://kclhi.org/tmrweb/transition/situation/add \
      --data `situation_id=PatientHasNoPain&situation_label=Patient%20has%20no%20pain'

    curl --request POST \
      --url https://kclhi.org/tmrweb/transition/situation/add \
      --data `situation_id=PatientHasPain&situation_label=Patient%20has%20pain&umlsCodes=C0030193'

    curl --request POST \
      --url https://kclhi.org/tmrweb/transition/add \
      --data `transition_id=Painkiller&prior_situation_id=PatientHasPain&post_situation_id=PatientHasNoPain'

    curl --request POST \
      --url https://kclhi.org/tmrweb/belief/add \
      --data `belief_id=IbuprofenPain&drug_cause_id=Ibuprofen&transition_effect_id=Painkiller&strength=L1&frequency=always&author=martin'

Next, we again model *Diuretic* and *Diuretic2* as part of a new
guideline group along with our new guideline *Painkiller*:

    curl --request POST \
      --url https://kclhi.org/tmrweb/guideline/create \
      --data `guideline_group_id=HT-OA&description=CIG%20for%20hypertension%20and%20osteoarthritis'

    curl --request POST \
      --url https://kclhi.org/tmrweb/guideline/add \
      --data `guideline_group_id=HT-OA&guideline_id=Diuretic&drug_id=Thiazide&belief_id=ThiazideBP&label=Reduce%20blood%20pressure&should_or_shouldnot=should&author=martin'

    curl --request POST \
      --url https://kclhi.org/tmrweb/guideline/add \
      --data `guideline_group_id=HT-OA&guideline_id=Diuretic2&drug_id=Ibuprofen&belief_id=IbuprofenBP&label=Avoid%20high%20blood%20pressure&should_or_shouldnot=should-not&author=martin'

    curl --request POST \
      --url https://kclhi.org/tmrweb/guideline/add \
      --data `guideline_group_id=HT-OA&guideline_id=Painkiller&drug_id=Ibuprofen&belief_id=IbuprofenPain&label=Reduce%20pain&should_or_shouldnot=should&author=martin'

We can now interrogate these guidelines in order to identify
interactions:

    curl --request POST \
      --url https://kclhi.org/tmrweb/guidelines/interactions \
      --data guideline_group_id=HT-OA

``` {frame="none"}
[interaction(http://anonymous.org/data/ContradictionRecHT-OA-PainkillerRecHT-OA-Diuretic2,Contradictory Norms,[http://anonymous.org/data/RecHT-OA-Diuretic2,http://anonymous.org/data/RecHT-OA-Painkiller],[]),
interaction(http://anonymous.org/data/ReparableTransitionRecHT-OA-Diuretic2RecHT-OA-Diuretic,Reparable Transition,[http://anonymous.org/data/RecHT-OA-Diuretic,http://anonymous.org/data/RecHT-OA-Diuretic2],[])]
```

This time, we have two interaction objects. This second we have already
seen, but the first now identifies the contradiction shown in Figure
[2](#HT-OA){reference-type="ref" reference="HT-OA"}. From this
information, we can deduce further information such as the fact that
*Painkiller* is, through the fact that *Diuretic2* and *Diuretic* are
alternatives (repairable transitions), also contradictory with
*Diuretic*.

If we identify the drugs involved in this inferred contradiction, then
we can further infer that *Thiazide* (Diuretic) and *Ibuprofen*
(Painkiller) should not be prescribed together.

    curl --request POST \
      --url https://kclhi.org/tmrweb/guidelines/drug \
      --data `guideline_id=http%3A%2F%2Fanonymous.org%2Fdata%2FRecHT-Diuretic&guideline_group_id=HT-OA'

``` {frame="none"}
http://anonymous.org/data/DrugCatThiazide
```

    curl --request POST \
      --url https://kclhi.org/tmrweb/guidelines/drug \
      --data `guideline_id=http%3A%2F%2Fanonymous.org%2Fdata%2FRecHT-OA-Painkiller&guideline_group_id=HT-OA'

``` {frame="none"}
http://anonymous.org/data/DrugTIbuprofen
```

[^1]: *Postman* (<https://www.getpostman.com/>), neatly wraps CURL
    commands.

[^2]: For now, we pre-configure TMRweb with each guideline group ID,
    outside of the REST interface.
