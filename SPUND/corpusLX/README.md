# R corpus linguistics essais
## 14525.notes
### content
the directory contains essais to a collocate analysis of a corpus of nouns/adjectives assembled. 
we try to assign a noun to a category of a set of categories. 92 out of a set of 918 nouns (the corpus) were assigned manually to one of 9 categories according to (Petterson-Traba, 2021)[^1] and 3 additional defined.

#### categories

| ***Category***                      | ***Label*** | ***Examples***                                                                                                        |
| ----------------------------------- | ----------- | --------------------------------------------------------------------------------------------------------------------- |
| food & drink                        | F&D         | **apple, beverage, bread, chicken, coffee, cup, drink, food, fruit, liquid, meal, omelet, rice, spice, tea, wine**    |
| plants & flowers                    | P&F         | **bloom, blossom, bower, flower, garden, geranium, grass, herb, lip\*, leaf, petal, pine, rose, shrub, vine, violet** |
| earth                               | EA          | **breeze, brook, dew, flood, gale, grove, hill, sea, vale, valley, wind**                                             |
| body                                | BO          | **arm, breath\*, cheek, face, flesh, hair, hand, head, limb, lip\*, lock, mouth, shoulder, skin, wrist**              |
| matter                              | MA          | **air, atmosphere, candle, cloud\*, dust, fume, gas, oil\*, night, smoke, steam, vapor**                              |
| sensation                           | SE          | **aroma, breath\*, flavor, incense, scent, smell, odor, taste**                                                       |
| aesthetics                          | AE          | **cologne, cosmetics, cream, oil\*, ointment, powder, talcum, wax**                                                   |
| cleaning                            | CL          | **deodorant, dish-water, disinfectant, napkin, soap, soap-powder, sponge, spray, suds, tissue, wash-ball**            |
| textile & clothing                  | T&C         | **blanket, cambric, cloth, dress, flannel, garment, glove, lace, linen, pillow, robe, sheet, shirt, silk**            |
| buildings & artificial environments | B&AE        |                                                                                                                       |
| humans                              | PE          |                                                                                                                       |
| animals                             | AN          |                                                                                                                       |
| abstract concepts                   | AC          |                                                                                                                       |


#### scripts
- [collocation-essai.R](collocation-essai.R): first essai of assigning categories on base of a collocational analysis
- [get-cats-from-training\_simple.R](get-cats-from-training_simple.R): cleaned up first script
- [random-factor\_effects-model.R](random-factor_effects-model.R): essai of modelling a feedback loop in above script
- [getcats\_model.R](getcats_model.R): apply the algorithm to a model set of words/categegories to prove certainty

#### premises
the major premise was, that the noun collocates among the categories share common associates. this would result in similar category assigments for nouns with lots of similar collocates. this hypothesis is to be tested. in the model we proved that the algorithm for assigning the correct category under common collocates is working, the recognition errorrate is 0%. theres ambiguos cases for which two assignments are possible, these have two correct assignments as well.   
above shows, that the simple computation of most-common collocates (cf. Sariyar, Murat, and Andreas Borg 2010)[^2] works fine.

#### evaluation
the script works with 100% correct assignment of the predefined categories and a 22% assignment of not known nouns. theres some inconsistencies in the training (92 predefined nouns) and gold (918 defined nouns) set where some assignments vary from training to gold set and some duplicate nouns in the gold set were assigned to different categories.  
after refining train and gold set and re-applying the first approach (simple match array frequencies [matches of testarray in train array / length(trainarray+testarray)] without the recordlinkage package) assignment proof rate vs goldset is 44% in unknown nouns.

[^1]:	Pettersson-Traba, Daniela. “A Diachronic Perspective on Near-Synonymy: The Concept of Sweet-Smelling in American English.” Corpus linguistics and linguistic theory 17.2 (2021): 319–349. [Web.](https://fu-berlin.primo.exlibrisgroup.com/permalink/49KOBV_FUB/5ami3a/cdi_openaire_primary_doi_8ad1d6d6057eb106694cfa96c9ff4fa3)
[^2]: Sariyar, Murat, and Andreas Borg. “The RecordLinkage Package: Detecting Errors in Data.” The R journal 2.2 (2010): 61–67. [Web.](https://fu-berlin.primo.exlibrisgroup.com/permalink/49KOBV_FUB/5ami3a/cdi_openaire_primary_doi_ba8b716343e3913e3b82aa41329d8948)