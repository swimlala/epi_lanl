CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   n   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     
references        (http://www.argodatamgt.org/Documentation   user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      history       X2009-03-26T08:51:37Z creation; 2015-10-19T16:06:39Z last update (coriolis COFC software)   comment_dmqc_operator         DPRIMARY | https://orcid.org/0000-0002-3512-2070 | Saout-Grit, Glazeo      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    8   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    8   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8$   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  8,   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  8l   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     9   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     90   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     9P   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9p   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       axis      T      
resolution        ?q   comment_on_resolution         �JULD resolution is 6 minutes, except when JULD = JULD_LOCATION or when JULD = JULD_FIRST_MESSAGE (TRAJ file variable); in that case, JULD resolution is 1 second        9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9|   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >��	4E�        9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  <l   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  <�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  >�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ?   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  Bt   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  B�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  D�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  E   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  F�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  H|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  H�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  J�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  K   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    L�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    O�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    R�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  U�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    V(   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    V,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    V0   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    V4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  V8   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    Vx   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    V�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    V�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         V�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         V�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        V�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    V�Argo profile    3.1 1.2 19500101000000  20090326085137  20200417103158  6900681 BIOArgo                                                         Antoine POTEAU                                                  PRES            TEMP            PSAL               D   IF  10680502                        2C  D   PROVOR_II                       n/a                             n/a                             841 @�K��-�1   @�K��-�@8~!�R�=�c��^5?}1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   A   A   A0  AT��A���A�  A�ffAə�A�ffA�33B
ffB��B!��B,��B9��BF  BR��B]��BhffBu33B���B�  B�  B�  B�  B���B���B���B���B���B���B���B���B�33B�  Bڙ�B���B�  B왚B�  B�33B���CffCffC33CffCffCL�CffCL�C� C��C ��C#�3C&� C)L�C,��C/� C2L�C5� C8� C;ffC>ffCAffCD33CGL�CJffCM� CPffCS33CV33CY� C\ffC_ffCbL�Ce� Ch��Ck��Cn��CqL�Ct� Cw� Cz� C}�C�&fC��fC�@ C��3C�33C���C��C��3C�33C��fC�&fC��3C�33C��fC�@ C��fC��C��3C�&fC���C�33C��3C�&fC��fC�33C���C�33C���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A0  AT��A���A�  A�ffAə�A�ffA�33B
ffB��B!��B,��B9��BF  BR��B]��BhffBu33B���B�  B�  B�  B�  B���B���B���B���B���B���B���B���B�33B�  Bڙ�B���B�  B왚B�  B�33B���CffCffC33CffCffCL�CffCL�C� C��C ��C#�3C&� C)L�C,��C/� C2L�C5� C8� C;ffC>ffCAffCD33CGL�CJffCM� CPffCS33CV33CY� C\ffC_ffCbL�Ce� Ch��Ck��Cn��CqL�Ct� Cw� Cz� C}�C�&fC��fC�@ C��3C�33C���C��C��3C�33C��fC�&fC��3C�33C��fC�@ C��fC��C��3C�&fC���C�33C��3C�&fC��fC�33C���C�33C���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��B49B49B49B8RB>wB@�BA�BA�BB�BC�BC�BC�BD�BD�BD�BD�BD�BE�BE�BE�BE�BF�BF�BF�BG�BG�BG�BI�BP�BYB_;BcTBffBgmBhsBhsB[#B�B�mB�LB�hB�\B�Bl�BffBdZB]/B8RB&�B!�B�B\BB��B�sB�#BB�B��B��Bv�Bm�BdZBC�B.B�B\BB
��B
��B
�B
ŢB
�RB
�!B
��B
��B
��B
��B
��B
�PB
bNB
M�B
A�B
9XB
+B
!�B
�B
oB
B	��B	�B	�fB	��B	ĜB	B	�jB	�B	��B	�7B	z�B	p�B	ffB	\)B	ZB	W
B	P�B	E�B	?}B	8RB	$�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B49B49B49B8RB>wB@�BA�BA�BB�BC�BC�BC�BD�BD�BD�BD�BD�BE�BE�BE�BE�BF�BF�BF�BG�BG�BG�BI�BP�BYB_;BcTBffBgmBhsBhsB[#B�B�mB�LB�hB�\B�Bl�BffBdZB]/B8RB&�B!�B�B\BB��B�sB�#BB�B��B��Bv�Bm�BdZBC�B.B�B\BB
��B
��B
�B
ŢB
�RB
�!B
��B
��B
��B
��B
��B
�PB
bNB
M�B
A�B
9XB
+B
!�B
�B
oB
B	��B	�B	�fB	��B	ĜB	B	�jB	�B	��B	�7B	z�B	p�B	ffB	\)B	ZB	W
B	P�B	E�B	?}B	8RB	$�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
A�`BA�VA�ZA�ZA�^5A�`BA�\)A�VA�S�A�O�A�O�A�M�A�M�A�M�A�I�A�I�A�K�A�M�A�M�A�K�A�K�A�I�A�I�A�I�A�E�A�?}A�9XA�&�A�/A�I�A�K�A�C�A�=qA�/A�+A��A�"�A�-A�$�A���A�JA�bNA��`A�/A���A�33A���A�hsA���A���A���A�VA��A�A��wA�7LA�v�A�(�A�A�v�A�  A�1'A���A���A�O�A��A���A���A�A�A�ƨA��TA��RA�
=A�`BA���A�ĜA���A�n�A�A��\A~�9Ay7LAwx�Aul�As��Aq��Ap�An �Al�Aj^5Ah��Af�Ad�Ac?}AbVA`�jA^{A[�AY&�AV��AU33AS+AQ�AQoAOt�AM7LAL�+AK7LAI\)AG�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�`BA�VA�ZA�ZA�^5A�`BA�\)A�VA�S�A�O�A�O�A�M�A�M�A�M�A�I�A�I�A�K�A�M�A�M�A�K�A�K�A�I�A�I�A�I�A�E�A�?}A�9XA�&�A�/A�I�A�K�A�C�A�=qA�/A�+A��A�"�A�-A�$�A���A�JA�bNA��`A�/A���A�33A���A�hsA���A���A���A�VA��A�A��wA�7LA�v�A�(�A�A�v�A�  A�1'A���A���A�O�A��A���A���A�A�A�ƨA��TA��RA�
=A�`BA���A�ĜA���A�n�A�A��\A~�9Ay7LAwx�Aul�As��Aq��Ap�An �Al�Aj^5Ah��Af�Ad�Ac?}AbVA`�jA^{A[�AY&�AV��AU33AS+AQ�AQoAOt�AM7LAL�+AK7LAI\)AG�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oPRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 1.1,  -CTD2018V01 & ARGO2018V01 -                                                                                                                                    202004171031582020041710315820200417103158  IF  CODMCOOA5.1 DMQCGL01                                                        20091026171322  QCP$TEMP            G�O�G�O�G�O�                IF  ARGQSCOO1.2                                                                 20091001150901  CF  TEMP            A0  C���@@                  IF  CODMCOOA5.1 DMQCGL01                                                        20091026173053  QCP$PSAL            G�O�G�O�G�O�                IF  ARGQCOAR1.0                                                                 20111010073858  QCP$                G�O�G�O�G�O�00840           IF  ARGQCOAR1.0                                                                 20111010073858  QCF$                G�O�G�O�G�O�00000           IF      SCOO1.4                                                                 20130109163602  QC                  G�O�G�O�G�O�                        CORA                                                                    20101008173409  SVP                 G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818104305  QCF$TEMP            D/  Dz  G�O�6               IF  CODMCOOA6.2 DMQCGL01                                                        20140818105436  QCF$PSAL            G�O�G�O�G�O�4               IF  CODMCOOA6.2 DMQCGL01                                                        20140818104143  QCF$TEMP            G�O�G�O�G�O�4               IF  CODMCOOA6.2 DMQCGL01                                                        20140818104219  QCF$TEMP            D/  Dz  G�O�6               IF      COFC2.7                                                                 20151019160639                      G�O�G�O�G�O�                IF  ARSQOW  1.1 CTD2018V01 & ARGO2018V01                                        20200417103158  IP  PSAL            A0  C���G�O�                