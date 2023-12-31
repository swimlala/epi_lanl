CDF   
   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     
references        (http://www.argodatamgt.org/Documentation   user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      history       X2009-03-26T08:49:40Z creation; 2015-10-19T16:05:16Z last update (coriolis COFC software)   comment_dmqc_operator         DPRIMARY | https://orcid.org/0000-0002-3512-2070 | Saout-Grit, Glazeo      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        =���   axis      Z        D  :�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  <�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        D  =�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  ?�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     D  @d   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  B�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  D�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  E�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  G�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  HX   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  J�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  L�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  Mt   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  O�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  PL   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  R�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    R�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    U�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    X�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  [�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    [�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    [�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    [�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    [�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  [�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    \<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    \L   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    \P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         \`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         \d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        \h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    \lArgo profile    3.1 1.2 19500101000000  20090326084940  20200417102504  6900680 BIOArgo                                                         Antoine POTEAU                                                  PSAL            PRES            TEMP               	A   IF  10680030                        2C  D   PROVOR_II                       n/a                             n/a                             841 @��l����1   @��l����@6zW���'�c�Ov_�1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   A   A   BffBffB)��B6��BA��BL  BX  Bd  Bp  B|ffB�ffB���B�ffB���B���B�ffB�33B���B�  B�  B�  B�  B�33B���B�ffB�  B䙚B�ffB�33B�33B�  CffCL�C  C33C� C��C�3C33C�C� C�3C"ffC%� C)�3C.ffC1�3C4� C733C:33C=L�C@� CCffCF�CIffCL�3CO33CR� CU  CXffC[33C^��Ca�3CdffCg33Cj�3Cm33Cp� Cs�Cv� CyL�C|�C~�fC�s3C�ٚC�@ C��fC���C��fC�L�C��fC�  C�� C��C���C��C�L�C�Y�C��fC��C���C���C�ٚC�L�C�ٚC���C�� C�33C�s3C�L�C��3C�33C���C��C���C�  C��3C�ffC���C�@ C��3C�&fC���C�&fC���C�33C��fC��CČ�C�  Cǌ�C�L�C��fC��C�33D �fD� DٚD3DFfD�fD%� D,3D2L�D8�fD>� DE  DKffDQ�fDW�3D^,�DdY�Dj�fDpٚDv�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BffBffB)��B6��BA��BL  BX  Bd  Bp  B|ffB�ffB���B�ffB���B���B�ffB�33B���B�  B�  B�  B�  B�33B���B�ffB�  B䙚B�ffB�33B�33B�  CffCL�C  C33C� C��C�3C33C�C� C�3C"ffC%� C)�3C.ffC1�3C4� C733C:33C=L�C@� CCffCF�CIffCL�3CO33CR� CU  CXffC[33C^��Ca�3CdffCg33Cj�3Cm33Cp� Cs�Cv� CyL�C|�C~�fC�s3C�ٚC�@ C��fC���C��fC�L�C��fC�  C�� C��C���C��C�L�C�Y�C��fC��C���C���C�ٚC�L�C�ٚC���C�� C�33C�s3C�L�C��3C�33C���C��C���C�  C��3C�ffC���C�@ C��3C�&fC���C�&fC���C�33C��fC��CČ�C�  Cǌ�C�L�C��fC��C�33D �fD� DٚD3DFfD�fD%� D,3D2L�D8�fD>� DE  DKffDQ�fDW�3D^,�DdY�Dj�fDpٚDv�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Bv�Bw�Bv�Bu�Br�Bl�BcTBW
B8RB/B6FB<jBR�BdZBm�Bw�B~�B�B�PB�VB�uB��B��B��B��B��B��B��B�B�B�!B�'B�LB�FB�dB�dB�jB�LB��B��B��B��B��B��B�VB�Bz�Bs�B`BB?}B)�B#�B�BB��B�fB�B�BǮB�qB�'B��B�oB�DBv�BT�B.B
��B
�)B
ŢB
�JB
v�B
O�B
{B
B	�TB	��B	��B	�hB	�%B	r�B	[#B	33B	�B��B�B�B�ZB�BB�BɺBÖB�wB�?B�B��B��B��B��B��B��B�VB�PB�PB�7B�B�B�B�B~�B|�B|�B~�B�B�B�B�B�B~�Bt�Bu�Bv�Bk�BbNBgmBk�B�%B��B��B��B	oB	�B	1'B	E�B	P�B	aHB	s�B	�B	�{B	��B	�?B	B	��B	�B	�51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bv�Bw�Bv�Bu�Br�Bl�BcTBW
B8RB/B6FB<jBR�BdZBm�Bw�B~�B�B�PB�VB�uB��B��B��B��B��B��B��B�B�B�!B�'B�LB�FB�dB�dB�jB�LB��B��B��B��B��B��B�VB�Bz�Bs�B`BB?}B)�B#�B�BB��B�fB�B�BǮB�qB�'B��B�oB�DBv�BT�B.B
��B
�)B
ŢB
�JB
v�B
O�B
{B
B	�TB	��B	��B	�hB	�%B	r�B	[#B	33B	�B��B�B�B�ZB�BB�BɺBÖB�wB�?B�B��B��B��B��B��B��B�VB�PB�PB�7B�B�B�B�B~�B|�B|�B~�B�B�B�B�B�B~�Bt�Bu�Bv�Bk�BbNBgmBk�B�%B��B��B��B	oB	�B	1'B	E�B	P�B	aHB	s�B	�B	�{B	��B	�?B	B	��B	�B	�51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
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
A��yA��`A��;A�ĜAыDA�9XA��#A���A���A�t�A���A�"�A��+A���A�v�A��FA���A�9XA�M�A���A� �A�?}A���A���A�A�A���A��FA��A�r�A�/A��A���A��A���A�bA��A�ȴA�1'A�(�A��A��HA�|�A���A�  A�\)A�M�A���A�5?A��#A�A�1'A�t�A��A���A��#A���A���A�jA�K�A��HA�JA�A�ȴA�M�A��#A�(�A��;A��A�~�A�ZA�5?A��9A��`A|�Az  AuO�As��AmhsAi�-Ah=qAe��AbjA]33AY&�AS�AP��AP^5AMAL��AK�
AH��AG�AFbAC�
AAA?�TA=��A<�+A<�A;�FA9�A5hsA3�hA3�A2��A0�!A.��A-33A,A)XA'�A'dZA&��A&ZA%
=A"(�A!�A ��A I�A/A�uA�!Al�@�E�@�7L@�~�@�I�@�Q�@��\@��@��@��`@�O�@��y@�?}@��y@�I�@�M�@�  @�hs@���@�(�@�X@��@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��yA��`A��;A�ĜAыDA�9XA��#A���A���A�t�A���A�"�A��+A���A�v�A��FA���A�9XA�M�A���A� �A�?}A���A���A�A�A���A��FA��A�r�A�/A��A���A��A���A�bA��A�ȴA�1'A�(�A��A��HA�|�A���A�  A�\)A�M�A���A�5?A��#A�A�1'A�t�A��A���A��#A���A���A�jA�K�A��HA�JA�A�ȴA�M�A��#A�(�A��;A��A�~�A�ZA�5?A��9A��`A|�Az  AuO�As��AmhsAi�-Ah=qAe��AbjA]33AY&�AS�AP��AP^5AMAL��AK�
AH��AG�AFbAC�
AAA?�TA=��A<�+A<�A;�FA9�A5hsA3�hA3�A2��A0�!A.��A-33A,A)XA'�A'dZA&��A&ZA%
=A"(�A!�A ��A I�A/A�uA�!Al�@�E�@�7L@�~�@�I�@�Q�@��\@��@��@��`@�O�@��y@�?}@��y@�I�@�M�@�  @�hs@���@�(�@�X@��@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oPSAL            PRES            TEMP            PSAL_ADJUSTED = PSAL                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustement was necessary. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 1.1,  -CTD2018V01 & ARGO2018V01 -                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              202004171025042020041710250420200417102504  TC      SCOO1.2                                                                 20091001150556  QC                  G�O�G�O�G�O�                IF  ARGQCOAR1.0                                                                 20111010073510  QCP$                G�O�G�O�G�O�00840           IF  ARGQCOAR1.0                                                                 20111010073510  QCF$                G�O�G�O�G�O�00000           IF      SCOO1.4                                                                 20130109162451  QC                  G�O�G�O�G�O�                        CORA                                                                    20090814143020  SVP                 G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818103651  QCF$TEMP            D/  Dz  G�O�6               IF  CODMCOOA6.2 DMQCGL01                                                        20140818103518  QCF$TEMP            D/  Dz  G�O�6               IF  CODMCOOA6.2 DMQCGL01                                                        20140818104546  QCP$PSAL            G�O�G�O�G�O�                IF      COFC2.7                                                                 20151019160516                      G�O�G�O�G�O�                IF  ARSQOW  1.1 CTD2018V01 & ARGO2018V01                                        20200417102504  IP  PSAL            BffDv�3G�O�                