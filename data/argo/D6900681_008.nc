CDF   
   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     
references        (http://www.argodatamgt.org/Documentation   user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      history       X2009-03-26T08:47:05Z creation; 2015-10-19T16:06:19Z last update (coriolis COFC software)   comment_dmqc_operator         DPRIMARY | https://orcid.org/0000-0002-3512-2070 | Saout-Grit, Glazeo      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        =���   axis      Z        L  :�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  =    PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        L  =�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  ?�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  @t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  B�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  E   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  E�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  G�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  H�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  J�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  M   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  M�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  O�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  P�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  R�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    S   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    V   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    Y   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  \   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    \4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    \8   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    \<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    \@   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  \D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    \�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    \�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    \�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         \�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         \�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        \�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    \�Argo profile    3.1 1.2 19500101000000  20090326084705  20200417103158  6900681 BIOArgo                                                         Antoine POTEAU                                                  PRES            TEMP            PSAL               A   IF  10680443                        2C  D   PROVOR_II                       n/a                             n/a                             841 @��-wwww1   @��-wwww@6��$tS��c��g��1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   A   A   A�  B��BffB  B)��B933BBffBNffBY��BfffBs33B�  B�ffB�ffB���B���B�33B�  B�ffB���B���B�33B�  B�33B�ffBә�B���B�33B�ffB뙚B���B�  B�33C��C� C�C
L�CffCffCffCffC��C��C� C"�C&�C(��C+��C.��C2L�C4�3C7��C:  C=� C@  CC33CF� CI�3CLL�CN� CS�CUffCX33C[ffC^L�CaffCdffCg� CkL�Cm� Cp33Cs33CwffCy��C|��C��C�33C�  C�@ C�� C�@ C�@ C�33C�ٚC�@ C��3C���C�ٚC�33C���C���C�33C��3C�Y�C��3C�&fC��3C�Y�C���C��C��fC��3C�� C�@ C���C�33C���C�33C��fC�33C�� C��C��3C�Y�C�� C�33C��fC��C�� C��CĦfC�L�C�� C�� C��3C��C��D L�D�fD@ D�DY�D� D%� D,�D2` D8�fD>� DE�DKY�DQ� DW� D^�DdffDj��Dp� Du` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�  B��BffB  B)��B933BBffBNffBY��BfffBs33B�  B�ffB�ffB���B���B�33B�  B�ffB���B���B�33B�  B�33B�ffBә�B���B�33B�ffB뙚B���B�  B�33C��C� C�C
L�CffCffCffCffC��C��C� C"�C&�C(��C+��C.��C2L�C4�3C7��C:  C=� C@  CC33CF� CI�3CLL�CN� CS�CUffCX33C[ffC^L�CaffCdffCg� CkL�Cm� Cp33Cs33CwffCy��C|��C��C�33C�  C�@ C�� C�@ C�@ C�33C�ٚC�@ C��3C���C�ٚC�33C���C���C�33C��3C�Y�C��3C�&fC��3C�Y�C���C��C��fC��3C�� C�@ C���C�33C���C�33C��fC�33C�� C��C��3C�Y�C�� C�33C��fC��C�� C��CĦfC�L�C�� C�� C��3C��C��D L�D�fD@ D�DY�D� D%� D,�D2` D8�fD>� DE�DKY�DQ� DW� D^�DdffDj��Dp� Du` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Bp�Bp�Bp�Bo�Bp�Bp�Bn�BcTB?}B8RBS�B[#B`BBm�B{�B� B� B|�B�hB�uB��B��B��B��B��B�B�B�B�B�B��B��B�B�'B�3B�3B�?B�FB�LB�LBĜBɺB��B��B��B�B�B�B�B�
B��B�}B�-B��B��B�{B�=B~�BiyBZB=qB=qB;dB/B�BB�#B��BŢB�dB��Bm�B?}B.B�BB
�HB
�wB
�B
��B
�%B
w�B
YB
K�B
2-B
�B	��B	�#B	��B	ɺB	B	�qB	�XB	�B	��B	�B	}�B	y�B	q�B	cTB	R�B	H�B	=qB	1'B	�B	
=B�B�#B��BĜB�qB��B�hB��B�oB�\B�uB�JB�B|�B{�Bx�Bv�Bz�By�Bt�Bk�Bk�B� B��B��BȴB��B�B	 �B	<jB	YB	~�B	��B	��B	�B	�jB	ƨB	��B	��B	��B	�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bp�Bp�Bp�Bo�Bp�Bp�Bn�BcTB?}B8RBS�B[#B`BBm�B{�B� B� B|�B�hB�uB��B��B��B��B��B�B�B�B�B�B��B��B�B�'B�3B�3B�?B�FB�LB�LBĜBɺB��B��B��B�B�B�B�B�
B��B�}B�-B��B��B�{B�=B~�BiyBZB=qB=qB;dB/B�BB�#B��BŢB�dB��Bm�B?}B.B�BB
�HB
�wB
�B
��B
�%B
w�B
YB
K�B
2-B
�B	��B	�#B	��B	ɺB	B	�qB	�XB	�B	��B	�B	}�B	y�B	q�B	cTB	R�B	H�B	=qB	1'B	�B	
=B�B�#B��BĜB�qB��B�hB��B�oB�\B�uB�JB�B|�B{�Bx�Bv�Bz�By�Bt�Bk�Bk�B� B��B��BȴB��B�B	 �B	<jB	YB	~�B	��B	��B	�B	�jB	ƨB	��B	��B	��B	�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
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
<#�
<#�
AхAщ7Aщ7AыDAэPAя\AэPA�\)A�VA�ZA�jA���A��TA��!A�A�M�A�A�A�/A�$�A�-A���A��A��`A�M�A��`A�XA� �A��A�~�A�K�A�$�A��^A�33A��#A��A��hA�Q�A��A�ƨA�n�A��#A�9XA���A��jA�|�A�A��
A�~�A�O�A��`A�`BA��/A�;dA��-A�&�A�1'A��A�XA�"�A��A�p�A��^A�"�A�&�A�~�A��-A�A�1A��FA�\)A�bNA�G�A�ZA�=qA�I�A��RA�z�A�
=A��TA�O�A�S�A�(�A�C�A��A}�;A{�Av �Ap��Am�hAl�/Ak7LAjbNAi�;Ag�;AcƨA_��A^��A^  A\��A[C�AXE�AV�!AT�AR=qAM�AJAG?}AC��A@ �A<�\A9"�A6I�A4ffA3�A2A�A1S�A0��A/�^A+�
A)`BA(bA&E�A%7LA z�AZA	@���@�ƨ@�V@̣�@őh@���@�E�@���@�o@��h@�M�@�ƨ@�hs@�1@��w@�x�@��
@���@�&�@��@�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AхAщ7Aщ7AыDAэPAя\AэPA�\)A�VA�ZA�jA���A��TA��!A�A�M�A�A�A�/A�$�A�-A���A��A��`A�M�A��`A�XA� �A��A�~�A�K�A�$�A��^A�33A��#A��A��hA�Q�A��A�ƨA�n�A��#A�9XA���A��jA�|�A�A��
A�~�A�O�A��`A�`BA��/A�;dA��-A�&�A�1'A��A�XA�"�A��A�p�A��^A�"�A�&�A�~�A��-A�A�1A��FA�\)A�bNA�G�A�ZA�=qA�I�A��RA�z�A�
=A��TA�O�A�S�A�(�A�C�A��A}�;A{�Av �Ap��Am�hAl�/Ak7LAjbNAi�;Ag�;AcƨA_��A^��A^  A\��A[C�AXE�AV�!AT�AR=qAM�AJAG?}AC��A@ �A<�\A9"�A6I�A4ffA3�A2A�A1S�A0��A/�^A+�
A)`BA(bA&E�A%7LA z�AZA	@���@�ƨ@�V@̣�@őh@���@�E�@���@�o@��h@�M�@�ƨ@�hs@�1@��w@�x�@��
@���@�&�@��@�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oPRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 1.1,  -CTD2018V01 & ARGO2018V01 -                                                                                                                                    202004171031582020041710315820200417103158  TC      SCOO1.2                                                                 20091001150954  QC                  G�O�G�O�G�O�                IF  ARGQCOAR1.0                                                                 20111010073746  QCP$                G�O�G�O�G�O�00840           IF  ARGQCOAR1.0                                                                 20111010073746  QCF$                G�O�G�O�G�O�00000           IF      SCOO1.4                                                                 20130109163605  QC                  G�O�G�O�G�O�                        CORA                                                                    20090813215900  SVP                 G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818103651  QCP$TEMP            G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818103518  QCP$TEMP            G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818104546  QCP$PSAL            G�O�G�O�G�O�                IF      COFC2.7                                                                 20151019160619                      G�O�G�O�G�O�                IF  ARSQOW  1.1 CTD2018V01 & ARGO2018V01                                        20200417103158  IP  PSAL            A�  Du` G�O�                