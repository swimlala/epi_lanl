CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:05Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               4A   AO  20111130140248  20190522121826  1727_5046_052                   2C  D   APEX                            2143                            040306                          846 @�X�׿�1   @�Xݻ���@7"M����c�bM��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A>ffA`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DU��DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Day�Db  Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� DrfDr�fDs  Ds� Dz&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @s33@�33@�33A��A8  AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq�3Cs�3Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC�ٚC�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDl�D�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD` D�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0��D1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU� DVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDa` Da�fDbffDb�fDcffDc�fDdffDd� DeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq��Drl�Dr�fDsffDz�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�~�A�~�A�~�A�t�A�t�A��A�z�A��7A��DA��A��A��+A��+A��+A��+A��DA��DA��7A��\A��\A��DA��DA��PA��A�jA�\)A�Q�A�=qA�(�A�%A���A���A���A���A���A���A���A���A��A��A�`BA���A�ffA�33A�?}A��;A��FA�A��TA��A�bA��A�1'A�=qA�;dA���A���A�"�A���A�I�A��A�XA���A���A��PA���A�ȴA��A��/A�l�A��7A�  A���A�l�A�`BA��A���A�33A���A�~�A�E�A���A�M�A�33A���A��A�;dA��-A���A�\)A�|�A�  A�bA��A��A�jA�K�A�-A�bA�A���A��yA��uA�~�A���A��HA�+A�%A��mA��A��A�ȴA�~�A�ZA�Q�A�M�A���A�G�A�
=A�?}A�A�&�A�-A}O�A|  Az  Aut�Ar�Ap��Am�Ak�Aj��Ah�AghsAd��Ac�#AcK�Ab��Aa��AaS�Aa/A`ffA^  A]�A]�A]"�A\��A\1'A[ƨAZ�AY��AW�^AU+AT�+AT �AS�AS�PAS
=AR�AQG�AP��AP�!AP �AN�+AL�AI;dAE\)AC��AA\)A?\)A=ƨA<��A<jA;t�A:�!A933A7��A6�uA5�#A5;dA4z�A2�HA1�TA0E�A/dZA.  A-XA,�jA,ffA+XA*ĜA)��A(�+A(1A'�A&��A&ĜA&9XA%��A%�hA$�A#�TA#;dA"�\A!�mA!�^A!O�A v�AȴAA��A�/A%A/AbNAC�A �A��A1'A1A
=AS�Az�A��A��A�A��A�jA
�yA	��A	��A��A5?A��A
=A  AC�A�TAx�AQ�AdZA �D@���@��@�@�  @��#@���@�R@�7@�j@�R@�hs@�bN@��@��m@�(�@�@��@�I�@���@�t�@�o@ޏ\@���@�A�@۝�@ڏ\@�+@���@�C�@җ�@���@�@�r�@���@���@��`@��@�C�@�M�@š�@ċD@öF@�o@°!@�n�@���@�V@�9X@���@�{@��D@�Z@��@���@�|�@�+@���@���@��D@��R@��@���@�bN@�n�@���@�v�@�V@�@�&�@�j@��@�l�@���@��\@���@��9@�  @�C�@�ȴ@�~�@�-@�V@��u@��m@�ȴ@���@���@�x�@�7L@���@�Ĝ@�  @�|�@�"�@�S�@��@�M�@��@���@��h@��@�1'@���@��+@�-@���@�@�hs@��`@���@���@�Q�@�  @��;@���@���@�l�@�S�@�C�@�;d@�"�@�ȴ@��\@��+@�~�@�n�@�^5@�^5@�^5@�^5@�^5@�ff@�E�@�-@�{@�J@�@���@�@���@���@�p�@�/@��`@�z�@��@���@��@��H@��\@�5?@��@���@�&�@��`@�Ĝ@��9@��@�z�@�Z@���@���@��P@�l�@�
=@�V@�n�@�-@�@���@���@���@�&�@��`@���@���@��D@�z�@�z�@�A�@��m@��F@�ƨ@���@���@��@�S�@�33@��R@��\@�$�@��#@��-@��h@��@�hs@�X@�?}@�7L@�r�@���@��w@��F@���@�\)@�C�@��@���@��@�
=@��@�o@�;d@���@��P@��+@�=q@�~�@���@��\@�x�@�z�@�"�@�o@��+@�M�@�{@��#@��7@�`B@�x�@�J@��\@��y@�
=@��@�~�@���@��@��@~�@}�T@}?}@{�
@yX1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�~�A�~�A�~�A�t�A�t�A��A�z�A��7A��DA��A��A��+A��+A��+A��+A��DA��DA��7A��\A��\A��DA��DA��PA��A�jA�\)A�Q�A�=qA�(�A�%A���A���A���A���A���A���A���A���A��A��A�`BA���A�ffA�33A�?}A��;A��FA�A��TA��A�bA��A�1'A�=qA�;dA���A���A�"�A���A�I�A��A�XA���A���A��PA���A�ȴA��A��/A�l�A��7A�  A���A�l�A�`BA��A���A�33A���A�~�A�E�A���A�M�A�33A���A��A�;dA��-A���A�\)A�|�A�  A�bA��A��A�jA�K�A�-A�bA�A���A��yA��uA�~�A���A��HA�+A�%A��mA��A��A�ȴA�~�A�ZA�Q�A�M�A���A�G�A�
=A�?}A�A�&�A�-A}O�A|  Az  Aut�Ar�Ap��Am�Ak�Aj��Ah�AghsAd��Ac�#AcK�Ab��Aa��AaS�Aa/A`ffA^  A]�A]�A]"�A\��A\1'A[ƨAZ�AY��AW�^AU+AT�+AT �AS�AS�PAS
=AR�AQG�AP��AP�!AP �AN�+AL�AI;dAE\)AC��AA\)A?\)A=ƨA<��A<jA;t�A:�!A933A7��A6�uA5�#A5;dA4z�A2�HA1�TA0E�A/dZA.  A-XA,�jA,ffA+XA*ĜA)��A(�+A(1A'�A&��A&ĜA&9XA%��A%�hA$�A#�TA#;dA"�\A!�mA!�^A!O�A v�AȴAA��A�/A%A/AbNAC�A �A��A1'A1A
=AS�Az�A��A��A�A��A�jA
�yA	��A	��A��A5?A��A
=A  AC�A�TAx�AQ�AdZA �D@���@��@�@�  @��#@���@�R@�7@�j@�R@�hs@�bN@��@��m@�(�@�@��@�I�@���@�t�@�o@ޏ\@���@�A�@۝�@ڏ\@�+@���@�C�@җ�@���@�@�r�@���@���@��`@��@�C�@�M�@š�@ċD@öF@�o@°!@�n�@���@�V@�9X@���@�{@��D@�Z@��@���@�|�@�+@���@���@��D@��R@��@���@�bN@�n�@���@�v�@�V@�@�&�@�j@��@�l�@���@��\@���@��9@�  @�C�@�ȴ@�~�@�-@�V@��u@��m@�ȴ@���@���@�x�@�7L@���@�Ĝ@�  @�|�@�"�@�S�@��@�M�@��@���@��h@��@�1'@���@��+@�-@���@�@�hs@��`@���@���@�Q�@�  @��;@���@���@�l�@�S�@�C�@�;d@�"�@�ȴ@��\@��+@�~�@�n�@�^5@�^5@�^5@�^5@�^5@�ff@�E�@�-@�{@�J@�@���@�@���@���@�p�@�/@��`@�z�@��@���@��@��H@��\@�5?@��@���@�&�@��`@�Ĝ@��9@��@�z�@�Z@���@���@��P@�l�@�
=@�V@�n�@�-@�@���@���@���@�&�@��`@���@���@��D@�z�@�z�@�A�@��m@��F@�ƨ@���@���@��@�S�@�33@��R@��\@�$�@��#@��-@��h@��@�hs@�X@�?}@�7L@�r�@���@��w@��F@���@�\)@�C�@��@���@��@�
=@��@�o@�;d@���@��P@��+@�=q@�~�@���@��\@�x�@�z�@�"�@�o@��+@�M�@�{@��#@��7@�`B@�x�@�J@��\@��y@�
=@��@�~�@���@��@��@~�@}�T@}?}@{�
@yX1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB_;B`BB`BB`BBaHBaHB_;B`BB`BB`BB_;B_;B_;B_;B_;B`BB`BB`BBaHBaHBdZBk�BjB~�B�PB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�?B��B�#B�#B�B��B��B��B��B��BǮBB�qB�XB�FB�?B�9B�'B�'B�'B��B��B��B�oB�PB�1B�B�Bw�Bp�Bm�BiyBgmBdZB`BBS�B?}B7LB.B+B&�B�BuBhBVB+B  B��B�B�`B�/B��BÖB�?B�B�B�B�B��B��B��B��B~�B\)BG�B?}B<jB9XB0!B,B&�BoB
��B
�B
�5B
��B
��B
�}B
�?B
�B
��B
w�B
XB
N�B
@�B
-B
�B
\B
B	��B	�B	�fB	�TB	�#B	�;B	�/B	�B	��B	��B	��B	��B	�}B	��B	��B	�}B	�qB	�^B	�LB	�'B	��B	��B	�DB	�+B	�B	�B	�B	|�B	w�B	v�B	u�B	s�B	p�B	k�B	\)B	@�B	$�B	�B	VB	B��B�B�B�B�mB�TB�HB�5B�#B�B��B��B��BŢB��B�qB�^B�XB�FB�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�hB�bB�VB�=B�B�B{�By�Bv�Bt�Bt�Br�Bq�BjBhsBe`BcTBcTBbNB_;B\)B[#BXBXBW
BT�BQ�BO�BL�BJ�BG�BD�BC�B@�B>wB<jB9XB7LB6FB5?B49B33B2-B1'B/B.B0!B.B/B1'B1'B2-B2-B2-B2-B2-B1'B2-B1'B/B/B0!B2-B1'B1'B2-B6FB5?B5?B8RB8RB9XB;dB;dB;dB<jB>wB@�BB�BC�BC�BB�BB�BB�BH�BJ�BK�BK�BK�BL�BM�BO�BN�BR�B^5BbNBaHBZBVBYB[#B[#B\)B^5B^5BaHBcTBffBm�Bx�Bz�B|�B}�B}�B~�B�B�B�+B�bB��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�FB�FB�FB�FB�XB�jB�qBBƨB��B��B��B��B�
B�B�B�5B�;B�NB�`B�mB�sB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	%B	VB	{B	�B	 �B	%�B	(�B	,B	1'B	2-B	6FB	7LB	9XB	?}B	B�B	D�B	D�B	G�B	G�B	H�B	L�B	O�B	P�B	R�B	W
B	[#B	_;B	bNB	cTB	e`B	e`B	ffB	jB	m�B	n�B	p�B	q�B	r�B	r�B	t�B	v�B	v�B	x�B	y�B	{�B	|�B	}�B	}�B	�B	�B	�%B	�1B	�1B	�1B	�7B	�7B	�7B	�=B	�DB	�DB	�PB	�PB	�PB	�PB	�\B	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�3B	�9B	�9B	�FB	�XB	�qB	ÖB	ŢB	ƨB	ƨB	ŢB	ÖB	ÖB	ĜB	B	ƨB	ƨB	ǮB	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B_;B`BB`BB`BBaHBaHB_;B`BB`BB`BB_;B_;B_;B_;B_;B`BB`BB`BBaHBaHBdZBk�BjB� B�PB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�^B��B�;B�/B�/B�
B��B��B��B��B��BƨBB�jB�dB�RB�FB�9B�3B�?B�3B��B��B��B�uB�JB�%B�=B}�Bs�Bo�BjBhsBgmBgmB[#BE�B9XB/B-B)�B"�B{BoBhB	7BB��B�B�sB�NB��BɺB�RB�!B�B�B�B��B��B��B��B�bBgmBK�B@�B=qB>wB1'B.B0!B �B
��B
�B
�NB
�B
�
B
ĜB
�RB
�-B
�B
�B
[#B
S�B
K�B
49B
!�B
�B
1B	��B	�B	�B	�B	�5B	�HB	�;B	�#B	��B	��B	��B	��B	B	��B	��B	��B	�}B	�jB	�^B	�FB	�B	��B	�PB	�1B	�%B	�B	�B	~�B	y�B	w�B	v�B	u�B	t�B	p�B	e`B	J�B	)�B	"�B	{B	%B��B�B�B�B�B�sB�ZB�BB�/B�#B�B��B��BȴBƨB�}B�jB�dB�^B�?B�9B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�\B�7B�B~�B|�By�Bv�Bu�Bu�Bv�Bm�Bl�BiyBe`BdZBcTBdZB_;B]/B[#BZBYBXBVBR�BQ�BL�BK�BG�BF�BD�B@�BA�B>wB;dB9XB8RB6FB5?B5?B49B1'B2-B49B5?B1'B33B49B33B33B33B33B49B49B49B33B/B33B33B33B2-B5?B5?B7LB7LB8RB9XB9XB;dB<jB=qB=qB=qB?}BA�BC�BD�BE�BE�BC�BE�BI�BK�BL�BL�BL�BM�BO�BQ�BQ�BR�B_;BffBdZB^5BXBZB\)B]/B^5B_;B_;BbNBdZBffBo�Bz�B|�B}�B~�B~�B�B�B�B�7B�hB��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�RB�LB�LB�RB�^B�qB�wBÖBǮB��B��B��B��B�B�B�#B�;B�;B�NB�`B�sB�yB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	+B	\B	�B	�B	!�B	&�B	)�B	-B	2-B	33B	7LB	8RB	:^B	@�B	B�B	D�B	E�B	G�B	G�B	I�B	M�B	O�B	Q�B	S�B	XB	[#B	`BB	cTB	dZB	ffB	e`B	gmB	k�B	m�B	o�B	p�B	q�B	r�B	s�B	u�B	w�B	v�B	x�B	z�B	{�B	}�B	~�B	~�B	�B	�B	�+B	�7B	�7B	�1B	�7B	�7B	�7B	�=B	�PB	�JB	�PB	�PB	�PB	�VB	�bB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�3B	�9B	�?B	�?B	�FB	�RB	�jB	ÖB	ŢB	ƨB	ǮB	ƨB	ŢB	ĜB	ƨB	B	ƨB	ǮB	ǮB	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<D��<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446522012010314465220120103144652  AO  ARGQ                                                                        20111130140248  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140248  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144652  IP                  G�O�G�O�G�O�                