CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:30Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142325  20190522121827  1727_5046_139                   2C  D   APEX                            2143                            040306                          846 @��Q(3��1   @��Q�b�@7������c�� ě�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D��D� D	  D	� D
  D
� D  D� D  D� D  Dy�D��D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D.��D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dss3Dy,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0  B8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C 33C"�C$�C&�C(33C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD��DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD  D�fD	fD	�fD
fD
�fDfD�fDfD�fDfD� D  D�fD�D�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/  D/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDe�De�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDsy�Dy331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��#A�5?A�
=A���A���A�M�A�&�A���A��#A���A���A�ƨA��-A��uA��PA��7A��+A�|�A�I�A�{A��A��`A��/A��/A��HA��`A��;A���A���A���A���A��
A��;A��/A���A��
A���A��9A���A��\A�M�A�1A���A���A��#A�oA�1A�A���A��uA���A��A��A�C�A��RA�z�A�\)A�E�A�$�A��`A��^A��\A�z�A���A��RA��-A��\A� �A�ĜA�M�A��A���A�r�A��A��PA�"�A��!A���A��
A�bA�"�A���A���A�C�A��RA�&�A���A��+A�bNA�JA�?}A�?}A�ƨA��A�bA�A�A��^A��9A���A�ffA���A�A��A�A�|�A�\)A�bNA��A��#A�%A��A�9XA�p�A�5?A���A���A��A��wA��hA�(�A��RA�$�A��A��wA��A��hA�dZA��A~�jA}%A{;dAz��Az9XAxA�At�`As�hAsK�As�Ar9XAq33ApI�AnQ�AmG�Al�uAjĜAh�Ag33Ae�hAd=qAc�Ac�Aa�A`$�A_"�A^�A]S�AZ5?AWx�AWoAV��AV(�AU�FAU7LAT�AS�^AR�AQ�AO�#AM�7ALAHn�AF�AD5?AB�`AA�FA@VA>��A=�7A=33A=C�A=C�A=�A=%A<��A:1A7��A6VA5�-A5�A4ZA1�;A1��A1�hA1�A.�A. �A-�hA,�!A+"�A*(�A(��A(�A'|�A&�A%��A$�A$A"�9A!�A!x�A ~�A�wAt�A��AJA
=A�mAp�Ar�A�-A�hA�PAx�A��A�^AG�A�A��A%AbA��A7LA�+A=qAJA�7A�yAQ�AAA�AK�A��AO�A
�!A	��A	%AM�A�hA�RA�AG�AE�AƨAoA�uA�-A �A v�@��@�9X@�x�@�Z@�\)@�;d@���@�G�@�1@���@�@��@�(�@��@�K�@���@�@���@�-@�@���@�&�@� �@�@�l�@���@��@��#@�7L@��@�b@ߕ�@�C�@���@�"�@���@�V@׶F@��H@�5?@�(�@��@�n�@�?}@�Q�@�ff@�j@˝�@�O�@�;d@�5?@�@��@��@��m@�S�@�@�/@���@�b@�t�@��H@��#@�&�@�bN@���@�|�@�
=@���@�^5@�-@��@�1'@��@��@��@���@�j@�Z@��w@���@�|�@���@��+@��-@�Ĝ@� �@�ƨ@�\)@���@��T@�A�@��y@��@���@�hs@��@���@���@��@�bN@�1'@��
@�l�@�5?@�7L@���@�b@�o@�^5@��h@�X@��/@��@�Q�@�r�@�b@�S�@���@��@���@��+@�$�@��T@��h@��@��j@��@��@�C�@���@�{@��#@���@��@��j@�r�@�A�@��@��@��P@���@�ff@�E�@��@��T@�@���@��7@�G�@�%@��/@��@�bN@�1@��y@�@��#@�x�@�7L@�&�@��/@�z�@� �@��@�|�@��@�bN@�Ĝ@�hs@���@���@�x�@�x�@�hs@�O�@�V@��@�I�@���@�b@��
@��@���@�5?@�5?@�J@���@���@��@�J@���@��@��@��@��@�x�@��@��/@��@�9X@� �@��m@���@��F@��@��P@�|�@�S�@��@��H@��y@���@�+@�dZ@�33@�$�@���@���@�`B@��@���@���@��@�9X@��@��@�ƨ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��#A�5?A�
=A���A���A�M�A�&�A���A��#A���A���A�ƨA��-A��uA��PA��7A��+A�|�A�I�A�{A��A��`A��/A��/A��HA��`A��;A���A���A���A���A��
A��;A��/A���A��
A���A��9A���A��\A�M�A�1A���A���A��#A�oA�1A�A���A��uA���A��A��A�C�A��RA�z�A�\)A�E�A�$�A��`A��^A��\A�z�A���A��RA��-A��\A� �A�ĜA�M�A��A���A�r�A��A��PA�"�A��!A���A��
A�bA�"�A���A���A�C�A��RA�&�A���A��+A�bNA�JA�?}A�?}A�ƨA��A�bA�A�A��^A��9A���A�ffA���A�A��A�A�|�A�\)A�bNA��A��#A�%A��A�9XA�p�A�5?A���A���A��A��wA��hA�(�A��RA�$�A��A��wA��A��hA�dZA��A~�jA}%A{;dAz��Az9XAxA�At�`As�hAsK�As�Ar9XAq33ApI�AnQ�AmG�Al�uAjĜAh�Ag33Ae�hAd=qAc�Ac�Aa�A`$�A_"�A^�A]S�AZ5?AWx�AWoAV��AV(�AU�FAU7LAT�AS�^AR�AQ�AO�#AM�7ALAHn�AF�AD5?AB�`AA�FA@VA>��A=�7A=33A=C�A=C�A=�A=%A<��A:1A7��A6VA5�-A5�A4ZA1�;A1��A1�hA1�A.�A. �A-�hA,�!A+"�A*(�A(��A(�A'|�A&�A%��A$�A$A"�9A!�A!x�A ~�A�wAt�A��AJA
=A�mAp�Ar�A�-A�hA�PAx�A��A�^AG�A�A��A%AbA��A7LA�+A=qAJA�7A�yAQ�AAA�AK�A��AO�A
�!A	��A	%AM�A�hA�RA�AG�AE�AƨAoA�uA�-A �A v�@��@�9X@�x�@�Z@�\)@�;d@���@�G�@�1@���@�@��@�(�@��@�K�@���@�@���@�-@�@���@�&�@� �@�@�l�@���@��@��#@�7L@��@�b@ߕ�@�C�@���@�"�@���@�V@׶F@��H@�5?@�(�@��@�n�@�?}@�Q�@�ff@�j@˝�@�O�@�;d@�5?@�@��@��@��m@�S�@�@�/@���@�b@�t�@��H@��#@�&�@�bN@���@�|�@�
=@���@�^5@�-@��@�1'@��@��@��@���@�j@�Z@��w@���@�|�@���@��+@��-@�Ĝ@� �@�ƨ@�\)@���@��T@�A�@��y@��@���@�hs@��@���@���@��@�bN@�1'@��
@�l�@�5?@�7L@���@�b@�o@�^5@��h@�X@��/@��@�Q�@�r�@�b@�S�@���@��@���@��+@�$�@��T@��h@��@��j@��@��@�C�@���@�{@��#@���@��@��j@�r�@�A�@��@��@��P@���@�ff@�E�@��@��T@�@���@��7@�G�@�%@��/@��@�bN@�1@��y@�@��#@�x�@�7L@�&�@��/@�z�@� �@��@�|�@��@�bN@�Ĝ@�hs@���@���@�x�@�x�@�hs@�O�@�V@��@�I�@���@�b@��
@��@���@�5?@�5?@�J@���@���@��@�J@���@��@��@��@��@�x�@��@��/@��@�9X@� �@��m@���@��F@��@��P@�|�@�S�@��@��H@��y@���@�+@�dZ@�33@�$�@���@���@�`B@��@���@���@��@�9X@��@��@�ƨ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��BBBVB �B+B1'B-B-B(�B$�B$�B,B2-B33BC�BJ�BM�BM�BG�B@�B<jB5?B,B'�B'�B&�B#�B&�B,B,B0!B?}BF�BE�BC�B;dB9XB6FB1'B5?B8RB?}B<jBL�BB�BF�B@�B7LB9XB;dBG�BM�BQ�BXBS�BF�BN�BM�BB�B-BB�HB��B��BgmBA�B�B��B��BŢB�FB��B��B�uB�Bv�BiyBP�BC�B9XB.B�BJB1B
��B
�B
�/B
��B
ŢB
�jB
�B
��B
�{B
�JB
�B
v�B
m�B
bNB
ZB
VB
P�B
E�B
7LB
1'B
/B
-B
'�B
$�B
�B
�B
bB
DB
B	��B	�B	�mB	�BB	�#B	�B	��B	ŢB	ƨB	ĜB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�JB	�B	z�B	o�B	dZB	R�B	F�B	2-B	!�B	hB	+B��B�B�B��B	  B	B	B��B�TB��BɺBǮBŢB��B�^B�XB�RB�?B�B��B��B��B��B��B��B��B��B��B��B�{B�bB�PB�JB�JB�DB�DB�=B�7B�1B�%B�B�B�B�B�B� B~�B|�B{�Bz�By�Bw�Bt�Bs�Bs�Br�Bs�Bs�Bs�Bq�Bp�Bn�Bl�BjBhsBgmBdZBbNB`BB_;B]/B\)BZBYBXBVBVBS�BS�BQ�BP�BO�BL�BK�BI�BI�BI�BI�BH�BF�BF�BF�BE�BD�BC�BA�BA�BB�BB�BC�BC�BB�B@�B@�BB�BD�BD�BD�BD�BE�BD�BE�BE�BE�BE�BD�BE�BH�BI�BI�BK�BL�BL�BP�BR�BQ�BR�BQ�BS�BYBYBZB\)B\)B\)B\)BbNBffBgmBhsBk�Bk�BjBiyBiyBiyBhsBhsBjBn�Bs�Bw�By�B{�Bz�By�By�B� B�1B�JB�VB�VB�bB�hB�hB�{B�{B�{B��B��B��B��B��B��B��B�B�B�B�!B�'B�'B�-B�3B�3B�9B�9B�?B�dB�wB�wB��BÖBȴBɺB��B��B�
B�#B�5B�5B�5B�BB�TB�fB�sB�B�B�B�B�B�B�B��B��B	B	B	B		7B	JB	\B	oB	uB	�B	�B	 �B	"�B	#�B	%�B	'�B	)�B	+B	,B	.B	1'B	33B	7LB	:^B	<jB	C�B	I�B	I�B	K�B	L�B	L�B	O�B	XB	]/B	aHB	dZB	l�B	p�B	s�B	w�B	z�B	{�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�=B	�DB	�PB	�hB	��B	�uB	�bB	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�9B	�RB	�XB	�^B	�^B	�dB	�wB	�}B	B	ÖB	ŢB	ŢB	ƨB	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BĜB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��BBBVB �B+B1'B-B.B)�B$�B$�B-B6FB33BC�BJ�BO�BP�BI�BA�B=qB8RB-B(�B(�B'�B$�B'�B-B,B/B?}BF�BF�BE�B=qB<jB8RB2-B7LB:^BB�B>wBN�BC�BJ�BD�B<jB;dB;dBH�BO�BS�BYBVBG�BP�BP�BG�B49BPB�`B�HB��Bq�BK�B)�BB�B��B�dB�B��B��B�%B|�Bt�BW
BF�B<jB33B"�BPBDBB
��B
�TB
��B
ȴB
B
�'B
��B
��B
�oB
�%B
|�B
r�B
gmB
[#B
XB
W
B
O�B
;dB
2-B
0!B
0!B
+B
'�B
$�B
�B
uB
hB
1B	��B	��B	�B	�NB	�/B	�/B	�B	ȴB	ǮB	ȴB	ÖB	�B	��B	��B	��B	��B	��B	��B	��B	�uB	�\B	�=B	�B	s�B	m�B	XB	L�B	6FB	$�B	�B	DB	  B�B�B��B	  B	B	+B	+B�yB�
B��BȴBɺBǮB�dB�XB�XB�qB�-B�B��B�B��B��B��B��B��B��B��B��B��B�bB�VB�\B�VB�JB�JB�JB�JB�=B�+B�+B�B�B�B�B�B� B}�B{�Bz�B{�Bw�Bt�Bt�Bt�Bt�Bt�Bt�Bs�Br�Bp�Bn�Bm�BiyBiyBhsBdZBbNBbNB_;B^5B]/B[#BZBYBXBVBVBT�BS�BO�BQ�BN�BN�BK�BK�BI�BI�BI�BI�BH�BH�BE�BC�BG�BD�BC�BD�BD�BC�BC�BE�BD�BD�BD�BD�BE�BD�BE�BE�BF�BF�BF�BF�BF�BI�BJ�BJ�BK�BL�BM�BO�BR�BS�BS�BS�BT�BW
BZB\)B]/B^5B\)B]/B\)BdZBgmBhsBjBl�Bl�Bk�BjBk�BjBjBiyBjBo�Bt�Bx�Bz�B}�B{�B{�B{�B� B�7B�JB�VB�\B�bB�hB�oB��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�'B�'B�-B�3B�3B�?B�?B�LB�qB�}B��BBĜBɺBɺB��B��B�B�#B�;B�;B�;B�BB�TB�fB�yB�B�B�B�B�B�B��B��B��B	B	B	%B	
=B	PB	\B	oB	uB	�B	�B	 �B	"�B	#�B	%�B	'�B	)�B	+B	,B	.B	1'B	33B	7LB	;dB	>wB	D�B	I�B	J�B	K�B	L�B	L�B	P�B	YB	^5B	aHB	cTB	k�B	o�B	r�B	w�B	{�B	z�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�=B	�JB	�JB	�PB	�hB	��B	��B	�oB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�?B	�^B	�^B	�^B	�dB	�jB	�wB	��B	B	ĜB	ŢB	ŢB	ƨB	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<D��<#�
<#�
<D��<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447222012010314472220120103144722  AO  ARGQ                                                                        20111130142325  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142325  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144723  IP                  G�O�G�O�G�O�                