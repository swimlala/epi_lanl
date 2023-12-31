CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:44Z UW 3.1 conversion   
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  eP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143344  20190522121827  1727_5046_190                   2C  D   APEX                            2143                            040306                          846 @�>I��1   @�>�?�@5�?|�h�c�?|�h1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�ff@���@���AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;�fD<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DV��DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @@  @���@���A   A   AA��Aa��A���A���A���A���A���A���A���A���B ffB  BffBffB ffB(  B0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@33CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fD�D�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD�D�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0� D1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;��D<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDO  DO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDW  DW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDn  Dn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�z�A�z�A�x�A�z�A�v�A�n�A�jA�hsA�ffA�bNA�VA�S�A�K�A�=qA�5?A�33A�1'A�1'A�+A�(�A�oA���A��A��HA���AĶFAđhA�+A���A�~�A��
A�-A�33A�bNA��hA�A��A�7LA��\A���A�ȴA��\A��hA��jA�7LA��A�l�A���A���A���A���A��yA�|�A�bA��RA�;dA��\A�XA��;A���A��A���A��-A���A��;A�VA�1A��\A�33A��;A�A�A���A�A�z�A�9XA���A�5?A��FA��A� �A�XA���A��FA��#A�=qA���A�XA�r�A���A�A�A��A��DA���A�=qA��A�hsA��\A�S�A�~�A�1A�G�A���A�^5A�C�A���A�A���A�Q�A��HA�XAy�-Au�-ArJAo��Aml�Ai�Ag\)Af{Ac+Aa
=A`ZA_t�A^�A]��AYdZAVVAT��ATA�ASƨAR��AOK�AN  AL{AG�hADr�ACC�AB�!A@r�A?�FA>ffA=XA=/A<��A;�wA9��A8�A5��A5�A333A1�A/�
A-A+ƨA*�yA(�/A'"�A&n�A$ĜA"r�A"JA ZA&�AM�AG�AjAE�A�-Al�AO�AĜA�mA/A�A�DA{A�wA�AVA�hAZA&�A^5A��A?}A��A
�A
��A
�uA	��A	x�A	�AZA�A��A�yAjAA�A�A��A��AC�AffAl�A"�A
=A��A~�An�AbNA1'At�A ĜA b@���@���@�x�@�(�@�Ĝ@��@���@� �@�C�@���@�`B@��@��@�bN@�
=@��@�r�@��@��@�O�@�Z@��@�@�hs@�%@�u@�j@���@�@�h@�9@�Z@߮@�dZ@��H@�5?@݁@�?}@���@�9X@�-@٩�@�7L@ؓu@׾w@���@�^5@�@���@ӥ�@�l�@�+@���@ҟ�@�~�@�{@���@�l�@�^5@͡�@�p�@�`B@�I�@�ȴ@�V@���@��`@�\)@�ȴ@�^5@ř�@Ĵ9@å�@�@���@�ƨ@���@�33@��@�~�@��@��@���@�Z@�^5@���@�7L@���@�r�@�ƨ@�dZ@�+@��@���@�`B@���@�\)@���@��7@�X@�&�@��`@��D@�I�@�1@��m@��F@��P@�dZ@�K�@�C�@�;d@���@���@�O�@�7L@���@�|�@��!@�-@���@���@�O�@�z�@�;d@��@��+@�5?@��@���@��T@���@��^@���@���@�
=@�@���@�n�@�$�@��-@��7@�p�@�`B@��@���@��w@�;d@��@��@��+@�ff@��7@�%@��@��/@���@���@�Ĝ@��9@���@� �@��w@��F@��w@��@�t�@�o@��R@�V@��@�?}@��@��/@��/@���@�1'@��@���@�t�@�;d@�o@��R@�v�@�^5@�-@���@��@�Ĝ@��u@��@�r�@�r�@�bN@�Q�@�1'@��m@���@�t�@�"�@�~�@��+@�ff@��#@�@�X@���@��9@��;@�ƨ@��w@��F@�|�@�"�@��y@���@��+@�^5@�M�@�-@���@���@���@��^@��h@�hs@�O�@�?}@��@���@��9@�A�@�A�@�9X@��@��
@��@���@���@��@��@���@�l�@�K�@�C�@�C�@�33@�+@��@���@�~�@��@��#@���@��@�hs@�7L@���@���@��D@�(�@�1@���@�+@���@��R@���@�ff@�^5@�5?@���@��j@�9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�z�A�z�A�x�A�z�A�v�A�n�A�jA�hsA�ffA�bNA�VA�S�A�K�A�=qA�5?A�33A�1'A�1'A�+A�(�A�oA���A��A��HA���AĶFAđhA�+A���A�~�A��
A�-A�33A�bNA��hA�A��A�7LA��\A���A�ȴA��\A��hA��jA�7LA��A�l�A���A���A���A���A��yA�|�A�bA��RA�;dA��\A�XA��;A���A��A���A��-A���A��;A�VA�1A��\A�33A��;A�A�A���A�A�z�A�9XA���A�5?A��FA��A� �A�XA���A��FA��#A�=qA���A�XA�r�A���A�A�A��A��DA���A�=qA��A�hsA��\A�S�A�~�A�1A�G�A���A�^5A�C�A���A�A���A�Q�A��HA�XAy�-Au�-ArJAo��Aml�Ai�Ag\)Af{Ac+Aa
=A`ZA_t�A^�A]��AYdZAVVAT��ATA�ASƨAR��AOK�AN  AL{AG�hADr�ACC�AB�!A@r�A?�FA>ffA=XA=/A<��A;�wA9��A8�A5��A5�A333A1�A/�
A-A+ƨA*�yA(�/A'"�A&n�A$ĜA"r�A"JA ZA&�AM�AG�AjAE�A�-Al�AO�AĜA�mA/A�A�DA{A�wA�AVA�hAZA&�A^5A��A?}A��A
�A
��A
�uA	��A	x�A	�AZA�A��A�yAjAA�A�A��A��AC�AffAl�A"�A
=A��A~�An�AbNA1'At�A ĜA b@���@���@�x�@�(�@�Ĝ@��@���@� �@�C�@���@�`B@��@��@�bN@�
=@��@�r�@��@��@�O�@�Z@��@�@�hs@�%@�u@�j@���@�@�h@�9@�Z@߮@�dZ@��H@�5?@݁@�?}@���@�9X@�-@٩�@�7L@ؓu@׾w@���@�^5@�@���@ӥ�@�l�@�+@���@ҟ�@�~�@�{@���@�l�@�^5@͡�@�p�@�`B@�I�@�ȴ@�V@���@��`@�\)@�ȴ@�^5@ř�@Ĵ9@å�@�@���@�ƨ@���@�33@��@�~�@��@��@���@�Z@�^5@���@�7L@���@�r�@�ƨ@�dZ@�+@��@���@�`B@���@�\)@���@��7@�X@�&�@��`@��D@�I�@�1@��m@��F@��P@�dZ@�K�@�C�@�;d@���@���@�O�@�7L@���@�|�@��!@�-@���@���@�O�@�z�@�;d@��@��+@�5?@��@���@��T@���@��^@���@���@�
=@�@���@�n�@�$�@��-@��7@�p�@�`B@��@���@��w@�;d@��@��@��+@�ff@��7@�%@��@��/@���@���@�Ĝ@��9@���@� �@��w@��F@��w@��@�t�@�o@��R@�V@��@�?}@��@��/@��/@���@�1'@��@���@�t�@�;d@�o@��R@�v�@�^5@�-@���@��@�Ĝ@��u@��@�r�@�r�@�bN@�Q�@�1'@��m@���@�t�@�"�@�~�@��+@�ff@��#@�@�X@���@��9@��;@�ƨ@��w@��F@�|�@�"�@��y@���@��+@�^5@�M�@�-@���@���@���@��^@��h@�hs@�O�@�?}@��@���@��9@�A�@�A�@�9X@��@��
@��@���@���@��@��@���@�l�@�K�@�C�@�C�@�33@�+@��@���@�~�@��@��#@���@��@�hs@�7L@���@���@��D@�(�@�1@���@�+@���@��R@���@�ff@�^5@�5?@���@��j@�9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBL�BL�BM�BL�BM�BN�BN�BL�BL�BL�BK�BK�BL�BL�BK�BK�BK�BK�BM�BP�BcTBp�Bq�Bq�Bs�Bs�Bu�B�B��B��B�XBÖBB��B�
B�B�#B�)B�5B�)B�
B��B�BB�ZB�TB�TB�`B�ZB�ZB�BB�BB�)B�B��B��B�qB�9B��B��B��B�bB�=B�Bt�Bn�BjBgmBaHB\)BW
BP�BG�B=qB49B&�B �B�BB��B��B�sB��B��B�wB�3B��B�hBt�BjBcTB]/BVBM�BD�B>wB33B{B
�B
�mB
�5B
��B
�qB
�?B
�3B
�B
�B
��B
��B
� B
I�B
�B	��B	�ZB	��B	�XB	��B	�hB	�%B	w�B	o�B	l�B	gmB	cTB	^5B	I�B	;dB	49B	-B	&�B	�B��B�B�ZB��B��B�jB�LB�!B�B��B��B��B��B��B��B�{B�oB�hB�PB�DB�=B�1B�+B�B�B�B�B� B�B� B� B� B~�B}�B{�B|�B}�B~�B}�B|�B{�B|�Bz�Bz�By�Bv�Bs�Bq�Bo�Bn�Bm�Bk�Bk�BiyBiyBjBiyBhsBhsBhsBgmBgmBgmBgmBffBffBffBe`Be`BdZBbNBbNBcTBcTBcTBcTBcTBcTBcTBbNBbNBbNBbNBdZBdZBdZBhsBo�Bq�Bq�Bs�Bt�Bw�Bx�By�By�Bx�By�B~�B� B~�B� B�B�%B�DB�JB�PB�PB�VB�PB�VB�\B�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�LB�jB�}B��BĜBȴBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B�B�
B�5B�5B�;B�;B�;B�;B�;B�;B�;B�;B�HB�ZB�mB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	1B	
=B	JB	�B	�B	"�B	#�B	#�B	%�B	(�B	.B	0!B	1'B	33B	33B	49B	49B	49B	5?B	9XB	C�B	G�B	G�B	H�B	J�B	K�B	M�B	O�B	P�B	P�B	R�B	S�B	\)B	`BB	aHB	cTB	gmB	gmB	l�B	o�B	q�B	q�B	r�B	r�B	r�B	r�B	r�B	w�B	{�B	|�B	|�B	|�B	~�B	� B	�B	�B	�B	�1B	�DB	�DB	�DB	�JB	�\B	�hB	�oB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�3B	�?B	�FB	�FB	�LB	�XB	�dB	�jB	�qB	�}B	B	ÖB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�5B	�HB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�fB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BL�BL�BM�BL�BM�BN�BN�BL�BL�BL�BK�BK�BL�BL�BK�BK�BK�BK�BM�BP�BcTBp�Bq�Bq�Bs�Bt�Bw�B�%B��B��B�jBǮBŢB��B�/B�)B�5B�5B�BB�;B�#B�B�ZB�fB�ZB�fB�mB�B�yB�ZB�`B�5B�#B�
B��BŢB�^B�B��B��B�uB�\B�%Bx�Bp�Bk�BiyBcTB^5BZBS�BJ�B@�B9XB(�B#�B�BB  B��B�B��B��B��B�FB�B��Bw�Bm�Be`B_;BZBP�BF�BA�B=qB �B
��B
�B
�NB
�B
��B
�FB
�?B
�B
�B
��B
��B
�PB
YB
!�B
B	�B	��B	ÖB	��B	��B	�VB	}�B	q�B	n�B	iyB	gmB	k�B	R�B	@�B	6FB	/B	+B	$�B	B��B�B�BŢB�}B�}B�3B�'B�B��B��B��B��B��B��B�uB��B�hB�\B�hB�DB�7B�=B�1B�%B�+B�%B�B�B�B�B�B�B�B~�B~�B� B� B� B~�B}�B|�B|�B{�B|�Bw�Bv�Bt�Br�Bp�Bn�Bm�Bo�Bl�Bk�BjBjBjBjBjBiyBhsBjBhsBgmBgmBffBffBffBe`BffBdZBdZBdZBdZBcTBcTBdZBdZBdZBdZBdZBe`Be`BffBl�Bq�Bs�Bs�Bt�Bv�Bx�By�By�Bz�Bz�B|�B� B�B�B�B�%B�7B�PB�PB�VB�VB�\B�VB�bB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�3B�XB�qB��BBƨBɺBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�;B�;B�BB�BB�BB�BB�;B�;B�;B�HB�NB�fB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	  B	1B	DB	VB	�B	 �B	"�B	#�B	$�B	&�B	+B	/B	1'B	2-B	33B	33B	49B	49B	49B	6FB	;dB	D�B	G�B	G�B	I�B	K�B	K�B	M�B	O�B	P�B	Q�B	S�B	VB	]/B	`BB	bNB	dZB	gmB	iyB	m�B	o�B	q�B	q�B	r�B	r�B	r�B	r�B	s�B	w�B	{�B	|�B	|�B	}�B	� B	�B	�B	�B	�B	�1B	�DB	�DB	�DB	�PB	�bB	�hB	�oB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�9B	�?B	�LB	�LB	�RB	�^B	�dB	�jB	�qB	�}B	ÖB	ÖB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�;B	�NB	�TB	�TB	�ZB	�`B	�ZB	�ZB	�fB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447412012010314474120120103144741  AO  ARGQ                                                                        20111130143344  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143344  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144741  IP                  G�O�G�O�G�O�                