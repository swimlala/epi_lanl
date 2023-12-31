CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:23Z UW 3.1 conversion   
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               rA   AO  20111130141747  20190522121826  1727_5046_114                   2C  D   APEX                            2143                            040306                          846 @ԩ7��
1   @ԩ��P@6�1&�x��d�\(��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�33B���B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C�fC�fC  C   C"�C$�C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDCfDC�fDD  DDy�DD��DE� DF  DF� DG  DG� DH  DHy�DH��DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg�fDh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�3Dy��D�fD�` D���D��3D��D�i�D��3D���D�  D�l�D�� D�� D�  D�s3Dڳ3D��3D�3D�` D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@@  @�33@�33A��A!��AA��Aa��A���A���A���A���A�  A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBh  BpffBxffB�33B�33B�33B�33B�33B�ffB�  B�33B�  B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C33C�C�C�C
�C�C�C�C�C�C�C�C  C  C�C �C"33C$33C&�C(�C*�C,�C.�C0�C2�C433C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp33Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB��DC�DC��DDfDD� DE  DE�fDFfDF�fDGfDG�fDHfDH� DI  DI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO� DPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDg�Dg��DhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDs��Dy� D��D�c3D���D��fD�  D�l�D��fD�� D�#3D�p D��3D��3D�#3D�vfDڶfD��fD�fD�c3D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�XA�S�A�O�A�ZA�ZA�ZA�O�A�`BA�r�AȁA�S�A�A�A�?}A�C�A�I�A�M�A�VA�^5A�jAȃAȍPAȓuAȗ�Aȝ�Aȡ�Aȥ�Aȧ�AȬAȰ!AȺ^AȼjA���A�A�ĜA�ĜA���A�A�~�AƁAĶFA���A��A���A��A���A�n�A�O�A�9XA��PA��A�z�A� �A���A��A���A��!A��/A��;A�jA���A�A��-A���A��mA��uA�Q�A��!A�v�A�XA�9XA��A��\A��yA�VA�bNA��A���A���A�5?A�|�A���A�5?A�n�A��uA���A�K�A���A�z�A���A�S�A���A���A�%A���A�1A���A��A��!A��A��9A�|�A�ƨA�&�A��A�n�A���A���A�O�A�O�A���A��^A�p�A��A���A�(�A��
A�A�A�A}�hA{hsAy�Ay+AvbNAq�An��Aj��Ai+Af�DAc�-A`��A_�A\ZAZ��AZAY��AY��AYhsAXAU�;AT�+AP(�ANM�AI��AHv�AF��AFffAE"�AC��AA��A>n�A>bA>  A=��A<ffA;�A;|�A;�A:$�A9S�A8�+A6��A4�uA1A/G�A-dZA+p�A*��A*��A)��A(r�A'|�A'�A&r�A%�
A%hsA$��A#ƨA"�A!��A ��A�FA
=A�wA�uA��A�A�yAv�A�RAx�A�A��A�!A5?A�
AO�A�+A��AG�A�Az�A7LA��AjA\)AbNA�A`BA
��A	�
A��A��AZA�AS�A~�A�yAffA�^A�A�yA��A7LA 1@�C�@��@��h@�b@��H@��@�l�@�+@�9@@@���@�P@�"�@��H@���@�@��`@�|�@��@�E�@܃@�ff@�/@�j@�b@��;@׾w@�\)@ְ!@��@�/@ӕ�@҇+@��@���@�hs@�(�@�K�@̼j@��;@�@ȃ@�9X@��@��@�33@� �@�C�@¸R@��@���@�(�@��y@��h@��/@�Z@�ff@��T@���@�hs@�/@�%@��@�9X@��@��@��@���@�=q@���@���@��7@�X@�Z@�ff@��-@��@�5?@�/@�  @��P@�33@���@��9@�1'@�t�@�"�@�n�@��9@�1@�{@�1'@��@��;@�ƨ@�l�@���@��+@�~�@�v�@�~�@���@�x�@���@��@�j@�(�@��
@���@�|�@�S�@�;d@��@���@��y@��R@��+@�V@�5?@��T@���@��-@���@�hs@�G�@�?}@���@��/@���@��j@�j@�9X@�  @��@���@���@�E�@��+@�ff@�@�hs@�?}@�7L@�G�@�?}@�7L@�7L@�?}@�&�@�%@�bN@�1@�l�@�ȴ@�M�@���@���@�x�@�/@��/@�z�@�K�@���@�=q@��@���@��7@�@���@�O�@��D@�j@�I�@���@��
@���@�;d@��H@���@���@��+@��@�x�@�/@��@�/@�7L@��@�V@���@��/@�Ĝ@��@�9X@��
@�  @�ƨ@��F@���@�t�@�dZ@�S�@�;d@�"�@�o@�
=@�@���@��@�{@�J@��T@���@��h@�x�@�?}@���@���@�bN@� �@���@��;@�\)@�;d@�33@�
=@��y@���@���@�~�@�^5@�E�@�=q@�-@�$�@�@��7@�O�@�?}@�?}@�7L@��`@�Ĝ@�Z@��@��@���@�|�@��@��@���@��!@�n�@�5?@�$�@�@���@��T@�@��@�hs@��@���@�Z@|9X@t�/@i��@a�^@UV@NV@G;d@@r�@9��@2�H@,I�@(bN@#�m@ 1'@�@�;@�H@�@�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�XA�S�A�O�A�ZA�ZA�ZA�O�A�`BA�r�AȁA�S�A�A�A�?}A�C�A�I�A�M�A�VA�^5A�jAȃAȍPAȓuAȗ�Aȝ�Aȡ�Aȥ�Aȧ�AȬAȰ!AȺ^AȼjA���A�A�ĜA�ĜA���A�A�~�AƁAĶFA���A��A���A��A���A�n�A�O�A�9XA��PA��A�z�A� �A���A��A���A��!A��/A��;A�jA���A�A��-A���A��mA��uA�Q�A��!A�v�A�XA�9XA��A��\A��yA�VA�bNA��A���A���A�5?A�|�A���A�5?A�n�A��uA���A�K�A���A�z�A���A�S�A���A���A�%A���A�1A���A��A��!A��A��9A�|�A�ƨA�&�A��A�n�A���A���A�O�A�O�A���A��^A�p�A��A���A�(�A��
A�A�A�A}�hA{hsAy�Ay+AvbNAq�An��Aj��Ai+Af�DAc�-A`��A_�A\ZAZ��AZAY��AY��AYhsAXAU�;AT�+AP(�ANM�AI��AHv�AF��AFffAE"�AC��AA��A>n�A>bA>  A=��A<ffA;�A;|�A;�A:$�A9S�A8�+A6��A4�uA1A/G�A-dZA+p�A*��A*��A)��A(r�A'|�A'�A&r�A%�
A%hsA$��A#ƨA"�A!��A ��A�FA
=A�wA�uA��A�A�yAv�A�RAx�A�A��A�!A5?A�
AO�A�+A��AG�A�Az�A7LA��AjA\)AbNA�A`BA
��A	�
A��A��AZA�AS�A~�A�yAffA�^A�A�yA��A7LA 1@�C�@��@��h@�b@��H@��@�l�@�+@�9@@@���@�P@�"�@��H@���@�@��`@�|�@��@�E�@܃@�ff@�/@�j@�b@��;@׾w@�\)@ְ!@��@�/@ӕ�@҇+@��@���@�hs@�(�@�K�@̼j@��;@�@ȃ@�9X@��@��@�33@� �@�C�@¸R@��@���@�(�@��y@��h@��/@�Z@�ff@��T@���@�hs@�/@�%@��@�9X@��@��@��@���@�=q@���@���@��7@�X@�Z@�ff@��-@��@�5?@�/@�  @��P@�33@���@��9@�1'@�t�@�"�@�n�@��9@�1@�{@�1'@��@��;@�ƨ@�l�@���@��+@�~�@�v�@�~�@���@�x�@���@��@�j@�(�@��
@���@�|�@�S�@�;d@��@���@��y@��R@��+@�V@�5?@��T@���@��-@���@�hs@�G�@�?}@���@��/@���@��j@�j@�9X@�  @��@���@���@�E�@��+@�ff@�@�hs@�?}@�7L@�G�@�?}@�7L@�7L@�?}@�&�@�%@�bN@�1@�l�@�ȴ@�M�@���@���@�x�@�/@��/@�z�@�K�@���@�=q@��@���@��7@�@���@�O�@��D@�j@�I�@���@��
@���@�;d@��H@���@���@��+@��@�x�@�/@��@�/@�7L@��@�V@���@��/@�Ĝ@��@�9X@��
@�  @�ƨ@��F@���@�t�@�dZ@�S�@�;d@�"�@�o@�
=@�@���@��@�{@�J@��T@���@��h@�x�@�?}@���@���@�bN@� �@���@��;@�\)@�;d@�33@�
=@��y@���@���@�~�@�^5@�E�@�=q@�-@�$�@�@��7@�O�@�?}@�?}@�7L@��`@�Ĝ@�Z@��@��@���@�|�@��@��@���@��!@�n�@�5?@�$�@�@���@��T@�@��@�hs@��@���@�Z@|9X@t�/@i��@a�^@UV@NV@G;d@@r�@9��@2�H@,I�@(bN@#�m@ 1'@�@�;@�H@�@�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB �B!�B!�B�B�B �B�B%�B+B1'B,B+B-B0!B5?B9XB<jB@�BC�BK�BO�BQ�BR�BT�BW
BXBYBZB[#B]/B^5B^5B_;B_;B_;BaHBaHB\)Bl�BN�B��B�B�B�B�B�sB�B��B-BcTB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�VB�+B~�Bt�Bk�BdZBXBI�B<jB33B'�B�B	7BB��B�yB�)B��B�jB��B��B�BjBT�BN�BC�B6FB!�B
��B
�B
�B
��B
�?B
��B
�bB
�7B
�B
s�B
ffB
T�B
L�B
F�B
5?B
&�B
�B
{B	��B	�;B	ƨB	�B	��B	�PB	z�B	jB	^5B	M�B	F�B	D�B	C�B	B�B	?}B	49B	)�B	"�B	JB��B�mB�B��B��BĜB�^B�'B�B��B��B��B��B��B��B��B��B��B�uB�\B�%B�Bw�Bp�Bo�Bn�Bl�Bk�BiyBgmBffBffBffBdZBcTBcTBbNBcTBbNBcTBbNBcTBcTBdZBdZBcTBaHBbNBcTBcTBcTBbNBbNBaHBaHBaHB`BB`BB_;B]/B[#BYBVBT�BS�BR�BQ�BP�BN�BN�BM�BL�BK�BJ�BG�BG�BE�BD�BD�BB�B@�B>wB=qB<jB<jB:^B9XB8RB6FB2-B/B.B-B+B)�B(�B'�B%�B#�B"�B�B�B�B�B�B�B �B!�B �B �B �B �B �B!�B!�B"�B#�B%�B$�B$�B(�B'�B,B,B)�B,B-B/B1'B1'B;dB<jB=qB=qB?}B?}BB�BF�BG�BH�BQ�BS�BVBW
BYB\)Be`BhsBq�Bw�B|�B~�B�B�B�B�B�1B�JB�hB��B��B��B��B��B��B�B�B�B�B�9B�?B�?B�FB�3B�'B�-B�RB�jB�qB�}BÖBƨBɺB��B��B�B�B�#B�)B�BB�NB�ZB�`B�fB�sB�yB�yB�B�B�B�B�B�B��B��B	B	B	+B	
=B	\B	hB	oB	oB	uB	�B	�B	�B	�B	�B	"�B	-B	49B	7LB	7LB	7LB	:^B	=qB	>wB	?}B	?}B	?}B	@�B	A�B	B�B	E�B	F�B	H�B	J�B	N�B	P�B	Q�B	S�B	S�B	T�B	VB	T�B	R�B	P�B	O�B	O�B	P�B	R�B	R�B	R�B	R�B	S�B	S�B	W
B	XB	YB	[#B	`BB	cTB	cTB	cTB	hsB	l�B	o�B	r�B	s�B	v�B	w�B	x�B	y�B	y�B	y�B	y�B	z�B	|�B	�B	�B	�%B	�+B	�+B	�+B	�1B	�7B	�=B	�JB	�PB	�PB	�\B	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�FB	�LB	�LB	�RB	�dB	�jB	�wB	�wB	�wB	�wB	��B	B	ÖB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�BB	�B
  B
\B
�B
$�B
,B
33B
:^B
@�B
G�B
M�B
R�B
XB
\)B
`BB
e`B
iyB
m�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B �B!�B!�B�B�B �B�B%�B+B2-B,B+B-B0!B5?B9XB<jB@�BB�BK�BO�BQ�BR�BT�BW
BXBYBZB[#B]/B^5B^5B_;B_;B_;BaHBbNB_;Bo�BZBDB��B��B��B�B�sB�B��B0!BgmB�%B��B��B��B��B��B�B�B�'B�B��B��B�B��B��B�B��B��B��B��B��B�B��B��B��B��B�oB�JB�By�Bn�BiyB^5BN�B?}B8RB/B�BDBB��B�B�HB��BB�B��B�JBq�BVBQ�BF�B;dB/B  B
�B
�B
�BB
�qB
��B
�oB
�JB
�+B
x�B
n�B
YB
N�B
M�B
;dB
)�B
�B
�B
+B	�`B	��B	�-B	��B	�uB	�B	n�B	e`B	Q�B	H�B	E�B	D�B	C�B	C�B	:^B	/B	/B	oB		7B�B�5B��B��BɺB��B�jB�B��B�B�B��B��B��B��B��B��B��B�uB�DB�1B{�Bt�Bp�Bo�Bn�Bn�Bk�BhsBhsBhsBgmBffBe`BffBffBe`Be`Be`BffBgmBffBe`Be`Be`BffBffBdZBdZBdZBdZBcTBcTBdZBdZBaHBaHBaHBaHB`BB]/BZBXBVBT�BS�BS�BQ�BO�BN�BN�BM�BM�BL�BI�BG�BF�BE�BC�BE�BB�B>wB>wB=qB=qB;dB:^B9XB7LB2-B/B/B-B,B)�B(�B)�B&�B#�B �B!�B �B!�B!�B!�B!�B"�B �B �B!�B!�B!�B"�B$�B$�B$�B%�B%�B&�B+B,B.B.B.B-B-B/B2-B5?B<jB=qB>wB?}B@�BA�BD�BG�BH�BK�BR�BT�BW
BXBYB]/BffBhsBr�Bx�B}�B� B�B�B�B�B�=B�\B�oB��B��B��B��B�B�B�B�'B�B�B�?B�FB�RB�LB�FB�9B�-B�RB�jB�wB��BÖBƨBɺB��B��B�B�B�)B�)B�HB�TB�ZB�`B�fB�sB�yB�yB�B�B�B�B�B�B��B��B	B	B	+B	
=B	\B	hB	oB	oB	{B	�B	�B	�B	�B	 �B	#�B	-B	49B	8RB	8RB	7LB	:^B	=qB	>wB	?}B	?}B	?}B	@�B	A�B	C�B	F�B	G�B	I�B	K�B	O�B	P�B	R�B	T�B	T�B	VB	XB	VB	S�B	P�B	O�B	O�B	P�B	R�B	R�B	S�B	R�B	S�B	T�B	W
B	YB	YB	[#B	`BB	cTB	cTB	dZB	iyB	m�B	o�B	r�B	s�B	v�B	w�B	x�B	y�B	y�B	z�B	z�B	{�B	|�B	�B	�B	�%B	�+B	�+B	�+B	�1B	�7B	�=B	�JB	�PB	�VB	�bB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�FB	�LB	�LB	�XB	�dB	�jB	�wB	�wB	�wB	�wB	��B	B	ÖB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�BB	�B
  B
\B
�B
$�B
,B
33B
:^B
@�B
G�B
M�B
R�B
XB
\)B
`BB
e`B
iyB
m�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447142012010314471420120103144714  AO  ARGQ                                                                        20111130141747  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141747  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144714  IP                  G�O�G�O�G�O�                