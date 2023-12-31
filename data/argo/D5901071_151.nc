CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:33Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142608  20190522121827  1727_5046_151                   2C  D   APEX                            2143                            040306                          846 @��ж;��1   @�������@6Ƨ�cå�S��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C��3C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=y�D>  D>� D?  D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dss3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�  B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C33C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX  CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C�  C��C��C��C��C�  C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD��D	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD�D�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%  D%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=� D>fD>�fD?fD?� D@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG��DHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDe  De�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDsy�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�XA�bNA�dZA�l�A�r�A�t�A�r�A�v�A�v�A�A�A�/A��A�%A��`A���A�ƨA���A���AʾwAʴ9A�dZA��
AȓuA�  AƧ�A��A�x�A�bAēuA��
A�z�A�(�A¡�A�C�A��`A�ȴA��A�{A��+A�ffA��TA��jA�A�A�%A��^A��PA�5?A��;A���A�~�A�JA�7LA��uA��A�XA�ĜA��FA���A��yA�ȴA��A��\A�XA�bA��HA���A�l�A�E�A�
=A��wA�jA�oA��DA�(�A��`A�ffA��DA���A�n�A�VA�+A���A�S�A�{A�
=A��FA�G�A�9XA���A��yA�O�A�r�A�bA���A�|�A�C�A���A��
A�`BA�%A���A�K�A���A���A��\A�XA�A���A��7A�bNA�K�A�G�A�A��A�ZA�dZA��wA��9A� �A��HA���A���A�A�$�A�A~��A~r�A}�A}��A}&�Axr�Au�PAsXAr-Ap�DAi��Ae��Ae|�Ad��AcS�AbbA`�HA_�-A_�A^��A^1A\ȴA[t�A[;dAZr�AY33AXA�AU��AS/AN�AI|�AG"�AD{AA�A>ffA=�7A;�A:jA7��A6��A6r�A5�#A4A�A3l�A3A2A�A0~�A/p�A.^5A.$�A-�hA-%A,n�A+��A+|�A+VA*��A*9XA)G�A(�jA(�A'�A&�yA&$�A%��A%33A$5?A"��A!��A!�^A!dZA ��A �!A �+A�A|�A��A�;A�7A�A��AffA��A?}AjA�AffA�AhsAVAȴA�AVA�A��A��A&�AbA
=A
��A
-A	��A�A��A�HA�A9XA|�AA 1'@��+@�`B@�&�@��@�I�@�S�@��@�b@��F@�+@�{@�O�@�?}@�V@���@���@�Z@��m@���@�@@�F@�v�@�(�@�
=@���@���@�Z@�K�@�v�@أ�@��@�1'@�"�@ҏ\@ѡ�@�hs@Л�@ЋD@��@�J@���@�Q�@���@�|�@���@�Z@�+@���@�/@�-@��@�dZ@�t�@�  @�`B@��@��@��#@��@�(�@�t�@�o@��H@��@�ȴ@�^5@��^@��@�7L@�&�@��@��/@�9X@�  @��
@�l�@��y@���@�5?@�V@��`@��D@�  @���@��@�Z@� �@�|�@���@���@�-@�$�@�@�v�@��w@�x�@��#@�J@�$�@�5?@�=q@�J@���@���@���@�G�@�&�@�(�@�5?@�&�@���@�@�=q@�p�@�O�@���@�(�@�@��@��@�I�@��@�^5@�&�@��@���@�b@�dZ@��@�@�dZ@�@���@��@�X@���@���@��!@���@��\@�E�@��-@�`B@���@��u@�I�@� �@���@���@�l�@��@���@���@�~�@�n�@�n�@�ff@�ff@�ff@�^5@�V@�V@�M�@�=q@�-@��@�`B@�Ĝ@��u@�I�@�b@��;@��w@�t�@�
=@�ȴ@��R@���@�v�@�E�@��@�J@�@��T@���@��h@�X@�7L@�V@��@��`@��/@���@���@��u@��D@��D@��@��@�Q�@��@�S�@�C�@�33@�@���@�ȴ@���@�n�@�{@��@��@��#@���@��^@���@���@�x�@�`B@�G�@��@��/@��@���@���@�z�@�(�@� �@�b@�1@�1@���@��;@�ƨ@��w@��F@��@�|�@�;d@�o@��@��+@�^5@�M�@�M�@�E�@�J@��@���@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�XA�bNA�dZA�l�A�r�A�t�A�r�A�v�A�v�A�A�A�/A��A�%A��`A���A�ƨA���A���AʾwAʴ9A�dZA��
AȓuA�  AƧ�A��A�x�A�bAēuA��
A�z�A�(�A¡�A�C�A��`A�ȴA��A�{A��+A�ffA��TA��jA�A�A�%A��^A��PA�5?A��;A���A�~�A�JA�7LA��uA��A�XA�ĜA��FA���A��yA�ȴA��A��\A�XA�bA��HA���A�l�A�E�A�
=A��wA�jA�oA��DA�(�A��`A�ffA��DA���A�n�A�VA�+A���A�S�A�{A�
=A��FA�G�A�9XA���A��yA�O�A�r�A�bA���A�|�A�C�A���A��
A�`BA�%A���A�K�A���A���A��\A�XA�A���A��7A�bNA�K�A�G�A�A��A�ZA�dZA��wA��9A� �A��HA���A���A�A�$�A�A~��A~r�A}�A}��A}&�Axr�Au�PAsXAr-Ap�DAi��Ae��Ae|�Ad��AcS�AbbA`�HA_�-A_�A^��A^1A\ȴA[t�A[;dAZr�AY33AXA�AU��AS/AN�AI|�AG"�AD{AA�A>ffA=�7A;�A:jA7��A6��A6r�A5�#A4A�A3l�A3A2A�A0~�A/p�A.^5A.$�A-�hA-%A,n�A+��A+|�A+VA*��A*9XA)G�A(�jA(�A'�A&�yA&$�A%��A%33A$5?A"��A!��A!�^A!dZA ��A �!A �+A�A|�A��A�;A�7A�A��AffA��A?}AjA�AffA�AhsAVAȴA�AVA�A��A��A&�AbA
=A
��A
-A	��A�A��A�HA�A9XA|�AA 1'@��+@�`B@�&�@��@�I�@�S�@��@�b@��F@�+@�{@�O�@�?}@�V@���@���@�Z@��m@���@�@@�F@�v�@�(�@�
=@���@���@�Z@�K�@�v�@أ�@��@�1'@�"�@ҏ\@ѡ�@�hs@Л�@ЋD@��@�J@���@�Q�@���@�|�@���@�Z@�+@���@�/@�-@��@�dZ@�t�@�  @�`B@��@��@��#@��@�(�@�t�@�o@��H@��@�ȴ@�^5@��^@��@�7L@�&�@��@��/@�9X@�  @��
@�l�@��y@���@�5?@�V@��`@��D@�  @���@��@�Z@� �@�|�@���@���@�-@�$�@�@�v�@��w@�x�@��#@�J@�$�@�5?@�=q@�J@���@���@���@�G�@�&�@�(�@�5?@�&�@���@�@�=q@�p�@�O�@���@�(�@�@��@��@�I�@��@�^5@�&�@��@���@�b@�dZ@��@�@�dZ@�@���@��@�X@���@���@��!@���@��\@�E�@��-@�`B@���@��u@�I�@� �@���@���@�l�@��@���@���@�~�@�n�@�n�@�ff@�ff@�ff@�^5@�V@�V@�M�@�=q@�-@��@�`B@�Ĝ@��u@�I�@�b@��;@��w@�t�@�
=@�ȴ@��R@���@�v�@�E�@��@�J@�@��T@���@��h@�X@�7L@�V@��@��`@��/@���@���@��u@��D@��D@��@��@�Q�@��@�S�@�C�@�33@�@���@�ȴ@���@�n�@�{@��@��@��#@���@��^@���@���@�x�@�`B@�G�@��@��/@��@���@���@�z�@�(�@� �@�b@�1@�1@���@��;@�ƨ@��w@��F@��@�|�@�;d@�o@��@��+@�^5@�M�@�M�@�E�@�J@��@���@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�yB�yB�yB�yB�yB�yB�yB�yB�yB�mB�mB�fB�fB�`B�fB�fB�fB�mB�mB�B(�Bn�Bp�Bm�Bm�BgmBcTBe`B^5B[#B\)B^5B`BBjBm�Bn�Bo�Bo�Bv�B�B{�Bw�Bw�B�B�+B�7B�B}�B{�Bz�B}�B�7B�{B��B�bBx�BZB,BF�BK�BK�BI�BG�BE�BC�BC�BC�BB�BC�BD�BF�BE�BC�B?}B9XB49B)�B �B�B�B�B�B�B\B%�B-B;dB?}B<jB-B&�B�BB�B�B�fB�#B��B�dB�?B�B��B��B�hB{�BgmBH�B6FB(�B&�B$�B#�B�BuB+B
��B
�B
�ZB
�B
ĜB
��B
�bB
�B
p�B
hsB
dZB
aHB
\)B
XB
N�B
;dB
.B
"�B
�B
JB	�sB	�
B	��B	��B	ɺB	ĜB	��B	�jB	�XB	�9B	�B	��B	��B	��B	��B	��B	�bB	�B	r�B	VB	B�B	49B	%�B	�B	+B	  B��B�B�mB�ZB�NB�HB�`B�fB�`B�TB�/B�B��B��B��BɺBŢBÖB��B��B�wB�wB��B��B��B��B�wB�}B�}B�qB�dB�RB�FB�?B�3B�-B�'B�!B�B�B��B��B��B��B��B��B��B��B��B�{B�VB�=B�1B�1B�%B�B|�Bx�Bu�Br�Bq�Bn�Bm�Bk�BiyBffBdZBaHB_;B]/B[#BZBXBW
BW
BW
BVBT�BS�BR�BQ�BQ�BQ�BP�BP�BQ�BQ�BP�BP�BP�BO�BN�BM�BJ�BI�BI�BG�BG�BD�BA�BF�BE�BE�BD�BE�BF�BH�BI�BI�BK�BK�BL�BK�BL�BL�BM�BK�BN�BQ�BXBXBZBe`Bm�B{�B~�B}�B�B�JB�=B�VB��B��B�B�B�B�B�!B�-B�9B�^B�}B��B��B��B��B��B��B��B��B�
B�B�B�
B�B�B�B�5B�BB�;B�/B�#B�B��B��B��B��B��B�B�HB�B�B�B��B��B��B	  B	  B	B	+B	DB	DB	\B	�B	�B	$�B	-B	5?B	A�B	E�B	F�B	H�B	J�B	K�B	K�B	I�B	H�B	F�B	G�B	J�B	N�B	Q�B	Q�B	Q�B	T�B	]/B	e`B	gmB	gmB	gmB	gmB	gmB	hsB	hsB	hsB	iyB	l�B	n�B	p�B	r�B	t�B	u�B	w�B	x�B	z�B	|�B	}�B	~�B	� B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�7B	�DB	�JB	�PB	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�9B	�?B	�?B	�?B	�?B	�FB	�FB	�FB	�LB	�LB	�RB	�XB	�^B	�dB	�dB	�dB	�jB	�wB	�wB	�wB	�wB	�wB	�wB	�}B	��B	��B	��B	��B	��B	B	ÖB	ĜB	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�yB�yB�yB�yB�yB�yB�yB�yB�B�mB�mB�fB�fB�`B�fB�fB�fB�mB�mB�B,Bs�Bv�Bn�Bo�BiyBdZBgmBaHB\)B]/B`BBffBl�Bn�Bo�Bq�Bw�B|�B�B|�Bz�B~�B�B�VB�DB�B~�B|�B|�B�B�DB��B��B��B� BiyB/BG�BL�BL�BJ�BI�BF�BD�BD�BD�BD�BE�BF�BH�BH�BF�BA�B<jB9XB/B!�B�B�B�B�B�BuB&�B/B?}BB�B?}B0!B+B �B
=B�B�B�yB�;B��B�qB�LB�!B�B��B��B�Bv�BQ�B<jB)�B'�B$�B%�B%�B�BPB
��B
��B
�sB
�HB
��B
�!B
��B
�VB
s�B
iyB
dZB
bNB
]/B
YB
XB
A�B
33B
%�B
�B
 �B	�B	�B	�
B	��B	��B	ȴB	ŢB	�wB	�dB	�FB	�9B	�B	��B	��B	��B	��B	��B	�DB	}�B	aHB	G�B	;dB	,B	 �B		7B	B��B��B�B�`B�ZB�`B�mB�mB�mB�sB�BB�#B��B��B��B��BǮBĜBÖBB��B��BBBÖBB��B��B��B��B��B�dB�LB�FB�?B�3B�-B�-B�!B�B�B��B��B��B��B��B��B��B��B��B�{B�VB�7B�7B�1B�%B�B{�By�Bs�Bt�Bq�Bn�Bm�Bk�Bm�BhsBcTBcTBaHB]/B\)B[#BZBYBW
BW
BVBVBW
BS�BR�BR�BR�BQ�BQ�BQ�BQ�BP�BQ�BP�BP�BO�BO�BN�BL�BK�BJ�BL�BK�BG�BG�BG�BH�BJ�BI�BJ�BJ�BK�BL�BK�BL�BL�BO�BN�BN�BN�BQ�BVBYBZB]/Be`Bl�B}�B�B}�B�B�bB�DB�PB��B�B�B�B�B�B�!B�-B�?B�dB��B��B��B��BBB��B��B��B�B�B�B�B�B�B�B�;B�TB�NB�5B�)B�B�B��B��B��B��B��B�5B�B�B�B��B��B��B	  B	  B	B	+B	DB	JB	oB	�B	�B	%�B	.B	6FB	A�B	F�B	G�B	J�B	L�B	L�B	L�B	J�B	J�B	H�B	H�B	J�B	O�B	R�B	R�B	Q�B	S�B	^5B	ffB	hsB	iyB	hsB	iyB	iyB	hsB	hsB	iyB	jB	l�B	o�B	q�B	s�B	t�B	v�B	w�B	y�B	{�B	|�B	}�B	~�B	� B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�=B	�DB	�JB	�PB	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�9B	�?B	�?B	�?B	�?B	�FB	�FB	�FB	�LB	�LB	�RB	�XB	�^B	�dB	�dB	�dB	�qB	�wB	�wB	�wB	�wB	�wB	�wB	�}B	��B	��B	��B	��B	B	B	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<��
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
<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447272012010314472720120103144727  AO  ARGQ                                                                        20111130142608  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142608  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144727  IP                  G�O�G�O�G�O�                