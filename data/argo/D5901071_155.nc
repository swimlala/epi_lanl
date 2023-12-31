CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:34Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142703  20190522121827  1727_5046_155                   2C  D   APEX                            2143                            040306                          846 @����W?�1   @�� �>�@5���R�c��\)1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD�fDE  DE� DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Dby�Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Dy�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C33C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0  C2  C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDB  DB�fDCfDC�fDDfDD��DEfDE�fDFfDF��DGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa��DbfDb� DcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs� Dy��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�C�A˼jA�v�A��AɼjAɇ+A��Aȧ�A�p�A���A���A�x�A�A�ȴA�l�A�K�A�=qAŕ�A�ĜAăA�1'AÝ�A��A�%A��A¥�A��jA�%A�A���A��
A��A��uA�t�A�9XA�=qA�(�A��9A�A�A�ZA��FA���A��A��yA��A�z�A�33A�A��A���A���A��A�M�A��A�1A�%A��A��;A��jA�~�A�VA�"�A�O�A��RA��PA�XA�-A���A��DA��A�{A���A��7A�9XA�Q�A�l�A��A�VA�?}A�JA�A��A�VA�JA�M�A�ĜA�;dA��DA�dZA���A�t�A���A��jA��wA���A�-A�ƨA�r�A��`A���A�7LA��RA��A��
A��A���A�t�A�ZA�5?A�/A�t�A�t�A��A��A�ĜA��mA�33A���A���A�jA�ffA{�AvZAt�`Aq%AiVAd�\AbȴAal�A_��AYl�AW�PAVM�AUƨAT��APZAO�wAOXAN�/AM�FAL�HAKVAI/AI"�AI�AHE�AGl�AFĜAE�wAD�RAC��ACoA@  A=��A<�A<M�A;A;C�A:I�A9�A8��A8{A7l�A6��A5ƨA4��A3\)A2�DA1��A0bA/&�A.M�A-�A-�^A-%A,jA,�A+?}A*A�A)t�A)/A)A'dZA&�A$^5A#��A"  A ��A �A�A��AbNAK�A$�A��Av�A�Ax�AI�A��A�Ax�A33A�At�A%A~�A�
A;dA�A��A�#A%A
�DA	%A�A�A��AXA��A�A�A�/Ar�A�
A��A �y@�C�@��!@���@�dZ@�A�@�^5@���@�j@��@��@���@�/@�r�@�V@�D@�K�@��@�@�@� �@�ƨ@�dZ@�
=@އ+@݉7@�C�@ڸR@�+@۝�@ە�@�S�@�/@��@�Ĝ@܃@۝�@�5?@�G�@Ұ!@��#@Гu@�  @�~�@͑h@�X@�Ĝ@�bN@�  @ˮ@�1'@�7L@�j@˕�@���@�E�@���@���@ɺ^@���@ɡ�@���@�(�@��@���@�V@�&�@ũ�@�E�@�{@���@�p�@�p�@��@���@���@ă@Ý�@�|�@°!@°!@�V@�^5@���@���@�A�@�J@��-@��!@�l�@�|�@��P@�|�@�+@�|�@�Z@��@�z�@�1@���@�K�@��@���@���@�o@�K�@�dZ@�J@�bN@�|�@�;d@��@�ff@�^5@�-@���@�j@��D@��@���@���@���@���@��H@��T@�I�@��@��P@��@�|�@�v�@��7@���@��9@�1'@���@�~�@�-@��@��7@��@��D@�9X@���@��@��-@���@�b@�b@�b@�t�@��@�X@���@�Z@�r�@�O�@�O�@�l�@���@�G�@���@��9@��@�Z@�Q�@�j@���@��@�A�@�(�@�1'@�1@�\)@�ff@���@��-@�7L@���@�dZ@�dZ@�l�@�l�@�K�@��H@���@�ȴ@���@���@��\@�M�@�J@���@��@���@��@�x�@�p�@�`B@�O�@�7L@��@�%@���@���@�%@�%@�%@���@���@���@��@��@���@���@�V@�/@�7L@�7L@�?}@�G�@�O�@�O�@�hs@�`B@�O�@�&�@���@���@�j@�Q�@�Q�@�1'@��@��P@�;d@��@�ȴ@���@��+@�V@��@���@�hs@��@�%@���@���@�Ĝ@��j@��9@���@��u@�r�@�bN@�Q�@�b@�ƨ@�C�@�S�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�C�A˼jA�v�A��AɼjAɇ+A��Aȧ�A�p�A���A���A�x�A�A�ȴA�l�A�K�A�=qAŕ�A�ĜAăA�1'AÝ�A��A�%A��A¥�A��jA�%A�A���A��
A��A��uA�t�A�9XA�=qA�(�A��9A�A�A�ZA��FA���A��A��yA��A�z�A�33A�A��A���A���A��A�M�A��A�1A�%A��A��;A��jA�~�A�VA�"�A�O�A��RA��PA�XA�-A���A��DA��A�{A���A��7A�9XA�Q�A�l�A��A�VA�?}A�JA�A��A�VA�JA�M�A�ĜA�;dA��DA�dZA���A�t�A���A��jA��wA���A�-A�ƨA�r�A��`A���A�7LA��RA��A��
A��A���A�t�A�ZA�5?A�/A�t�A�t�A��A��A�ĜA��mA�33A���A���A�jA�ffA{�AvZAt�`Aq%AiVAd�\AbȴAal�A_��AYl�AW�PAVM�AUƨAT��APZAO�wAOXAN�/AM�FAL�HAKVAI/AI"�AI�AHE�AGl�AFĜAE�wAD�RAC��ACoA@  A=��A<�A<M�A;A;C�A:I�A9�A8��A8{A7l�A6��A5ƨA4��A3\)A2�DA1��A0bA/&�A.M�A-�A-�^A-%A,jA,�A+?}A*A�A)t�A)/A)A'dZA&�A$^5A#��A"  A ��A �A�A��AbNAK�A$�A��Av�A�Ax�AI�A��A�Ax�A33A�At�A%A~�A�
A;dA�A��A�#A%A
�DA	%A�A�A��AXA��A�A�A�/Ar�A�
A��A �y@�C�@��!@���@�dZ@�A�@�^5@���@�j@��@��@���@�/@�r�@�V@�D@�K�@��@�@�@� �@�ƨ@�dZ@�
=@އ+@݉7@�C�@ڸR@�+@۝�@ە�@�S�@�/@��@�Ĝ@܃@۝�@�5?@�G�@Ұ!@��#@Гu@�  @�~�@͑h@�X@�Ĝ@�bN@�  @ˮ@�1'@�7L@�j@˕�@���@�E�@���@���@ɺ^@���@ɡ�@���@�(�@��@���@�V@�&�@ũ�@�E�@�{@���@�p�@�p�@��@���@���@ă@Ý�@�|�@°!@°!@�V@�^5@���@���@�A�@�J@��-@��!@�l�@�|�@��P@�|�@�+@�|�@�Z@��@�z�@�1@���@�K�@��@���@���@�o@�K�@�dZ@�J@�bN@�|�@�;d@��@�ff@�^5@�-@���@�j@��D@��@���@���@���@���@��H@��T@�I�@��@��P@��@�|�@�v�@��7@���@��9@�1'@���@�~�@�-@��@��7@��@��D@�9X@���@��@��-@���@�b@�b@�b@�t�@��@�X@���@�Z@�r�@�O�@�O�@�l�@���@�G�@���@��9@��@�Z@�Q�@�j@���@��@�A�@�(�@�1'@�1@�\)@�ff@���@��-@�7L@���@�dZ@�dZ@�l�@�l�@�K�@��H@���@�ȴ@���@���@��\@�M�@�J@���@��@���@��@�x�@�p�@�`B@�O�@�7L@��@�%@���@���@�%@�%@�%@���@���@���@��@��@���@���@�V@�/@�7L@�7L@�?}@�G�@�O�@�O�@�hs@�`B@�O�@�&�@���@���@�j@�Q�@�Q�@�1'@��@��P@�;d@��@�ȴ@���@��+@�V@��@���@�hs@��@�%@���@���@�Ĝ@��j@��9@���@��u@�r�@�bN@�Q�@�b@�ƨ@�C�@�S�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	7BBB��B��BBBB��B  BBB%B
=BDBoB!�B"�B�B!�B�BhB�BT�Bt�Bw�BdZBL�BW
By�Br�BZB8RB#�B,BA�BZB[#B]/B_;B^5B`BBbNBn�Bq�Bt�Bz�B}�B~�B�B�JB��B��B�B�B�B�!B�'B�3B�XB��B�NBBB  B��B�;B��B��BÖB�FB��B��B��B��B�bB�By�BbNBS�BJ�BB�B<jB9XB,B#�B�B
=B%BB��B��B�B�ZB�B��B��B��B��BǮBÖB�dB�'B��B�uB�bB�JB�7B�Br�BA�B�BbB
��B
�/B
�qB
�B
��B
|�B
T�B
1'B
+B	�NB	��B	�B	~�B	dZB	W
B	H�B	8RB	uB	%B	B��B�B�sB��B��B��B��B�B�B�B�yB�sB�`B�NB�;B�B��B��BƨB�LB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�VB�JB�PB�PB�7B�+B�B�Bx�Bu�Bs�Bp�Bp�Bo�Bo�Bo�Bp�Bp�Bo�Bn�Bl�Bn�Bo�Bo�Bp�Bp�Bp�Bn�Bm�BjBhsBbNB]/BYBXBW
BW
BVBS�BS�BT�BW
B_;Be`BffBe`BdZBaHB`BB_;B_;BaHB`BB^5B[#BZBXBT�BO�BO�BQ�BN�BM�BN�BO�BS�BW
BXB[#BbNBl�Bs�Bx�Bz�B}�B�bB�{B�{B�{B��B��B�JB�+B�hB�\B�VB�DB�DB�=B�PB�\B��B��B��B�B�9B�XB�qB�}B��B��BÖBŢBǮBŢB��B�jB�^B�^B��B��B�
B�B�/B�BB�`B�B�B�B�B�B��B��B��B	B	1B		7B	B��B��B	B	�B	!�B	$�B	)�B	-B	,B	2-B	9XB	?}B	@�B	C�B	C�B	D�B	E�B	H�B	M�B	R�B	XB	\)B	ZB	YB	[#B	\)B	]/B	^5B	dZB	ffB	l�B	z�B	{�B	{�B	|�B	|�B	�B	�7B	�VB	�=B	�+B	�=B	�VB	�bB	�hB	�hB	�bB	�\B	�\B	�PB	�7B	�1B	�+B	�+B	�+B	�%B	�%B	�%B	�B	�B	�B	�B	�B	�B	�B	}�B	q�B	p�B	o�B	o�B	s�B	w�B	v�B	m�B	`BB	YB	XB	XB	YB	\)B	^5B	aHB	dZB	ffB	gmB	iyB	jB	jB	jB	l�B	p�B	r�B	r�B	t�B	y�B	z�B	{�B	{�B	}�B	�B	�B	�B	�B	�B	�B	�7B	�DB	�JB	�PB	�\B	�bB	�hB	�hB	�oB	�oB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�'B	�3B	�9B	�?B	�?B	�FB	�LB	�RB	�^B	�jB	�jB	�qB	�wB	��B	ÖB	ÖB	ĜB	ƨB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B%BB  B  BBBBBB+B%B+BJBJBuB%�B&�B�B#�B�BuB�BT�Bu�B{�BgmBM�BW
B}�Bz�BaHB=qB$�B,BA�B[#B]/B`BBaHB^5B`BBdZBo�Br�Bu�B{�B}�B� B�%B�PB��B��B�B�B�B�!B�-B�9B�dB��B�HBBBB��B�NB�B��BȴB�jB�B��B��B��B��B�B�BgmBXBO�BE�B=qB<jB/B&�B�BbB	7BB  B��B�B�B�/B�
B��B��B��B��BƨB�}B�XB��B�{B�hB�PB�=B�DB�BM�B&�B�B
��B
�`B
��B
�'B
�B
�%B
]/B
<jB
hB	�`B	�B	B	�=B	iyB	[#B	M�B	I�B	�B	
=B	B	  B��B�B��B	  B��B��B��B�B�B�yB�B�mB�ZB�NB�/B�
B��B��B�qB�'B�B�B�B�B�B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�VB�oB�hB�DB�DB�1B�%B�By�Bz�Bt�Bq�Bp�Br�Bq�Bp�Bp�Bp�Bp�Bo�Bo�Bq�Bq�Br�Bq�Bq�Bp�Bo�Bl�Bl�BffBaHB[#BZBYBZBW
BVBT�BW
BXBaHBiyBgmBgmBhsBffBcTB`BBaHBe`BcTB`BB^5B[#B[#BXBQ�BS�BW
BP�BN�BO�BP�BT�BXBYB^5BcTBl�Bs�Bx�Bz�B{�B�bB��B�{B��B��B��B�bB�1B�uB�bB�bB�JB�DB�DB�VB�bB��B��B��B�B�?B�^B�wB��B��B��BÖBŢBȴBƨBĜB�wB�^B�^B��B��B�
B�B�5B�BB�fB�B�B�B�B�B��B��B��B	B	DB	VB	+B	B��B	  B	�B	!�B	$�B	)�B	.B	,B	1'B	8RB	@�B	A�B	D�B	C�B	D�B	E�B	H�B	M�B	R�B	XB	^5B	\)B	ZB	[#B	\)B	^5B	^5B	dZB	e`B	iyB	z�B	{�B	{�B	|�B	z�B	�B	�7B	�bB	�PB	�1B	�=B	�VB	�hB	�uB	�uB	�hB	�\B	�bB	�bB	�7B	�7B	�1B	�1B	�1B	�+B	�+B	�+B	�+B	�+B	�+B	�B	�B	�B	�B	�+B	s�B	q�B	p�B	o�B	r�B	w�B	z�B	t�B	e`B	YB	XB	XB	YB	\)B	^5B	aHB	dZB	ffB	gmB	iyB	jB	k�B	k�B	m�B	p�B	s�B	t�B	u�B	y�B	z�B	{�B	{�B	~�B	�B	�B	�B	�B	�B	�B	�7B	�DB	�JB	�PB	�\B	�bB	�hB	�hB	�oB	�oB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�3B	�9B	�?B	�?B	�LB	�RB	�XB	�dB	�jB	�jB	�qB	�wB	��B	ĜB	ÖB	ŢB	ƨB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�t�<D��<#�
<#�
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
<���<49X<#�
<#�
<#�
<�C�<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447282012010314472820120103144728  AO  ARGQ                                                                        20111130142703  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142703  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144728  IP                  G�O�G�O�G�O�                