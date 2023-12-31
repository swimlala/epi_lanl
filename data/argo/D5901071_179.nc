CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:41Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143205  20190522121827  1727_5046_179                   2C  D   APEX                            2143                            040306                          846 @�����1   @��hK��@4��1'�c��t�j1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?� D@  D@� DAfDA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Dh��Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dss3Dy�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@�33A��A!��AA��Aa��A�  A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8��B@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C33C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD33CF33CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fD  D� DfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD� DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>��D?fD?�fD@fD@�fDA�DA��DBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN��DO�DO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDi  Di�fDj�Dj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDsy�Dyٚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AٶFAٛ�A�l�A��AضFAأ�Aؗ�AؑhA؇+A؁A�x�A�r�A�p�A�n�A�hsA�ffA�dZA�dZA�`BA�`BA�`BA�`BA�`BA�\)A�XA�XA�ZA�VA�S�A�VA�?}A�bAԝ�A�1A���Ȧ+A��A�E�A�I�A��yA��;A�A�=qA��A���A��A��A�I�A�-A���A�O�A�O�A���A�ĜA���A���A�9XA���A��A�=qA���A���A���A��wA�I�A�+A�1A�dZA���A�A���A��FA���A��A��A�ƨA�`BA��jA��uA��HA��A�oA�ZA��-A���A�;dA���A���A�1'A��A�`BA�+A���A���A��A�v�A���A�r�A���A�O�A���A�$�A�5?A��-A�A��hA��HA�t�A�ƨA��A~ȴA|�Az�DAv�Au�At�uAq�;Ap�Ao��Al��AiAd�9A`(�AZ�AY�AX$�AV�RAT��ARJAQ;dAOl�AK"�AI�hAI�AHjAG|�AE+ADbNADbACG�AA�FA@��A?��A?
=A<�/A9��A9
=A8��A7�A6ĜA5t�A3��A21A1oA0z�A/\)A/�A/�A/oA.��A+�hA)��A(�A'�;A&��A&��A%�;A$��A"�`A!�;A!��A!XA!\)A!;dA!/A!�A!�A!oA!�A ��A ffA�;A=qA`BA�jAM�AK�AVA�AȴAoAv�A�PA��An�A?}A�9A�#A�A��A�AdZA�A
��A	K�A�#A�uAG�Ax�A�`A$�A�mA�PAl�A/A ��A 1'@�~�@��^@��-@���@�x�@�I�@�M�@�V@��@�o@�bN@�;d@��@�v�@�ff@�=q@���@�1'@�F@�l�@�$�@웦@�ƨ@ꗍ@�@蛦@�
=@�-@�z�@�w@��@�7L@�b@߾w@�;d@�O�@��y@ٙ�@�O�@ؼj@�l�@���@�^5@�@���@���@Л�@�@�G�@��@˥�@�@�v�@��H@�E�@�`B@�r�@�v�@��@��T@�O�@���@�5?@���@�p�@��@�1'@��;@�
=@�M�@�^5@��@��@�33@�M�@��@��@�V@���@��-@��@�
=@�~�@��T@��@���@�Z@�  @�l�@���@��#@�z�@���@�(�@�v�@���@�{@���@�hs@�O�@���@�I�@��@�@�dZ@��@�ff@��#@���@���@�@��-@�X@�V@��/@�Q�@��;@�t�@��@��y@��!@��+@�ff@�M�@���@��h@�7L@��@���@��@�Z@�9X@�9X@�A�@�I�@�Z@�j@�j@�bN@�(�@�  @�K�@�~�@�J@��-@��@��@��u@�I�@� �@��@�t�@�33@�J@�7L@��9@�Z@�b@��;@���@�dZ@�l�@���@��;@���@�l�@�o@��@��@�M�@��T@���@��7@�V@��@�Q�@��;@���@��@�S�@�"�@��R@��!@��!@��!@�V@��7@�X@��@���@��j@�bN@�b@��
@���@��@�dZ@�33@�ȴ@�5?@��-@���@���@�hs@�?}@�%@��/@��u@��@���@��@���@���@���@�~�@�v�@�^5@�E�@�$�@��#@�p�@��@��`@�I�@��@�|�@�t�@�\)@�"�@��!@�@�`B@��@��j@�bN@��@��@��+@�O�@��@�Ĝ@��9@�Z@�A�@�1@�+@��+@���@�E�@�M�@���@��h@�p�@�/@���@���@��9@�z�@�Z@�A�@�(�@��@�1@��
@���@�K�@�+@��@�
=@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AٶFAٛ�A�l�A��AضFAأ�Aؗ�AؑhA؇+A؁A�x�A�r�A�p�A�n�A�hsA�ffA�dZA�dZA�`BA�`BA�`BA�`BA�`BA�\)A�XA�XA�ZA�VA�S�A�VA�?}A�bAԝ�A�1A���Ȧ+A��A�E�A�I�A��yA��;A�A�=qA��A���A��A��A�I�A�-A���A�O�A�O�A���A�ĜA���A���A�9XA���A��A�=qA���A���A���A��wA�I�A�+A�1A�dZA���A�A���A��FA���A��A��A�ƨA�`BA��jA��uA��HA��A�oA�ZA��-A���A�;dA���A���A�1'A��A�`BA�+A���A���A��A�v�A���A�r�A���A�O�A���A�$�A�5?A��-A�A��hA��HA�t�A�ƨA��A~ȴA|�Az�DAv�Au�At�uAq�;Ap�Ao��Al��AiAd�9A`(�AZ�AY�AX$�AV�RAT��ARJAQ;dAOl�AK"�AI�hAI�AHjAG|�AE+ADbNADbACG�AA�FA@��A?��A?
=A<�/A9��A9
=A8��A7�A6ĜA5t�A3��A21A1oA0z�A/\)A/�A/�A/oA.��A+�hA)��A(�A'�;A&��A&��A%�;A$��A"�`A!�;A!��A!XA!\)A!;dA!/A!�A!�A!oA!�A ��A ffA�;A=qA`BA�jAM�AK�AVA�AȴAoAv�A�PA��An�A?}A�9A�#A�A��A�AdZA�A
��A	K�A�#A�uAG�Ax�A�`A$�A�mA�PAl�A/A ��A 1'@�~�@��^@��-@���@�x�@�I�@�M�@�V@��@�o@�bN@�;d@��@�v�@�ff@�=q@���@�1'@�F@�l�@�$�@웦@�ƨ@ꗍ@�@蛦@�
=@�-@�z�@�w@��@�7L@�b@߾w@�;d@�O�@��y@ٙ�@�O�@ؼj@�l�@���@�^5@�@���@���@Л�@�@�G�@��@˥�@�@�v�@��H@�E�@�`B@�r�@�v�@��@��T@�O�@���@�5?@���@�p�@��@�1'@��;@�
=@�M�@�^5@��@��@�33@�M�@��@��@�V@���@��-@��@�
=@�~�@��T@��@���@�Z@�  @�l�@���@��#@�z�@���@�(�@�v�@���@�{@���@�hs@�O�@���@�I�@��@�@�dZ@��@�ff@��#@���@���@�@��-@�X@�V@��/@�Q�@��;@�t�@��@��y@��!@��+@�ff@�M�@���@��h@�7L@��@���@��@�Z@�9X@�9X@�A�@�I�@�Z@�j@�j@�bN@�(�@�  @�K�@�~�@�J@��-@��@��@��u@�I�@� �@��@�t�@�33@�J@�7L@��9@�Z@�b@��;@���@�dZ@�l�@���@��;@���@�l�@�o@��@��@�M�@��T@���@��7@�V@��@�Q�@��;@���@��@�S�@�"�@��R@��!@��!@��!@�V@��7@�X@��@���@��j@�bN@�b@��
@���@��@�dZ@�33@�ȴ@�5?@��-@���@���@�hs@�?}@�%@��/@��u@��@���@��@���@���@���@�~�@�v�@�^5@�E�@�$�@��#@�p�@��@��`@�I�@��@�|�@�t�@�\)@�"�@��!@�@�`B@��@��j@�bN@��@��@��+@�O�@��@�Ĝ@��9@�Z@�A�@�1@�+@��+@���@�E�@�M�@���@��h@�p�@�/@���@���@��9@�z�@�Z@�A�@�(�@��@�1@��
@���@�K�@�+@��@�
=@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B�JB~�B|�B~�B�oB�uB��B��B��B�B�B�RB�LB��BÖBǮB��BÖB��B�RB�-B��B��B��B�B��B��B�+Bx�Be`BD�B{BJB1BB��B�;B��BĜB�!B��B�B��B��B��B�B��B�hB�Bv�Bs�Bu�B�%B�VB��B��B��B�DB�B�By�Be`BT�BG�B=qB+B\B
��B
�BB
ĜB
��B
�VB
~�B
r�B
ffB
H�B
:^B
1'B
,B
�B
PB	��B	�B	�;B	��B	�qB	�'B	�oB	t�B	N�B	#�B	PB	
=B	1B	B��B�B�B�B�`B�ZB�`B�ZB�B�sB�sB�B�B�B�B�yB�`B�)B�B��B��B��BɺBŢBŢB�jB�^B�RB�LB�LB�LB�?B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�\B�PB�JB�DB�7B�+B�%B�B�B~�B|�B{�By�Bw�Bv�Bt�Br�Br�Bp�Bm�Bn�Bm�Bl�BiyBe`BbNBaHBbNBaHBaHBaHBaHB`BB`BB`BB`BBe`Be`Be`Be`BdZBaHB_;B^5B]/B[#B]/B^5B^5B^5B^5B_;BaHBaHBbNBaHB`BB`BB_;B`BB`BB_;B`BB`BB`BBaHB`BBbNBcTBbNB`BB^5B_;BbNBaHBaHBcTBdZBdZBcTBcTBdZBjBk�Bo�Bq�Bp�Bn�Bs�Bw�By�Bz�Bz�Bx�B{�B{�B{�Bx�Bx�B|�B}�B�B�B�B�+B�\B�\B�PB�DB�PB�bB��B��B�B�FB��B�/B�5B�BB�TB��B	1B	JB	VB	hB	{B	�B	�B	�B	 �B	 �B	)�B	1'B	6FB	8RB	9XB	<jB	=qB	=qB	>wB	D�B	G�B	H�B	K�B	L�B	K�B	K�B	L�B	M�B	N�B	S�B	XB	ZB	[#B	^5B	_;B	_;B	`BB	aHB	aHB	bNB	aHB	aHB	aHB	bNB	dZB	e`B	ffB	ffB	gmB	hsB	hsB	l�B	o�B	p�B	r�B	q�B	q�B	q�B	s�B	u�B	x�B	x�B	{�B	�1B	�=B	�DB	�PB	�PB	�\B	�bB	�bB	�hB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�LB	�RB	�RB	�RB	�qB	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�;B	�HB	�NB	�NB	�TB	�`B	�`B	�fB	�fB	�fB	�fB	�yB	�yB	�yB	�sB	�mB	�sB	�mB	�TB	�BB	�;B	�BB	�NB	�NB	�TB	�NB	�;B	�5B	�BB	�TB	�ZB	�ZB	�TB	�TB	�ZB	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�'B��B�1B�B�%B�{B��B��B�B�B�!B�9B�XB�^BÖBŢB��B��BŢBÖB�^B�?B�B��B��B�B��B��B�7B|�Bl�BQ�B�BPB	7B+B��B�TB�#B��B�?B��B�!B��B��B��B�B��B��B�7B~�B{�By�B�7B�\B��B��B��B�\B�B�B�Bn�BYBL�BC�B49B{B
��B
�fB
��B
�B
�hB
�B
u�B
q�B
K�B
?}B
33B
2-B
!�B
�B	��B	�B	�fB	��B	��B	�XB	��B	�B	[#B	33B	hB	PB	JB	+B	  B��B��B��B�B�fB�mB�mB�B�B�yB�B��B�B�B�B�B�ZB�B��B��B��B��B��BɺB�}B�jB�dB�RB�LB�LB�LB�RB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB��B�bB�VB�PB�PB�DB�7B�1B�+B�B� B}�B{�B{�Bx�Bw�Bw�Bu�Bs�Bs�Bs�Br�Bq�Bn�BjBgmBgmBdZBcTBbNBbNBaHBaHBaHBaHBbNBffBe`Be`Be`BffBdZBaHB`BB_;B_;B_;B_;B^5B^5B_;BaHBbNBbNBcTBcTBcTBbNBaHBbNBbNBbNBbNBcTBbNBcTBdZBdZBdZBdZBdZBcTBbNBcTBbNBdZBdZBe`Be`Be`BffBjBm�Bn�Bp�Bq�Bp�Bm�Bs�Bx�B{�B|�B}�By�B{�B|�B~�By�By�B|�B~�B�B�B�B�1B�\B�bB�\B�JB�\B�uB��B��B�-B�'B��B�/B�;B�HB�BB��B		7B	PB	\B	oB	�B	�B	�B	�B	#�B	 �B	+B	2-B	6FB	8RB	:^B	=qB	>wB	>wB	>wB	D�B	H�B	I�B	K�B	L�B	K�B	K�B	M�B	M�B	N�B	T�B	YB	[#B	\)B	^5B	_;B	_;B	`BB	aHB	bNB	bNB	bNB	aHB	bNB	cTB	dZB	e`B	ffB	ffB	gmB	hsB	hsB	l�B	o�B	p�B	r�B	r�B	s�B	r�B	t�B	v�B	x�B	y�B	|�B	�1B	�=B	�JB	�VB	�\B	�hB	�hB	�hB	�oB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�9B	�LB	�RB	�RB	�XB	�}B	��B	B	ĜB	ŢB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�BB	�NB	�NB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�sB	�B	�yB	�B	�yB	�sB	�yB	�B	�`B	�HB	�;B	�BB	�TB	�NB	�ZB	�ZB	�BB	�;B	�BB	�TB	�ZB	�`B	�TB	�ZB	�ZB	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<�t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<D��<u<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447372012010314473720120103144737  AO  ARGQ                                                                        20111130143205  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143205  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144737  IP                  G�O�G�O�G�O�                