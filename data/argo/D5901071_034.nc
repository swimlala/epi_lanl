CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:00Z UW 3.1 conversion   
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g$   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               "A   AO  20111130135826  20190522121825  1727_5046_034                   2C  D   APEX                            2143                            040306                          846 @�A�	��1   @�A��u�@7b�\(���c����m1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D�fD  Dy�D  D� DfD� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D;��D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� DpfDp� Dp��Dq� Dr  Dr� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @9��@�  @�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBp��Bx��B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD��DfD� DfD�fD�D�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD  D�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<  D<� D=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJ�DJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDo�Do�fDp�Dp�fDq  Dq�fDrfDr�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�z�A�z�A�x�A�z�A�t�A�z�AԁAԉ7AԍPAԕ�Aԕ�Aԗ�AԓuAԕ�Aԥ�Aԣ�Aԟ�AԬAԴ9AԼjA���A�A���AԶFAԬAԛ�A�\)A��A�K�A���Aѩ�A�n�A� �A�
=A�bAŝ�A�  A�A���A��yA�hsA���A��A���A�A�S�A�-A��;A�ȴA���A�S�A���A�&�A�bNA���A�dZA���A�l�A�VA�hsA�&�A�t�A���A��A���A�VA�  A��wA�ffA���A���A���A���A�7LA�I�A��#A�33A�ffA���A��wA�dZA���A���A��A�C�A���A�JA�~�A���A�VA�|�A���A�VA��A��A�-A��A�S�A��A���A���A��A��A�l�A�1'A���A��/A��A���A��;A��A�XA���A�=qA�~�A��!A�z�A~ĜA~^5A|�A{��Ay��Aw�Au�At��AsO�Aq��Ap�Ao�;An�RAn=qAm�
Am33Al�yAl�RAl{AkoAj�!Ah�HAf�Ad��AcO�Ab�yAb$�A`��A_XA^�A]A]K�A]/A\��A\5?AZn�AX��AV�9AS�
AQ��AQt�AQ
=AO�AM��AK�;AJAG��AFv�AE�TAEhsAE33AC��AAXA?\)A>$�A=|�A=/A<��A:��A9/A8(�A7+A6��A5��A4bA2M�A2A1A/��A.��A.��A-ƨA,��A+l�A*�A)hsA)
=A(ĜA'��A&��A%�TA$�!A#��A#��A#�A"  A!x�A ��A�Ap�Al�A��A7LAE�A��AG�AjAp�A��A�
AVA�AhsA�RA��A�HAVA�A��A�hA7LA�A��A
��A
^5A
 �A	��A�+AO�A9XAQ�A?}A%A�+A�
AO�A ��A 1@�V@�p�@�?}@��@��T@��@�l�@���@��@���@�-@��@�j@�r�@��;@�F@�|�@��H@�
=@�M�@�p�@�"�@��@�7@�v�@��T@���@�hs@��@�p�@�t�@݉7@�A�@�1'@ܬ@�
=@أ�@ם�@֏\@�p�@ԃ@���@�v�@љ�@��@Ο�@̛�@�Q�@�I�@��@��@��@��@���@ˮ@�1'@̋D@�1'@�dZ@ʟ�@�ff@ʧ�@���@ɩ�@�7L@��@ȃ@��
@�@�$�@�z�@�|�@\@�&�@���@��D@��h@�`B@���@��@��y@��+@���@���@�A�@��@�l�@�v�@��#@�/@��@�Z@�b@�+@�v�@�5?@�@��^@��@��9@�@���@���@���@�K�@��y@�^5@�@��@���@�1'@�+@���@�ff@�{@���@�hs@�G�@�&�@���@�(�@��@�o@�$�@���@�x�@��`@�9X@�;d@��@��@���@�x�@��@��u@�Q�@�Q�@�1'@��@�"�@�M�@�J@���@��@��@�Z@���@��w@��@�dZ@�33@�"�@��H@�n�@���@�/@��`@��9@��D@�Z@�I�@�b@���@�t�@�S�@�K�@�C�@�
=@��R@�~�@�$�@��-@�O�@��@�Q�@�Z@�A�@��w@��@�|�@�|�@�t�@�;d@��R@��@�5?@��@�{@�J@��@��^@��@���@��`@��`@��/@��@�1@�ƨ@�t�@��@�M�@�{@�J@���@���@��^@��7@�O�@�&�@��`@���@�I�@�  @��
@�l�@�S�@�;d@�+@���@���@���@��\@�ff@�$�@�J@��@���@���@�x�@�X@�%@���@�1'@��
@��w@��@��P@�t�@�S�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�z�A�z�A�x�A�z�A�t�A�z�AԁAԉ7AԍPAԕ�Aԕ�Aԗ�AԓuAԕ�Aԥ�Aԣ�Aԟ�AԬAԴ9AԼjA���A�A���AԶFAԬAԛ�A�\)A��A�K�A���Aѩ�A�n�A� �A�
=A�bAŝ�A�  A�A���A��yA�hsA���A��A���A�A�S�A�-A��;A�ȴA���A�S�A���A�&�A�bNA���A�dZA���A�l�A�VA�hsA�&�A�t�A���A��A���A�VA�  A��wA�ffA���A���A���A���A�7LA�I�A��#A�33A�ffA���A��wA�dZA���A���A��A�C�A���A�JA�~�A���A�VA�|�A���A�VA��A��A�-A��A�S�A��A���A���A��A��A�l�A�1'A���A��/A��A���A��;A��A�XA���A�=qA�~�A��!A�z�A~ĜA~^5A|�A{��Ay��Aw�Au�At��AsO�Aq��Ap�Ao�;An�RAn=qAm�
Am33Al�yAl�RAl{AkoAj�!Ah�HAf�Ad��AcO�Ab�yAb$�A`��A_XA^�A]A]K�A]/A\��A\5?AZn�AX��AV�9AS�
AQ��AQt�AQ
=AO�AM��AK�;AJAG��AFv�AE�TAEhsAE33AC��AAXA?\)A>$�A=|�A=/A<��A:��A9/A8(�A7+A6��A5��A4bA2M�A2A1A/��A.��A.��A-ƨA,��A+l�A*�A)hsA)
=A(ĜA'��A&��A%�TA$�!A#��A#��A#�A"  A!x�A ��A�Ap�Al�A��A7LAE�A��AG�AjAp�A��A�
AVA�AhsA�RA��A�HAVA�A��A�hA7LA�A��A
��A
^5A
 �A	��A�+AO�A9XAQ�A?}A%A�+A�
AO�A ��A 1@�V@�p�@�?}@��@��T@��@�l�@���@��@���@�-@��@�j@�r�@��;@�F@�|�@��H@�
=@�M�@�p�@�"�@��@�7@�v�@��T@���@�hs@��@�p�@�t�@݉7@�A�@�1'@ܬ@�
=@أ�@ם�@֏\@�p�@ԃ@���@�v�@љ�@��@Ο�@̛�@�Q�@�I�@��@��@��@��@���@ˮ@�1'@̋D@�1'@�dZ@ʟ�@�ff@ʧ�@���@ɩ�@�7L@��@ȃ@��
@�@�$�@�z�@�|�@\@�&�@���@��D@��h@�`B@���@��@��y@��+@���@���@�A�@��@�l�@�v�@��#@�/@��@�Z@�b@�+@�v�@�5?@�@��^@��@��9@�@���@���@���@�K�@��y@�^5@�@��@���@�1'@�+@���@�ff@�{@���@�hs@�G�@�&�@���@�(�@��@�o@�$�@���@�x�@��`@�9X@�;d@��@��@���@�x�@��@��u@�Q�@�Q�@�1'@��@�"�@�M�@�J@���@��@��@�Z@���@��w@��@�dZ@�33@�"�@��H@�n�@���@�/@��`@��9@��D@�Z@�I�@�b@���@�t�@�S�@�K�@�C�@�
=@��R@�~�@�$�@��-@�O�@��@�Q�@�Z@�A�@��w@��@�|�@�|�@�t�@�;d@��R@��@�5?@��@�{@�J@��@��^@��@���@��`@��`@��/@��@�1@�ƨ@�t�@��@�M�@�{@�J@���@���@��^@��7@�O�@�&�@��`@���@�I�@�  @��
@�l�@�S�@�;d@�+@���@���@���@��\@�ff@�$�@�J@��@���@���@�x�@�X@�%@���@�1'@��
@��w@��@��P@�t�@�S�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBS�BS�BS�BS�BR�BS�BT�BVBXB[#BZB[#BZB\)BaHB`BB_;BbNBe`BiyBn�Br�Bv�Bx�By�By�B{�B�B�B�?B�LBȴB�yBǮBɺB�mB�`B�5B��BÖB��B�^B�qBĜBȴB��B�}B�wB�qB�qB�dB�}BŢBƨBBBǮB��B��B�B�sB�B�B�B�B�B�mB�sB�B�B�ZB��B�9B��B��B�oB�VB�Bm�BbNBQ�BJ�BA�B>wB9XB1'B$�BhBB��B�fB��B�wB�B��B�oB�JB�Bp�BjBXB2-B�BoBVB%B
�B
�5B
��B
ǮB
�FB
�B
��B
��B
�{B
�JB
~�B
r�B
n�B
gmB
`BB
W
B
K�B
C�B
<jB
6FB
/B
)�B
&�B
!�B
�B
�B
�B
�B
uB
\B
1B
B	��B	�B	�HB	�B	�B	��B	ɺB	ÖB	��B	�jB	�XB	�RB	�?B	�B	��B	��B	�VB	}�B	q�B	r�B	n�B	gmB	\)B	P�B	D�B	?}B	8RB	<jB	:^B	7LB	-B	�B	JB	B	B	B	B��B�B�yB�fB�B�HB��BĜB�}B�qB�XB�?B�3B�B�B��B��B��B��B��B��B��B��B�{B�oB�hB�\B�VB�PB�DB�7B�%B�B�B�B�B� B~�B|�B|�B{�Bz�Bz�B{�By�Bx�Bw�Bx�Bv�Br�Bm�Bq�Bv�Bu�Br�Bp�Br�Bs�Br�Bm�BhsBbNBZBT�BS�BQ�BP�BP�BP�BJ�BG�BF�BH�BF�BE�BE�BD�BC�BB�BB�BB�BA�BA�B@�BB�BE�BO�BR�BT�BVBQ�BF�B<jB:^B2-B1'B6FB@�BH�BI�BD�B?}B=qBH�BO�BM�BK�BI�BH�BI�BM�BT�BR�BP�BN�BK�BJ�BR�Bo�Bz�B� B|�Bt�Bv�By�B� B�B�%B�1B�=B�VB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�'B�-B�?B�?B�FB�LB�LB�?B�3B�^B��BƨBȴBɺBɺBɺBɺBɺBĜBŢBɺB��B��B��B��B��B��B��B�B�B�B�#B�5B�;B�HB�NB�NB�ZB�mB�yB�B�B�B�B��B��B	  B	  B	1B	JB	JB	PB	bB	hB	hB	uB	�B	�B	�B	!�B	!�B	!�B	$�B	-B	1'B	2-B	5?B	5?B	5?B	6FB	8RB	;dB	?}B	B�B	D�B	E�B	F�B	G�B	H�B	I�B	K�B	M�B	N�B	N�B	O�B	P�B	S�B	T�B	W
B	ZB	]/B	bNB	cTB	ffB	iyB	l�B	m�B	m�B	n�B	o�B	o�B	o�B	r�B	x�B	}�B	~�B	~�B	�B	�B	�+B	�7B	�=B	�=B	�=B	�JB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�3B	�9B	�FB	�RB	�XB	�^B	�^B	�dB	�jB	�qB	�wB	��B	��B	ŢB	ȴB	ȴB	ɺB	��B	��B	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BS�BS�BS�BS�BR�BS�BT�BVBXB[#BZB[#BZB\)BaHB`BB_;BbNBe`BiyBn�Br�Bv�Bx�By�Bz�B}�B�DB�B�FB�XB��B�B��B��B�yB�yB�B�BŢBB�wB�}BǮB��BƨB��B�wB�wB�wB�qBÖBɺB��BÖBƨB��B��B��B�#B�B�B�B�B�B�B�sB�B�B�B�yB��B�^B��B��B��B�oB�DBq�BhsBVBM�BC�B@�B<jB5?B-B�BB��B�B��BŢB�3B��B�{B�\B�7Bq�Bm�BcTB9XB�BuBbBPB
�B
�HB
�B
��B
�^B
�!B
�B
��B
��B
�oB
�B
s�B
r�B
k�B
e`B
]/B
Q�B
G�B
@�B
<jB
33B
,B
+B
#�B
�B
�B
�B
�B
�B
oB

=B
	7B	��B	�B	�`B	�#B	�B	�
B	��B	ŢB	ĜB	�qB	�XB	�XB	�RB	�?B	�B	��B	��B	�B	r�B	s�B	q�B	k�B	aHB	T�B	I�B	C�B	9XB	=qB	;dB	;dB	33B	�B	\B	%B	B	B		7B��B�B�B�mB�B�fB�BŢBÖB��B�jB�LB�FB�3B�!B�B��B��B��B��B��B��B��B��B�uB�uB�uB�bB�bB�VB�JB�PB�1B�+B�B�B�B�B� B~�B}�B|�B|�B}�B{�B{�By�By�Bz�Bt�Bn�Br�Bx�Bw�Bu�Bq�Bs�Bu�Bv�Bq�Bl�BhsB^5BVBVBT�BR�BQ�BT�BM�BI�BG�BJ�BJ�BH�BF�BF�BF�BG�BD�BC�BB�BB�BA�BB�BF�BP�BR�BW
BVBW
BF�B?}B?}B2-B1'B7LB@�BI�BL�BF�BA�B=qBH�BQ�BP�BL�BJ�BJ�BJ�BN�BW
BS�BR�BP�BN�BJ�BL�Bn�By�B�B�Bt�Bv�Bx�B� B�B�+B�7B�=B�VB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�'B�-B�3B�3B�FB�FB�RB�RB�RB�LB�3B�dBBǮBɺBɺB��B��B��B��BǮBŢBɺB��B��B��B��B�B��B��B�B�B�B�)B�5B�BB�HB�NB�TB�`B�sB�B�B�B�B��B��B��B	B	B	1B	JB	PB	VB	bB	hB	hB	{B	�B	�B	 �B	!�B	!�B	"�B	&�B	.B	1'B	2-B	5?B	5?B	5?B	7LB	9XB	<jB	@�B	C�B	D�B	E�B	F�B	G�B	H�B	J�B	K�B	M�B	N�B	N�B	O�B	Q�B	S�B	VB	XB	[#B	^5B	cTB	cTB	ffB	jB	l�B	m�B	m�B	n�B	o�B	p�B	p�B	r�B	x�B	}�B	~�B	~�B	�B	�B	�+B	�7B	�=B	�=B	�DB	�PB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�3B	�9B	�FB	�XB	�XB	�^B	�^B	�dB	�jB	�qB	�}B	��B	��B	ƨB	ȴB	ȴB	ɺB	��B	��B	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446452012010314464520120103144645  AO  ARGQ                                                                        20111130135826  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135826  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144645  IP                  G�O�G�O�G�O�                