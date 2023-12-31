CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:35Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142744  20190522121827  1727_5046_158                   2C  D   APEX                            2143                            040306                          846 @��� 1   @������@5�~��"��c�����1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� DfD� D  D� D  D� D  D�fD  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\  D\� D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsl�Dy�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�33A��A!��AA��Aa��A���A�  A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBx��B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP  CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj  Cl�Cn�Cp�Cr33Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fD�D�fDfD�fDfD�fDfD��DfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD�D�fDfD�fDfD�fD�D�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5�D5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDY  DY�fDZfDZ�fD[fD[�fD\fD\�fD]  D]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg� DhfDh�fDifDi�fDj�Dj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDss3Dy��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��;A��`A��`A��A��AѾwAёhAЗ�A͙�AˋDA�bNAɾwA�^5Aȡ�A�z�A� �AǍPA�VAƩ�A�XAŅA�"�A�AËDA�z�A�n�A�=qA�ZA�^5A�C�A�I�A�jA��-A��FA��!A�t�A�-A�v�A�E�A�5?A�5?A���A�t�A�;dA�{A�=qA��TA�l�A��TA�7LA�\)A�I�A�/A���A��HA���A�\)A�A��PA�9XA�
=A��-A�v�A�VA���A�K�A� �A�A�;dA�
=A��mA���A�M�A�$�A��#A�^5A�=qA��DA�JA�ffA�"�A��mA��uA�  A�v�A���A�ĜA�A�A��`A�x�A�
=A���A�r�A��+A���A���A�Q�A��wA�/A�5?A��A�p�A�=qA���A��jA�&�A��FA�r�A�A�A�JA�+A�"�A���A��yA��jA�1'A���A�A���A��DA��A���A���A��A�A�A���A~ȴAxjAs�PAqVAo�wAjz�AgC�Af�Ae�7Ad�yAc�mA^��AW�AV�RAV{AUdZATĜATr�AT9XAT{AS�mASG�AR9XAP��AM��AK�AGp�AD�yAB��A@��A@M�A?��A>ffA=�#A=�FA=+A:I�A7�FA733A6v�A4��A4ffA3C�A1"�A/XA.1A,(�A*��A)K�A(�A(z�A(1'A'��A&��A%A$-A#�
A#��A"��A�
AhsA�/A~�AZA��AAG�AĜAE�A��A��A�\AM�AE�AbA�
A��A\)A�A�A\)AVA�yAĜAr�AAJA?}A��A
=AXA
�A
r�A	��A	O�A�A�DA�A �+@���@�K�@�^5@�@��-@�Q�@���@�\)@���@���@�1'@���@��@�@�S�@��@�ȴ@�!@�!@旍@�+@�ff@�-@��`@�33@�+@ᙚ@��D@�dZ@ޟ�@��@�9X@�"�@�5?@���@� �@��@�`B@�`B@Ͼw@ΰ!@�%@��;@ʧ�@�z�@���@Ƈ+@�o@Ǿw@�  @� �@��@Ǖ�@�^5@�`B@�G�@�%@öF@�o@��@��@�$�@���@���@��@�hs@�?}@��@��/@�I�@�1@�ƨ@���@�\)@�+@���@���@��-@�Q�@��@��\@���@�p�@�A�@�b@�1@�C�@�E�@�E�@�n�@�E�@�G�@���@���@��@��@�\)@��R@�-@���@�&�@�%@��j@�Z@�Z@� �@���@�;d@��@�n�@�5?@�$�@���@�$�@���@��@�7L@���@��;@��@�C�@��\@�5?@�$�@�{@��@�p�@�X@�/@�&�@�V@��@�bN@�(�@�  @��
@��P@�C�@��R@�ff@�@���@���@��h@��@�G�@�%@��j@�z�@�  @��F@���@�|�@�t�@�l�@�C�@��@�ȴ@���@���@��\@�n�@�$�@���@��T@��-@���@�p�@�O�@��@���@���@���@�p�@��@�-@��#@���@��h@��@�/@�Q�@�ff@���@���@���@�n�@�S�@�9X@���@��@�G�@���@�r�@�Q�@�1'@�(�@�(�@�(�@��@���@���@��@��@�S�@�"�@�@��H@��R@���@�n�@�V@�ff@�^5@�{@��T@���@���@��^@�`B@�r�@�b@���@��w@���@���@��;@��@��@�n�@�$�@���@�?}@���@��/@�bN@�ƨ@�l�@�
=@���@�v�@�ff@�V@�V@�-@�-@�5?@�5?@�@��^@�hs@�/@�&�@�/@�&�@��@��@�?}@�?}@�/@�&�@��
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��;A��`A��`A��A��AѾwAёhAЗ�A͙�AˋDA�bNAɾwA�^5Aȡ�A�z�A� �AǍPA�VAƩ�A�XAŅA�"�A�AËDA�z�A�n�A�=qA�ZA�^5A�C�A�I�A�jA��-A��FA��!A�t�A�-A�v�A�E�A�5?A�5?A���A�t�A�;dA�{A�=qA��TA�l�A��TA�7LA�\)A�I�A�/A���A��HA���A�\)A�A��PA�9XA�
=A��-A�v�A�VA���A�K�A� �A�A�;dA�
=A��mA���A�M�A�$�A��#A�^5A�=qA��DA�JA�ffA�"�A��mA��uA�  A�v�A���A�ĜA�A�A��`A�x�A�
=A���A�r�A��+A���A���A�Q�A��wA�/A�5?A��A�p�A�=qA���A��jA�&�A��FA�r�A�A�A�JA�+A�"�A���A��yA��jA�1'A���A�A���A��DA��A���A���A��A�A�A���A~ȴAxjAs�PAqVAo�wAjz�AgC�Af�Ae�7Ad�yAc�mA^��AW�AV�RAV{AUdZATĜATr�AT9XAT{AS�mASG�AR9XAP��AM��AK�AGp�AD�yAB��A@��A@M�A?��A>ffA=�#A=�FA=+A:I�A7�FA733A6v�A4��A4ffA3C�A1"�A/XA.1A,(�A*��A)K�A(�A(z�A(1'A'��A&��A%A$-A#�
A#��A"��A�
AhsA�/A~�AZA��AAG�AĜAE�A��A��A�\AM�AE�AbA�
A��A\)A�A�A\)AVA�yAĜAr�AAJA?}A��A
=AXA
�A
r�A	��A	O�A�A�DA�A �+@���@�K�@�^5@�@��-@�Q�@���@�\)@���@���@�1'@���@��@�@�S�@��@�ȴ@�!@�!@旍@�+@�ff@�-@��`@�33@�+@ᙚ@��D@�dZ@ޟ�@��@�9X@�"�@�5?@���@� �@��@�`B@�`B@Ͼw@ΰ!@�%@��;@ʧ�@�z�@���@Ƈ+@�o@Ǿw@�  @� �@��@Ǖ�@�^5@�`B@�G�@�%@öF@�o@��@��@�$�@���@���@��@�hs@�?}@��@��/@�I�@�1@�ƨ@���@�\)@�+@���@���@��-@�Q�@��@��\@���@�p�@�A�@�b@�1@�C�@�E�@�E�@�n�@�E�@�G�@���@���@��@��@�\)@��R@�-@���@�&�@�%@��j@�Z@�Z@� �@���@�;d@��@�n�@�5?@�$�@���@�$�@���@��@�7L@���@��;@��@�C�@��\@�5?@�$�@�{@��@�p�@�X@�/@�&�@�V@��@�bN@�(�@�  @��
@��P@�C�@��R@�ff@�@���@���@��h@��@�G�@�%@��j@�z�@�  @��F@���@�|�@�t�@�l�@�C�@��@�ȴ@���@���@��\@�n�@�$�@���@��T@��-@���@�p�@�O�@��@���@���@���@�p�@��@�-@��#@���@��h@��@�/@�Q�@�ff@���@���@���@�n�@�S�@�9X@���@��@�G�@���@�r�@�Q�@�1'@�(�@�(�@�(�@��@���@���@��@��@�S�@�"�@�@��H@��R@���@�n�@�V@�ff@�^5@�{@��T@���@���@��^@�`B@�r�@�b@���@��w@���@���@��;@��@��@�n�@�$�@���@�?}@���@��/@�bN@�ƨ@�l�@�
=@���@�v�@�ff@�V@�V@�-@�-@�5?@�5?@�@��^@�hs@�/@�&�@�/@�&�@��@��@�?}@�?}@�/@�&�@��
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�?B�?B�?B�?B�?B�LB�XBŢB�BBVBVB{B
=B%�B1'B/B33B33B1'B"�B�B(�B)�B-B5?BD�BG�BT�BgmB�\B�B�B��B��B�B�}B��B��B�}B��B��B��B��B�BB�BB�/B�B��B��BĜBŢBŢBÖBĜBĜBƨBŢBĜBǮBɺB��B�B�B�BB�/BŢB��B��B��B�B��B�BÖBȴB��B�B��B��B��B��B��B��B��B�uB�7Bm�BQ�B=qB9XB6FB49B0!B)�B�BhB��B�B�B�BB�B��B�dB�B��B��B�PB�+B�B{�BiyBYBP�B?}B(�B{BB
��B
�B
��B
�B
�VB
o�B
VB
E�B
9XB
�B	��B	��B	�XB	�B	�1B	r�B	iyB	dZB	^5B	Q�B	0!B	\B	
=B	+B	B	B��B��B��B��B��B�B�sB�
BȴB�jB�3B�B��B��B��B��B��B��B��B��B�oB�\B�DB�1B�B� Bx�Bt�Bo�BiyBe`Be`BbNBaHB`BB`BBaHBcTBdZBdZBdZBgmBjBiyBhsBgmBffBe`Be`Be`BiyBjBiyBhsBffBe`Be`Be`BffBhsBjBm�Bs�Bo�Bp�B|�B�B�B� B~�By�Bq�Bl�Bp�B|�B~�B}�Bw�Bu�Bn�B\)BXBXBXBXBYBZB\)B_;B\)B]/B]/BZBXBVBS�BXBXBXBXBXBXBXBW
BW
B\)BhsBk�BiyBhsBk�Bo�By�B|�B}�B|�B~�B}�By�Bs�Bp�Bq�Bt�Bw�Bx�Bx�Bw�Bw�B|�B�%B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�!B�'B�'B�9B�3B�LB�qB��BƨBɺBǮBɺB��B��B��B�
B�#B�5B�NB�HB�HB�ZB�fB�mB�yB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	+B		7B		7B		7B	DB	bB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	$�B	&�B	)�B	,B	/B	1'B	2-B	2-B	33B	5?B	7LB	9XB	:^B	?}B	B�B	D�B	E�B	E�B	F�B	G�B	J�B	K�B	L�B	M�B	M�B	N�B	P�B	Q�B	R�B	T�B	XB	YB	^5B	dZB	jB	p�B	t�B	x�B	|�B	�B	�%B	�%B	�%B	�%B	�B	~�B	y�B	u�B	q�B	o�B	p�B	w�B	}�B	}�B	z�B	w�B	z�B	|�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�1B	�7B	�7B	�DB	�JB	�JB	�DB	�JB	�PB	�VB	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�3B	�?B	�?B	�FB	�LB	�XB	�XB	�XB	�^B	�jB	�}B	��B	��B	B	B	B	ĜB	ɺB	��B	��B	��B	��B	�#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�?B�?B�?B�?B�FB�RB�jB��B��B
=BhBbB�BDB'�B49B1'B5?B49B49B'�B�B+B)�B-B6FBG�BG�BT�BhsB�bB�'B�!B��B��B�BB��B��B�}B��B��B��B��B�ZB�NB�;B�#B��B��BĜBƨBƨBÖBĜBƨBɺBƨBƨBȴB��B��B�B�#B�NB�TB��B�B��B��B�B��B�BŢB��BǮB�'B��B��B��B��B��B��B��B��B�hBs�BXB?}B;dB8RB9XB49B.B �B�B��B��B�B�ZB�BB��BÖB�-B��B��B�\B�1B�B�Bo�B[#BVBG�B2-B�BBB
�B
�B
�3B
��B
w�B
ZB
H�B
?}B
.B	��B	��B	�jB	�LB	�bB	u�B	k�B	ffB	aHB	aHB	D�B	hB	JB		7B	%B	B	  B��B��B��B��B��B�B�HB�BŢB�dB�9B�B��B��B��B��B��B��B��B�{B�hB�\B�7B�1B�B}�Bw�Bt�Bl�BjBgmBcTBbNBbNBcTBdZBhsBe`Be`BhsBo�Bl�Bk�BiyBhsBgmBffBgmBgmBk�Bo�Bm�Bl�BgmBe`BffBffBgmBiyBm�Bs�Bx�Bp�Bq�B}�B�B�B�B�B}�Bs�Bp�Bq�B}�B�B� B� By�Bv�B_;BZBYBYBYBZB\)B_;Be`B_;BcTB`BB`BB\)BYBXBYBXBXBXBXBXBXBXBZB_;BjBm�Bk�BjBm�Br�B{�B~�B� B� B�B� B}�Bs�Bs�Bs�Bw�Bz�B{�B|�Bw�Bx�B{�B�B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�!B�'B�-B�9B�9B�XB�wB��BƨB��BɺBɺB��B��B��B�
B�#B�5B�ZB�TB�HB�`B�fB�sB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	1B		7B		7B	
=B	JB	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	%�B	'�B	+B	-B	0!B	1'B	2-B	2-B	33B	5?B	8RB	9XB	;dB	@�B	B�B	D�B	E�B	E�B	F�B	H�B	J�B	K�B	L�B	M�B	M�B	O�B	P�B	Q�B	R�B	T�B	XB	YB	]/B	cTB	iyB	o�B	s�B	w�B	|�B	�B	�%B	�%B	�%B	�+B	�B	�B	{�B	w�B	r�B	o�B	o�B	v�B	~�B	� B	}�B	x�B	z�B	|�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�1B	�7B	�7B	�DB	�JB	�PB	�DB	�JB	�PB	�VB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�?B	�?B	�FB	�LB	�XB	�XB	�XB	�^B	�qB	��B	��B	��B	B	B	B	ĜB	ɺB	��B	��B	��B	��B	�#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<u<��
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447292012010314472920120103144729  AO  ARGQ                                                                        20111130142744  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142744  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144729  IP                  G�O�G�O�G�O�                