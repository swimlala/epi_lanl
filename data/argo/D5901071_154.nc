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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142649  20190522121827  1727_5046_154                   2C  D   APEX                            2143                            040306                          846 @�ܶ	@1   @�ܶ�?�@5���l�D�c��t�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'y�D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*  C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD�D��DfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'  D'� D(  D(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB��DCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]  D]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�;dA�=qA�9XA� �A��A��A�bA�VA�
=A�
=A�A�  A���A���A���A��Aǰ!A�$�A�(�A�1AĲ-A�JAå�A�  A��A��PA�E�A�G�A��A�bNA�
=A�{A���A��\A�Q�A���A�I�A��A��wA��PA�+A���A�l�A�$�A���A���A�p�A�=qA��A���A�9XA��A�O�A�JA��
A�ĜA�z�A�{A���A�`BA�/A�oA��A���A���A�$�A��!A�ffA�/A��wA��#A��`A�|�A�A�{A� �A���A�G�A��A�|�A��wA���A���A���A�^5A�&�A��A�XA�1A��TA���A� �A���A�r�A�1A�(�A��A�XA���A�K�A��A��FA��HA�1'A�?}A���A��+A���A�O�A��A��wA��A��HA���A�33A���A��7A���A�1A}&�A{AyS�AwK�Au�At9XAs�Aq�hAqdZAq/Ap��ApAo�wAo;dAlz�Ahr�Ag�Ac\)A_�TA]��A[�#AY�AW�PAU�hAT5?ASS�AR�AR�AQ�AP{AN�RAM&�AK��AK33AKAJv�AI+AG��AFȴAF  AEXAD9XAC/AAG�A?��A?�A>�!A=G�A<{A9�A8�uA6�/A49XA2=qA1�^A0�\A-�FA+�A)ƨA(r�A'��A%�
A$�DA"��A!�7A!A��AoA"�AbA�`AA�!A5?A�A�yA�Ax�A�A~�A-A��A�7A�!AbA�hAA�A�Av�AffA �A�A��A
��A
^5A
 �A	�wA��A�yAM�A�A�A�+AG�A�A�wAr�A ��A Q�@��F@�@�%@��j@��m@��@�J@��7@���@�(�@�S�@��^@�|�@�`B@�Q�@�%@�r�@�ƨ@�O�@��@�M�@���@�&�@�1@�K�@�!@�~�@�@�l�@�;d@���@�+@�@�7@�G�@�%@�j@߮@�l�@޸R@�{@�G�@ܼj@��@���@ش9@ش9@�z�@� �@׮@׾w@ו�@��;@�K�@�X@�;d@ҸR@�5?@�p�@д9@�(�@Ϯ@��@��H@�X@��@��T@��/@ě�@�A�@���@���@��@��@��+@�M�@�=q@�x�@��w@��@��#@�$�@�E�@�G�@�z�@�1'@��;@��F@�l�@���@��@�V@�9X@��@��H@�-@�J@��T@��-@�p�@��`@�I�@�  @���@�1@��P@�@��@��R@�v�@�ff@�V@�{@���@�O�@��@�b@� �@���@�|�@��@���@�@���@���@��^@�&�@�%@��9@��j@��@�Z@�9X@���@��w@�|�@�S�@�\)@�dZ@�t�@��P@���@�-@��#@��7@�/@�7L@��/@�z�@�S�@�ƨ@�(�@�I�@��@��@�o@�~�@�^5@�V@�V@�V@�~�@��H@��y@���@�V@��T@��T@��^@�x�@�`B@�X@�%@�z�@�bN@�A�@��@��@�dZ@�C�@�@���@�"�@�C�@�;d@��@��y@��H@��H@���@�~�@�E�@��@���@�x�@�`B@�O�@�G�@�7L@�%@��j@���@��u@�I�@� �@��;@��w@��F@�dZ@�+@��\@�@��^@��^@��-@�x�@�p�@�G�@�7L@�&�@���@��`@��`@��@��@��@��w@���@�dZ@���@�{@�@��@��@��T@�hs@���@��9@��;@�ƨ@���@���@��^@��`@�r�@��;@��@�S�@�C�@�
=@��H@��@���@��R@���@���@�^511111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�;dA�=qA�9XA� �A��A��A�bA�VA�
=A�
=A�A�  A���A���A���A��Aǰ!A�$�A�(�A�1AĲ-A�JAå�A�  A��A��PA�E�A�G�A��A�bNA�
=A�{A���A��\A�Q�A���A�I�A��A��wA��PA�+A���A�l�A�$�A���A���A�p�A�=qA��A���A�9XA��A�O�A�JA��
A�ĜA�z�A�{A���A�`BA�/A�oA��A���A���A�$�A��!A�ffA�/A��wA��#A��`A�|�A�A�{A� �A���A�G�A��A�|�A��wA���A���A���A�^5A�&�A��A�XA�1A��TA���A� �A���A�r�A�1A�(�A��A�XA���A�K�A��A��FA��HA�1'A�?}A���A��+A���A�O�A��A��wA��A��HA���A�33A���A��7A���A�1A}&�A{AyS�AwK�Au�At9XAs�Aq�hAqdZAq/Ap��ApAo�wAo;dAlz�Ahr�Ag�Ac\)A_�TA]��A[�#AY�AW�PAU�hAT5?ASS�AR�AR�AQ�AP{AN�RAM&�AK��AK33AKAJv�AI+AG��AFȴAF  AEXAD9XAC/AAG�A?��A?�A>�!A=G�A<{A9�A8�uA6�/A49XA2=qA1�^A0�\A-�FA+�A)ƨA(r�A'��A%�
A$�DA"��A!�7A!A��AoA"�AbA�`AA�!A5?A�A�yA�Ax�A�A~�A-A��A�7A�!AbA�hAA�A�Av�AffA �A�A��A
��A
^5A
 �A	�wA��A�yAM�A�A�A�+AG�A�A�wAr�A ��A Q�@��F@�@�%@��j@��m@��@�J@��7@���@�(�@�S�@��^@�|�@�`B@�Q�@�%@�r�@�ƨ@�O�@��@�M�@���@�&�@�1@�K�@�!@�~�@�@�l�@�;d@���@�+@�@�7@�G�@�%@�j@߮@�l�@޸R@�{@�G�@ܼj@��@���@ش9@ش9@�z�@� �@׮@׾w@ו�@��;@�K�@�X@�;d@ҸR@�5?@�p�@д9@�(�@Ϯ@��@��H@�X@��@��T@��/@ě�@�A�@���@���@��@��@��+@�M�@�=q@�x�@��w@��@��#@�$�@�E�@�G�@�z�@�1'@��;@��F@�l�@���@��@�V@�9X@��@��H@�-@�J@��T@��-@�p�@��`@�I�@�  @���@�1@��P@�@��@��R@�v�@�ff@�V@�{@���@�O�@��@�b@� �@���@�|�@��@���@�@���@���@��^@�&�@�%@��9@��j@��@�Z@�9X@���@��w@�|�@�S�@�\)@�dZ@�t�@��P@���@�-@��#@��7@�/@�7L@��/@�z�@�S�@�ƨ@�(�@�I�@��@��@�o@�~�@�^5@�V@�V@�V@�~�@��H@��y@���@�V@��T@��T@��^@�x�@�`B@�X@�%@�z�@�bN@�A�@��@��@�dZ@�C�@�@���@�"�@�C�@�;d@��@��y@��H@��H@���@�~�@�E�@��@���@�x�@�`B@�O�@�G�@�7L@�%@��j@���@��u@�I�@� �@��;@��w@��F@�dZ@�+@��\@�@��^@��^@��-@�x�@�p�@�G�@�7L@�&�@���@��`@��`@��@��@��@��w@���@�dZ@���@�{@�@��@��@��T@�hs@���@��9@��;@�ƨ@���@���@��^@��`@�r�@��;@��@�S�@�C�@�
=@��H@��@���@��R@���@���@�^511111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�#B�#B�#B�#B�#B�#B�)B�HB�B1BJBB��B��B�ZB�)B�)B��B�B�B��BBBB	7B(�B6FBD�BP�BQ�BR�BR�BhsBz�B�B�=B�bB��B��B��B��B��B�'B�wB�jB�wBB��BǮB��B�}B��B��B��BÖB��B��B��B��B��B��B��B��BÖB�FB�-B�!B�B��B��B�{Bt�BD�B#�BB�B�B�B��BBJB �B6FB49BhB�NB��BB��B�5B��BŢB�XB�B�\B�B~�Bt�BhsB]/BK�B:^B\B
�B
��B
�qB
�oB
ffB
/B
�B
	7B	��B	�B	�HB	��B	��B	ÖB	B	��B	�jB	�LB	�9B	�B	��B	�%B	t�B	dZB	M�B	E�B	5?B	&�B	�B	{B	
=B	1B	�B	-B	+B	$�B	$�B	�B	�B	{B	{B	uB	�B	�B	{B	bB	DB	%B	B��B�B�B�B�B�B�ZB�/B��BB�jB�RB�!B��B��B��B��B�{B�uB�\B�DB�7B�+B�B�B�B|�Bx�Bz�B� B�7B�JB�JB�JB�JB�JB�JB�PB�DB�=B�DB�\B�PB�JB�JB�PB�VB�\B�\B�\B�\B�VB�\B�VB�PB�{B��B��B��B��B��B��B��B�VB�1B�+B�%B�B�B�B�B�B�B�=B�hB�{B��B��B�hB�DB�=B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�3B�3B�9B�9B�9B�FB�RB�dB�jB�}BɺB��B��B��B��B�B�B�B�B�#B�B�#B��B��B��B��BƨB�dB�B�B�'B�3B�?B�RB�wB�jB�qB�}BŢBȴB��B��B��B��B�
B�
B��B��B��B��B��B��B�#B�)B�)B�5B�HB�`B�yB�B�B�B��B��B��B	  B	B	B	B	B	B	B	+B	
=B	�B	�B	�B	 �B	!�B	$�B	$�B	$�B	)�B	+B	,B	0!B	7LB	:^B	;dB	<jB	>wB	A�B	B�B	D�B	K�B	M�B	N�B	P�B	S�B	T�B	VB	XB	[#B	\)B	[#B	[#B	]/B	cTB	e`B	ffB	ffB	gmB	iyB	l�B	m�B	n�B	n�B	n�B	r�B	z�B	}�B	~�B	~�B	~�B	~�B	� B	�B	�B	�B	�B	�%B	�+B	�1B	�=B	�VB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�9B	�?B	�FB	�XB	�wB	�wB	�wB	�wB	�}B	�}B	��B	��B	��B	B	B	��B	ÖB	ǮB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�#B�#B�#B�#B�#B�)B�;B�`B�B	7BVBBB��B�fB�/B�)BB��B�B  BBBBPB+B7LBF�BQ�BS�BS�BVBjB|�B�B�DB�hB��B��B��B��B��B�-B�}B�jB�}BĜB��BȴBB��BBB��BŢB��B��B��B��B�B��B��B��BȴB�dB�9B�'B�B�B��B��B{�BJ�B.B+B�B�B�B��BBVB"�B7LB;dB�B�mB��BĜB��B�NB�B��B�qB�RB�hB�+B�Bx�Bl�BbNBQ�BK�B�B
��B
�B
ǮB
��B
v�B
5?B
�B
PB
  B	�B	�ZB	�B	��B	ĜB	ÖB	��B	�wB	�RB	�FB	�?B	��B	�DB	� B	n�B	T�B	K�B	=qB	-B	#�B	�B	PB		7B	�B	33B	.B	)�B	)�B	!�B	�B	�B	�B	�B	�B	�B	�B	oB	VB		7B	1B��B�B�B��B�B�B�sB�NB�BǮB�wB�jB�RB��B��B��B��B��B��B��B�VB�DB�JB�DB�1B�B� Bz�B}�B�B�=B�\B�\B�PB�VB�PB�PB�VB�JB�JB�PB�hB�hB�\B�VB�PB�\B�bB�bB�hB�hB�\B�bB�bB�PB��B��B��B��B��B��B��B��B�uB�=B�7B�7B�B�B�B�B�B�%B�=B�{B��B��B��B��B�PB�7B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�FB�?B�3B�9B�9B�?B�LB�RB�dB�jB��B��B��B��B��B�B�
B�B�#B�#B�HB�)B�5B�
B��B��B��B��BB�'B�B�-B�3B�?B�XB��B�wB�}B�}BŢB��B��B��B��B��B�
B�B�
B��B��B��B��B�B�#B�)B�)B�;B�NB�fB�B�B�B�B��B��B��B	B	B	B	B	B	B	%B	1B	
=B	�B	�B	�B	 �B	"�B	$�B	$�B	$�B	+B	+B	,B	0!B	7LB	:^B	;dB	<jB	>wB	A�B	B�B	D�B	K�B	M�B	N�B	Q�B	T�B	VB	W
B	YB	[#B	]/B	\)B	]/B	\)B	bNB	e`B	ffB	gmB	hsB	jB	l�B	m�B	n�B	n�B	n�B	r�B	z�B	}�B	� B	� B	~�B	~�B	� B	�B	�B	�B	�B	�%B	�+B	�1B	�DB	�VB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�?B	�?B	�LB	�^B	�wB	�wB	�wB	�wB	�}B	�}B	��B	��B	��B	B	B	B	ĜB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�C�<T��<#�
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
<49X<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447282012010314472820120103144728  AO  ARGQ                                                                        20111130142649  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142649  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144728  IP                  G�O�G�O�G�O�                