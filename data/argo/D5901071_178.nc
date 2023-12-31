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
_FillValue                 �  A@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ex   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  o    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �l   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143152  20190522121827  1727_5046_178                   2C  D   APEX                            2143                            040306                          846 @���l���1   @�����@4��t�j�c���$�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BY33B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=� D>  D>� D?  D?� D@  D@� DA  DAy�DB  DB� DCfDC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DSy�DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DyS311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@Fff@�33@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffB��BffBffB ffB(ffB0ffB8ffB@ffBHffBPffBY��B`  BhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C33C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C�  C�  C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<��D=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA� DBfDB�fDC�DC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS� DTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDyY�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�"�A��A�VA���A��A��HA���A�A׶FAײ-A׮A׮A׬Aץ�Aץ�Aץ�Aץ�Aץ�Aף�Aף�Aס�Aס�Aס�Aס�Aם�A׍PA�$�A�ZA�9XA�VA�n�A�1'A�  A�dZA�=qAę�A���Aã�A��A�ffA��7A�\)A���A�n�A�=qA��A��uA�&�A�`BA���A�ĜA��A�7LA�ffA��#A��hA��A�/A�ƨA�A��A�I�A��
A�Q�A���A�/A��A���A���A���A��DA�I�A���A�?}A�=qA���A��uA��yA��A�z�A�?}A��!A���A���A�%A���A�ƨA�7LA�oA��;A�bA���A�jA��A��9A�{A�+A�A��!A��wA���A���A��/A�&�A��yA���A�v�A�VA�$�A�VA�#A}Az��AwS�At�yAshsAq�mApbAl-Ah�/Ag��Ae�TAc�PA_�A^�A^A\z�AY�AV�AU�AT^5AR=qAOXAL�yAK�AKVAJ��AJ�AIAIXAG�AC"�AA7LA>�DA<Q�A:�RA:E�A9VA6ĜA5�mA4�`A3�PA2�DA0��A/
=A-��A-�A,�DA*��A)�-A(��A%�#A#�A"�RA!?}A�7A�/A��A~�A&�A�
A��A=qAK�A�`A  A
=AQ�Av�A`BA%A��A9XA�A��A1'AA�PA�AĜA^5A��A��A1Ap�A
��A
��A	ƨA	K�A	oA�A�uA=qA�A��A��A��A�+A�Al�A��A�DA{A/A ff@�~�@��@� �@��
@�E�@�9X@�t�@��@�@�u@�9X@��@�?}@�z�@�=q@�j@�K�@��y@�+@�E�@�7@���@���@�j@�j@�t�@�M�@��@�J@�7@⟾@�J@��@�G�@�^5@��/@ܬ@܃@�I�@��@��m@ۥ�@�@��#@���@և+@�E�@�J@ԃ@�  @��
@�l�@�@��@�r�@�-@�&�@̴9@�z�@�9X@���@˝�@��@��@�Z@�o@�n�@š�@��/@�r�@�(�@Å@�o@�^5@�7L@�A�@���@�@��@��/@���@��9@�I�@�C�@��\@�=q@�@�@���@���@�`B@��@���@��@���@�z�@�Q�@�1'@�  @��y@�E�@�J@�x�@�bN@��m@�dZ@��H@���@���@�n�@���@�G�@��u@�9X@�1@��;@�ƨ@��F@���@�|�@�"�@�{@�%@��j@��@���@�Q�@� �@�t�@�
=@��@���@�V@�5?@�{@�@�hs@�O�@�7L@��@���@�  @�+@���@��y@���@���@���@�ff@��@�7L@���@��@���@��`@���@��D@���@�1'@���@�C�@�~�@�=q@���@�x�@�?}@�%@��@��@��m@��F@���@�dZ@���@�ȴ@�v�@�J@�@���@��@�V@�Ĝ@��u@�r�@�Q�@� �@��@���@�ƨ@��@�+@��@���@�E�@��-@��7@�p�@�hs@�X@�X@�?}@��@���@���@�Z@��m@�ƨ@�33@���@��@���@��+@��@��#@��-@��@��@�j@�(�@�1@��;@��F@���@�t�@�\)@��@�J@��^@�hs@�7L@���@�Z@���@��F@���@���@�l�@�C�@�o@�ȴ@�~�@�-@���@�@�`B@�t�@�~�@�M�@��@���@�?}@�bN@��P@�+@��+@���@�@���@��@�/@�%@�%@���@�Ĝ@���@�Q�@�1@�ƨ@�33@�o@���@���@�%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�"�A��A�VA���A��A��HA���A�A׶FAײ-A׮A׮A׬Aץ�Aץ�Aץ�Aץ�Aץ�Aף�Aף�Aס�Aס�Aס�Aס�Aם�A׍PA�$�A�ZA�9XA�VA�n�A�1'A�  A�dZA�=qAę�A���Aã�A��A�ffA��7A�\)A���A�n�A�=qA��A��uA�&�A�`BA���A�ĜA��A�7LA�ffA��#A��hA��A�/A�ƨA�A��A�I�A��
A�Q�A���A�/A��A���A���A���A��DA�I�A���A�?}A�=qA���A��uA��yA��A�z�A�?}A��!A���A���A�%A���A�ƨA�7LA�oA��;A�bA���A�jA��A��9A�{A�+A�A��!A��wA���A���A��/A�&�A��yA���A�v�A�VA�$�A�VA�#A}Az��AwS�At�yAshsAq�mApbAl-Ah�/Ag��Ae�TAc�PA_�A^�A^A\z�AY�AV�AU�AT^5AR=qAOXAL�yAK�AKVAJ��AJ�AIAIXAG�AC"�AA7LA>�DA<Q�A:�RA:E�A9VA6ĜA5�mA4�`A3�PA2�DA0��A/
=A-��A-�A,�DA*��A)�-A(��A%�#A#�A"�RA!?}A�7A�/A��A~�A&�A�
A��A=qAK�A�`A  A
=AQ�Av�A`BA%A��A9XA�A��A1'AA�PA�AĜA^5A��A��A1Ap�A
��A
��A	ƨA	K�A	oA�A�uA=qA�A��A��A��A�+A�Al�A��A�DA{A/A ff@�~�@��@� �@��
@�E�@�9X@�t�@��@�@�u@�9X@��@�?}@�z�@�=q@�j@�K�@��y@�+@�E�@�7@���@���@�j@�j@�t�@�M�@��@�J@�7@⟾@�J@��@�G�@�^5@��/@ܬ@܃@�I�@��@��m@ۥ�@�@��#@���@և+@�E�@�J@ԃ@�  @��
@�l�@�@��@�r�@�-@�&�@̴9@�z�@�9X@���@˝�@��@��@�Z@�o@�n�@š�@��/@�r�@�(�@Å@�o@�^5@�7L@�A�@���@�@��@��/@���@��9@�I�@�C�@��\@�=q@�@�@���@���@�`B@��@���@��@���@�z�@�Q�@�1'@�  @��y@�E�@�J@�x�@�bN@��m@�dZ@��H@���@���@�n�@���@�G�@��u@�9X@�1@��;@�ƨ@��F@���@�|�@�"�@�{@�%@��j@��@���@�Q�@� �@�t�@�
=@��@���@�V@�5?@�{@�@�hs@�O�@�7L@��@���@�  @�+@���@��y@���@���@���@�ff@��@�7L@���@��@���@��`@���@��D@���@�1'@���@�C�@�~�@�=q@���@�x�@�?}@�%@��@��@��m@��F@���@�dZ@���@�ȴ@�v�@�J@�@���@��@�V@�Ĝ@��u@�r�@�Q�@� �@��@���@�ƨ@��@�+@��@���@�E�@��-@��7@�p�@�hs@�X@�X@�?}@��@���@���@�Z@��m@�ƨ@�33@���@��@���@��+@��@��#@��-@��@��@�j@�(�@�1@��;@��F@���@�t�@�\)@��@�J@��^@�hs@�7L@���@�Z@���@��F@���@���@�l�@�C�@�o@�ȴ@�~�@�-@���@�@�`B@�t�@�~�@�M�@��@���@�?}@�bN@��P@�+@��+@���@�@���@��@�/@�%@�%@���@�Ĝ@���@�Q�@�1@�ƨ@�33@�o@���@���@�%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�jB�qB�jB�qB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�qB�qB�qB�qB�qB�qB�qB�qB�jB�FB��B��B��B��B�-B��BÖBĜBĜBB�dB��Bp�BffBcTBcTBbNBbNBl�Bs�B�+B��B�BɺB��B��B�B��B��B��B��B��B��B��BĜBŢBÖB�dB�LB�jB�qBɺBƨBƨBɺB��B�9B��B�%Bl�BQ�BG�BD�BA�B33B�B  B�yB�;B��BĜB��B�jB�-B�B�7BZB:^B#�B�B�BhB1B
��B
��B
�B
�HB
��B
��B
�VB
�B
u�B
hsB
\)B
B�B
2-B
�B
\B
B	��B	�B	�
B	�jB	�3B	��B	�bB	{�B	z�B	~�B	n�B	`BB	W
B	R�B	L�B	B�B	2-B	)�B	&�B	"�B	�B	�B	�B	hB	%B��B�B�BB�B��B��BɺBŢBB�}B�jB�RB�3B�B�B��B��B��B��B��B�{B�oB�VB�JB�=B�=B�7B�+B�B�B�B�B� B~�B|�B{�Bx�Bv�Bv�Bu�Bs�Br�Bq�Bq�Bp�Bp�Bo�Bo�Bn�Bl�BjBjBk�Bk�Bk�BjBl�Bl�Bk�Bl�Bk�Bk�Bk�BjBhsBgmBhsBhsBhsBgmBffBdZBaHB^5B\)B]/BaHBgmBdZBcTBaHB_;B_;B`BBcTBw�By�By�Bv�Bt�Bu�Bu�Bx�B|�B�B�B�B�B�B�B�%B�1B�DB�VB�hB�bB�bB�VB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�9B�?B�FB�LB�LB�RB�^B�wBǮB��B��B�B�)B�5B�;B�NB�`B�fB�sB�yB�yB�B��B��B��B��B��B	B		7B	JB	VB	\B	bB	bB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	&�B	.B	0!B	33B	5?B	5?B	6FB	6FB	8RB	;dB	?}B	A�B	B�B	C�B	D�B	D�B	E�B	E�B	F�B	K�B	P�B	Q�B	R�B	R�B	T�B	VB	\)B	_;B	_;B	_;B	cTB	ffB	hsB	k�B	m�B	n�B	n�B	n�B	o�B	r�B	t�B	u�B	v�B	y�B	}�B	~�B	�B	�B	�B	�7B	�7B	�=B	�JB	�JB	�VB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�'B	�9B	�?B	�FB	�FB	�LB	�RB	�XB	�XB	�XB	�^B	�jB	�qB	�wB	��B	ÖB	ÖB	ĜB	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�/B	�5B	�;B	�BB	�BB	�NB	�`B	�`B	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�yB	�sB	�mB	�mB	�fB	�`B	�TB	�TB	�NB	�NB	�HB	�BB	�BB	�BB	�HB	�ZB	�`B	�`B	�fB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�jB�qB�jB�qB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�qB�qB�qB�qB�qB�qB�qB�qB��B�^B��B��B��B�B�FBÖBȴBǮBǮBĜBȴB��Bt�BgmBffBe`BhsBffBl�Bu�B�DB��B�B��B��B��B�B�B��B��B��B��B��B�B��BȴBǮB�qB�RB�qB�}B��BƨBǮB��BB�RB��B�DBt�BVBJ�BE�BD�B8RB$�B	7B�B�`B��BŢBB��B�9B�FB��BgmBC�B(�B�B�B�B\B
��B
��B
�B
�yB
�B
��B
�{B
�B
y�B
jB
bNB
F�B
:^B
 �B
oB
1B	��B	��B	�5B	�}B	�LB	�B	��B	� B	z�B	�B	w�B	ffB	[#B	W
B	R�B	K�B	9XB	-B	)�B	$�B	 �B	�B	�B	�B	{B��B�B�mB�/B��B��B��BǮBŢBB�}B�qB�LB�-B�B�B��B��B��B��B��B��B�oB�hB�JB�=B�=B�DB�7B�+B�B�B�B�B� B~�B~�Bz�Bw�Bv�Bu�Bu�Bt�Bs�Br�Bq�Bq�Bp�Bp�Bo�Bn�Bl�Bm�Bm�Bl�Bn�Bn�Bm�Bm�Bm�Bl�Bl�Bl�Bl�Bl�BjBjBiyBiyBhsBgmBffBcTBaHB^5B^5BbNBiyBgmBdZBcTBe`BaHBaHBaHBx�Bz�B|�Bx�Bv�Bv�Bv�By�B}�B�B�B�B�%B�%B�B�%B�1B�JB�uB�oB�hB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�?B�?B�FB�LB�RB�XB�jB��BɺB��B��B�B�/B�;B�BB�TB�fB�sB�yB�B�B�B��B��B��B��B��B	B	
=B	JB	VB	\B	bB	bB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	(�B	/B	1'B	49B	5?B	5?B	6FB	7LB	9XB	<jB	@�B	A�B	B�B	C�B	D�B	D�B	E�B	F�B	H�B	M�B	P�B	Q�B	R�B	R�B	T�B	W
B	]/B	_;B	_;B	`BB	cTB	ffB	iyB	l�B	m�B	n�B	n�B	o�B	p�B	s�B	t�B	u�B	v�B	y�B	}�B	~�B	�B	�B	�B	�7B	�7B	�=B	�JB	�JB	�VB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�9B	�?B	�FB	�FB	�LB	�RB	�XB	�XB	�XB	�dB	�jB	�qB	�}B	��B	ÖB	ÖB	ĜB	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�5B	�;B	�;B	�BB	�HB	�TB	�`B	�`B	�fB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�mB	�mB	�mB	�fB	�ZB	�ZB	�TB	�TB	�NB	�BB	�BB	�BB	�NB	�ZB	�`B	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<�o<T��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447362012010314473620120103144736  AO  ARGQ                                                                        20111130143152  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143152  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144736  IP                  G�O�G�O�G�O�                