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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142407  20190522121827  1727_5046_142                   2C  D   APEX                            2143                            040306                          846 @��0�.��1   @��1UU@
@7V��c�M���1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�fC  C�C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D��D� D   D � D!  D!� D"  D"� D#  D#� D$fD$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DB��DCy�DD  DD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dp��Dqy�Dq��Dr� Ds  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @@  @�33@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@��BHffBPffBXffB`��BhffBpffBxffB�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C  C�C33C33C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP33CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD� DfD�fDfD�fD  D�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$�D$�fD%  D%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDC  DC� DDfDD��DEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDL�DL��DMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf��DgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl� DmfDm�fDnfDn�fDofDo�fDpfDp� Dq  Dq� Dr  Dr�fDsf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A��A��yA��TA��HA��HA���A��FA���A��+A�C�A��`A�ZA���A���A���A�{A��jA��A�9XA��A�oA�JA�
=A�A�%A�A���A��A��A�  A���A�ȴA��wA��FA��9A��-A��FA��A���A�jA� �A��A�bA�  A��A��mA���A�n�A��\A���A���A�bNA���A��wA���A��7A�hsA��^A�K�A�1A���A�S�A�;dA�33A� �A��A���A��`A�ĜA�ZA�7LA���A�oA��`A���A�`BA�A�A���A�\)A�;dA�jA���A�n�A��A��#A�dZA�x�A�A��uA���A��TA���A��A��7A��A���A���A�A�l�A��DA�ƨA�x�A�A�t�A��A�jA�G�A���A�ffA��-A���A��/A�I�A�A��TA�z�A���A�ffA���A�n�A��A&�A}�hAz��AwG�Au�
Au��AuS�At1'ArJAn�Al��Aj�Aj-Ai�Ahr�Ag\)Ae33Ab�+A`M�A^��AZ��AY�#AY;dAX9XAV~�AUt�AR1'AO�#AOC�AN��AN�!AN �ALjAL�AK�;AKVAI��AHM�AF��AD1AC��ACp�AB�`AAoA>=qA=|�A=C�A=
=A<��A<1'A;dZA:��A9`BA8�A7p�A6�A6ffA5�A4r�A3x�A1A0��A/��A/S�A.��A-��A,�RA+�;A*��A*{A(�uA&�A%�TA#dZA!ƨA ^5A�DAĜAbA�mA�
A�PA�A��A�Al�A~�A�A�A5?AE�Av�A33A��A��A&�A��AffA�;A;dA �A	�-A��A�RAhsA�uAVA=qA$�A��A7LA(�Ap�A7LA �@���@���@�&�@�t�@�O�@��@�7L@���@�P@�V@���@��@��T@�D@��#@�9X@�K�@�~�@�hs@�@㝲@�&�@�@�S�@���@�p�@��/@�1@ץ�@�S�@�+@�
=@�ȴ@֗�@�-@��@��
@ҸR@�5?@�|�@Ώ\@Ͳ-@�z�@��#@ǅ@�X@���@�A�@�dZ@�S�@�C�@�C�@�;d@�o@\@�{@�@��h@�?}@��`@��j@�Q�@�@��@���@��7@�hs@�O�@��j@��w@��@�l�@�l�@�+@��H@���@��\@�ff@�E�@�{@�O�@��u@��@�n�@��@�S�@�5?@��T@�p�@�7L@���@��m@�l�@��!@��-@��u@�bN@��@���@��w@�|�@�V@���@���@��@��@��D@�bN@�1'@�(�@�1@��@���@�"�@�ff@��@���@��@�Z@�1'@��P@��@���@�X@�Z@�1@��
@���@���@���@��m@��;@�;d@���@�v�@���@�hs@��@�Ĝ@��D@�1@�S�@��@���@�-@�-@���@��^@���@�hs@��@�  @�dZ@���@�ff@�=q@���@���@��9@�I�@��@��@�t�@��@��@��+@��@��@�O�@��/@�Z@�9X@�  @���@���@��P@��@�\)@�
=@���@�{@���@�Ĝ@���@��@��9@��9@��j@��9@���@��D@��D@�r�@�I�@� �@��P@�dZ@�dZ@�|�@�dZ@�C�@��@���@���@�ff@���@��@�-@�{@���@���@�p�@�`B@�G�@�O�@��@�r�@���@���@�;d@�o@���@��@��@�7L@���@���@���@��D@��@��9@��9@���@��@��`@���@���@���@��9@��@�Z@�(�@��@��
@���@���@��@��F111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A��A��yA��TA��HA��HA���A��FA���A��+A�C�A��`A�ZA���A���A���A�{A��jA��A�9XA��A�oA�JA�
=A�A�%A�A���A��A��A�  A���A�ȴA��wA��FA��9A��-A��FA��A���A�jA� �A��A�bA�  A��A��mA���A�n�A��\A���A���A�bNA���A��wA���A��7A�hsA��^A�K�A�1A���A�S�A�;dA�33A� �A��A���A��`A�ĜA�ZA�7LA���A�oA��`A���A�`BA�A�A���A�\)A�;dA�jA���A�n�A��A��#A�dZA�x�A�A��uA���A��TA���A��A��7A��A���A���A�A�l�A��DA�ƨA�x�A�A�t�A��A�jA�G�A���A�ffA��-A���A��/A�I�A�A��TA�z�A���A�ffA���A�n�A��A&�A}�hAz��AwG�Au�
Au��AuS�At1'ArJAn�Al��Aj�Aj-Ai�Ahr�Ag\)Ae33Ab�+A`M�A^��AZ��AY�#AY;dAX9XAV~�AUt�AR1'AO�#AOC�AN��AN�!AN �ALjAL�AK�;AKVAI��AHM�AF��AD1AC��ACp�AB�`AAoA>=qA=|�A=C�A=
=A<��A<1'A;dZA:��A9`BA8�A7p�A6�A6ffA5�A4r�A3x�A1A0��A/��A/S�A.��A-��A,�RA+�;A*��A*{A(�uA&�A%�TA#dZA!ƨA ^5A�DAĜAbA�mA�
A�PA�A��A�Al�A~�A�A�A5?AE�Av�A33A��A��A&�A��AffA�;A;dA �A	�-A��A�RAhsA�uAVA=qA$�A��A7LA(�Ap�A7LA �@���@���@�&�@�t�@�O�@��@�7L@���@�P@�V@���@��@��T@�D@��#@�9X@�K�@�~�@�hs@�@㝲@�&�@�@�S�@���@�p�@��/@�1@ץ�@�S�@�+@�
=@�ȴ@֗�@�-@��@��
@ҸR@�5?@�|�@Ώ\@Ͳ-@�z�@��#@ǅ@�X@���@�A�@�dZ@�S�@�C�@�C�@�;d@�o@\@�{@�@��h@�?}@��`@��j@�Q�@�@��@���@��7@�hs@�O�@��j@��w@��@�l�@�l�@�+@��H@���@��\@�ff@�E�@�{@�O�@��u@��@�n�@��@�S�@�5?@��T@�p�@�7L@���@��m@�l�@��!@��-@��u@�bN@��@���@��w@�|�@�V@���@���@��@��@��D@�bN@�1'@�(�@�1@��@���@�"�@�ff@��@���@��@�Z@�1'@��P@��@���@�X@�Z@�1@��
@���@���@���@��m@��;@�;d@���@�v�@���@�hs@��@�Ĝ@��D@�1@�S�@��@���@�-@�-@���@��^@���@�hs@��@�  @�dZ@���@�ff@�=q@���@���@��9@�I�@��@��@�t�@��@��@��+@��@��@�O�@��/@�Z@�9X@�  @���@���@��P@��@�\)@�
=@���@�{@���@�Ĝ@���@��@��9@��9@��j@��9@���@��D@��D@�r�@�I�@� �@��P@�dZ@�dZ@�|�@�dZ@�C�@��@���@���@�ff@���@��@�-@�{@���@���@�p�@�`B@�G�@�O�@��@�r�@���@���@�;d@�o@���@��@��@�7L@���@���@���@��D@��@��9@��9@���@��@��`@���@���@���@��9@��@�Z@�(�@��@��
@���@���@��@��F111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B�B��B��B��B��B��B��B��B��B��B��B�{B�uB�\B�%B�B�B�B�B�JB�PB�VB�VB�\B�\B�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B��BŢBƨB��B�5B�sB�B��B��B��B��BBBBJB{BuB�B�B�B�B�B�B%�B&�B �B�B�B�B�BPB	7B%BBB��B��B�B�5B��B��B��B��B�=B��B�%BgmB\)BYBJ�B+B+B�B�`B�5BɺB�dB�B��Bx�BgmBP�B49B�B{BPBB
��B
�B
�NB
ɺB
�B
��B
{�B
w�B
q�B
jB
dZB
]/B
O�B
9XB
#�B
�B
�B
�B
DB	��B	�B	�5B	�/B	�fB	�B	�yB	�mB	�fB	�B	ȴB	�wB	�B	��B	��B	��B	��B	�\B	}�B	u�B	r�B	q�B	o�B	m�B	iyB	ffB	dZB	_;B	\)B	YB	K�B	B�B	A�B	@�B	9XB	+B	�B	�B	�B	�B	�B	�B	hB	DB	B	B	B	B	B	  B��B��B��B�B�B�B�yB�TB�;B�B�
B��B��BB�^B��B��B�bB�1B�DB�VB�oB�hB�\B�uB�=B�1B�%B�B�B�B�B�B�B~�Bz�Bw�Bu�Bt�Bs�Br�Bo�BjBdZB_;B]/B]/B\)B[#BZBYBW
BT�BS�BR�BQ�BO�BN�BM�BK�BI�BH�BG�BG�BF�BE�BD�BB�BB�BA�B?}B>wB>wB>wB=qB<jB;dB9XB9XB9XB;dB<jB<jB=qB=qB=qB=qB>wB=qB=qB=qB<jB<jB=qB>wB<jB?}B@�BA�BA�BF�BJ�BO�BP�BQ�BT�BVBW
BW
BYBYB\)B^5B_;B_;B`BBcTBdZBl�Bp�Bq�Br�Br�Br�Br�Bs�By�Bz�Bz�Bz�B{�B}�B}�B~�B~�B~�B~�B�B�+B�7B�7B�1B�7B�PB�bB�{B�{B�{B��B�{B�{B�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�3B�FB�RB�RB�jBBĜBƨBƨBȴBɺB��B��B�)B�NB�TB�sB�B�B�B�B��B��B��B	B		7B	PB	bB	oB	oB	�B	�B	#�B	%�B	(�B	+B	,B	.B	.B	49B	6FB	8RB	9XB	>wB	A�B	B�B	E�B	I�B	M�B	N�B	Q�B	VB	W
B	YB	[#B	]/B	^5B	^5B	_;B	bNB	e`B	k�B	o�B	v�B	y�B	z�B	z�B	{�B	{�B	}�B	� B	�B	�1B	�=B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�3B	�3B	�3B	�3B	�9B	�9B	�3B	�-B	�9B	�?B	�9B	�9B	�3B	�9B	�?B	�?B	�LB	�XB	�dB	�qB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�1B�B�B�B�B�JB�PB�VB�VB�\B�\B�bB�uB��B��B��B��B��B��B��B��B��B��B�B��B��B�B�9B��BŢBƨB��B�NB�B�B��B��B��B  BBB%BVB�B�B�B�B�B�B�B�B)�B-B"�B�B�B �B�B\B
=B+BBB  B��B�B�TB��B�B��B��B�PB��B�VBk�B]/B]/BR�B7LBVB��B�mB�`B��B�}B�?B��B}�BjBXB<jB�B�BbB%B
��B
�B
�B
��B
�FB
��B
|�B
y�B
t�B
m�B
ffB
bNB
XB
C�B
'�B
�B
�B
�B
oB
%B	�B	�TB	�BB	�yB	�B	�B	�B	�B	�BB	��B	ǮB	�!B	�B	��B	��B	��B	��B	�B	v�B	s�B	r�B	p�B	q�B	jB	gmB	ffB	cTB	`BB	^5B	R�B	C�B	B�B	B�B	?}B	33B	�B	�B	�B	�B	�B	�B	uB	\B		7B	+B	%B	B	B	B	  B��B��B��B�B�B�B�fB�NB�/B�B�
B��BŢB��B�B��B�{B�JB�PB�VB�oB�oB�{B��B�JB�7B�1B�%B�B�B�B�1B�1B�B� By�Bv�Bu�Bu�Bt�Bs�Br�BjBbNBaHB`BB]/B[#B[#BZBYBYBVBS�BS�BR�BP�BP�BN�BM�BL�BJ�BH�BH�BG�BG�BF�BD�BD�BD�BA�B@�B@�B?}B>wB=qB=qB?}B=qB=qB=qB=qB?}B>wB>wB=qB>wB=qB=qB>wB=qB>wB>wB?}B@�B@�BA�BC�BE�BI�BM�BP�BQ�BR�BT�BVBW
BW
BYBZB]/B_;B_;B`BBaHBcTBe`Bn�Br�Br�Br�Br�Br�Bs�Bu�By�Bz�Bz�B{�B|�B}�B}�B~�B~�B� B~�B�B�1B�JB�DB�DB�DB�VB�hB��B�{B��B��B��B��B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�9B�LB�XB�^B�wBÖBĜBƨBƨBȴBɺB��B��B�/B�NB�ZB�yB�B�B�B�B��B��B��B	B		7B	PB	bB	oB	uB	�B	 �B	#�B	%�B	(�B	+B	-B	.B	0!B	5?B	6FB	8RB	:^B	?}B	B�B	C�B	F�B	J�B	M�B	O�B	Q�B	VB	W
B	YB	[#B	]/B	^5B	^5B	`BB	cTB	ffB	l�B	p�B	v�B	y�B	z�B	z�B	{�B	{�B	}�B	� B	�B	�1B	�=B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�3B	�9B	�3B	�3B	�9B	�?B	�9B	�3B	�?B	�FB	�9B	�?B	�9B	�9B	�FB	�FB	�LB	�XB	�dB	�qB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447242012010314472420120103144724  AO  ARGQ                                                                        20111130142407  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142407  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144724  IP                  G�O�G�O�G�O�                