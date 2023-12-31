CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:06Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               7A   AO  20111130140330  20190522121826  1727_5046_055                   2C  D   APEX                            2143                            040306                          846 @�\��< 1   @�\�`� @7<�hr��c�V�u1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF�CH  CI�fCL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D��D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.fD.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?� D?��D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  DsffDys31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffB  B  B ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�ffB�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�  B�  B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB  CD�CF33CH�CJ  CL�CN�CP�CR�CT�CV�CX33CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD� DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD� DfD�fDfD�fD  D�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(  D(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.�D.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9  D9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>��D?fD?�fD@  D@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDG�DG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP� DQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDsl�Dyy�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�\)A�VA�S�A�S�A�XA�hsA�jA�p�A�|�A�~�A��A��A�|�A�~�A��A��7A��7A��+A��A��A��A��A��+A��7A��DA��DA��\A��hA��PA��DA��+A��+A��A�v�A�`BA�"�A�n�A���A��A�A��yA��FA�JA�  A��TA�jA��wA��+A�5?A�ĜA�v�A�&�A�{A��A�ĜA�%A��A� �A��yA�ZA�r�A��A���A�O�A�JA���A�"�A��A�A��FA�|�A�O�A�E�A��A�;dA��+A��/A�/A���A�33A���A�hsA�;dA��A���A��jA�bA�bA�(�A���A�ffA��A���A���A��wA��RA��-A�r�A���A�1A�v�A�;dA�E�A��A��A��A�`BA��RA�5?A�mA?}A~v�A}�
A}��A}Ax�As%Ao|�Ak�hAh��Ae��AdZAc�7Aa��A_G�A[oAX�AU�ATQ�AS��AR�+ARjARVAQhsAP�+AM�FAK�AIAES�AA�mA@��A?�hA>�A<��A< �A:�+A9S�A8�jA8 �A7��A6ZA5/A4�A4��A4ZA3��A2�A2Q�A0��A/\)A/VA.��A.jA-A,ZA*�A*VA)S�A(bNA(bA'��A&�A&�jA&~�A%�wA$bNA#VA"n�A!|�A �\A��A�A��A��AbNA�A?}A��A �Al�Az�A��A&�A��AjA �A�A�^A|�AhsA%A�9A{A�-A�A�A�A�^A��A��A�hA|�AC�A�/AbAƨA��AoA�DAz�AAS�A
�\A
A	��A	�
A	�#A	\)AĜA9XA�#Ap�A�yA-A�
AA�^AS�A�\AZA�A=qAE�AffAM�A��@���@�C�@��`@��@�!@��@��@�@�ff@���@���@��@�V@�
=@�v�@�V@��@�K�@�x�@��`@�r�@�1'@ӶF@�\)@�x�@�E�@�X@�bN@�t�@��#@��H@�5?@�@�?}@��@þw@ÍP@��y@+@�J@�%@�Z@��@�ƨ@���@�33@���@��@��\@�-@���@�O�@��@�^5@��h@��/@���@���@�bN@���@�33@��@��R@�J@�%@��@��F@��P@�$�@�z�@�ƨ@��R@���@��h@�ȴ@���@���@��@�x�@���@���@��/@�z�@���@�(�@�%@��#@�p�@���@�&�@��+@�ff@�J@���@�V@�1'@�1'@�9X@���@�^5@�9X@�l�@��7@��T@���@��P@��H@�ff@�;d@��y@�v�@��@�J@�$�@�@���@�{@�^5@��@���@��^@�ff@�  @�S�@���@��^@��y@��/@�J@�`B@�&�@��w@��y@���@�-@��+@��@��F@�ƨ@��F@��w@���@���@��@�I�@�Z@�bN@�j@�Q�@���@��j@��D@��@��F@��
@��@��!@�5?@�V@� �@��;@��;@��@�1@�1'@�Z@�  @��F@�%@�@�M�@�`B@�C�@��@�t�@�t�@�r�@��@��@�~�@�I�@�V@�x�@��-@���@��/@��@���@�E�@�hs@���@��j@��@�Z@� �@��P@�l�@�t�@�K�@�;d@��@���@�ff@�=q@��T@�x�@�/@��9@��D@�bN@� �@�dZ@�33@�K�@�t�@��P@��@���@��P@���@���@���@�~�@�E�@�$�@�J@���@��#@��-@�x�@�`B@�O�@�7L@�%@���@� �@+@\)@\)@
=@|�j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�\)A�VA�S�A�S�A�XA�hsA�jA�p�A�|�A�~�A��A��A�|�A�~�A��A��7A��7A��+A��A��A��A��A��+A��7A��DA��DA��\A��hA��PA��DA��+A��+A��A�v�A�`BA�"�A�n�A���A��A�A��yA��FA�JA�  A��TA�jA��wA��+A�5?A�ĜA�v�A�&�A�{A��A�ĜA�%A��A� �A��yA�ZA�r�A��A���A�O�A�JA���A�"�A��A�A��FA�|�A�O�A�E�A��A�;dA��+A��/A�/A���A�33A���A�hsA�;dA��A���A��jA�bA�bA�(�A���A�ffA��A���A���A��wA��RA��-A�r�A���A�1A�v�A�;dA�E�A��A��A��A�`BA��RA�5?A�mA?}A~v�A}�
A}��A}Ax�As%Ao|�Ak�hAh��Ae��AdZAc�7Aa��A_G�A[oAX�AU�ATQ�AS��AR�+ARjARVAQhsAP�+AM�FAK�AIAES�AA�mA@��A?�hA>�A<��A< �A:�+A9S�A8�jA8 �A7��A6ZA5/A4�A4��A4ZA3��A2�A2Q�A0��A/\)A/VA.��A.jA-A,ZA*�A*VA)S�A(bNA(bA'��A&�A&�jA&~�A%�wA$bNA#VA"n�A!|�A �\A��A�A��A��AbNA�A?}A��A �Al�Az�A��A&�A��AjA �A�A�^A|�AhsA%A�9A{A�-A�A�A�A�^A��A��A�hA|�AC�A�/AbAƨA��AoA�DAz�AAS�A
�\A
A	��A	�
A	�#A	\)AĜA9XA�#Ap�A�yA-A�
AA�^AS�A�\AZA�A=qAE�AffAM�A��@���@�C�@��`@��@�!@��@��@�@�ff@���@���@��@�V@�
=@�v�@�V@��@�K�@�x�@��`@�r�@�1'@ӶF@�\)@�x�@�E�@�X@�bN@�t�@��#@��H@�5?@�@�?}@��@þw@ÍP@��y@+@�J@�%@�Z@��@�ƨ@���@�33@���@��@��\@�-@���@�O�@��@�^5@��h@��/@���@���@�bN@���@�33@��@��R@�J@�%@��@��F@��P@�$�@�z�@�ƨ@��R@���@��h@�ȴ@���@���@��@�x�@���@���@��/@�z�@���@�(�@�%@��#@�p�@���@�&�@��+@�ff@�J@���@�V@�1'@�1'@�9X@���@�^5@�9X@�l�@��7@��T@���@��P@��H@�ff@�;d@��y@�v�@��@�J@�$�@�@���@�{@�^5@��@���@��^@�ff@�  @�S�@���@��^@��y@��/@�J@�`B@�&�@��w@��y@���@�-@��+@��@��F@�ƨ@��F@��w@���@���@��@�I�@�Z@�bN@�j@�Q�@���@��j@��D@��@��F@��
@��@��!@�5?@�V@� �@��;@��;@��@�1@�1'@�Z@�  @��F@�%@�@�M�@�`B@�C�@��@�t�@�t�@�r�@��@��@�~�@�I�@�V@�x�@��-@���@��/@��@���@�E�@�hs@���@��j@��@�Z@� �@��P@�l�@�t�@�K�@�;d@��@���@�ff@�=q@��T@�x�@�/@��9@��D@�bN@� �@�dZ@�33@�K�@�t�@��P@��@���@��P@���@���@���@�~�@�E�@�$�@�J@���@��#@��-@�x�@�`B@�O�@�7L@�%@���@� �@+@\)@\)@
=@|�j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB?}B?}B?}B>wBA�BE�BF�BG�BI�BI�BJ�BK�BI�BI�BK�BL�BL�BL�BL�BK�BK�BL�BL�BM�BM�BM�BM�BN�BN�BO�BP�BP�BQ�BQ�BQ�BQ�BR�BcTBm�B~�B�hB�B��B��B��B��B��B��B�oB�hB�\B�\B�VB�VB�JB�DB�+B�B�B~�Bx�Bo�BhsBdZB`BBYBQ�BI�BB�B>wB:^B7LB6FB2-B$�B�B  B�B�B�HB�#B��B�dB��B�+BS�B�BB
��B
�B
�mB
�TB
�HB
�5B
�)B
�)B
�B
��B
��B
��B
|�B
hsB
]/B
C�B
5?B
!�B
�B

=B
B	��B	��B	�B	�B	�mB	�/B	ÖB	��B	{�B	hsB	O�B	D�B	?}B	6FB	&�B	{B��B��B�B�sB�ZB�TB�NB�BB�;B�;B�mB�mB�#B��BŢB�qB�dB�RB�FB�-B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�dB�dB�^B�XB�qB��B�}B�}B��BÖBŢBƨBǮBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BȴBÖB�qB�jB�jB�dB�XB�?B�-B�!B�RB�qB�}B�wB�LB��B��B�bB�%Bv�BjBgmBcTBaHB`BBjBaHBO�BE�BD�BC�BB�BA�B@�B@�B@�B?}B?}B>wB?}BD�BC�BB�BB�BB�BG�BH�BH�BH�BK�BL�BL�BP�BR�BT�BXBYBYBZB[#B]/B^5B_;B`BBcTBffBiyBr�B|�B�B�B�B�B�%B�7B�=B�DB�DB�PB�hB��B��B��B��B��B��B��B��B�B�?B�LB�jB�B	�B	\B	B�B�#BɺB�qB��B�#B�)B�HB��B	�B	�B	�B	$�B	8RB	D�B	K�B	O�B	Q�B	P�B	J�B	I�B	D�B	M�B	ZB	[#B	ZB	^5B	dZB	e`B	cTB	_;B	bNB	e`B	e`B	ffB	hsB	jB	iyB	hsB	q�B	r�B	hsB	]/B	ZB	S�B	H�B	33B	2-B	2-B	2-B	.B	.B	0!B	49B	8RB	<jB	A�B	C�B	D�B	G�B	I�B	K�B	R�B	XB	ZB	ZB	\)B	`BB	e`B	gmB	jB	k�B	l�B	n�B	q�B	s�B	q�B	o�B	q�B	q�B	q�B	r�B	s�B	s�B	u�B	u�B	u�B	� B	�B	y�B	~�B	�=B	�=B	�\B	�bB	��B	��B	��B	��B	�VB	�+B	�B	�7B	�JB	�PB	�PB	�\B	�bB	�PB	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�FB	�LB	�dB	��B	ÖB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�
B	�
B	�B	�B	�/B	�5B	�s1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B?}B?}B?}B>wBA�BE�BF�BG�BI�BI�BJ�BK�BI�BI�BK�BL�BL�BL�BL�BK�BK�BL�BL�BM�BM�BM�BM�BN�BN�BO�BP�BP�BQ�BQ�BR�BS�BZBiyBr�B�B��B�B�B�B��B��B��B��B�{B�uB�hB�\B�\B�\B�hB�VB�7B�+B�%B�B�Br�BjBe`BbNB[#BT�BK�BC�B?}B;dB7LB7LB6FB'�B�BB�B�B�TB�/B�B��B�B��BgmB�B1B
��B
�B
�yB
�ZB
�NB
�5B
�)B
�)B
�)B
�B
��B
�?B
�B
n�B
e`B
H�B
;dB
&�B
�B
PB
B	��B	��B	�B	�B	�B	�B	��B	��B	�1B	r�B	YB	H�B	A�B	:^B	.B	�B	B��B�B�B�mB�TB�NB�TB�NB�mB�B�B�TB��BǮB��B�wB�dB�LB�FB�'B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�9B�dB�jB�dB�dB�wBB��B��B��BÖBŢBƨBǮB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��BŢB�wB�jB�jB�qB�jB�FB�FB�3B�RB�qB��B��B�wB�-B��B�{B�VB�Bl�BiyBdZBbNBaHBp�Bl�B[#BF�BE�BF�BE�BD�BA�BA�BA�B@�B@�BB�BE�BF�BE�BD�BE�BG�BH�BH�BI�BJ�BL�BL�BM�BQ�BS�BVBYBYBZBZB\)B]/B^5B`BBaHBdZBgmBk�Bu�B}�B�B�B�B�%B�+B�=B�DB�DB�JB�PB�uB��B��B��B��B��B��B��B��B��B�FB�FB�B�HB	�B	oB		7B��B�BB��B�jB��B�)B�#B�HB�B	�B	�B	�B	�B	5?B	D�B	K�B	P�B	S�B	S�B	J�B	L�B	C�B	J�B	ZB	\)B	[#B	]/B	e`B	ffB	hsB	aHB	bNB	e`B	e`B	ffB	hsB	l�B	k�B	e`B	p�B	w�B	hsB	^5B	\)B	YB	R�B	8RB	33B	33B	5?B	0!B	/B	0!B	33B	7LB	<jB	A�B	C�B	D�B	G�B	I�B	J�B	Q�B	XB	ZB	ZB	\)B	_;B	e`B	gmB	k�B	k�B	l�B	o�B	r�B	t�B	s�B	q�B	q�B	q�B	q�B	r�B	s�B	s�B	v�B	u�B	s�B	~�B	�B	z�B	{�B	�DB	�7B	�\B	�VB	��B	��B	��B	��B	�oB	�1B	�B	�7B	�PB	�VB	�VB	�hB	�hB	�VB	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�FB	�LB	�dB	��B	ÖB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�/B	�5B	�s1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<�o<D��<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<e`B<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446532012010314465320120103144653  AO  ARGQ                                                                        20111130140330  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140330  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144653  IP                  G�O�G�O�G�O�                