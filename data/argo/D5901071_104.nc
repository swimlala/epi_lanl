CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:20Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               hA   AO  20111130141525  20190522121826  1727_5046_104                   2C  D   APEX                            2143                            040306                          846 @ԜW� 1   @Ԝ�_�@6e�����c��t�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B��B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DW��DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dz�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@Fff@�33@�33A   A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBh��BpffBxffB�  B�  B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C 33C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn  Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD�D�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"�D"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.��D/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;� D<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDG�DG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDX  DX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDz  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AӅAӅAӇ+AӅAӇ+AӇ+AӇ+AӉ7AӉ7AӉ7AӋDAӋDAӏ\Aӏ\AӉ7AӑhA�p�A�(�Aҕ�A�33AѬA�I�A�JA�ƨA�x�A�VAϾwAϑhA�ffA��AΑhA�"�A��A��;A���A���A�Q�A�ȴA�(�A�1'A�A�A��-A�E�A���A���A�bNA���A���A�C�A���A�%A��A�v�A�hsA���A��
A�z�A�bNA�
=A���A���A���A��mA���A�E�A��TA�x�A��FA�ȴA�C�A�"�A�1A��A��;A��-A���A��A��A��PA��A��A�v�A�VA���A�?}A�ZA��A�K�A�ĜA��A���A�&�A��A�JA�|�A��FA�`BA�?}A�C�A��9A���A�
=A�|�A��RA�n�A���A�K�A���A�  A���A�%A���A��wA�=qA���A�l�A���A���A��PA��A���A�=qA�#A}�Ax�`Au�AtM�Ar�Ao|�An(�Al��Aj(�AhI�Adr�AbbA]��AY�7AU�mAP�HAM��AK�7AJ�jAI��AG�mAEoADbAB��AAhsA@z�A>�DA=x�A<�!A<-A:~�A8M�A7O�A6�A6I�A3�A2VA1�wA1hsA0��A/��A-�
A,�yA, �A+VA*��A*ȴA*��A*��A*��A*ȴA*ZA)�A(�/A'�TA'ƨA'7LA$�A$�A${A"bA ��A"�AK�A=qA1AƨA
=A��A�A9XA�TAt�AC�A�A��A�A�jA��A��A1'Al�A��Ax�A�A$�AXA"�A��A�+AS�An�A�7A$�A�RAoA	��A��AZAZA��A v�@�
=@�`B@�9X@�C�@��@���@���@���@�+@��T@�1@�`B@�(�@�=q@���@�G�@�A�@�l�@���@�@��@�t�@◍@�@��`@���@�~�@�p�@���@�Ĝ@ܴ9@�b@۶F@�S�@�^5@���@�M�@�A�@��y@��T@�O�@У�@Ο�@���@˝�@�n�@ɩ�@��/@�9X@ǶF@�33@�E�@őh@�r�@�1@��
@���@�`B@��D@���@�7L@�M�@��;@�ȴ@���@�5?@��@�A�@� �@�ƨ@�+@���@���@�hs@�r�@���@��@�@���@��@�C�@�M�@���@��@�?}@�%@�Q�@��@��@���@��u@��
@��@�;d@��#@��`@���@��@��@���@�dZ@�$�@���@�z�@�bN@�b@�bN@���@�n�@�=q@���@��@���@��@�j@�bN@�j@�bN@� �@�C�@���@�ƨ@���@��
@��
@��;@��m@��m@��m@��w@��@�C�@�
=@��@��@�ȴ@���@���@���@�^5@�V@�$�@�@��@��@���@���@�J@�J@�J@�{@�-@�=q@�-@�$�@�{@���@��T@�@��@��@��@��9@�Q�@�9X@��@�ƨ@��\@�X@��9@�Z@���@�K�@��@�ȴ@���@�n�@�V@�@��@�O�@���@�(�@���@�E�@��@���@���@�~�@�-@�-@�^5@��@��\@�@�`B@��9@�b@�  @��
@��F@�|�@�;d@�|�@��P@��@�|�@�|�@�l�@�S�@�@��y@��H@��@���@���@���@�^5@�E�@�$�@��@�J@��T@��-@��h@��7@�hs@�?}@��@���@�z�@�Z@�A�@���@��;@��;@��w@���@���@���@��P@�\)@�"�@�
=@��y@��H@�n�@��@��h@�7L@��@�V@���@��@���@��@�z�@�bN@�I�@�w@�@~�@z�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AӅAӅAӇ+AӅAӇ+AӇ+AӇ+AӉ7AӉ7AӉ7AӋDAӋDAӏ\Aӏ\AӉ7AӑhA�p�A�(�Aҕ�A�33AѬA�I�A�JA�ƨA�x�A�VAϾwAϑhA�ffA��AΑhA�"�A��A��;A���A���A�Q�A�ȴA�(�A�1'A�A�A��-A�E�A���A���A�bNA���A���A�C�A���A�%A��A�v�A�hsA���A��
A�z�A�bNA�
=A���A���A���A��mA���A�E�A��TA�x�A��FA�ȴA�C�A�"�A�1A��A��;A��-A���A��A��A��PA��A��A�v�A�VA���A�?}A�ZA��A�K�A�ĜA��A���A�&�A��A�JA�|�A��FA�`BA�?}A�C�A��9A���A�
=A�|�A��RA�n�A���A�K�A���A�  A���A�%A���A��wA�=qA���A�l�A���A���A��PA��A���A�=qA�#A}�Ax�`Au�AtM�Ar�Ao|�An(�Al��Aj(�AhI�Adr�AbbA]��AY�7AU�mAP�HAM��AK�7AJ�jAI��AG�mAEoADbAB��AAhsA@z�A>�DA=x�A<�!A<-A:~�A8M�A7O�A6�A6I�A3�A2VA1�wA1hsA0��A/��A-�
A,�yA, �A+VA*��A*ȴA*��A*��A*��A*ȴA*ZA)�A(�/A'�TA'ƨA'7LA$�A$�A${A"bA ��A"�AK�A=qA1AƨA
=A��A�A9XA�TAt�AC�A�A��A�A�jA��A��A1'Al�A��Ax�A�A$�AXA"�A��A�+AS�An�A�7A$�A�RAoA	��A��AZAZA��A v�@�
=@�`B@�9X@�C�@��@���@���@���@�+@��T@�1@�`B@�(�@�=q@���@�G�@�A�@�l�@���@�@��@�t�@◍@�@��`@���@�~�@�p�@���@�Ĝ@ܴ9@�b@۶F@�S�@�^5@���@�M�@�A�@��y@��T@�O�@У�@Ο�@���@˝�@�n�@ɩ�@��/@�9X@ǶF@�33@�E�@őh@�r�@�1@��
@���@�`B@��D@���@�7L@�M�@��;@�ȴ@���@�5?@��@�A�@� �@�ƨ@�+@���@���@�hs@�r�@���@��@�@���@��@�C�@�M�@���@��@�?}@�%@�Q�@��@��@���@��u@��
@��@�;d@��#@��`@���@��@��@���@�dZ@�$�@���@�z�@�bN@�b@�bN@���@�n�@�=q@���@��@���@��@�j@�bN@�j@�bN@� �@�C�@���@�ƨ@���@��
@��
@��;@��m@��m@��m@��w@��@�C�@�
=@��@��@�ȴ@���@���@���@�^5@�V@�$�@�@��@��@���@���@�J@�J@�J@�{@�-@�=q@�-@�$�@�{@���@��T@�@��@��@��@��9@�Q�@�9X@��@�ƨ@��\@�X@��9@�Z@���@�K�@��@�ȴ@���@�n�@�V@�@��@�O�@���@�(�@���@�E�@��@���@���@�~�@�-@�-@�^5@��@��\@�@�`B@��9@�b@�  @��
@��F@�|�@�;d@�|�@��P@��@�|�@�|�@�l�@�S�@�@��y@��H@��@���@���@���@�^5@�E�@�$�@��@�J@��T@��-@��h@��7@�hs@�?}@��@���@�z�@�Z@�A�@���@��;@��;@��w@���@���@���@��P@�\)@�"�@�
=@��y@��H@�n�@��@��h@�7L@��@�V@���@��@���@��@�z�@�bN@�I�@�w@�@~�@z�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBB�wB�XB�?B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�wBĜB��B��B��B��B��B��B��B��B��B��B��BǮBŢBɺB��B��BȴBǮB��B�}B�qB�jB�XB�LB�?B�B��B�JB�1B�%B�%B�B�B� Bv�Bm�BdZB[#BT�BP�BJ�BF�BB�B@�B<jB%�B�B�B\B	7BBB��B��B�yB�HB��B�}B�-B��B�\Bs�BT�BN�BG�B9XB/B�BDB
�ZB
B
�?B
�B
��B
��B
��B
�7B
x�B
m�B
cTB
[#B
R�B
A�B
#�B
hB
1B	��B	�B	�ZB	�B	ƨB	�RB	��B	�hB	|�B	[#B	;dB	
=B��B��B�B�B�TB�/B�/B��B��B��BǮBÖB�}B�dB�FB�3B�B�B��B��B��B��B��B��B�{B�oB�bB�\B�VB�VB�VB�VB�PB�PB�PB�JB�=B�DB�PB�JB�1B�%B�7B�+B�B~�Bz�Bz�B{�B{�Bz�B{�B{�B{�B{�Bz�Bz�Bz�By�By�By�Bx�Bx�Bw�Bu�Bs�Bq�Bp�Bn�Bm�Bl�Bk�BjBgmBe`BbNB^5BYBT�BO�BK�BH�BE�B@�B=qB;dB:^B8RB7LB5?B33B1'B0!B/B.B,B)�B)�B)�B'�B'�B&�B%�B$�B#�B#�B#�B"�B!�B!�B!�B�B�B �B �B �B�B�B�B�B�B�B!�B%�B(�B-B-B+B+B+B)�B)�B'�B%�B%�B%�B/B33B49B9XB:^B9XB8RB7LB7LB49B0!B-B-B/B.B/B2-B5?B7LB8RB9XB:^B<jB;dB=qB?}BA�BD�BE�BG�BM�BQ�BVBVBW
BXB\)BaHBaHBhsBp�By�B{�B�B�\B��B��B��B��B��B��B��B�B�!B�9B�RB��B��B�B�
B�
B�
B�5B�`B�B�B�B�B�B�B��B	  B	B	B	B	B	B	B	%B	
=B	PB	bB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	$�B	$�B	%�B	%�B	&�B	'�B	(�B	)�B	)�B	+B	-B	.B	1'B	6FB	=qB	?}B	B�B	G�B	G�B	G�B	H�B	M�B	O�B	N�B	M�B	M�B	N�B	P�B	P�B	T�B	[#B	^5B	^5B	bNB	hsB	iyB	hsB	ffB	ffB	ffB	dZB	bNB	aHB	bNB	bNB	e`B	n�B	p�B	q�B	q�B	r�B	s�B	s�B	s�B	u�B	x�B	� B	�B	�B	�+B	�1B	�1B	�1B	�7B	�=B	�DB	�JB	�PB	�PB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�3B	�3B	�3B	�9B	�FB	�XB	�jB	�qB	�wB	�wB	��B	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�y11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBŢBÖB��B�dB�RB�3B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�3B�?B�XBÖBɺB��B��B��B��B��B��B��B��B��B��B��B��BɺB��B��B��B��B��BB��BB��B�qB�qB�^B�-B��B�VB�7B�%B�%B�B�B�Bz�Bq�BgmB]/BVBR�BL�BH�BD�BE�BC�B(�B�B�BoBDBBBB��B�B�mB��BB�FB��B��B}�BW
BQ�BK�B<jB5?B&�B�B
��B
ȴB
�XB
�!B
��B
��B
��B
�uB
� B
s�B
ffB
^5B
ZB
O�B
,B
�B
VB
B	�B	�sB	�BB	��B	��B	��B	��B	�7B	e`B	F�B	\B��B��B�B�B�sB�;B�;B�B��B��B��BŢB��B�}B�jB�FB�!B�B�B��B��B��B��B��B��B��B�uB�oB�\B�VB�VB�VB�PB�PB�VB�PB�PB�VB�VB�VB�\B�1B�7B�JB�+B�B�B}�B|�B|�B|�B}�B{�B|�B|�B|�B{�B{�Bz�Bz�Bz�Bx�Bx�By�Bx�Bv�Bu�Br�Br�Bp�Bm�Bl�Bl�Bl�BiyBffBdZB_;B[#BVBT�BK�BL�BH�BA�B>wB<jB:^B8RB8RB6FB33B2-B1'B/B/B.B,B-B(�B(�B(�B'�B'�B%�B$�B$�B$�B#�B"�B"�B#�B!�B!�B!�B �B �B �B �B �B �B �B%�B'�B+B.B.B.B-B,B+B+B(�B&�B&�B&�B0!B49B5?B:^B:^B;dB:^B8RB8RB8RB49B1'B/B/B/B1'B33B5?B8RB9XB:^B;dB=qB=qB?}B@�BC�BE�BG�BJ�BM�BR�BVBW
BXBZB^5BbNBcTBjBq�By�B|�B�B�hB��B��B��B��B��B��B��B�B�!B�9B�RB�}B��B�B�
B�B�B�5B�`B�B�B�B�B�B�B��B	  B	B	B	B	B	B	B	%B	
=B	VB	bB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	$�B	$�B	%�B	%�B	&�B	'�B	(�B	)�B	)�B	+B	-B	.B	2-B	7LB	=qB	@�B	C�B	G�B	G�B	H�B	J�B	O�B	P�B	O�B	N�B	N�B	N�B	P�B	P�B	T�B	[#B	^5B	_;B	bNB	iyB	jB	iyB	hsB	gmB	gmB	ffB	dZB	aHB	bNB	bNB	dZB	o�B	q�B	r�B	r�B	s�B	s�B	s�B	s�B	u�B	x�B	� B	�B	�B	�+B	�1B	�1B	�1B	�=B	�=B	�DB	�JB	�PB	�PB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�-B	�3B	�3B	�3B	�9B	�FB	�XB	�jB	�qB	�wB	�}B	B	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�y11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<�o<#�
<#�
<#�
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
<D��<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447102012010314471020120103144710  AO  ARGQ                                                                        20111130141525  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141525  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144710  IP                  G�O�G�O�G�O�                