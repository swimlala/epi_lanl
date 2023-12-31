CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:36Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142825  20190522121827  1727_5046_161                   2C  D   APEX                            2143                            040306                          846 @����(7�1   @��ŕθ@6�`A�7L�c��-V1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D+��D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6�fD7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Fff@�33@�33A   A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8��B@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�ffB�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD��DfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,  D,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6��D7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDN  DN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aҝ�A�oA���A�v�A�VA�?}A�-A��A�JA���A��A��HA��
A���A�ƨAϾwAϸRAϲ-AϬAϧ�Aϣ�Aϛ�A�`BA�l�A��AʾwA�oA�I�A���AĴ9A�v�A�ffA�ȴA���A�S�A�A���A�S�A�Q�A���A���A�~�A�A�A�33A���A��A�;dA�A�~�A��A��A��^A���A�dZA���A���A�dZA��A��
A���A��uA�r�A�?}A��A��A��DA�hsA��A�ffA��A��A�S�A�
=A���A���A�|�A�v�A�O�A�JA���A�t�A�{A��yA���A���A�oA�hsA�ĜA��!A�"�A�I�A��/A�v�A�A�"�A���A���A���A��yA��A��jA�XA�;dA�/A��A���A��`A�I�A���A�|�A�Q�A��uA�5?A�`BA��9A���A��A��!A�A��A��-A���A�/A��A��A�M�A���A��HA� �A�ffA�A}/AwƨArJAnZAi��AgC�Ae�AdI�Abn�A`1A]S�A\�A[�AZ�/AZ=qAY��AYAU�AN�AL�uAJĜAH��AF�!AD�AB�!A@�uA?33A=��A<�jA<~�A<bNA;�wA:ȴA6�/A333A2�\A0�A/�;A/�PA.�`A.ffA.  A-\)A+�PA*bNA)�A)�-A)l�A)C�A)+A(��A(9XA&��A&ZA%&�A#t�A"�+A!��A �A�AI�A�TA��A��AS�A�A"�AJAA��AS�A�A�-A��AhsA%A�;A�uA�-AG�A%A
^5A	t�A��AbNA�A�AjAn�A=qA7LAA�mA ��A z�@�n�@��@�@���@���@�  @�;d@���@��@�O�@�C�@���@�G�@�o@�j@�I�@�bN@�@�z�@���@�~�@��@�z�@�r�@�b@睲@�{@���@�R@�p�@���@�@���@�X@�(�@۾w@�+@���@ى7@�  @֟�@��#@�(�@�
=@�`B@�9X@���@���@˥�@�S�@ʟ�@�V@��@�@�/@�%@���@���@�\)@���@��@�&�@ēu@��@�dZ@�
=@°!@�E�@���@���@�"�@��y@���@��R@�@���@��@�?}@���@��u@�
=@�bN@��@�1@��
@�ƨ@��@��P@�t�@�S�@�"�@��y@�v�@��@�A�@�  @��;@�K�@�~�@�-@��T@��7@�r�@�|�@�@���@��\@�E�@�{@�G�@��@�A�@��@�l�@�@�M�@�hs@�X@��@���@���@���@�Q�@�  @���@�t�@�K�@�+@��y@��+@�$�@��@�{@��@���@�?}@�7L@�O�@�x�@���@��#@�E�@�$�@���@�I�@��
@�|�@�
=@���@�@�@��y@��@�33@��!@��-@��T@���@���@�X@�7L@�r�@���@�K�@�+@��@�"�@�C�@�K�@�\)@���@��m@��w@�dZ@�"�@�ȴ@�^5@��@�&�@��D@���@�"�@��!@�{@��T@��@�V@��@�
=@�S�@�{@�X@�V@�9X@�1'@��P@���@��R@��R@�~�@�$�@��T@���@��^@���@��@�`B@��u@��m@��P@�S�@��@�V@��@���@�V@��@���@���@�1'@�(�@� �@��@��@�b@�  @��m@��w@�dZ@�~�@�V@�-@�$�@�{@��@��-@��7@�hs@�?}@��/@�z�@�1'@���@���@��w@��@���@���@���@���@��P@�|�@�l�@�\)@��@�{@���@��h@�p�@�V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aҝ�A�oA���A�v�A�VA�?}A�-A��A�JA���A��A��HA��
A���A�ƨAϾwAϸRAϲ-AϬAϧ�Aϣ�Aϛ�A�`BA�l�A��AʾwA�oA�I�A���AĴ9A�v�A�ffA�ȴA���A�S�A�A���A�S�A�Q�A���A���A�~�A�A�A�33A���A��A�;dA�A�~�A��A��A��^A���A�dZA���A���A�dZA��A��
A���A��uA�r�A�?}A��A��A��DA�hsA��A�ffA��A��A�S�A�
=A���A���A�|�A�v�A�O�A�JA���A�t�A�{A��yA���A���A�oA�hsA�ĜA��!A�"�A�I�A��/A�v�A�A�"�A���A���A���A��yA��A��jA�XA�;dA�/A��A���A��`A�I�A���A�|�A�Q�A��uA�5?A�`BA��9A���A��A��!A�A��A��-A���A�/A��A��A�M�A���A��HA� �A�ffA�A}/AwƨArJAnZAi��AgC�Ae�AdI�Abn�A`1A]S�A\�A[�AZ�/AZ=qAY��AYAU�AN�AL�uAJĜAH��AF�!AD�AB�!A@�uA?33A=��A<�jA<~�A<bNA;�wA:ȴA6�/A333A2�\A0�A/�;A/�PA.�`A.ffA.  A-\)A+�PA*bNA)�A)�-A)l�A)C�A)+A(��A(9XA&��A&ZA%&�A#t�A"�+A!��A �A�AI�A�TA��A��AS�A�A"�AJAA��AS�A�A�-A��AhsA%A�;A�uA�-AG�A%A
^5A	t�A��AbNA�A�AjAn�A=qA7LAA�mA ��A z�@�n�@��@�@���@���@�  @�;d@���@��@�O�@�C�@���@�G�@�o@�j@�I�@�bN@�@�z�@���@�~�@��@�z�@�r�@�b@睲@�{@���@�R@�p�@���@�@���@�X@�(�@۾w@�+@���@ى7@�  @֟�@��#@�(�@�
=@�`B@�9X@���@���@˥�@�S�@ʟ�@�V@��@�@�/@�%@���@���@�\)@���@��@�&�@ēu@��@�dZ@�
=@°!@�E�@���@���@�"�@��y@���@��R@�@���@��@�?}@���@��u@�
=@�bN@��@�1@��
@�ƨ@��@��P@�t�@�S�@�"�@��y@�v�@��@�A�@�  @��;@�K�@�~�@�-@��T@��7@�r�@�|�@�@���@��\@�E�@�{@�G�@��@�A�@��@�l�@�@�M�@�hs@�X@��@���@���@���@�Q�@�  @���@�t�@�K�@�+@��y@��+@�$�@��@�{@��@���@�?}@�7L@�O�@�x�@���@��#@�E�@�$�@���@�I�@��
@�|�@�
=@���@�@�@��y@��@�33@��!@��-@��T@���@���@�X@�7L@�r�@���@�K�@�+@��@�"�@�C�@�K�@�\)@���@��m@��w@�dZ@�"�@�ȴ@�^5@��@�&�@��D@���@�"�@��!@�{@��T@��@�V@��@�
=@�S�@�{@�X@�V@�9X@�1'@��P@���@��R@��R@�~�@�$�@��T@���@��^@���@��@�`B@��u@��m@��P@�S�@��@�V@��@���@�V@��@���@���@�1'@�(�@� �@��@��@�b@�  @��m@��w@�dZ@�~�@�V@�-@�$�@�{@��@��-@��7@�hs@�?}@��/@�z�@�1'@���@���@��w@��@���@���@���@���@��P@�|�@�l�@�\)@��@�{@���@��h@�p�@�V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBH�BE�BD�BD�BE�BE�BF�BF�BG�BG�BG�BG�BH�BH�BI�BI�BI�BI�BI�BI�BJ�BI�BJ�B@�B8RB5?B/B2-B1'B.B(�B/BD�BF�BK�BW
B\)B_;Be`Bs�Bt�B�B�B�7B�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�?B�^B�}BɺB��B��B�
B�
B�B�B�
B��B��BƨBÖB��B�jB�?B�jB��B�qB��B�B�B�DB�DB~�Bx�Bo�BaHBG�B33B(�B�BDB��B�/B��B�wB�'B��B{�BgmBF�B.B%B
�5B
��B
�dB
�B
��B
�bB
n�B
5?B
JB	�fB	�jB	��B	�B	q�B	hsB	]/B	P�B	=qB	0!B	'�B	#�B	�B	�B	�B	VB��B�;BǮB�wB�-B��B��B��B��B��B��B��B��B��B�oB�PB�=B�PB�{B�JB�7B�DB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�oB�VB�DB�1B�%B�B�B�B� B}�B|�B|�B{�Bx�Bz�Bv�Bv�Bu�Bu�Bv�Bs�Bp�BjBjBk�BjBk�Bm�Bm�Bn�Bn�Bm�Bl�Bl�BjBhsBe`Be`BffBdZB`BB]/B\)B[#B[#B[#B[#B[#BZBYBYBW
BT�BVBXBZB\)B^5B`BBbNBcTBbNBbNBbNBcTBcTBdZBe`BffBgmBhsBgmBjBl�Bl�BjBiyBhsBk�Bm�Bn�Bo�Bp�Bs�Bv�Bx�B�B�7B�=B�=B�JB�JB�JB�JB�VB�\B�\B�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�dB�jB�jB�jB�jB�qB�qB�qB�wB�wB�}B��BŢBɺB��B��B��B��B��B��B��B�B�)B�/B�/B�/B�5B�5B�NB�fB�mB�B�B�B��B��B��B	  B	B	B	%B	1B	DB	VB	bB	bB	bB	hB	�B	�B	�B	!�B	"�B	)�B	/B	1'B	2-B	33B	5?B	7LB	@�B	E�B	C�B	H�B	N�B	N�B	M�B	N�B	P�B	P�B	R�B	VB	_;B	`BB	cTB	ffB	iyB	iyB	iyB	l�B	r�B	q�B	q�B	q�B	q�B	r�B	t�B	u�B	w�B	z�B	~�B	~�B	~�B	�B	� B	� B	}�B	}�B	� B	~�B	}�B	}�B	|�B	~�B	�B	�+B	�=B	�JB	�JB	�JB	�JB	�DB	�JB	�PB	�PB	�VB	�\B	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�-B	�3B	�9B	�LB	�RB	�XB	�XB	�XB	�XB	�^B	�dB	�dB	�qB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�
B	�)B	�;B	�;B	�;B	�B111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BH�BK�BF�BE�BE�BE�BF�BF�BG�BG�BG�BG�BH�BH�BI�BI�BI�BI�BI�BI�BJ�BJ�BM�BF�B<jB?}B6FB49B6FB33B.B6FBE�BG�BM�BZB]/B_;BffBv�Bx�B�B�B�=B�{B��B��B�B�B��B��B��B��B�B��B��B�B�B�B�B�B�B�B�B��B��B��B�B��B��B��B��B��B�B�B�!B�-B�FB�dB��B��B��B�B�B�B�/B�)B�
B�
B��BȴBŢBÖB�}B�LB�jB��BƨB��B�%B�%B�hB�bB� Bz�Bt�BjBO�B5?B0!B�BuBB�`B��B��B�FB��B� Bo�BJ�B7LB\B
�NB
��B
�wB
�B
��B
��B
~�B
A�B
�B	��B	ǮB	�B	�7B	u�B	m�B	cTB	XB	E�B	49B	)�B	%�B	!�B	�B	�B	�B	PB�mB��BŢB�RB�!B��B��B��B��B��B��B��B��B��B��B�{B�\B��B�\B�=B�PB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�\B�PB�7B�%B�B�B�B�B�B� B~�B}�B}�By�Bw�Bw�Bx�Bw�Bv�Bs�Bl�Bk�Bl�Bl�Bn�Bo�Bo�Bp�Bq�Bo�Bl�Bm�Bm�Bl�Bk�BhsBgmBhsBdZB_;B^5B\)B]/B\)B\)B\)B[#B\)BZBZBYBZBYBZB\)B^5BbNBdZBe`BdZBbNBcTBdZBffBgmBiyBhsBhsBiyBjBl�Bn�Bm�Bk�BjBjBm�Bo�Bo�Bq�Br�Bv�Bx�B{�B�B�7B�DB�DB�PB�JB�JB�PB�VB�\B�bB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�B�!B�!B�B�!B�?B�jB�jB�jB�jB�jB�qB�qB�qB�wB�wB��BBƨBɺB��B��B��B��B��B��B��B�B�/B�/B�/B�5B�5B�;B�TB�mB�sB�B�B�B��B��B��B	B	B	B	+B		7B	JB	VB	bB	bB	hB	oB	�B	�B	�B	!�B	#�B	+B	/B	1'B	2-B	33B	5?B	6FB	@�B	G�B	D�B	I�B	O�B	O�B	M�B	N�B	P�B	P�B	R�B	VB	`BB	bNB	cTB	ffB	iyB	jB	iyB	m�B	t�B	r�B	q�B	q�B	q�B	r�B	t�B	u�B	v�B	z�B	~�B	� B	� B	�B	�B	�B	� B	~�B	�B	� B	~�B	~�B	|�B	~�B	�B	�%B	�=B	�JB	�VB	�PB	�JB	�JB	�JB	�VB	�VB	�VB	�\B	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�-B	�3B	�?B	�LB	�RB	�XB	�XB	�XB	�XB	�^B	�dB	�jB	�}B	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�;B	�;B	�BB	�B111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<D��<e`B<�o<49X<e`B<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447302012010314473020120103144730  AO  ARGQ                                                                        20111130142825  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142825  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144730  IP                  G�O�G�O�G�O�                