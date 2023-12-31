CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:23Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140823  20181024140823  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               jA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��%r1   @��%�/{@3�z�G��cҗ�O�;1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      jA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0ffB8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B���B���B�  B�  B�  B�ffB���B�  B�  B���B�  B���B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,�C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCc�fCf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C��C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C��C��C��C��C��C�  C�  C��3C��3C��3C�  C�  C��C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��D fD � D  D� D  D� D  D� DfD� D  D� DfD� D  Dy�D  D� D	  D	� D
  D
� D  D� DfD� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(fD(� D)  D)� D*  D*�fD+fD+� D,fD,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D9��D:� D;  D;� D<  D<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DN  DN� DO  DO�fDP  DP� DQ  DQ� DR  DR�fDS  DSy�DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DYfDY�fDZfDZ�fD[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Dby�Db��Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dg��Dhy�Dh��Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDs  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDw�3Dy�qD�@RD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@ƸRA\)A#\)AC\)Ac\)A��A��A��A��A��AѮA�A�B �
B�
B�
B�
B �
B)=pB1=pB9=pB@�
BH�
BP�
BX�
B`�
Bh�
Bp�
Bx�
B�k�B�k�B�k�B�k�B���B���B�8RB�8RB�k�B�k�B�k�B���B�B�k�B�k�B�8RB�k�B�8RB�8RB�k�B�k�B�k�B�k�B�k�B�k�B�8RB�k�B�k�B�k�B�k�B�k�B�k�C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C 5�C"5�C$5�C&5�C(5�C*O]C,O]C.5�C05�C25�C45�C65�C85�C:5�C<5�C>O]C@5�CB5�CD5�CF5�CH5�CJ5�CL5�CN5�CP5�CR5�CT5�CV5�CX5�CZ5�C\5�C^5�C`5�Cb)Cd)Cf5�Ch5�Cj5�Cl5�CnO]Cp5�Cr5�Ct5�Cv5�Cx5�Cz5�C|5�C~5�C��C��C��C��C�'�C��C��C�'�C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C�C��C�'�C��C��C��C�'�C�'�C��C�C��C��C�C��C��C��C��C��C��C�'�C��C��C��C�'�C��C�C�C��C��C��C�'�C�'�C�'�C�'�C�'�C��C��C�C�C�C��C��C�'�C�'�C��C��C�'�C�'�C��C��C��C��C��C��C��C��C��C�'�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C�'�D �D �qDqD�qDqD�qDqD�qD�D�qDqD�qD�D�qDqD�DqD�qD	qD	�qD
qD
�qDqD�qD�D�qDqD��DqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(�D(�qD)qD)�qD*qD*��D+�D+�qD,�D,��D-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:D:�qD;qD;�qD<qD<�qD=qD=�qD>D>�qD?qD?�qD@qD@�qDAqDA�qDBqDB��DCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDI�DI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�DNqDN�qDOqDO��DPqDP�qDQqDQ�qDRqDR��DSqDS�DTqDT�qDUqDU�qDVqDV�qDWqDW��DXqDX�qDY�DY��DZ�DZ��D[�D[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�DcDc�qDdqDd�qDe�De�qDfqDf�qDgqDg�qDhDh�DiDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn��DoqDo�qDpqDp�qDqqDq�qDrqDr��DsqDs�qDtqDt�qDuqDu�qDvqDv�qDwqDw��Dw�Dy��D�G
D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A�O�A�O�A�S�A�O�A�5?A��A��A��mA��HA��;A��#A���A���A�ȴA���A߮Aߛ�A�|�A�VA�7LA�bA�hsAݼjA�O�A��Aه+A���A�hsA���A�;dA�\)A��A�n�A�p�Aӕ�A�dZA��;A�JA���A���A��A���A���A��A�n�A��mA�  A�ZA�-A�p�A�5?A�ĜA�n�A���A�G�A�&�A�VA��
A��A�-A��A�=qA��yA�;dA���A�1A���A���A�v�A�"�A�G�A�7LA�bA�t�A��7A�&�A��mA�x�A�;dA�bA���A�p�A���A�`BA��A�7LA�O�A�
=A�I�A���A�VA�C�A��PA��\A�t�A��A�p�A�K�A���A�bA��`A�VA�/A�+A���A�A}/AzI�AxjAw��Aw�At5?Ap�jAp(�Ao��Ao?}Am/Aj��Ah��AcG�A_�A\E�AZVAV��AT�AS?}AR�`ARv�AQ�AO�;AL�9AK��AH�HAF�RAE�TAD�HAA��A?��A?A>�A>�9A=
=A;�hA9��A6�uA5K�A2�9A1K�A0�A0ZA.�HA.=qA-\)A+��A*�A*  A)�A(ZA'|�A&1'A$�9A#�A#dZA"�A �AbA\)AC�A�jA{AĜA�mA��A�AbNA�A~�A�yA^5AƨA�!A �A�A&�A�jAZA��A�jA�A�-At�A	�;A��A�AO�A^5A�A��A$�A"�Av�A1'A��A�A ��@���@��^@���@��@��@��@��@���@�b@�"�@�O�@��y@�A�@�@�@�j@�P@�+@�(�@�G�@�~�@���@�Ĝ@���@�S�@���@��@�ƨ@۝�@�S�@�hs@��@��m@��
@׾w@���@��;@���@ץ�@�l�@��/@�1'@�x�@���@���@�7L@ԣ�@�z�@�
=@��#@�"�@�ȴ@�t�@Ϯ@Ͼw@Ϯ@�;d@��@��;@���@�=q@ɲ-@�?}@��@ȼj@��/@�x�@�bN@ǝ�@�33@��H@Ƨ�@ư!@�V@�-@ģ�@Å@�|�@�|�@�\)@�@�ff@�J@��^@�/@��u@���@��@���@��@�ȴ@��!@�M�@��@�O�@��@�r�@��;@���@��@��+@�hs@�Ĝ@��@��u@�r�@�A�@�l�@��@�hs@�x�@�&�@��@���@���@���@��`@���@�@�~�@�V@�M�@�V@�^5@�$�@��-@���@�b@���@�\)@���@�x�@��@�V@�O�@�X@�X@�`B@�X@���@�j@�9X@�  @�|�@�+@���@���@���@���@��\@�ff@�E�@�=q@�-@��T@���@�@�@�@���@��h@�p�@�O�@�?}@��@�r�@�I�@�9X@���@���@�l�@�S�@���@��T@���@��@�V@���@�9X@�b@���@�33@�
=@���@�~�@�^5@�M�@�=q@�J@��@��^@���@�p�@���@�b@���@��w@�K�@�;d@��@���@�$�@��@�/@��/@��`@��9@�(�@��
@�K�@��@�ȴ@��\@�M�@�{@�@���@���@��j@��@�r�@���@�t�@�dZ@�;d@���@��@�`B@�V@���@�ƨ@�33@�33@��@���@���@�$�@�@�7L@��u@�1'@��F@��P@��@���@�n�@�$�@�-@�=q@�{@�p�@�7L@��@���@��@��@��@� �@��@�l�@��H@���@�v�@�E�@�$�@���@��h@�O�@��@���@��/@��u@�bN@�9X@� �@�  @��
@��@�K�@��@��R@��+@�V@��@��@��^@��7@��@�X@�z�@xC-@d/�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�K�A�O�A�O�A�S�A�O�A�5?A��A��A��mA��HA��;A��#A���A���A�ȴA���A߮Aߛ�A�|�A�VA�7LA�bA�hsAݼjA�O�A��Aه+A���A�hsA���A�;dA�\)A��A�n�A�p�Aӕ�A�dZA��;A�JA���A���A��A���A���A��A�n�A��mA�  A�ZA�-A�p�A�5?A�ĜA�n�A���A�G�A�&�A�VA��
A��A�-A��A�=qA��yA�;dA���A�1A���A���A�v�A�"�A�G�A�7LA�bA�t�A��7A�&�A��mA�x�A�;dA�bA���A�p�A���A�`BA��A�7LA�O�A�
=A�I�A���A�VA�C�A��PA��\A�t�A��A�p�A�K�A���A�bA��`A�VA�/A�+A���A�A}/AzI�AxjAw��Aw�At5?Ap�jAp(�Ao��Ao?}Am/Aj��Ah��AcG�A_�A\E�AZVAV��AT�AS?}AR�`ARv�AQ�AO�;AL�9AK��AH�HAF�RAE�TAD�HAA��A?��A?A>�A>�9A=
=A;�hA9��A6�uA5K�A2�9A1K�A0�A0ZA.�HA.=qA-\)A+��A*�A*  A)�A(ZA'|�A&1'A$�9A#�A#dZA"�A �AbA\)AC�A�jA{AĜA�mA��A�AbNA�A~�A�yA^5AƨA�!A �A�A&�A�jAZA��A�jA�A�-At�A	�;A��A�AO�A^5A�A��A$�A"�Av�A1'A��A�A ��@���@��^@���@��@��@��@��@���@�b@�"�@�O�@��y@�A�@�@�@�j@�P@�+@�(�@�G�@�~�@���@�Ĝ@���@�S�@���@��@�ƨ@۝�@�S�@�hs@��@��m@��
@׾w@���@��;@���@ץ�@�l�@��/@�1'@�x�@���@���@�7L@ԣ�@�z�@�
=@��#@�"�@�ȴ@�t�@Ϯ@Ͼw@Ϯ@�;d@��@��;@���@�=q@ɲ-@�?}@��@ȼj@��/@�x�@�bN@ǝ�@�33@��H@Ƨ�@ư!@�V@�-@ģ�@Å@�|�@�|�@�\)@�@�ff@�J@��^@�/@��u@���@��@���@��@�ȴ@��!@�M�@��@�O�@��@�r�@��;@���@��@��+@�hs@�Ĝ@��@��u@�r�@�A�@�l�@��@�hs@�x�@�&�@��@���@���@���@��`@���@�@�~�@�V@�M�@�V@�^5@�$�@��-@���@�b@���@�\)@���@�x�@��@�V@�O�@�X@�X@�`B@�X@���@�j@�9X@�  @�|�@�+@���@���@���@���@��\@�ff@�E�@�=q@�-@��T@���@�@�@�@���@��h@�p�@�O�@�?}@��@�r�@�I�@�9X@���@���@�l�@�S�@���@��T@���@��@�V@���@�9X@�b@���@�33@�
=@���@�~�@�^5@�M�@�=q@�J@��@��^@���@�p�@���@�b@���@��w@�K�@�;d@��@���@�$�@��@�/@��/@��`@��9@�(�@��
@�K�@��@�ȴ@��\@�M�@�{@�@���@���@��j@��@�r�@���@�t�@�dZ@�;d@���@��@�`B@�V@���@�ƨ@�33@�33@��@���@���@�$�@�@�7L@��u@�1'@��F@��P@��@���@�n�@�$�@�-@�=q@�{@�p�@�7L@��@���@��@��@��@� �@��@�l�@��H@���@�v�@�E�@�$�@���@��h@�O�@��@���@��/@��u@�bN@�9X@� �@�  @��
@��@�K�@��@��R@��+@�V@��@��@��^@��7@��@�X@�z�@xC-@d/�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B6FB6FB6FB6FB5?B33B1'B/B.B.B-B-B-B,B,B,B/B2-B49B6FB6FB6FB:^B?}B@�B;dB �B\B+B
��B
��B
��B
��BB9XBH�B_;B��B�9BȴB�#B�`B�yBB
=BVBVB�B(�B)�B.B9XB>wB>wBgmBv�Bv�B}�B�1B�1B�JB�JB�BbNBm�B�B|�B{�BZB9XBVB  B��B�B�B"�B�B{B\BDB	7BB��B�B�;B��B�B�Bn�BbNBXB@�B#�B�B�B{BDB	7B%B
��B
�B
��B
��B
��B
�B
^5B
O�B
;dB
&�B
�B
�B
PB	��B	�yB	�mB	�fB	�fB	�/B	��B	�}B	��B	�B	iyB	YB	A�B	49B	/B	,B	'�B	 �B	�B	%B	B��B�B�B�sB�BB�B�B�B�
B��B��BȴBŢB��B�dB�^B�LB�?B�?B�9B�3B�B�B�B��B�?B�wB�XB�FB�FB�LB�XB�3B��B�B�B�B�B�'B�!B�B��B��B��B��B��B��B��B��B�=B�=B�bB�\B�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�uB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B��BǮBȴB��B��B��B�fB��B�B�sB�sB�yB�mB�ZB�/B�B��B�B�/B�;B�;B�;B�5B�5B�5B�;B�BB�BB�BB�BB�HB�fB�B�B�B�B�B�B�B�B��B��B��B	B	B	%B	+B		7B	DB	DB	JB	PB	oB	�B	�B	�B	�B	�B	"�B	&�B	+B	,B	0!B	2-B	2-B	49B	7LB	=qB	A�B	B�B	C�B	C�B	D�B	J�B	XB	_;B	iyB	iyB	iyB	iyB	iyB	iyB	iyB	p�B	u�B	y�B	}�B	� B	�B	�B	�1B	�=B	�=B	�DB	�JB	�DB	�DB	�VB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�3B	�3B	�9B	�FB	�LB	�LB	�dB	�wB	�}B	��B	ÖB	ǮB	��B	��B	��B	�B	�B	�)B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�;B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�HB	�TB	�ZB	�ZB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
JB
JB
JB
JB
JB
PB
PB
VB
VB
VB
\B
\B
uB
�B
#B
1�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B6FB6FB6FB6FB5?B33B1'B/B.B.B-B-B-B,B,B,B/B2-B49B6FB6FB6FB:^B?}B@�B;dB �B\B+B
��B
��B
��B
��BB9XBH�B_;B��B�9BȴB�#B�`B�yBB
=BVBVB�B(�B)�B.B9XB>wB>wBgmBv�Bv�B}�B�1B�1B�JB�JB�BbNBm�B�B|�B{�BZB9XBVB  B��B�B�B"�B�B{B\BDB	7BB��B�B�;B��B�B�Bn�BbNBXB@�B#�B�B�B{BDB	7B%B
��B
�B
��B
��B
��B
�B
^5B
O�B
;dB
&�B
�B
�B
PB	��B	�yB	�mB	�fB	�fB	�/B	��B	�}B	��B	�B	iyB	YB	A�B	49B	/B	,B	'�B	 �B	�B	%B	B��B�B�B�sB�BB�B�B�B�
B��B��BȴBŢB��B�dB�^B�LB�?B�?B�9B�3B�B�B�B��B�?B�wB�XB�FB�FB�LB�XB�3B��B�B�B�B�B�'B�!B�B��B��B��B��B��B��B��B��B�=B�=B�bB�\B�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�uB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B��BǮBȴB��B��B��B�fB��B�B�sB�sB�yB�mB�ZB�/B�B��B�B�/B�;B�;B�;B�5B�5B�5B�;B�BB�BB�BB�BB�HB�fB�B�B�B�B�B�B�B�B��B��B��B	B	B	%B	+B		7B	DB	DB	JB	PB	oB	�B	�B	�B	�B	�B	"�B	&�B	+B	,B	0!B	2-B	2-B	49B	7LB	=qB	A�B	B�B	C�B	C�B	D�B	J�B	XB	_;B	iyB	iyB	iyB	iyB	iyB	iyB	iyB	p�B	u�B	y�B	}�B	� B	�B	�B	�1B	�=B	�=B	�DB	�JB	�DB	�DB	�VB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�3B	�3B	�9B	�FB	�LB	�LB	�dB	�wB	�}B	��B	ÖB	ǮB	��B	��B	��B	�B	�B	�)B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�;B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�HB	�TB	�ZB	�ZB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
JB
JB
JB
JB
JB
PB
PB
VB
VB
VB
\B
\B
uB
�B
#B
1�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.21 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140823                              AO  ARCAADJP                                                                    20181024140823    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140823  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140823  QCF$                G�O�G�O�G�O�0               