CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:09Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BP   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ih   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       RH   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       [(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b@   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       d   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       k    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  r8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       t    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  {   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140809  20181024140809  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׶�	1   @׶� <��@3O\(��c���E�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   B   @�ff@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<�C>  C@�CB�CD�CF  CH�CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D�fD  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D.��D/� D0  D0� D1  D1� D2  D2�fD3  D3� D4  D4� D5  D5� D6  D6�fD7  D7� D8  D8� D9  D9� D:  D:� D;  D;�fD<  D<� D=  D=� D>  D>� D?  D?y�D@  D@�fDA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DSy�DT  DT� DU  DUy�DV  DV�fDW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[�fD\fD\� D]  D]� D]��D^� D_  D_� D`fD`� Da  Da� DbfDb� Dc  Dc� Dd  Dd� De  De� DffDf�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dy��D�1H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@ƸRA\)A$��AC\)Ac\)A��A��A��A��A��AѮA�A�B �
B�
B�
B�
B �
B(�
B0�
B8�
B@�
BH�
BP�
BX�
B`�
Bh�
Bpp�Bx�
B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C 5�C"5�C$5�C&5�C(5�C*5�C,5�C.5�C05�C2)C45�C65�C85�C:5�C<O]C>5�C@O]CBO]CDO]CF5�CHO]CJ5�CL5�CN5�CP5�CR5�CT5�CV5�CX)CZ5�C\5�C^5�C`5�Cb5�Cd5�Cf)Ch5�Cj5�Cl5�Cn5�Cp5�Cr5�Ct5�Cv5�Cx5�Cz5�C|5�C~O]C�'�C��C�C��C��C�C��C��C��C��C��C��C��C��C��C��C�C�C��C��C�C�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C�C��C��C��C��C��C��C��C��C��C�C�C�C�C�C��C��C��C��C��C��C��C��C��C�'�C�'�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C�C��D	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD�D��DqD�qDqD�qDqD�qDqD�qDqD�qDqD��DqD�qD�D�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/D/�qD0qD0�qD1qD1�qD2qD2��D3qD3�qD4qD4�qD5qD5�qD6qD6��D7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;��D<qD<�qD=qD=�qD>qD>�qD?qD?�D@qD@��DAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG��DHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�DNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�DTqDT�qDUqDU�DVqDV��DWqDW�qDXqDX��DYqDY�qDZqDZ�qD[qD[��D\�D\�qD]qD]�qD^D^�qD_qD_�qD`�D`�qDaqDa�qDb�Db�qDcqDc�qDdqDd�qDeqDe�qDf�Df��DgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDy�)D�8 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�1A�  A�"�A�{A��A�A�JA�%A�
=A�/A�/A�1'A�-A�(�A�$�A��A�VA��A��A֧�A�~�A�oA�ƨA�l�A��/A�K�A���A���A�O�A�9XA�S�A�E�A���A͸RA�A�A̗�A��A˸RA˥�AˍPAʣ�Aɇ+A�A�A���Aȇ+A�K�A���A�r�A�"�Aƕ�AŴ9Aě�A�A�\)A��jA���A�VA�|�A���A�XA���A��uA�VA��DA�C�A�I�A�jA�A�n�A�  A�ȴA�ƨA�VA��A�ZA�
=A��/A��FA�/A�z�A�;dA��9A��!A�v�A�A��A��+A�VA�ȴA��A�7LA�M�A���A�%A��A� �A�$�A�33A��A���A�oA�oA�O�A��!A�33A��;A�~�A�A��hA�1A��A�n�A�A��A�dZA�/A���A�A�C�A�p�A~��A{Az�+Aw��As�wArZAqG�Ap~�Ao�^AoS�Ao&�Ao�An��Am��Aj(�Ag�Af�RAd��Ab=qA_��A]��AZAWS�AU�AT��APĜAMt�ALr�AJ��AJ~�AJ�AI
=AHAG+AF-AD�uAA��A@ �A?O�A=K�A:�HA9�TA9��A9�A8Q�A7"�A5��A4VA3�7A3�A1�PA0�/A/�^A-��A+�A+G�A*v�A*=qA)|�A(��A'
=A&I�A$A�A#t�A!�hA�PA�uAA�A�`A��A�A��A��A�7A?}A�A��AbNA�mAƨAp�Al�A�A=qA��A�wA�TAZA��AXA��At�A�\A
��A
E�A	��A	x�A��A��A�
A�uAVA��A;dAQ�AS�A1A7LA �A ȴA -@�A�@�G�@��u@��D@�  @��H@�|�@���@�G�@�9X@�;d@��@�9@�;d@�\@�M�@��@�9@�&�@�Q�@Ͼw@��y@�@͑h@�x�@�x�@���@� �@��m@˝�@�l�@�dZ@�|�@�dZ@�v�@ȴ9@���@�l�@ƸR@�E�@�/@�\)@�$�@���@�G�@��9@�t�@�n�@�{@�hs@��@��;@��F@��@�
=@�ff@���@�%@��w@�o@��
@��m@��m@��@��@��@�5?@���@�7L@�9X@�K�@���@��w@���@�M�@��@���@�7L@��@�(�@��
@���@�\)@�+@��y@�E�@���@�x�@��@���@�t�@���@��@��#@���@�?}@���@� �@�;d@���@�n�@��@���@�X@�%@���@���@�z�@��
@�K�@��@�=q@��@���@�?}@���@�Ĝ@���@��@��@�Z@� �@���@�t�@�dZ@��H@�$�@��@��@���@���@�Ĝ@��@�1'@�ƨ@���@���@���@�|�@��@�v�@��T@��-@�`B@���@�z�@� �@�1@���@��@�dZ@�|�@�1@�A�@��@�  @�ƨ@�|�@�"�@���@��@��h@�`B@�V@�Ĝ@�z�@�Q�@�ƨ@���@���@�|�@�K�@�@���@�$�@�&�@��`@��9@���@���@���@��@�A�@���@���@�|�@�;d@�@��@��!@�v�@�M�@�M�@�M�@�E�@�-@��@�hs@���@��@���@��j@��u@�Q�@��
@�33@���@�~�@�5?@�J@���@�p�@�X@�?}@���@�z�@�1'@��@��m@�\)@j�@k�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�1A�  A�"�A�{A��A�A�JA�%A�
=A�/A�/A�1'A�-A�(�A�$�A��A�VA��A��A֧�A�~�A�oA�ƨA�l�A��/A�K�A���A���A�O�A�9XA�S�A�E�A���A͸RA�A�A̗�A��A˸RA˥�AˍPAʣ�Aɇ+A�A�A���Aȇ+A�K�A���A�r�A�"�Aƕ�AŴ9Aě�A�A�\)A��jA���A�VA�|�A���A�XA���A��uA�VA��DA�C�A�I�A�jA�A�n�A�  A�ȴA�ƨA�VA��A�ZA�
=A��/A��FA�/A�z�A�;dA��9A��!A�v�A�A��A��+A�VA�ȴA��A�7LA�M�A���A�%A��A� �A�$�A�33A��A���A�oA�oA�O�A��!A�33A��;A�~�A�A��hA�1A��A�n�A�A��A�dZA�/A���A�A�C�A�p�A~��A{Az�+Aw��As�wArZAqG�Ap~�Ao�^AoS�Ao&�Ao�An��Am��Aj(�Ag�Af�RAd��Ab=qA_��A]��AZAWS�AU�AT��APĜAMt�ALr�AJ��AJ~�AJ�AI
=AHAG+AF-AD�uAA��A@ �A?O�A=K�A:�HA9�TA9��A9�A8Q�A7"�A5��A4VA3�7A3�A1�PA0�/A/�^A-��A+�A+G�A*v�A*=qA)|�A(��A'
=A&I�A$A�A#t�A!�hA�PA�uAA�A�`A��A�A��A��A�7A?}A�A��AbNA�mAƨAp�Al�A�A=qA��A�wA�TAZA��AXA��At�A�\A
��A
E�A	��A	x�A��A��A�
A�uAVA��A;dAQ�AS�A1A7LA �A ȴA -@�A�@�G�@��u@��D@�  @��H@�|�@���@�G�@�9X@�;d@��@�9@�;d@�\@�M�@��@�9@�&�@�Q�@Ͼw@��y@�@͑h@�x�@�x�@���@� �@��m@˝�@�l�@�dZ@�|�@�dZ@�v�@ȴ9@���@�l�@ƸR@�E�@�/@�\)@�$�@���@�G�@��9@�t�@�n�@�{@�hs@��@��;@��F@��@�
=@�ff@���@�%@��w@�o@��
@��m@��m@��@��@��@�5?@���@�7L@�9X@�K�@���@��w@���@�M�@��@���@�7L@��@�(�@��
@���@�\)@�+@��y@�E�@���@�x�@��@���@�t�@���@��@��#@���@�?}@���@� �@�;d@���@�n�@��@���@�X@�%@���@���@�z�@��
@�K�@��@�=q@��@���@�?}@���@�Ĝ@���@��@��@�Z@� �@���@�t�@�dZ@��H@�$�@��@��@���@���@�Ĝ@��@�1'@�ƨ@���@���@���@�|�@��@�v�@��T@��-@�`B@���@�z�@� �@�1@���@��@�dZ@�|�@�1@�A�@��@�  @�ƨ@�|�@�"�@���@��@��h@�`B@�V@�Ĝ@�z�@�Q�@�ƨ@���@���@�|�@�K�@�@���@�$�@�&�@��`@��9@���@���@���@��@�A�@���@���@�|�@�;d@�@��@��!@�v�@�M�@�M�@�M�@�E�@�-@��@�hs@���@��@���@��j@��u@�Q�@��
@�33@���@�~�@�5?@�J@���@�p�@�X@�?}@���@�z�@�1'@��@��m@�\)@j�@k�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
t�B
v�B
v�B
z�B
y�B
r�B
v�B
v�B
v�B
w�B
|�B
|�B
|�B
|�B
|�B
|�B
{�B
{�B
{�B
{�B
}�B
�B
�JB
�^B
��B
�
B
�ZB%B
��B+B
=B
��B
�ZB
�HB
�;B
�mB
�ZB
��B1B#�BA�BYBq�B|�B�\B��BƨBɺB��B��B��B�B��B��B��B��BuB�B�B�B{B\BbB�BN�B^5B|�B��B�^BĜBƨBB�dB�qBƨB��B�HB�5B�TB�BB�B��B�dB�B�hBT�B?}B?}BXBk�BYB?}B0!B'�B�B��B��B��B�B�NB��B��B{�Bp�BdZB=qBJB
�5B
�B
��B
��B
ŢB
��B
�RB
��B
�1B
�B
|�B
p�B
e`B
O�B
C�B
!�B
�B
B	�NB	�
B	��B	ȴB	ÖB	��B	�wB	�qB	�XB	�!B	��B	�7B	�B	t�B	e`B	VB	F�B	33B	#�B	�B	hB	B��B�B�sB�`B�TB�;B�B�
B��B��BǮBÖB��B�jB�?B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�{B�oB�oB�uB�uB�hB�bB�bB�oB�oB�hB�hB�hB�hB�hB�bB�VB�PB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB�oB]/B��B��B��B�B�B�'B�FB�RB�jBǮBȴBɺB��B��B�B�B�5B�ZB�mB�sB�B�B�B��B��B	B	B	B	DB	hB	oB	�B	�B	�B	�B	�B	!�B	"�B	#�B	'�B	%�B	-B	49B	5?B	5?B	5?B	5?B	6FB	7LB	49B	2-B	.B	+B	,B	&�B	&�B	,B	-B	/B	33B	7LB	:^B	=qB	>wB	@�B	B�B	D�B	H�B	N�B	O�B	R�B	W
B	\)B	bNB	gmB	hsB	iyB	l�B	p�B	s�B	y�B	}�B	~�B	� B	�B	�B	�+B	�1B	�7B	�7B	�PB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�9B	�LB	�wB	�qB	�wB	��B	ÖB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�/B	�TB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
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
+B
1B
	7B

=B
DB
JB
JB
PB
PB
PB
PB
VB
\B
bB
bB
bB
oB
B
-�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
t�B
v�B
v�B
z�B
y�B
r�B
v�B
v�B
v�B
w�B
|�B
|�B
|�B
|�B
|�B
|�B
{�B
{�B
{�B
{�B
}�B
�B
�JB
�^B
��B
�
B
�ZB%B
��B+B
=B
��B
�ZB
�HB
�;B
�mB
�ZB
��B1B#�BA�BYBq�B|�B�\B��BƨBɺB��B��B��B�B��B��B��B��BuB�B�B�B{B\BbB�BN�B^5B|�B��B�^BĜBƨBB�dB�qBƨB��B�HB�5B�TB�BB�B��B�dB�B�hBT�B?}B?}BXBk�BYB?}B0!B'�B�B��B��B��B�B�NB��B��B{�Bp�BdZB=qBJB
�5B
�B
��B
��B
ŢB
��B
�RB
��B
�1B
�B
|�B
p�B
e`B
O�B
C�B
!�B
�B
B	�NB	�
B	��B	ȴB	ÖB	��B	�wB	�qB	�XB	�!B	��B	�7B	�B	t�B	e`B	VB	F�B	33B	#�B	�B	hB	B��B�B�sB�`B�TB�;B�B�
B��B��BǮBÖB��B�jB�?B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�{B�oB�oB�uB�uB�hB�bB�bB�oB�oB�hB�hB�hB�hB�hB�bB�VB�PB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB�oB]/B��B��B��B�B�B�'B�FB�RB�jBǮBȴBɺB��B��B�B�B�5B�ZB�mB�sB�B�B�B��B��B	B	B	B	DB	hB	oB	�B	�B	�B	�B	�B	!�B	"�B	#�B	'�B	%�B	-B	49B	5?B	5?B	5?B	5?B	6FB	7LB	49B	2-B	.B	+B	,B	&�B	&�B	,B	-B	/B	33B	7LB	:^B	=qB	>wB	@�B	B�B	D�B	H�B	N�B	O�B	R�B	W
B	\)B	bNB	gmB	hsB	iyB	l�B	p�B	s�B	y�B	}�B	~�B	� B	�B	�B	�+B	�1B	�7B	�7B	�PB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�9B	�LB	�wB	�qB	�wB	��B	ÖB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�/B	�TB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
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
+B
1B
	7B

=B
DB
JB
JB
PB
PB
PB
PB
VB
\B
bB
bB
bB
oB
B
-�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.21 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140809                              AO  ARCAADJP                                                                    20181024140809    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140809  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140809  QCF$                G�O�G�O�G�O�0               