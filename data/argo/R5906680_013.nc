CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-11-04T11:00:49Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20211104110049  20211104110049  5906680 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  8780                            2B  A   NAVIS_A                         1308                            170425                          863 @٠ϥ�1   @٠hK�@(���Q��d���$�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A   @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���A�33B��B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D��3D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�)�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @ ��@��R@ƸRA\)A#\)AC\)Ac\)A��A��A��A��A��AѮA�z�A�z�B p�Bp�B�
B�
B �
B(�
B0�
B8�
BA=pBH�
BP�
BX�
B`�
Bh�
Bp�
Bx�
B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B���B�k�B�k�B�k�B�k�B�8RB�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�C5�C5�C5�C)C5�C 5�C"5�C$5�C&5�C(5�C*5�C,5�C.5�C05�C25�C45�C65�C85�C:5�C<5�C>5�C@5�CB5�CD5�CF5�CH5�CJ5�CL5�CN5�CP5�CR5�CT5�CV5�CX5�CZ5�C\5�C^5�C`5�Cb5�Cd5�Cf5�Ch5�Cj5�Cl5�Cn5�Cp5�Cr5�Ct5�Cv5�Cx5�Cz5�C|5�C~5�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C�C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�'�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD��DqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt�qDuqDu�qDvqDv�qDwqDw�qDxqDx�qDyqDy�qDzqDz�qD{qD{�qD|qD|�qD}qD}�qD~qD~�qDqD�qD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D�D�ƸD��D�F�DÆ�D�ƸD��D�F�DĆ�D�ƸD��D�F�Dņ�D�ƸD��D�F�DƆ�D�ƸD��D�F�Dǆ�D�ƸD��D�F�DȆ�D�ƸD��D�F�DɆ�D�ƸD��D�F�Dʆ�D���D��D�F�Dˆ�D�ƸD��D�F�D̆�D�ƸD��D�F�D͆�D�ƸD��D�F�DΆ�D�ƸD��D�F�Dφ�D�ƸD��D�F�DІ�D�ƸD��D�F�Dц�D�ƸD��D�F�D҆�D�ƸD��D�F�Dӆ�D�ƸD��D�F�DԆ�D�ƸD��D�F�DՆ�D�ƸD��D�F�Dֆ�D�ƸD��D�F�D׆�D�ƸD��D�F�D؆�D�ƸD��D�F�Dن�D�ƸD��D�F�Dچ�D�ƸD��D�F�Dۆ�D�ƸD��D�F�D܆�D�ƸD��D�F�D݆�D�ƸD��D�F�Dކ�D�ƸD��D�F�D߆�D�ƸD��D�F�D���D�ƸD��D�F�DᆸD�ƸD��D�F�D↸D�ƸD��D�F�DㆸD�ƸD��D�F�D䆸D�ƸD��D�F�D冸D�ƸD��D�F�D憸D�ƸD��D�F�D熸D�ƸD��D�F�D膸D�ƸD��D�F�D醸D�ƸD��D�F�DꆸD�ƸD��D�F�D놸D�ƸD��D�F�D솸D�ƸD��D�F�D톸D�ƸD��D�F�DD�ƸD��D�F�DD�ƸD��D�F�D���D�ƸD��D�F�D�D�ƸD��D�F�D�D�ƸD��D�F�D�D�ƸD��D�F�D�D�ƸD��D�F�D���D�ƸD�	�D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD��D�F�D���D�ƸD�	�D�0R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�33A�5?A�7LA�7LA�7LA�9XA�9XA�;dA�7LA�7LA�9XA�;dA�;dA�7LA�-A��#AΗ�A�5?A�bA�ĜA�
=AǕ�AƇ+A�|�A�JA���A�1'A�p�A�ƨA��+A��A�  A��#A�1A�n�A���A�Q�A��`A�p�A�x�A�ĜA�S�A��^A�oA�{A�Q�A�~�A�?}A��/A�O�A��A�C�A�O�A�t�A��^A�1A��wA��A�&�A�C�A���A�ffA�=qA�~�A�l�A���Aw�hAs�ArI�Ann�Ai�^AhJAb5?A^�\A[�AWXAO�mAG�^AC�hAB�ADJAE�7AEVAE?}AE�AD��AD$�AB�jA@�jA@  A?��A?�hA?O�A>�`A>��A>�+A>$�A=dZA;�A:ĜA:A8��A6�A5�A4��A4Q�A4  A3�^A3��A3��A37LA2��A25?A1��A1�hA0�`A0E�A0(�A0JA/A/�A.v�A.$�A-��A-t�A,�jA, �A+��A+��A+�A*^5A*1'A* �A)�mA)7LA)VA(�yA(��A(�RA(�DA(M�A(�A(JA'�A'�A'��A&�/A&jA&5?A%�A%�FA%�7A%x�A%dZA%"�A$~�A#A#|�A#�A"��A"Q�A"bA!�#A!�^A!��A!�PA!O�A!oA �yA ��A ��A $�AA��AhsA�A�/A�\AQ�A-AbA�;A��AdZA��A��A^5A�hAG�A&�A��A�HA�A�+Av�Ar�AjAZAbA\)A��A�!AI�A�A|�AhsA�`AbNA�A�7AdZA;dA+A%Av�A(�A��A;dA~�AbA�-AG�AVA�RAffA �A�;A&�A��A�uA�A��A�-A�7Ax�AXA&�A�jA�uA�A�mA�^Ax�A�At�A
��A
9XA	�A	��A	�A	��A	�Az�A�;Ax�A33Av�AXAVA��AVAI�A �`A =q@���@���@�
=@�p�@�bN@�l�@�7L@�@�@���@�-@�`B@�X@�G�@���@@��@�\@�V@�$�@�@�%@�9@�j@��@�dZ@�+@�hs@蛦@���@�P@�C�@�ȴ@�n�@��T@�h@���@�@��@�w@㕁@�;d@�V@�@��@��;@�K�@�M�@���@�&�@� �@��@��m@���@۾w@ۮ@۝�@ۅ@�l�@�S�@���@�v�@٩�@���@ش9@ؓu@��;@�dZ@�S�@�+@�ȴ@֏\@�5?@�X@���@��/@ԃ@��m@ӍP@���@�ȴ@�E�@��@ёh@�%@��/@мj@�r�@�1@�t�@�ȴ@ΰ!@�=q@͡�@�7L@��@���@�j@˶F@�l�@�K�@�@�n�@��T@ɉ7@ɉ7@�7L@��@�V@��/@ȃ@�(�@ǶF@�o@Ə\@�E�@�@���@�p�@�&�@ģ�@��@�S�@���@�^5@�@���@��7@�p�@���@��;@�S�@���@�V@���@�O�@���@��m@��@�K�@��H@�n�@�$�@�@�`B@�%@�j@�A�@��m@��@��@��@���@�V@��^@��h@�V@�r�@�1'@��;@�t�@�K�@��@���@��\@�V@�{@�J@��#@�hs@���@�A�@��F@�t�@�;d@��@��\@�^5@�=q@�{@��7@�G�@�Ĝ@�bN@� �@���@�|�@�K�@���@�E�@�@���@�&�@�%@���@��@�Q�@�(�@���@�S�@���@���@�n�@�E�@��-@��@���@��j@��9@��@���@��u@�Q�@�Q�@�A�@�  @���@�;d@�"�@�o@�"�@�@�v�@��@��h@��@��@�r�@�1@�dZ@��y@��+@�V@�J@���@���@�/@���@�9X@���@���@�o@���@�M�@���@�x�@�G�@�Ĝ@�A�@��@�b@���@��m@��
@�l�@�+@�@�ȴ@�5?@�hs@���@���@�bN@�(�@��@��@�"�@���@���@�E�@��^@�x�@���@���@�Z@�(�@��m@�dZ@��@�ȴ@���@�=q@���@�G�@��j@��u@�bN@��
@�C�@���@�M�@���@���@���@��7@�X@��@��@��@��u@�j@��@���@��@��P@�;d@�n�@�-@�-@�J@��@��-@�&�@���@��j@���@�z�@�Q�@�  @��@��
@��@�K�@�;d@�o@��!@�n�@�V@�V@�-@��@��^@��@��@�%@�Ĝ@�I�@�;@�w@�@|�@}�T@}��@}�@}V@|�@{�m@{ƨ@{��@{"�@z��@zn�@y��@y&�@x�`@x�@w�@wl�@w;d@vȴ@vV@u��@u?}@t�@t�@t9X@s��@s@r��@r�\@r~�@rM�@r=q@r-@q��@q&�@q&�@p�`@pr�@p  @o�@nff@m�-@l�/@l9X@ko@j��@i7L@hr�@h  @g�w@g�P@g+@f5?@e@e�@d1@c33@b�!@bn�@b-@a��@ax�@`r�@_�w@_l�@_K�@_+@_
=@^�y@^5?@]V@\�j@\��@[��@[�F@[�@["�@Z^5@Y��@Y�7@YX@Y&�@X��@X��@XQ�@X1'@X �@X �@W�;@WK�@V��@VV@U��@U?}@T�@TZ@T9X@T9X@T�@S��@S�
@Sƨ@St�@SC�@S@R~�@Q�#@Q%@O��@N��@Nff@NV@NE�@N$�@N{@M�-@M�h@M�@M?}@L��@Lz�@L�@K�
@K�F@K�F@K��@K"�@J��@J^5@Ix�@I&�@H�`@HQ�@Hb@Hb@G�w@G�P@G|�@Gl�@G;d@G
=@F�@F�+@Fff@F$�@E�@E��@E�@D��@D�@D��@D�@D�D@Dj@D�@C��@C��@CS�@B��@Bn�@BM�@A��@A��@AX@AG�@A7L@A%@@�`@AX@@Ĝ@@Ĝ@@  @>��@>@=p�@<��@<�j@<�@<�j@<�@=V@<�@<�D@<j@<j@<I�@<(�@<(�@;�m@;�m@;ƨ@;�@;33@:��@:-@9��@9x�@8��@8��@81'@7\)@7+@7�@6�R@6V@6{@5@5�@4�/@49X@3S�@2��@2~�@2�@1�#@1�^@1�@1�^@1��@1hs@0��@0�u@0�@/��@.�@.��@.V@.@-�-@-��@-`B@-V@,�@,z�@,9X@+ƨ@+"�@*^5@)��@)hs@)X@(�`@(Ĝ@(r�@(1'@(  @'�@'�@'l�@'
=@&��@&v�@&V@&$�@%��@%�h@%O�@$�/@$�/@$�/@$�/@$�@$9X@$1@#�m@#��@#33@"��@"=q@!��@!��@!hs@!%@ �`@ �9@ r�@   @�;@��@�w@|�@\)@+@��@��@5?@{@@�h@��@�j@�j@Z@9X@�@��@�@��@-@��@�@�@�#@�#@�#@��@x�@G�@�@%@�`@��@Ĝ@�9@��@�@r�@bN@A�@��@�P@K�@;d@�@�@�@ȴ@��@��@V@@�@{@�T@�@`B@p�@O�@V@�@��@�j@�@9X@(�@(�@�@�@1@1@��@�@33@"�@@o@o@@�@�@�@�H@��@�!@�\@�\@n�@^5@M�@M�@=q@=q@-@-@�@�#@��@��@��@��@x�@�@�u@r�@bN@Q�@Q�@Q�@Q�@A�@1'@�@�P@K�@\)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�1'A�33A�5?A�7LA�7LA�7LA�9XA�9XA�;dA�7LA�7LA�9XA�;dA�;dA�7LA�-A��#AΗ�A�5?A�bA�ĜA�
=AǕ�AƇ+A�|�A�JA���A�1'A�p�A�ƨA��+A��A�  A��#A�1A�n�A���A�Q�A��`A�p�A�x�A�ĜA�S�A��^A�oA�{A�Q�A�~�A�?}A��/A�O�A��A�C�A�O�A�t�A��^A�1A��wA��A�&�A�C�A���A�ffA�=qA�~�A�l�A���Aw�hAs�ArI�Ann�Ai�^AhJAb5?A^�\A[�AWXAO�mAG�^AC�hAB�ADJAE�7AEVAE?}AE�AD��AD$�AB�jA@�jA@  A?��A?�hA?O�A>�`A>��A>�+A>$�A=dZA;�A:ĜA:A8��A6�A5�A4��A4Q�A4  A3�^A3��A3��A37LA2��A25?A1��A1�hA0�`A0E�A0(�A0JA/A/�A.v�A.$�A-��A-t�A,�jA, �A+��A+��A+�A*^5A*1'A* �A)�mA)7LA)VA(�yA(��A(�RA(�DA(M�A(�A(JA'�A'�A'��A&�/A&jA&5?A%�A%�FA%�7A%x�A%dZA%"�A$~�A#A#|�A#�A"��A"Q�A"bA!�#A!�^A!��A!�PA!O�A!oA �yA ��A ��A $�AA��AhsA�A�/A�\AQ�A-AbA�;A��AdZA��A��A^5A�hAG�A&�A��A�HA�A�+Av�Ar�AjAZAbA\)A��A�!AI�A�A|�AhsA�`AbNA�A�7AdZA;dA+A%Av�A(�A��A;dA~�AbA�-AG�AVA�RAffA �A�;A&�A��A�uA�A��A�-A�7Ax�AXA&�A�jA�uA�A�mA�^Ax�A�At�A
��A
9XA	�A	��A	�A	��A	�Az�A�;Ax�A33Av�AXAVA��AVAI�A �`A =q@���@���@�
=@�p�@�bN@�l�@�7L@�@�@���@�-@�`B@�X@�G�@���@@��@�\@�V@�$�@�@�%@�9@�j@��@�dZ@�+@�hs@蛦@���@�P@�C�@�ȴ@�n�@��T@�h@���@�@��@�w@㕁@�;d@�V@�@��@��;@�K�@�M�@���@�&�@� �@��@��m@���@۾w@ۮ@۝�@ۅ@�l�@�S�@���@�v�@٩�@���@ش9@ؓu@��;@�dZ@�S�@�+@�ȴ@֏\@�5?@�X@���@��/@ԃ@��m@ӍP@���@�ȴ@�E�@��@ёh@�%@��/@мj@�r�@�1@�t�@�ȴ@ΰ!@�=q@͡�@�7L@��@���@�j@˶F@�l�@�K�@�@�n�@��T@ɉ7@ɉ7@�7L@��@�V@��/@ȃ@�(�@ǶF@�o@Ə\@�E�@�@���@�p�@�&�@ģ�@��@�S�@���@�^5@�@���@��7@�p�@���@��;@�S�@���@�V@���@�O�@���@��m@��@�K�@��H@�n�@�$�@�@�`B@�%@�j@�A�@��m@��@��@��@���@�V@��^@��h@�V@�r�@�1'@��;@�t�@�K�@��@���@��\@�V@�{@�J@��#@�hs@���@�A�@��F@�t�@�;d@��@��\@�^5@�=q@�{@��7@�G�@�Ĝ@�bN@� �@���@�|�@�K�@���@�E�@�@���@�&�@�%@���@��@�Q�@�(�@���@�S�@���@���@�n�@�E�@��-@��@���@��j@��9@��@���@��u@�Q�@�Q�@�A�@�  @���@�;d@�"�@�o@�"�@�@�v�@��@��h@��@��@�r�@�1@�dZ@��y@��+@�V@�J@���@���@�/@���@�9X@���@���@�o@���@�M�@���@�x�@�G�@�Ĝ@�A�@��@�b@���@��m@��
@�l�@�+@�@�ȴ@�5?@�hs@���@���@�bN@�(�@��@��@�"�@���@���@�E�@��^@�x�@���@���@�Z@�(�@��m@�dZ@��@�ȴ@���@�=q@���@�G�@��j@��u@�bN@��
@�C�@���@�M�@���@���@���@��7@�X@��@��@��@��u@�j@��@���@��@��P@�;d@�n�@�-@�-@�J@��@��-@�&�@���@��j@���@�z�@�Q�@�  @��@��
@��@�K�@�;d@�o@��!@�n�@�V@�V@�-@��@��^@��@��@�%@�Ĝ@�I�@�;@�w@�@|�@}�T@}��@}�@}V@|�@{�m@{ƨ@{��@{"�@z��@zn�@y��@y&�@x�`@x�@w�@wl�@w;d@vȴ@vV@u��@u?}@t�@t�@t9X@s��@s@r��@r�\@r~�@rM�@r=q@r-@q��@q&�@q&�@p�`@pr�@p  @o�@nff@m�-@l�/@l9X@ko@j��@i7L@hr�@h  @g�w@g�P@g+@f5?@e@e�@d1@c33@b�!@bn�@b-@a��@ax�@`r�@_�w@_l�@_K�@_+@_
=@^�y@^5?@]V@\�j@\��@[��@[�F@[�@["�@Z^5@Y��@Y�7@YX@Y&�@X��@X��@XQ�@X1'@X �@X �@W�;@WK�@V��@VV@U��@U?}@T�@TZ@T9X@T9X@T�@S��@S�
@Sƨ@St�@SC�@S@R~�@Q�#@Q%@O��@N��@Nff@NV@NE�@N$�@N{@M�-@M�h@M�@M?}@L��@Lz�@L�@K�
@K�F@K�F@K��@K"�@J��@J^5@Ix�@I&�@H�`@HQ�@Hb@Hb@G�w@G�P@G|�@Gl�@G;d@G
=@F�@F�+@Fff@F$�@E�@E��@E�@D��@D�@D��@D�@D�D@Dj@D�@C��@C��@CS�@B��@Bn�@BM�@A��@A��@AX@AG�@A7L@A%@@�`@AX@@Ĝ@@Ĝ@@  @>��@>@=p�@<��@<�j@<�@<�j@<�@=V@<�@<�D@<j@<j@<I�@<(�@<(�@;�m@;�m@;ƨ@;�@;33@:��@:-@9��@9x�@8��@8��@81'@7\)@7+@7�@6�R@6V@6{@5@5�@4�/@49X@3S�@2��@2~�@2�@1�#@1�^@1�@1�^@1��@1hs@0��@0�u@0�@/��@.�@.��@.V@.@-�-@-��@-`B@-V@,�@,z�@,9X@+ƨ@+"�@*^5@)��@)hs@)X@(�`@(Ĝ@(r�@(1'@(  @'�@'�@'l�@'
=@&��@&v�@&V@&$�@%��@%�h@%O�@$�/@$�/@$�/@$�/@$�@$9X@$1@#�m@#��@#33@"��@"=q@!��@!��@!hs@!%@ �`@ �9@ r�@   @�;@��@�w@|�@\)@+@��@��@5?@{@@�h@��@�j@�j@Z@9X@�@��@�@��@-@��@�@�@�#@�#@�#@��@x�@G�@�@%@�`@��@Ĝ@�9@��@�@r�@bN@A�@��@�P@K�@;d@�@�@�@ȴ@��@��@V@@�@{@�T@�@`B@p�@O�@V@�@��@�j@�@9X@(�@(�@�@�@1@1@��@�@33@"�@@o@o@@�@�@�@�H@��@�!@�\@�\@n�@^5@M�@M�@=q@=q@-@-@�@�#@��@��@��@��@x�@�@�u@r�@bN@Q�@Q�@Q�@Q�@A�@1'@�@�P@K�@\)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bp�Bp�Bq�Bq�Bq�Bq�Br�Bt�B	�VB
�B
�NB
�TBB	7BbB'�BC�BZB�{B��B�B�qB��B�B�5B�B�B��B�B�B�B�B�B�sB�ZB�#B�B�B��B��BB�LB�B��B��B�PB�Bo�BbNB]/BR�B/BoB
�fB
�}B
��B
�B
B�B
�B	�B	��B	B	�LB	��B	�uB	z�B	m�B	e`B	^5B	l�B	�bB	��B	��B	��B
{B
�B
D�B
Q�B
R�B
XB
]/B
iyB
z�B
�7B
�\B
��B
�dB
ŢB
ȴB
��B
�B
�HB
�ZB
�fB
�mB
�mB
�mB
�`B
�`B
�`B
�ZB
�ZB
�ZB
�TB
�TB
�NB
�HB
�HB
�BB
�;B
�5B
�5B
�/B
�/B
�#B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
ɺB
ȴB
ɺB
ȴB
ǮB
ǮB
ƨB
ƨB
ÖB
��B
��B
�}B
�wB
�wB
�wB
�qB
�jB
�^B
�RB
�FB
�?B
�FB
�FB
�?B
�9B
�9B
�3B
�3B
�3B
�-B
�'B
�!B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�{B
�{B
�uB
�oB
�\B
�JB
�DB
�=B
�7B
�+B
�+B
�%B
�B
�B
�B
� B
~�B
~�B
}�B
|�B
|�B
z�B
z�B
w�B
w�B
v�B
t�B
t�B
r�B
q�B
p�B
o�B
n�B
m�B
l�B
k�B
jB
iyB
hsB
hsB
gmB
ffB
e`B
dZB
cTB
bNB
cTB
e`B
ffB
iyB
ffB
cTB
aHB
`BB
`BB
`BB
]/B
ZB
ZB
ZB
W
B
T�B
O�B
I�B
H�B
E�B
B�B
=qB
:^B
8RB
6FB
33B
0!B
/B
.B
+B
(�B
&�B
&�B
%�B
$�B
$�B
$�B
$�B
#�B
"�B
!�B
!�B
!�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
hB
bB
\B
bB
hB
bB
bB
bB
bB
bB
bB
\B
\B
\B
VB
VB
PB
PB
PB
VB
\B
\B
hB
oB
oB
oB
hB
hB
hB
hB
hB
hB
hB
bB
bB
\B
\B
bB
bB
bB
hB
hB
hB
hB
bB
bB
bB
bB
bB
bB
bB
\B
bB
bB
hB
\B
JB
DB
\B
bB
bB
hB
oB
hB
bB
bB
bB
\B
bB
\B
\B
\B
\B
\B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
PB
PB
PB
JB
DB
DB
DB
DB
JB
JB
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
\B
VB
VB
VB
\B
\B
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
bB
\B
bB
bB
bB
bB
hB
bB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
uB
uB
{B
{B
uB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
)�B
+B
,B
,B
+B
+B
+B
+B
,B
,B
-B
-B
-B
-B
,B
,B
,B
,B
,B
,B
-B
-B
.B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
6FB
6FB
6FB
7LB
7LB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
<jB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
L�B
O�B
O�B
N�B
M�B
M�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
XB
XB
XB
YB
YB
[#B
\)B
\)B
[#B
ZB
YB
YB
YB
YB
[#B
\)B
]/B
^5B
^5B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
dZB
dZB
dZB
dZB
cTB
cTB
cTB
cTB
cTB
cTB
e`B
ffB
e`B
gmB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
jB
jB
jB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bp�Bp�Bq�Bq�Bq�Bq�Br�Bt�B	�VB
�B
�NB
�TBB	7BbB'�BC�BZB�{B��B�B�qB��B�B�5B�B�B��B�B�B�B�B�B�sB�ZB�#B�B�B��B��BB�LB�B��B��B�PB�Bo�BbNB]/BR�B/BoB
�fB
�}B
��B
�B
B�B
�B	�B	��B	B	�LB	��B	�uB	z�B	m�B	e`B	^5B	l�B	�bB	��B	��B	��B
{B
�B
D�B
Q�B
R�B
XB
]/B
iyB
z�B
�7B
�\B
��B
�dB
ŢB
ȴB
��B
�B
�HB
�ZB
�fB
�mB
�mB
�mB
�`B
�`B
�`B
�ZB
�ZB
�ZB
�TB
�TB
�NB
�HB
�HB
�BB
�;B
�5B
�5B
�/B
�/B
�#B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
ɺB
ȴB
ɺB
ȴB
ǮB
ǮB
ƨB
ƨB
ÖB
��B
��B
�}B
�wB
�wB
�wB
�qB
�jB
�^B
�RB
�FB
�?B
�FB
�FB
�?B
�9B
�9B
�3B
�3B
�3B
�-B
�'B
�!B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�{B
�{B
�uB
�oB
�\B
�JB
�DB
�=B
�7B
�+B
�+B
�%B
�B
�B
�B
� B
~�B
~�B
}�B
|�B
|�B
z�B
z�B
w�B
w�B
v�B
t�B
t�B
r�B
q�B
p�B
o�B
n�B
m�B
l�B
k�B
jB
iyB
hsB
hsB
gmB
ffB
e`B
dZB
cTB
bNB
cTB
e`B
ffB
iyB
ffB
cTB
aHB
`BB
`BB
`BB
]/B
ZB
ZB
ZB
W
B
T�B
O�B
I�B
H�B
E�B
B�B
=qB
:^B
8RB
6FB
33B
0!B
/B
.B
+B
(�B
&�B
&�B
%�B
$�B
$�B
$�B
$�B
#�B
"�B
!�B
!�B
!�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
hB
bB
\B
bB
hB
bB
bB
bB
bB
bB
bB
\B
\B
\B
VB
VB
PB
PB
PB
VB
\B
\B
hB
oB
oB
oB
hB
hB
hB
hB
hB
hB
hB
bB
bB
\B
\B
bB
bB
bB
hB
hB
hB
hB
bB
bB
bB
bB
bB
bB
bB
\B
bB
bB
hB
\B
JB
DB
\B
bB
bB
hB
oB
hB
bB
bB
bB
\B
bB
\B
\B
\B
\B
\B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
PB
PB
PB
JB
DB
DB
DB
DB
JB
JB
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
\B
VB
VB
VB
\B
\B
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
bB
\B
bB
bB
bB
bB
hB
bB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
uB
uB
{B
{B
uB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
)�B
+B
,B
,B
+B
+B
+B
+B
,B
,B
-B
-B
-B
-B
,B
,B
,B
,B
,B
,B
-B
-B
.B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
6FB
6FB
6FB
7LB
7LB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
<jB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
L�B
O�B
O�B
N�B
M�B
M�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
XB
XB
XB
YB
YB
[#B
\)B
\)B
[#B
ZB
YB
YB
YB
YB
[#B
\)B
]/B
^5B
^5B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
dZB
dZB
dZB
dZB
cTB
cTB
cTB
cTB
cTB
cTB
e`B
ffB
e`B
gmB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
jB
jB
jB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.21 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20211104110049                              AO  ARCAADJP                                                                    20211104110049    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20211104110049  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20211104110049  QCF$                G�O�G�O�G�O�0               