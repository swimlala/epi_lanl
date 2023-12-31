CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-23T07:09:56Z creation;2018-07-23T07:10:04Z conversion to V3.1;2019-12-23T06:18:43Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180723070956  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               JA   JA  I2_0675_074                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�q_ƻZ�1   @�q`����@9` ě���c,����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@y��@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DJ��DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΃3D�� D�  D�C3Dπ D�� D�  D�@ DЃ3D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @J=q@��@θRA\)A'\)AG\)Ag\)A��A��A��A�z�AîAӮA�A�B�
B	�
Bp�B�
B!�
B)�
B1�
B9�
BA�
BI�
BQ�
BY�
Ba�
Bi�
Bq�
By�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C u�Cu�Cu�Cu�Cu�C
u�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�C u�C"u�C$u�C&u�C(u�C*u�C,u�C.u�C0u�C2u�C4u�C6u�C8u�C:u�C<u�C>u�C@u�CBu�CDu�CFu�CHu�CJu�CLu�CNu�CPu�CRu�CTu�CVu�CXu�CZu�C\u�C^u�C`u�Cbu�Cdu�Cfu�Chu�Cju�Clu�Cnu�Cpu�Cru�Ctu�Cvu�Cxu�Czu�C|u�C~u�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�G�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�G�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�G�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�
DqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"��D#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDK
DK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt�qDuqDu�qDvqDv�qDwqDw�qDxqDx�qDyqDy�qDzqDz�qD{qD{�qD|qD|�qD}qD}�qD~qD~�qDqD�qD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�˅D��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D�D�θD��D�N�DÎ�D�θD��D�N�DĎ�D�θD��D�N�DŎ�D�θD��D�N�DƎ�D�θD��D�N�Dǎ�D�θD��D�N�DȎ�D�θD��D�N�DɎ�D�θD��D�N�Dʎ�D�θD��D�N�Dˎ�D�θD��D�N�D̎�D�θD��D�N�D͎�D�θD��D�N�DΑ�D�θD��D�Q�Dώ�D�θD��D�N�DБ�D�θD��D�N�Dю�D�θD��D�N�DҎ�D�θD��D�N�Dӎ�D�θD��D�N�DԎ�D�θD��D�N�DՎ�D�θD��D�N�D֎�D�θD��D�N�D׎�D�θD��D�N�D؎�D�θD��D�N�Dَ�D�θD��D�N�Dڎ�D�θD��D�N�Dێ�D�θD��D�N�D܎�D�θD��D�N�Dݎ�D�θD��D�N�Dގ�D�θD��D�N�Dߎ�D�θD��D�N�D���D�θD��D�N�DᎸD�θD��D�N�D⎸D�θD��D�N�D㎸D�θD��D�N�D䎸D�θD��D�N�D厸D�θD��D�N�D掸D�θD��D�N�D玸D�θD��D�N�D��D�θD��D�N�D鎸D�θD��D�N�DꎸD�θD��D�N�D뎸D�θD��D�N�D쎸D�θD��D�N�D편D�θD��D�K�DD�θD��D�N�DD�θD��D�N�D���D�θD��D�N�D�D�θD��D�N�D�D�θD��D�N�D�D�θD��D�N�D�D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�HR11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�^5A�^5A�`BA�dZA�ffA�dZA�bNA�dZA�dZA�ffA�dZA�dZA�bNA�bNA�dZA�\)A�C�AǴ9A���A�~�A��yA��A�1A��A�7LA��^A�A�ȴA�$�A�p�A�/A��uA�\)A�1A��uA�+A�$�A��+A��A��FA�O�A��TA�-A��/A��uA�ƨA�hsA���A�A��A�\)A�"�A�ffA��A�VA�n�A��A��7A�I�A��A���A�|�A�(�A��A���A��wA�ȴA�%A�z�A���A���A��PA�jA�%A��RA�7LA��A���A��mA���A�bA�XA��A���A���A���A�1A�r�A��A��A���A�S�A���A�ƨA���A�;dA��A�A�A�A���A���A��RA�  A��A��TA�p�A�M�A��uA���A�PA}�A{%AxE�Au��As\)Ar��ArM�Aq\)Ap�DAn��Am%Ak�Aj�Ah�!Af�Ae%Ac�7Aa�A_C�A]�A]%A\��A[
=AX�+AW33AU�hAS;dAP�AO|�ANĜAL  AK|�AJ�9AI�AI7LAH��AH(�AG�7AF�`AF1'AD�DAD�ACXAB��AA��AA\)A@�A>r�A=��A<��A;�hA:�yA:�A9�A6�\A57LA4=qA3%A2A0��A01A.ZA-;dA,bA+A)�TA(�+A'��A'��A(JA(Q�A'�TA&�A$M�A#�^A"�!A!O�A�hAVA�A\)A`BA��AS�A��A/A;dA�AXA��Ap�A��AAXAjA��A��AO�A�A��A�HA�/AĜA��A7LA�/A�DAQ�A9XA��A�A
�`A
bA�/A��A7LA��A��A��AS�Az�A�;A/A�A�#A\)A Ĝ@��F@�M�@���@�(�@��H@��H@�7L@��@��@��;@@�h@���@�^5@�G�@�D@��
@�^@�V@�G�@��@�R@�v�@���@��/@�bN@�dZ@��/@�J@�`B@�A�@�v�@��#@�5?@��@�I�@Ӆ@�K�@�
=@��@�(�@�;d@�@�b@�+@��@�/@��@�+@Ə\@Ƨ�@Ɵ�@�n�@�r�@��@î@��@�n�@�ff@�ff@�n�@§�@�+@��@��@°!@�v�@�{@���@�Ĝ@�bN@� �@���@�n�@��@���@�t�@�S�@�;d@�o@�n�@�O�@�A�@�\)@��R@�-@�&�@�A�@�ƨ@�|�@�
=@���@��-@�V@�\)@���@�M�@�@�V@�bN@��@�dZ@��@�M�@�@���@��@��/@��D@�1'@��w@�33@���@�V@�{@���@�X@��@��
@���@�\)@��@��@��R@�V@���@�`B@���@�9X@�A�@�(�@��w@�33@��H@���@���@���@���@���@��\@�~�@�~�@��T@�X@��@�(�@�1'@�1@���@��P@���@��+@�v�@�M�@�n�@�^5@��-@���@���@��u@�j@���@���@�\)@�K�@�33@���@�n�@�-@�{@���@���@�hs@�X@�p�@��@��9@�z�@���@��@�K�@�+@�+@�@��@��R@��!@���@�v�@�ff@�V@�E�@�$�@���@��h@�x�@�p�@�hs@�7L@�V@���@��`@���@��j@���@�z�@�A�@�  @��m@�ƨ@��F@�|�@�S�@��H@��@���@�ff@��@��T@�@�p�@�X@�?}@��@���@���@��j@�j@�(�@��@��w@��@�S�@�"�@���@�V@�@�`B@�V@���@�z�@��@���@��@� �@���@�o@���@�~�@�$�@��T@�?}@��@�%@���@��/@���@��D@�r�@�bN@�Z@�Q�@�A�@�  @���@�t�@�S�@�33@��@�o@��@���@���@���@�v�@�$�@�^5@�M�@�@��h@�hs@�G�@��@��`@��D@�A�@��@�r�@�;@�b@�@}p�@|�j@|z�@{��@{S�@z�\@z~�@z~�@z^5@zJ@y�#@y�^@z=q@y&�@y�7@z�@y�#@y�7@y&�@y%@x��@x�9@xr�@xA�@xb@w|�@v��@vV@u�@u��@u��@u?}@t��@tZ@t9X@t(�@t�@s�F@sS�@s@r��@r�!@r~�@q��@q��@q�7@q%@pQ�@ol�@o\)@o;d@n�R@nE�@n$�@m�@n{@m��@m`B@mV@l��@l��@l��@m`B@m�h@m��@m�h@m�@mp�@m?}@l�j@l9X@kƨ@k��@k"�@j-@iG�@hr�@h1'@hr�@h�`@h��@hr�@g�w@g�@f��@fȴ@fff@e�@d(�@d(�@d(�@d1@c�F@ct�@b�@b�\@b=q@bJ@aX@`�@_�@_K�@^��@^5?@]��@]��@]/@]V@\�/@\�@[�F@[dZ@Z�\@Y��@Yhs@XĜ@X�@X  @W��@W�w@W�P@W\)@W
=@V�@V@Up�@UO�@UV@T�@Tj@T1@S�
@R�@R��@R��@RM�@R-@RJ@Q�#@Q��@Q�7@Qx�@P��@PQ�@O�w@O��@O|�@O+@N�y@N��@NV@N@MO�@L�@Lj@L�@Kt�@J�@K@J�@J�H@J��@J��@Jn�@JJ@I��@I7L@H��@H��@H1'@H  @G�@G�@G�w@G��@F�R@FV@F5?@F@E��@E�h@E/@D�j@Dz�@Dj@DZ@D1@C�
@C�F@C��@Ct�@CdZ@CC�@CS�@CS�@C"�@B~�@A�@A��@A7L@A&�@A&�@A&�@@��@@�u@@  @?�@?l�@?;d@?
=@>�@>�R@>��@>�+@>V@>{@=��@=�-@=��@=�h@=O�@=�@<��@<�/@<�j@<�@<��@<��@<�D@<z�@<z�@<9X@;�
@;�@;@:~�@:n�@:=q@9�@9�^@9��@9��@9hs@9�@8��@8r�@81'@8  @7�;@7�w@7l�@6�y@6�+@6E�@5�@5��@5O�@4��@4��@4(�@3�
@3ƨ@3��@3S�@3@2�\@2n�@2^5@2=q@2�@1�@1��@1��@1�7@1&�@0�`@0r�@0bN@0b@/��@/�@/��@/l�@/;d@.�R@.��@.v�@.ff@.V@-�@-`B@-/@,��@,j@,(�@+�m@+��@+S�@+o@*�H@*��@*�\@*=q@*J@)��@)�@)��@)X@)&�@(��@( �@'��@'|�@'
=@&ȴ@&ff@&@%��@%p�@%V@$��@$(�@$1@#�F@#��@#C�@#@"�H@"�!@"��@"^5@"-@!�@!��@!x�@!7L@!�@ �`@ �@ r�@ 1'@   @�@�w@|�@l�@K�@;d@�@
=@ȴ@�+@�+@V@$�@�T@@�-@��@�@`B@?}@V@��@��@��@j@I�@9X@�m@ƨ@��@t�@dZ@o@��@n�@=q@-@�@�@��@��@G�@&�@%@�`@��@Ĝ@��@�u@�@r�@ �@  @�;@|�@K�@;d@��@ȴ@��@�+@5?@5?@{@@�-@�h@?}@�/@�@�D@9X@�m@ƨ@��@33@�H@��@��@��@�\@n�@�@�@�#@�^@��@�7@G�@��@��@Ĝ@��@�u@r�@Q�@1'@ �@  @�w@|�@K�@��@�R@��@�+@5?@@�@�T@�T@��@��@�@p�@`B@�@�/@�j@�D@j@(�@1@�
@ƨ@�F@��@�@dZ@33@o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�^5A�^5A�`BA�dZA�ffA�dZA�bNA�dZA�dZA�ffA�dZA�dZA�bNA�bNA�dZA�\)A�C�AǴ9A���A�~�A��yA��A�1A��A�7LA��^A�A�ȴA�$�A�p�A�/A��uA�\)A�1A��uA�+A�$�A��+A��A��FA�O�A��TA�-A��/A��uA�ƨA�hsA���A�A��A�\)A�"�A�ffA��A�VA�n�A��A��7A�I�A��A���A�|�A�(�A��A���A��wA�ȴA�%A�z�A���A���A��PA�jA�%A��RA�7LA��A���A��mA���A�bA�XA��A���A���A���A�1A�r�A��A��A���A�S�A���A�ƨA���A�;dA��A�A�A�A���A���A��RA�  A��A��TA�p�A�M�A��uA���A�PA}�A{%AxE�Au��As\)Ar��ArM�Aq\)Ap�DAn��Am%Ak�Aj�Ah�!Af�Ae%Ac�7Aa�A_C�A]�A]%A\��A[
=AX�+AW33AU�hAS;dAP�AO|�ANĜAL  AK|�AJ�9AI�AI7LAH��AH(�AG�7AF�`AF1'AD�DAD�ACXAB��AA��AA\)A@�A>r�A=��A<��A;�hA:�yA:�A9�A6�\A57LA4=qA3%A2A0��A01A.ZA-;dA,bA+A)�TA(�+A'��A'��A(JA(Q�A'�TA&�A$M�A#�^A"�!A!O�A�hAVA�A\)A`BA��AS�A��A/A;dA�AXA��Ap�A��AAXAjA��A��AO�A�A��A�HA�/AĜA��A7LA�/A�DAQ�A9XA��A�A
�`A
bA�/A��A7LA��A��A��AS�Az�A�;A/A�A�#A\)A Ĝ@��F@�M�@���@�(�@��H@��H@�7L@��@��@��;@@�h@���@�^5@�G�@�D@��
@�^@�V@�G�@��@�R@�v�@���@��/@�bN@�dZ@��/@�J@�`B@�A�@�v�@��#@�5?@��@�I�@Ӆ@�K�@�
=@��@�(�@�;d@�@�b@�+@��@�/@��@�+@Ə\@Ƨ�@Ɵ�@�n�@�r�@��@î@��@�n�@�ff@�ff@�n�@§�@�+@��@��@°!@�v�@�{@���@�Ĝ@�bN@� �@���@�n�@��@���@�t�@�S�@�;d@�o@�n�@�O�@�A�@�\)@��R@�-@�&�@�A�@�ƨ@�|�@�
=@���@��-@�V@�\)@���@�M�@�@�V@�bN@��@�dZ@��@�M�@�@���@��@��/@��D@�1'@��w@�33@���@�V@�{@���@�X@��@��
@���@�\)@��@��@��R@�V@���@�`B@���@�9X@�A�@�(�@��w@�33@��H@���@���@���@���@���@��\@�~�@�~�@��T@�X@��@�(�@�1'@�1@���@��P@���@��+@�v�@�M�@�n�@�^5@��-@���@���@��u@�j@���@���@�\)@�K�@�33@���@�n�@�-@�{@���@���@�hs@�X@�p�@��@��9@�z�@���@��@�K�@�+@�+@�@��@��R@��!@���@�v�@�ff@�V@�E�@�$�@���@��h@�x�@�p�@�hs@�7L@�V@���@��`@���@��j@���@�z�@�A�@�  @��m@�ƨ@��F@�|�@�S�@��H@��@���@�ff@��@��T@�@�p�@�X@�?}@��@���@���@��j@�j@�(�@��@��w@��@�S�@�"�@���@�V@�@�`B@�V@���@�z�@��@���@��@� �@���@�o@���@�~�@�$�@��T@�?}@��@�%@���@��/@���@��D@�r�@�bN@�Z@�Q�@�A�@�  @���@�t�@�S�@�33@��@�o@��@���@���@���@�v�@�$�@�^5@�M�@�@��h@�hs@�G�@��@��`@��D@�A�@��@�r�@�;@�b@�@}p�@|�j@|z�@{��@{S�@z�\@z~�@z~�@z^5@zJ@y�#@y�^@z=q@y&�@y�7@z�@y�#@y�7@y&�@y%@x��@x�9@xr�@xA�@xb@w|�@v��@vV@u�@u��@u��@u?}@t��@tZ@t9X@t(�@t�@s�F@sS�@s@r��@r�!@r~�@q��@q��@q�7@q%@pQ�@ol�@o\)@o;d@n�R@nE�@n$�@m�@n{@m��@m`B@mV@l��@l��@l��@m`B@m�h@m��@m�h@m�@mp�@m?}@l�j@l9X@kƨ@k��@k"�@j-@iG�@hr�@h1'@hr�@h�`@h��@hr�@g�w@g�@f��@fȴ@fff@e�@d(�@d(�@d(�@d1@c�F@ct�@b�@b�\@b=q@bJ@aX@`�@_�@_K�@^��@^5?@]��@]��@]/@]V@\�/@\�@[�F@[dZ@Z�\@Y��@Yhs@XĜ@X�@X  @W��@W�w@W�P@W\)@W
=@V�@V@Up�@UO�@UV@T�@Tj@T1@S�
@R�@R��@R��@RM�@R-@RJ@Q�#@Q��@Q�7@Qx�@P��@PQ�@O�w@O��@O|�@O+@N�y@N��@NV@N@MO�@L�@Lj@L�@Kt�@J�@K@J�@J�H@J��@J��@Jn�@JJ@I��@I7L@H��@H��@H1'@H  @G�@G�@G�w@G��@F�R@FV@F5?@F@E��@E�h@E/@D�j@Dz�@Dj@DZ@D1@C�
@C�F@C��@Ct�@CdZ@CC�@CS�@CS�@C"�@B~�@A�@A��@A7L@A&�@A&�@A&�@@��@@�u@@  @?�@?l�@?;d@?
=@>�@>�R@>��@>�+@>V@>{@=��@=�-@=��@=�h@=O�@=�@<��@<�/@<�j@<�@<��@<��@<�D@<z�@<z�@<9X@;�
@;�@;@:~�@:n�@:=q@9�@9�^@9��@9��@9hs@9�@8��@8r�@81'@8  @7�;@7�w@7l�@6�y@6�+@6E�@5�@5��@5O�@4��@4��@4(�@3�
@3ƨ@3��@3S�@3@2�\@2n�@2^5@2=q@2�@1�@1��@1��@1�7@1&�@0�`@0r�@0bN@0b@/��@/�@/��@/l�@/;d@.�R@.��@.v�@.ff@.V@-�@-`B@-/@,��@,j@,(�@+�m@+��@+S�@+o@*�H@*��@*�\@*=q@*J@)��@)�@)��@)X@)&�@(��@( �@'��@'|�@'
=@&ȴ@&ff@&@%��@%p�@%V@$��@$(�@$1@#�F@#��@#C�@#@"�H@"�!@"��@"^5@"-@!�@!��@!x�@!7L@!�@ �`@ �@ r�@ 1'@   @�@�w@|�@l�@K�@;d@�@
=@ȴ@�+@�+@V@$�@�T@@�-@��@�@`B@?}@V@��@��@��@j@I�@9X@�m@ƨ@��@t�@dZ@o@��@n�@=q@-@�@�@��@��@G�@&�@%@�`@��@Ĝ@��@�u@�@r�@ �@  @�;@|�@K�@;d@��@ȴ@��@�+@5?@5?@{@@�-@�h@?}@�/@�@�D@9X@�m@ƨ@��@33@�H@��@��@��@�\@n�@�@�@�#@�^@��@�7@G�@��@��@Ĝ@��@�u@r�@Q�@1'@ �@  @�w@|�@K�@��@�R@��@�+@5?@@�@�T@�T@��@��@�@p�@`B@�@�/@�j@�D@j@(�@1@�
@ƨ@�F@��@�@dZ@33@o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BBBBBBBBBBBBB%B%B%B%B1B{Bl�Bs�Bx�B�B�\B�oB�{B��B�uB�bB�VB�JB�JB�VB�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�3B�jBÖBŢBɺBɺB��B��B��BǮBĜB��B�^B�LB��B��B�1B� Bt�Be`B^5B[#BS�BM�BG�BD�BC�B8RB)�B�B��B�BB��BÖB�}B�9B��B��B�bB}�BiyBbNBR�B?}B8RB1'B'�B �B�B
=B
��B
�B
�B
�
B
�jB
��B
� B
l�B
_;B
M�B
;dB
,B
�B
B	��B	��B	�B	�B	�BB	�B	��B	ÖB	�RB	��B	��B	�{B	�7B	{�B	s�B	m�B	iyB	bNB	S�B	J�B	A�B	5?B	+B	!�B	�B	
=B	B	  B��B��B�B�B�B�B�B�mB�ZB�NB�;B�B�B��B��BB�}B�LB�LB�XB�!B��B�DB�B{�Bs�Br�Bq�Bl�BiyBiyBhsBffBe`BdZBo�Bx�B� B~�B}�Bt�Bq�Bq�Bq�Bk�Bk�Be`BdZBe`B`BBXBN�BK�BO�BS�BI�BD�BC�BB�BC�B@�B?}B<jB<jB;dB:^B:^B;dB<jB=qB?}B=qB=qBA�BA�B@�B@�B?}B>wB<jB;dB9XB9XB8RB5?B49B6FB5?B49B33B33B2-B1'B1'B0!B.B-B-B-B.B+B'�B$�B!�B!�B&�B(�B+B1'B49B33B.B2-B6FB8RB9XB:^B:^B;dB:^B:^B8RB.B.B.B-B-B0!B33B33B5?B6FB6FB9XB;dB<jB<jB=qB<jB;dB>wB=qB>wBA�BB�BB�BG�BD�BD�BC�BD�BF�BF�BI�BM�BR�B[#B_;B`BBbNBcTBe`BhsBl�Bm�Bn�Bo�Bm�Bn�Bo�Bp�Bq�Bq�Br�Bq�Bq�Br�Bt�Bu�Bv�By�B|�B|�B}�B~�B}�B� B�B�B�B�%B�1B�JB�VB�VB�oB��B��B��B��B��B��B��B��B��B�B�3B�XB�dB�qB��BBɺB��B��B��B��B��B�
B�B�#B�#B�#B�BB�HB�TB�mB�B�B�B�B��B��B��B��B��B	  B	B	B	1B	VB	oB	oB	�B	�B	�B	�B	!�B	#�B	&�B	)�B	/B	0!B	33B	5?B	;dB	<jB	@�B	A�B	B�B	G�B	G�B	H�B	I�B	J�B	K�B	L�B	N�B	P�B	Q�B	R�B	R�B	VB	[#B	`BB	aHB	aHB	cTB	dZB	dZB	e`B	ffB	iyB	k�B	l�B	m�B	n�B	r�B	w�B	w�B	x�B	x�B	{�B	|�B	}�B	~�B	� B	�B	�B	�B	�B	�7B	�=B	�JB	�VB	�\B	�hB	�hB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�9B	�FB	�LB	�LB	�LB	�XB	�XB	�^B	�dB	�jB	�}B	ÖB	ŢB	ƨB	ǮB	ƨB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�#B	�)B	�/B	�;B	�HB	�TB	�`B	�`B	�`B	�`B	�sB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
1B
+B
+B
+B
+B
1B
1B
	7B
DB
DB

=B

=B
DB
JB
PB
VB
\B
\B
\B
\B
\B
\B
bB
bB
bB
\B
VB
PB
PB
PB
\B
hB
oB
oB
uB
uB
uB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
.B
.B
/B
/B
.B
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
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
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
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
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
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
XB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
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
jB
k�B
k�B
k�B
k�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BBB�BBB�B�B�BBB�BB�BBB�BBFBlqBs�Bx�B�B�BB�:B�FB�mB�@B�.B�"B�B�B�<B�(B�4B�YB�kB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�PB�aB�mBɠBɆB̘B̳B˒B�zBāB�iB�DB�B��B�mB��B�Bt�BeFB^BZ�BS�BM�BGzBDgBCaB88B)�B�B��B�BϫB�aB�HB�B��B��B�.B}�Bi_Bb4BR�B?cB8B1B'�B �B_B
	B
�B
�WB
�eB
��B
�PB
�kB
�B
lWB
_!B
M�B
;0B
+�B
MB
�B	��B	��B	�B	�QB	�B	��B	ˬB	�aB	�B	��B	��B	�FB	�B	{�B	s�B	m]B	iDB	bB	S�B	J�B	AUB	5B	*�B	!�B	~B	
	B	�B��B��B��B�|B�cB�]B�WB�QB�8B�&B�B�B��B��BѷB�UB�[B�HB�B�B�$B��B�eB�B��B{�Bs�Br|BqvBlWBiDBiDBh>Bf2Be,Bd&BoiBx�B�B~�B}�Bt�Bq[BqvBqvBkQBkQBe,Bd&Be,B`BW�BN�BK�BO�BS�BI�BDgBCaBB[BCaB@4B?HB<6B<6B;0B:*B:*B;0B<6B=<B?HB=<B=<BAUBAUB@OB@OB?HB>BB<6B;0B9$B9$B8B4�B4B6B5B3�B2�B2�B1�B0�B0�B/�B-�B,�B,�B,�B-�B*�B'�B$�B!�B!�B&�B(�B*�B0�B4B2�B-�B1�B6B8B9$B:B:*B;B:*B:B8B-�B-�B-�B,�B,�B/�B2�B2�B5B6B6B9$B;0B<B<B=<B<6B;0B>BB=<B>BBAUBB[BB[BGzBDgBDgBCaBDgBFtBFtBI�BM�BR�BZ�B^�B`Ba�BcBe,Bh>BlWBm]BncBoOBm]BncBoOBpoBqvBqvBr|Bq[BqvBr|Bt�Bu�Bv�By�B|�B|�B}�B~�B}�B�B��B��B��B��B��B��B�"B�"B�:B�_B�xB��B�vB��B��B��B��B��B��B��B�$B�0B�<B�4B�[BɆB�rB�~BΊBϑBѝB��B��B��B��B��B�B��B� B�8B�KB�WB�oB�hB�nB��B��B��B��B��B	 �B	�B	�B	B	 B	:B	MB	SB	xB	�B	!�B	#�B	&�B	)�B	.�B	/�B	2�B	5B	;0B	<6B	@OB	A;B	B[B	GzB	GzB	HfB	IlB	J�B	K�B	L�B	N�B	P�B	Q�B	R�B	R�B	U�B	Z�B	_�B	`�B	aB	cB	dB	dB	e,B	fB	i*B	k6B	lWB	mCB	nIB	r|B	w�B	w�B	x�B	x�B	{�B	|�B	}�B	~�B	�B	��B	��B	��B	��B	�B	��B	�B	�"B	�(B	�B	�4B	�B	�&B	�,B	�YB	�qB	�]B	��B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	�	B	�	B	�*B	�0B	�6B	�HB	�aB	�mB	�YB	�zB	�tB	�SB	�zB	ȀB	�lB	�xB	͟B	ΥB	ΊB	ЗB	��B	��B	��B	��B	��B	��B	ּB	ּB	��B	��B	��B	��B	��B	��B	�B	��B	�B	�,B	�,B	�B	�B	�$B	�DB	�DB	�>B	�DB	�0B	�QB	�QB	�=B	�=B	�iB	�UB	�iB	�vB	�vB	�oB	�cB	�IB	�OB	�oB	�oB	�UB	�oB	�oB	�[B	�[B	�|B	�tB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	B
	B
	B
	B
�B
�B
�B
�B
�B
�B
�B
	B
B
B

	B

	B
B
B
B
B
(B
(B
B
(B
(B
(B
.B
.B
B
(B
"B
B
B
B
(B
4B
 B
 B
@B
&B
@B
FB
FB
FB
,B
FB
,B
2B
MB
9B
9B
SB
9B
9B
SB
MB
FB
,B
@B
@B
FB
FB
MB
MB
MB
SB
SB
SB
?B
YB
_B
eB
eB
QB
QB
kB
WB
xB
]B
]B
�B
jB
�B
�B
pB
 �B
 vB
 vB
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
1�B
1�B
1�B
2�B
2�B
2�B
4B
3�B
5B
4�B
5B
5B
6B
6�B
7B
7B
8B
8B
8B
8B
8B
8B
9$B
9$B
:*B
:*B
:*B
:*B
;0B
;0B
<B
<B
<6B
<B
<B
="B
>(B
>(B
>BB
>BB
>(B
>BB
>BB
?HB
?HB
@4B
@OB
@OB
AUB
A;B
BAB
BAB
B[B
BAB
CGB
CaB
CGB
DMB
DMB
DgB
DMB
DgB
DgB
DgB
DgB
DMB
ESB
EmB
EmB
EmB
ESB
FtB
FtB
FYB
FtB
FtB
GzB
G_B
GzB
GzB
GzB
GzB
HfB
HfB
HfB
IlB
IlB
J�B
J�B
J�B
K�B
K�B
KxB
K�B
KxB
KxB
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
^B
^B
_B
^�B
^�B
_B
_B
_B
_B
`B
_�B
aB
aB
aB
bB
bB
c B
cB
cB
c B
c B
c B
cB
c B
cB
d&B
dB
d&B
d&B
d&B
dB
d&B
e,B
eB
e,B
eB
e,B
eB
e,B
f2B
f2B
f2B
f2B
f2B
g8B
g8B
g8B
g8B
g8B
h>B
h$B
h$B
h>B
h>B
h>B
h$B
i*B
iDB
i*B
iDB
iDB
iDB
jKB
jKB
j0B
jKB
j0B
j0B
jKB
jKB
j0B
kQB
kQB
kQB
k611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.46(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807180041082018071800410820180718004108201807190035242018071900352420180719003524JA  ARFMdecpA19c                                                                20180723153828  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180723070956  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180723071000  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180723071003  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180723071004  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180723071004  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180723071004  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180723071004  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20180723071004                      G�O�G�O�G�O�                JA  ARUP                                                                        20180723072033                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180712153747  CV  JULD            G�O�G�O�FÊ�                JM  ARCAJMQC2.0                                                                 20180717154108  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180717154108  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180718153524  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                