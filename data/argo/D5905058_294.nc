CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2021-02-06T21:39:51Z creation;2021-02-06T21:39:54Z conversion to V3.1;2023-06-29T05:47:19Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ],   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210206213951  20230705041505  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              &A   JA  I2_0675_294                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�\f���1   @�\gF)�@6��8�YK�b���'RT1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�	�D�,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@θRA\)A'\)AG\)Ag\)A��A��A��A��AîAӮA�A�B�
B	�
B�
B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BĸRB��B��B��B��B��B��B��B��B��B��B��B��B��B��C u�Cu�Cu�Cu�Cu�C
u�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�C u�C"u�C$u�C&�\C(u�C*u�C,u�C.u�C0u�C2u�C4u�C6u�C8u�C:u�C<u�C>u�C@u�CBu�CDu�CFu�CHu�CJu�CLu�CNu�CPu�CRu�CTu�CVu�CXu�CZu�C\u�C^u�C`u�Cbu�Cdu�Cfu�Chu�Cju�Clu�Cnu�Cpu�Cru�Ctu�Cvu�Cxu�Czu�C|u�C~u�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�.C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�G�C�:�C�:�D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt�qDuqDu�qDvqDv�qDwqDw�qDxqDx�qDyqDy�qDzqDz�qD{qD{�qD|qD|�qD}qD}�qD~qD~�qDqD�qD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�˅D��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D�D�θD��D�N�DÎ�D�θD��D�N�DĎ�D�θD��D�N�DŎ�D�θD��D�N�DƎ�D�θD��D�N�Dǎ�D�θD��D�N�DȎ�D�θD��D�N�DɎ�D�θD��D�N�Dʎ�D�θD��D�N�Dˎ�D�θD��D�N�D̎�D�θD��D�N�D͎�D�θD��D�N�DΎ�D�θD��D�N�Dώ�D�θD��D�N�DЎ�D�θD��D�N�Dю�D�θD��D�N�DҎ�D�θD��D�N�Dӎ�D�θD��D�N�DԎ�D�θD��D�N�DՎ�D�θD��D�N�D֎�D�θD��D�N�D׎�D�θD��D�N�D؎�D�θD��D�N�Dَ�D�θD��D�N�Dڎ�D�θD��D�N�Dێ�D�θD��D�N�D܎�D�θD��D�N�Dݎ�D�θD��D�N�Dގ�D�θD��D�N�Dߎ�D�θD��D�N�D���D�θD��D�N�DᎸD�θD��D�N�D⎸D�θD��D�N�D㎸D�θD��D�N�D䎸D�θD��D�N�D厸D�θD��D�N�D掸D�θD��D�N�D玸D�θD��D�N�D莸D�θD��D�N�D鎸D�θD��D�N�DꎸD�θD��D�N�D뎸D�θD��D�N�D쎸D�θD��D�N�D편D�θD��D�N�DD�θD��D�N�DD�θD��D�N�D���D�θD��D�N�D�D�θD��D�N�D�D�θD��D�N�D�D�θD��D�N�D�D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D���D�RD�;�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�`BA�hsA�n�A�p�A�n�A�p�A�r�A�z�A�|�A�x�A�v�A�v�A�t�A�p�A�p�A�p�A�p�A�~�A��A�~�A��A��A��A��A��DA��DA��FA��FA��-A��-A���A���A���A���A���A��uA��hA��\A��PA��+A�z�A�l�A�\)A��A�C�A���A�t�A�1A�I�A�l�A���A��A�dZA���A��A�n�A��A�A�33A��#A�A�hsA���A�ffA�?}A�ĜA��A���A�S�A�1A���A�~�A���A���A��/A�E�A��#A�|�A� �A�t�A�`BA�5?A��A���A� �A��7A�O�A�9XA�VA���A��TA��9A��!A�z�A��RA�C�A�bA�5?A���A�S�A���A��+A��`A�K�A��PA�
=A�?}A���A~�A}�Ay�Ay"�Av��Ar��ApVAlĜAk��AjffAhAd��AaƨA_��A]7LAZ-AV-AS�AQXAP�`AP9XAN��AM��ALȴAI\)AF��AC�-AA��A@��A?�-A>�A<�A<bNA:��A9��A7��A7oA6bNA5"�A3��A2�yA2E�A/�#A-��A,^5A+33A*5?A(r�A'7LA&r�A%�^A$�A#��A#x�A!�A A�A�A�TA��AS�AA��AI�A�
A��AƨA?}Ar�A��A��A �A��A��A�A�RA�uAbNAC�A�/A�A1AVAjA �AA	��A	%Av�A$�A1A��A��AVA��A  A�jA�9A�jA��A�uA�^A�`AJA �u@��@���@�5?@���@�j@��!@�n�@���@�bN@�^5@�?}@�I�@�P@�-@��T@�@��@�/@���@�1@���@��@�7L@��y@柾@�\@��@�x�@���@��@��m@�
=@ޗ�@�{@�r�@�|�@ڗ�@�x�@�9X@�hs@Ӆ@�33@�V@���@�+@��#@�7L@���@̬@�V@���@ɺ^@�hs@�V@�Q�@�l�@��T@�Q�@�bN@�Z@¸R@�`B@��D@�ƨ@�ȴ@��@�x�@�j@�1@�  @��@��F@�\)@�ȴ@���@��+@��7@���@���@�Z@�(�@��w@�\)@���@�-@�@��`@��;@�+@��R@�^5@��@��@�Ĝ@�9X@���@�@���@��@�r�@�9X@�ƨ@�C�@��@�n�@���@��h@�7L@��@�j@���@�C�@���@�r�@��@�C�@�@��y@��@��\@�=q@���@��7@�G�@���@��@���@��D@��@���@�+@��R@�E�@���@��-@���@�G�@���@���@�j@�1'@��
@�|�@�dZ@�@�ȴ@���@�M�@���@��7@�p�@�G�@���@��9@���@��@��j@��j@��D@�Q�@�b@���@���@��P@�|�@�S�@�33@��y@��R@��\@��+@�M�@��@��^@���@��h@��@�p�@�O�@�/@��@�V@���@���@��@��/@���@��9@��u@�r�@�A�@�1'@�(�@��@���@���@��@�\)@�33@�o@�
=@�
=@�
=@���@���@�M�@��@���@�?}@��@�%@���@���@��@�r�@�Z@�I�@�A�@�1'@��;@�l�@�@��\@�~�@�ff@�-@�{@�J@���@��-@���@�?}@��@�bN@�1'@�  @��
@��@�K�@�33@�
=@���@�E�@�J@��^@�p�@��@��j@��@��u@�z�@�bN@�A�@��@��@��@�t�@�K�@�+@��@���@��\@��+@�=q@��@���@��@�X@�?}@��@��/@��9@���@�Q�@�(�@���@��
@�ƨ@��F@���@�t�@�S�@��y@���@�=q@�x�@�X@��@��@��j@��@�(�@�b@�@;d@�@~�R@~v�@~v�@~ff@~ff@}��@}/@|�j@|9X@{��@{@z��@z��@z~�@z^5@z=q@y��@xbN@w�@w\)@v��@vV@u�-@t��@tz�@t9X@s�@r�\@r-@q��@qhs@q&�@p�@o�@ol�@n��@n�+@nff@nE�@m�-@mp�@m?}@l��@lz�@l(�@k��@kƨ@k��@kt�@k33@k"�@j��@j=q@i��@iX@i�@h�9@g�w@g;d@f�R@fff@f{@e��@ep�@eO�@d��@dz�@dZ@d(�@c�m@c��@cS�@b��@b-@a��@a�@a��@a��@a��@a��@a�^@aX@`�`@`Ĝ@`�u@`r�@`Q�@` �@_�@_�@_l�@_+@^�R@^v�@^V@^5?@]�@]@]`B@]�@]V@]V@\��@\��@\z�@\(�@\�@\1@[��@Z�@Z�H@Z�H@Z�H@Z��@Z=q@Y�@Y��@Yx�@YX@Y&�@X�`@X�@XQ�@XA�@X1'@Xb@X  @W�@W�;@W�w@W�P@W
=@V5?@UO�@U�@T�@T��@T�@Tj@S�
@S��@SS�@S"�@R��@RJ@Q�#@Q�7@QX@P�`@P��@P�@PbN@PQ�@P1'@P  @O�@O�@OK�@N��@N�R@N��@Nv�@Nff@N@M��@M�@Mp�@M?}@L(�@K��@K��@K�@Kt�@K"�@J��@I�7@Ihs@IX@I7L@I7L@H��@Hr�@HA�@G�@Gl�@F��@F��@F�+@Fff@F5?@F{@E�T@E@E��@Ep�@EO�@EO�@E�@D�/@D��@D�D@D9X@D1@D1@C��@C�F@CC�@B�@B�\@B^5@A��@A�7@AG�@@��@@ �@?�@?��@?�@?\)@>V@=�@=�-@=O�@=�@=�@<��@<�j@<�@<�D@;�
@;t�@;dZ@;33@:�@:M�@:J@9�#@9��@9��@9x�@9%@8��@7�;@7+@6�y@6��@6��@6��@6�+@6V@6$�@6{@5��@5`B@5�@4��@4z�@41@3��@3dZ@3@2��@2��@2~�@2^5@2-@1�#@1��@1G�@1�@0�`@0��@0r�@0Q�@0 �@0b@/K�@/
=@.�@.ȴ@.��@.�+@.ff@.5?@.@-��@-��@-?}@-?}@,�@,�D@,(�@,(�@+�m@+�
@+ƨ@+��@+��@+��@+�@+C�@*��@*��@*��@*�\@*�@)��@)�7@)hs@)�@(Ĝ@(�u@(�u@(�u@(�u@(�u@(Q�@(b@'��@'\)@';d@'K�@'K�@'K�@';d@&��@&�+@&V@&$�@%�@%@%�-@%�@%`B@%V@$�/@$�j@$z�@$Z@$(�@#��@#ƨ@#��@#t�@#S�@#33@#@"�H@"n�@"n�@"n�@"-@"�@"�@"^5@"=q@"�@"J@!�#@!�7@!�7@!x�@!X@!7L@ �`@ bN@  �@ A�@ A�@ 1'@ b@�w@+@ȴ@��@v�@ff@V@E�@{@�T@��@�-@`B@�@9X@1@��@��@�F@�@33@@�@�H@��@�\@M�@��@��@G�@�`@��@��@A�@  @�@��@��@�P@+@��@�y@�@�+@5?@��@�@O�@�@��@�j@(�@�F@S�@C�@�H@�\@-@��@��@x�@&�@Ĝ@�9@�@Q�@  @  @�@�;@��@��@�P@K�@�@��@�@�+@E�@@��@��@�h@�@`B@O�@O�@O�@?}@�@��@�D@j@j@Z@(�@��@�F@�@dZ@33@o@
�@
�H@
��@
�!@
��@
��@
^5@
J@	��@	�7@	x�@	7L@	�@	%@��@��@�9@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�`BA�hsA�n�A�p�A�n�A�p�A�r�A�z�A�|�A�x�A�v�A�v�A�t�A�p�A�p�A�p�A�p�A�~�A��A�~�A��A��A��A��A��DA��DA��FA��FA��-A��-A���A���A���A���A���A��uA��hA��\A��PA��+A�z�A�l�A�\)A��A�C�A���A�t�A�1A�I�A�l�A���A��A�dZA���A��A�n�A��A�A�33A��#A�A�hsA���A�ffA�?}A�ĜA��A���A�S�A�1A���A�~�A���A���A��/A�E�A��#A�|�A� �A�t�A�`BA�5?A��A���A� �A��7A�O�A�9XA�VA���A��TA��9A��!A�z�A��RA�C�A�bA�5?A���A�S�A���A��+A��`A�K�A��PA�
=A�?}A���A~�A}�Ay�Ay"�Av��Ar��ApVAlĜAk��AjffAhAd��AaƨA_��A]7LAZ-AV-AS�AQXAP�`AP9XAN��AM��ALȴAI\)AF��AC�-AA��A@��A?�-A>�A<�A<bNA:��A9��A7��A7oA6bNA5"�A3��A2�yA2E�A/�#A-��A,^5A+33A*5?A(r�A'7LA&r�A%�^A$�A#��A#x�A!�A A�A�A�TA��AS�AA��AI�A�
A��AƨA?}Ar�A��A��A �A��A��A�A�RA�uAbNAC�A�/A�A1AVAjA �AA	��A	%Av�A$�A1A��A��AVA��A  A�jA�9A�jA��A�uA�^A�`AJA �u@��@���@�5?@���@�j@��!@�n�@���@�bN@�^5@�?}@�I�@�P@�-@��T@�@��@�/@���@�1@���@��@�7L@��y@柾@�\@��@�x�@���@��@��m@�
=@ޗ�@�{@�r�@�|�@ڗ�@�x�@�9X@�hs@Ӆ@�33@�V@���@�+@��#@�7L@���@̬@�V@���@ɺ^@�hs@�V@�Q�@�l�@��T@�Q�@�bN@�Z@¸R@�`B@��D@�ƨ@�ȴ@��@�x�@�j@�1@�  @��@��F@�\)@�ȴ@���@��+@��7@���@���@�Z@�(�@��w@�\)@���@�-@�@��`@��;@�+@��R@�^5@��@��@�Ĝ@�9X@���@�@���@��@�r�@�9X@�ƨ@�C�@��@�n�@���@��h@�7L@��@�j@���@�C�@���@�r�@��@�C�@�@��y@��@��\@�=q@���@��7@�G�@���@��@���@��D@��@���@�+@��R@�E�@���@��-@���@�G�@���@���@�j@�1'@��
@�|�@�dZ@�@�ȴ@���@�M�@���@��7@�p�@�G�@���@��9@���@��@��j@��j@��D@�Q�@�b@���@���@��P@�|�@�S�@�33@��y@��R@��\@��+@�M�@��@��^@���@��h@��@�p�@�O�@�/@��@�V@���@���@��@��/@���@��9@��u@�r�@�A�@�1'@�(�@��@���@���@��@�\)@�33@�o@�
=@�
=@�
=@���@���@�M�@��@���@�?}@��@�%@���@���@��@�r�@�Z@�I�@�A�@�1'@��;@�l�@�@��\@�~�@�ff@�-@�{@�J@���@��-@���@�?}@��@�bN@�1'@�  @��
@��@�K�@�33@�
=@���@�E�@�J@��^@�p�@��@��j@��@��u@�z�@�bN@�A�@��@��@��@�t�@�K�@�+@��@���@��\@��+@�=q@��@���@��@�X@�?}@��@��/@��9@���@�Q�@�(�@���@��
@�ƨ@��F@���@�t�@�S�@��y@���@�=q@�x�@�X@��@��@��j@��@�(�@�b@�@;d@�@~�R@~v�@~v�@~ff@~ff@}��@}/@|�j@|9X@{��@{@z��@z��@z~�@z^5@z=q@y��@xbN@w�@w\)@v��@vV@u�-@t��@tz�@t9X@s�@r�\@r-@q��@qhs@q&�@p�@o�@ol�@n��@n�+@nff@nE�@m�-@mp�@m?}@l��@lz�@l(�@k��@kƨ@k��@kt�@k33@k"�@j��@j=q@i��@iX@i�@h�9@g�w@g;d@f�R@fff@f{@e��@ep�@eO�@d��@dz�@dZ@d(�@c�m@c��@cS�@b��@b-@a��@a�@a��@a��@a��@a��@a�^@aX@`�`@`Ĝ@`�u@`r�@`Q�@` �@_�@_�@_l�@_+@^�R@^v�@^V@^5?@]�@]@]`B@]�@]V@]V@\��@\��@\z�@\(�@\�@\1@[��@Z�@Z�H@Z�H@Z�H@Z��@Z=q@Y�@Y��@Yx�@YX@Y&�@X�`@X�@XQ�@XA�@X1'@Xb@X  @W�@W�;@W�w@W�P@W
=@V5?@UO�@U�@T�@T��@T�@Tj@S�
@S��@SS�@S"�@R��@RJ@Q�#@Q�7@QX@P�`@P��@P�@PbN@PQ�@P1'@P  @O�@O�@OK�@N��@N�R@N��@Nv�@Nff@N@M��@M�@Mp�@M?}@L(�@K��@K��@K�@Kt�@K"�@J��@I�7@Ihs@IX@I7L@I7L@H��@Hr�@HA�@G�@Gl�@F��@F��@F�+@Fff@F5?@F{@E�T@E@E��@Ep�@EO�@EO�@E�@D�/@D��@D�D@D9X@D1@D1@C��@C�F@CC�@B�@B�\@B^5@A��@A�7@AG�@@��@@ �@?�@?��@?�@?\)@>V@=�@=�-@=O�@=�@=�@<��@<�j@<�@<�D@;�
@;t�@;dZ@;33@:�@:M�@:J@9�#@9��@9��@9x�@9%@8��@7�;@7+@6�y@6��@6��@6��@6�+@6V@6$�@6{@5��@5`B@5�@4��@4z�@41@3��@3dZ@3@2��@2��@2~�@2^5@2-@1�#@1��@1G�@1�@0�`@0��@0r�@0Q�@0 �@0b@/K�@/
=@.�@.ȴ@.��@.�+@.ff@.5?@.@-��@-��@-?}@-?}@,�@,�D@,(�@,(�@+�m@+�
@+ƨ@+��@+��@+��@+�@+C�@*��@*��@*��@*�\@*�@)��@)�7@)hs@)�@(Ĝ@(�u@(�u@(�u@(�u@(�u@(Q�@(b@'��@'\)@';d@'K�@'K�@'K�@';d@&��@&�+@&V@&$�@%�@%@%�-@%�@%`B@%V@$�/@$�j@$z�@$Z@$(�@#��@#ƨ@#��@#t�@#S�@#33@#@"�H@"n�@"n�@"n�@"-@"�@"�@"^5@"=q@"�@"J@!�#@!�7@!�7@!x�@!X@!7L@ �`@ bN@  �@ A�@ A�@ 1'@ b@�w@+@ȴ@��@v�@ff@V@E�@{@�T@��@�-@`B@�@9X@1@��@��@�F@�@33@@�@�H@��@�\@M�@��@��@G�@�`@��@��@A�@  @�@��@��@�P@+@��@�y@�@�+@5?@��@�@O�@�@��@�j@(�@�F@S�@C�@�H@�\@-@��@��@x�@&�@Ĝ@�9@�@Q�@  @  @�@�;@��@��@�P@K�@�@��@�@�+@E�@@��@��@�h@�@`B@O�@O�@O�@?}@�@��@�D@j@j@Z@(�@��@�F@�@dZ@33@o@
�@
�H@
��@
�!@
��@
��@
^5@
J@	��@	�7@	x�@	7L@	�@	%@��@��@�9@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BbNBcTBcTBcTBcTBdZBdZBe`Be`BdZBcTBdZBcTBbNBbNBbNBaHBdZBdZBdZBdZBdZBdZBdZBffBffBy�B�B�PB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�5B�B��B��B�B��BoB$�B&�B&�B)�B7LB=qB;dB;dB:^B8RB9XB7LB5?B5?B7LB;dB/B0!B(�B#�B�B�B�B�B�BoBPB��B�B�B�B�sB�BǮB�qB�B��B�BgmBW
BB�B/B"�BDB
��B
�HB
�qB
��B
�bB
�B
z�B
m�B
L�B
8RB
+B
�B
	7B	��B	�HB	��B	�?B	�B	��B	��B	�B	k�B	]/B	M�B	:^B	'�B	�B	DB	%B	B��B��B�B�BB��BŢB�RB�9B�B��B��B��B��B��B�bB�JB�7B�B� B|�Bw�Bw�Bp�Bm�BjBjBgmBdZBbNBbNBaHB`BB]/B[#BXBYBYBXBXBXBW
BVBVBT�BQ�BN�BL�BK�BH�BE�BD�BK�BG�BF�BE�BE�BC�BA�B?}B<jB9XB9XB8RB9XB;dB>wB@�BA�B@�BA�BA�BC�BD�BC�B>wB@�BA�BB�BC�BB�B<jB6FB/B0!B.B.B.B/B)�B)�B,B,B,B-B-B,B.B-B-B,B,B33B2-B2-B33B5?B6FB6FB6FB6FB7LB;dB7LB9XB9XB8RB8RB8RB8RB9XB9XB;dB?}BB�BA�BC�BH�BL�BP�BR�BR�BS�B_;B`BB`BB`BB`BB_;B_;BaHBe`BjBr�Bq�Bo�Bm�Bn�Bp�Br�Bt�Bx�By�By�By�Bz�B{�B}�B}�B~�B�B�%B�+B�1B�=B�JB�PB�VB�bB�hB��B��B��B��B��B��B��B��B��B�B�!B�FB�RB�dB�jB�}B��BBÖBɺB��B��B�B�/B�;B�HB�mB�B�B�B��B��B	  B	B	B	
=B	\B	bB	uB	�B	�B	�B	�B	�B	#�B	'�B	,B	/B	1'B	33B	6FB	:^B	;dB	<jB	>wB	A�B	D�B	E�B	J�B	M�B	O�B	R�B	W
B	YB	ZB	\)B	`BB	dZB	e`B	ffB	hsB	iyB	k�B	m�B	n�B	p�B	p�B	q�B	q�B	r�B	s�B	v�B	x�B	y�B	y�B	{�B	~�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�=B	�=B	�=B	�DB	�DB	�JB	�PB	�\B	�bB	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�?B	�RB	�dB	�dB	�dB	�jB	�qB	�qB	�}B	�}B	�}B	B	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�;B	�;B	�BB	�HB	�NB	�NB	�NB	�TB	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
%B
+B
1B
1B
	7B

=B

=B
DB
DB
DB
JB
PB
PB
VB
\B
\B
\B
bB
bB
bB
bB
hB
hB
hB
oB
oB
oB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
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
/B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
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
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
;dB
<jB
<jB
=qB
>wB
>wB
?}B
?}B
?}B
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
D�B
E�B
E�B
E�B
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
I�B
I�B
I�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
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
]/B
]/B
\)B
^5B
^5B
^5B
^5B
_;B
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
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
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
bNB
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
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
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
m�B
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
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bb4Bc Bc Bc Bc Bd&Bd&BeFBeFBd&Bc Bd@Bc:Bb4BbBb4BaBd@Bd@Bd@Bd@Bd@Bd&Bd&BfBf2By�B��B�6B�[B�SB�mB�SB�SB�SB�mB�mB�mB��B��B��B��B�YB�CB�B��B��B��B��B��B�DB��B�aB �BFB%�B'�B)*B.�B:�B>(B<6B<jB:�B9rB:�B7�B6�B8B:B=�B1'B2B*B%B vBkB�BjBBBHB �B��B�B�wB�B��B�B��B��B�B��Bi�BZBE9B1vB&2BpB
�rB
�LB
��B
�IB
�B
��B
}�B
r�B
OvB
:�B
.cB

B
JB
 B	�tB	�[B	��B	�B	��B	�1B	�gB	n/B	`�B	Q�B	>�B	*�B	�B	�B	+B	�B�B��B�B�B�2BǮB��B��B��B�8B��B�vB�jB��B�4B�PB��B��B�B~BBz�BzDBrGBo Bk�BlqBh�BeFBc:BcTBbhBa-B_!B]BYBZBYeBX_BXEBXyBW�BV�BW$BV9BR�BO�BM�BL�BI�BFtBE�BMBHBF�BFBF�BDMBB�BA�B=�B:B9�B9�B:�B<B>�B@�BA�B@�BA�BB'BC�BE�BD�B>]B@iBA�BCBD�BC�B=�B7�B/�B0�B.cB.}B.�B/�B*0B*B,�B-)B,�B-�B-�B,�B.B-B-CB,�B-�B4B2aB2�B4TB6FB6`B6FB6�B6�B9$B;�B8B9�B9�B8�B9>B8�B8�B:*B:^B<�B@iBB�BB'BDgBI�BMjBQBS&BS[BU2B_;B`BB`\B`vB`�B_�B`'Ba�Be`Bj�Bs�BrGBp!BnBo BqBr�Bu?Bx�By�By�By�Bz�B|B}�B~(B}B�GB�B�+B�KB�XB�~B��B��B��B��B��B��B��B��B��B�B�,B�$B�KB��B��B�zB��B�B��B��B��BªB��BɺB�B��B�EB�~B߾B�4B�
B�B�B�B��B��B	 B	-B	9B	
XB	\B	}B	uB	gB	mB	�B	�B	�B	#�B	'�B	+�B	/B	1B	33B	6FB	:DB	;dB	<jB	>wB	A�B	D�B	E�B	J�B	M�B	O�B	SB	V�B	X�B	ZB	\CB	`BB	dB	e,B	f2B	hXB	iyB	k�B	mwB	n�B	p�B	p�B	qvB	q�B	r�B	s�B	v�B	x�B	y�B	y�B	|B	~�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�	B	�#B	�)B	�B	�0B	�6B	�BB	�HB	�NB	�4B	�oB	�{B	�gB	�mB	�yB	�B	�B	�QB	��B	�kB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�AB	�ZB	�lB	�0B	�JB	�dB	�PB	�VB	�VB	�HB	�cB	��B	��B	ƨB	ǔB	ȚB	ɠB	��B	��B	̳B	͹B	��B	��B	��B	�B	��B	�B	�B	��B	��B	�B	�B	��B	�	B	�)B	�/B	�B	�B	�!B	�'B	�-B	�4B	�4B	�4B	�TB	�FB	�`B	�2B	�RB	�RB	�RB	�>B	�DB	�_B	�eB	�QB	�kB	�WB	�WB	�WB	�wB	�B	��B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B
 B
B
�B
B
B
9B
?B
B
1B
KB
	RB

#B

=B
DB
)B
DB
dB
B
6B
VB
BB
BB
BB
HB
HB
bB
HB
NB
NB
NB
:B
:B
:B
TB
TB
uB
uB
aB
{B
�B
�B
�B
�B
�B
yB
yB
eB
eB
B
�B
�B
�B
�B
�B
xB
�B
�B
~B
�B
�B
jB
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
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
+B
,"B
,�B
,�B
-�B
-�B
-�B
.B
.�B
.�B
/ B
/B
0!B
1B
1B
1B
1'B
1�B
1�B
1�B
2B
2B
1�B
2B
2B
3B
2�B
3B
3B
4B
4B
4B
4B
5B
5%B
5%B
5?B
6FB
6�B
72B
7B
7LB
72B
7fB
88B
8B
8B
88B
8B
8RB
9>B
9XB
9>B
:^B
:DB
;0B
;0B
;JB
;JB
;JB
;JB
;JB
<PB
<6B
;0B
<PB
<PB
=<B
>]B
>]B
?HB
?.B
?cB
>]B
?cB
?HB
?cB
?HB
?cB
?cB
@OB
@�B
AoB
@OB
@OB
@iB
@�B
@�B
AoB
AUB
AUB
AUB
A;B
AoB
B[B
BuB
B[B
B�B
CaB
C{B
C{B
C{B
D�B
D�B
EmB
D�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
GzB
GzB
G�B
G�B
G�B
G�B
G�B
I�B
I�B
I�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
Y�B
Y�B
ZB
Y�B
Y�B
Z�B
\�B
\�B
]B
\�B
\�B
\�B
\�B
\�B
]B
]B
]B
]B
[�B
^B
^B
^B
^5B
_;B
^B
^B
^B
]�B
_B
_B
_!B
_!B
_!B
_!B
_;B
_!B
`'B
`'B
`B
`B
`B
a-B
a-B
bB
a�B
aB
a-B
a-B
aB
a-B
a-B
b4B
b4B
bB
b4B
b4B
c:B
c:B
cB
c B
c:B
c B
c:B
dB
d@B
d@B
d@B
d@B
d@B
d&B
eFB
e,B
eFB
eFB
ffB
f2B
fLB
gmB
g8B
gmB
hXB
hXB
h>B
h>B
i_B
i_B
i_B
i_B
jKB
jKB
jKB
j0B
jKB
jeB
jeB
jKB
kkB
kkB
kQB
kkB
lqB
lqB
lqB
mwB
m]B
mCB
mwB
mCB
m]B
ncB
ncB
ncB
n}B
n}B
n}B
nIB
oiB
o�B
o�B
o�B
o�B
o�B
p�B
p�B
poB
qvB
qvB
qvB
q[B
qvB
q�B
q�B
q�B
r|B
r�B
r|B
r�B
raB
r|B
r�B
r�B
r|B
r|B
r|1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.46(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202102120034272021021200342720210212003427202306231727342023062317273420230623172734202102130021582021021300215820210213002158  JA  ARFMdecpA19c                                                                20210207063949  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210206213951  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210206213952  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210206213953  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210206213953  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210206213953  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210206213954  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210206213954  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210206213954  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210206213954                      G�O�G�O�G�O�                JA  ARUP                                                                        20210206215204                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20210207153152  CV  JULD            G�O�G�O�F��0                JM  ARGQJMQC2.0                                                                 20210207153152  CV  JULD_LOCATION   G�O�G�O�F��\                JM  ARGQJMQC2.0                                                                 20210207153152  CV  LATITUDE        G�O�G�O�A�M�                JM  ARGQJMQC2.0                                                                 20210207153152  CV  LONGITUDE       G�O�G�O���%                JM  ARCAJMQC2.0                                                                 20210211153427  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210211153427  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2019V1                                                       20210212152158  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082734  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705041505                      G�O�G�O�G�O�                