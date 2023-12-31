CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-01-18T18:38:35Z creation;2020-01-18T18:38:40Z conversion to V3.1;2023-06-29T05:50:13Z update;     
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
resolution        =���   axis      Z        X  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I\   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  `d   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ۼ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200118183835  20230705031507  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_204                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @��&Y�T 1   @��'-�� @7`��
=q�b�Ov_خ1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~y�D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D���D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D���D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@ӅA	A+\)AK\)Ak\)A��A��A��A��AŮAծA�A��B�
B
�
B�
B�
B"�
B*�
B2�
B:�
BB�
BJ�
BR�
B[=qBb�
Bj�
Br�
Bz�
B�k�B�k�B�k�B�k�B�k�B�k�B�8RB�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�8RB�k�B�k�B�k�B�k�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�D -qD �qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD	-qD	�qD
-qD
�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD -qD �qD!-qD!�qD"-qD"�qD#-qD#�qD$-qD$�qD%-qD%�qD&-qD&�qD'-qD'�qD(-qD(�qD)-qD)�qD*-qD*�qD+-qD+�qD,-qD,�qD--qD-�qD.-qD.�qD/-qD/�qD0-qD0�qD1-qD1�qD2-qD2�qD3-qD3�qD4-qD4�qD5-qD5�qD6-qD6�qD7-qD7�qD8-qD8�qD9-qD9�qD:-qD:�qD;-qD;�qD<-qD<�qD=-qD=�qD>-qD>�qD?-qD?�qD@-qD@�qDA-qDA�qDB-qDB�qDC-qDC�qDD-qDD�qDE-qDE�qDF-qDF�qDG-qDG�qDH-qDH�qDI-qDI�qDJ-qDJ�qDK-qDK�qDL-qDL�qDM-qDM�qDN-qDN�qDO-qDO�qDP-qDP�
DQ-qDQ�qDR-qDR�qDS-qDS�qDT-qDT�qDU-qDU�qDV-qDV�qDW-qDW��DX-qDX�qDY-qDY�qDZ-qDZ�qD[-qD[�qD\-qD\�qD]-qD]�qD^-qD^�qD_-qD_�qD`-qD`�qDa-qDa�qDb-qDb�qDc-qDc�qDd-qDd�qDe-qDe�qDf-qDf�qDg-qDg�qDh-qDh�qDi-qDi�qDj-qDj�qDk-qDk�qDl-qDl�qDm-qDm�qDn-qDn�qDo-qDo�qDp-qDp�qDq-qDq�qDr-qDr�qDs-qDs�qDt-qDt�qDu-qDu�qDv-qDv�qDw-qDw�qDx-qDx�qDy-qDy�qDz-qDz�qD{-qD{�qD|-qD|�qD}-qD}�qD~-qD~�
D-qD�qD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�S�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D�D�ָD��D�V�DÖ�D�ָD��D�V�DĖ�D�ָD��D�V�DŖ�D�ָD��D�V�DƖ�D�ָD��D�V�Dǖ�D�ָD��D�V�DȖ�D�ָD��D�V�Dɖ�D�ָD��D�V�Dʖ�D�ָD��D�V�D˖�D�ָD��D�V�D̖�D�ָD��D�V�D͖�D�ָD��D�V�DΖ�D�ָD��D�V�Dϖ�D�ָD��D�V�DЖ�D�ָD��D�V�Dі�D�ָD��D�V�DҖ�D�ָD��D�V�DӖ�D�ָD��D�V�DԖ�D�ָD��D�S�DՖ�D�ָD��D�V�D֖�D�ָD��D�V�Dז�D�ָD��D�V�Dؖ�D�ָD��D�V�Dٖ�D�ָD��D�V�Dږ�D�ָD��D�V�Dۖ�D�ָD��D�V�Dܖ�D�ָD��D�V�Dݖ�D�ָD��D�V�Dޖ�D�ָD��D�V�Dߖ�D�ָD��D�V�D���D�ָD��D�V�DᖸD�ָD��D�V�D▸D�ָD��D�V�D㖸D�ָD��D�V�D䖸D�ָD��D�V�D喸D�ָD��D�V�D斸D�ָD��D�V�D疸D�ָD��D�V�D薸D�ָD��D�V�D閸D�ָD��D�V�DꖸD�ָD��D�V�D떸D�ָD��D�V�D언D�ָD��D�V�D햸D�ָD��D�V�DD�ָD��D�V�DD�ָD��D�V�D�D�ָD��D�V�D�D���D��D�V�D�D�ָD��D�V�D�D�ָD��D�V�D���D�ָD��D�V�D���D�ָ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�?}A�I�A�G�A�O�A�VA�S�A�VA�VA�VA�XA�bNA�dZA�dZA�ffA�ffA�hsA�hsA�bNA�ffA�hsA�hsA�dZA�bNA�bNA�`BA�hsA�jA�jA�v�AA+AA�A§�A°!A´9A�ĜA�ĜA�ƨA�A�A�A�A�A�ĜA�ĜA�ƨA�ȴA���A���A���A���A���A���A���A���A�A���A�{A��`A�ȴA��A��A���A�VA���A�7LA���A�I�A�=qA�K�A�l�A��mA�M�A��yA���A�?}A��DA���A��TA�=qA��-A�|�A��HA�ffA���A��
A��A�l�A��jA��hA��+A�ȴA�
=A�&�A�9XA�hsA�+A��!A��DA��#A���A�v�A�v�A���A�Q�A���A�=qA�v�A��hA���A���A��A��9A��`A�jA�VA���A���A~��A|~�A{�Ax��Av��Av�9Ar��Ao��Ao�Ao�An=qAk�mAj-Ag�;AdffAaƨAa�A_�mA^��A\��A[VAZ(�AYXAVQ�AR~�AP��AP�AN�jAL�yAJ�AH�AFJADȴAC��AAl�A?��A=�;A<��A;��A;C�A:jA85?A6�RA6bA5`BA4��A4��A3hsA1dZA0�!A/��A.�A.E�A-oA+�A+�A*(�A)"�A(��A'�wA'%A&bNA%x�A$z�A#��A"��A"9XA!ƨA ��AƨA
=AbNA  At�A��A��AE�A�AoA�A��A�A33A^5A�mAoA�+A5?AG�AZA�
AoA-A�#A|�A��A  A\)AȴA��A
=A
�A	XA�9AjAhsA�\A��A��A�#@�S�@��`@�ƨ@��w@�A�@���@��#@��#@��/@�dZ@���@�%@�=q@���@@�7@�t�@ꟾ@�5?@�@�
=@���@�C�@���@߅@�;d@��@�?}@� �@ڇ+@؛�@�ff@�j@��@ӍP@ҏ\@�hs@У�@�b@��@Ο�@�J@�X@̓u@˕�@ʟ�@�t�@�j@�|�@�M�@ɑh@�Ĝ@ȃ@�b@��;@�33@��@��@���@�5?@���@���@�"�@��R@�M�@���@��R@���@��w@�t�@��y@�v�@�J@�/@��@�O�@���@��m@��\@�?}@�r�@�  @�1@�9X@�ƨ@�O�@��u@�b@��;@���@��F@�1@��
@�~�@�J@�{@��@�5?@�/@�1@�;d@�~�@��@��@��@�I�@��P@�I�@�x�@��@�`B@���@�5?@��T@�O�@��@���@��@��h@���@�?}@�hs@�p�@��@���@�Z@���@��#@�?}@�7L@�7L@�p�@�X@��`@�hs@�{@�+@�p�@�\)@�1'@���@��^@���@�bN@�7L@���@�Q�@��@���@��@���@���@�C�@���@�33@���@��@�  @���@�dZ@���@�v�@�$�@�{@��@��T@�`B@�&�@�Z@�A�@��@��@��y@��^@�J@�M�@�^5@��@�@��-@�@��h@���@���@�S�@���@�ff@���@�V@��@�(�@�S�@�"�@�ff@��T@��^@�hs@�O�@���@��-@��D@���@�dZ@��@���@���@�Ĝ@�O�@���@��/@�%@�bN@�1'@�1@�1'@���@�@�&�@��@�|�@�S�@��@��@�J@�~�@��y@���@�^5@�@���@�x�@���@���@�bN@� �@��@��;@���@�|�@�\)@�33@�@��y@��!@�V@�J@���@���@���@��h@�`B@�7L@��@��@���@��@��D@�r�@�9X@�b@�1@���@���@�|�@�S�@�33@��@���@���@��!@��+@�n�@�M�@�J@��T@���@�@��h@�p�@�7L@�V@���@��`@���@���@�r�@�Z@�I�@� �@�  @��@+@~�@~v�@~@}p�@}`B@}/@|��@|z�@{�F@{S�@z��@z~�@y��@y��@yhs@x��@x��@x�u@x  @w�@wK�@w�@v��@v�@v��@vV@u�T@u�-@up�@t�/@t�@tZ@t9X@sƨ@sdZ@r��@rn�@r^5@rJ@q��@qG�@q7L@q�@q%@p�`@p��@pQ�@o��@o+@o�@n�y@nv�@m�T@m`B@l��@lZ@l9X@l9X@l�@k�m@kdZ@ko@j~�@i��@h�`@h�@hb@g�@g|�@g\)@f�y@f�R@f��@fff@e��@e`B@e�@d��@dj@c�F@c�@cS�@c33@c33@c@b�!@bM�@bM�@bJ@a�#@a�@`bN@_�w@_�P@^�@^ff@^$�@]��@]?}@]V@\Z@\9X@[t�@["�@Z��@Zn�@Y��@Yhs@Y�@X��@X1'@W�@W�w@W��@W+@V��@V�+@Vv�@V$�@U��@U`B@U?}@U?}@T��@T�D@T9X@S�
@SdZ@So@R�H@R��@R�\@R~�@R-@Q�^@Q��@Q��@Q�@P��@PĜ@Pr�@Pb@O�@O��@O�P@O��@Ol�@O
=@Nȴ@N�R@N��@N�+@N$�@M�-@M`B@MV@L�@L�/@L��@L�@LI�@K�m@K��@KdZ@K"�@J��@J^5@JJ@I�7@Ihs@IX@I7L@HĜ@H�@H��@Hb@Gl�@G
=@Fȴ@FV@F@E@E�h@E/@D�D@D1@D1@D1@Cƨ@C��@CdZ@C33@B�\@B=q@A�^@Ax�@AX@A�@@Ĝ@@�@@bN@@A�@@ �@@  @?�w@?��@?l�@?;d@?
=@>�@>��@>5?@=�@=@=��@=p�@=V@<��@<�D@<Z@;�
@;��@;"�@:�!@:�\@:M�@:J@9�#@9�7@9X@9G�@97L@8��@8r�@8Q�@8b@7�@7�;@7�;@7��@7+@6ȴ@6E�@6{@5�@5��@5`B@5V@4�j@4��@4Z@4�@3��@333@3"�@2�@2�!@2n�@2�@1�#@1��@1�7@1X@17L@1&�@0��@0�9@0�u@0r�@0A�@0 �@0  @/�;@/�w@/�w@/�@/|�@/l�@/+@/
=@.��@.E�@.@-@-O�@,�j@,j@,9X@+�
@+t�@+@*�!@*~�@*n�@*=q@*-@*�@)�@)��@)�7@)hs@)7L@)%@(�`@(Ĝ@(��@(�u@(bN@(b@'�;@'��@'�@'l�@';d@'
=@&ȴ@&v�@&ff@&$�@%�@%@%`B@$��@$��@$�@$��@$9X@$1@#ƨ@#�@#C�@#"�@"��@"�\@"n�@"=q@"�@!��@!��@!G�@ �`@ �9@ �@ bN@ A�@ b@�;@��@��@�w@��@K�@��@�+@v�@v�@V@{@��@��@p�@O�@?}@?}@�@V@��@�/@�j@��@9X@1@��@�
@��@t�@S�@"�@�@�\@M�@�@�#@��@X@��@Ĝ@bN@b@�;@�w@|�@;d@��@�@ȴ@v�@E�@{@�@��@��@�@O�@�@��@��@�@z�@I�@��@ƨ@ƨ@��@t�@C�@o@�H@�!@�\@~�@^5@=q@�@J@�@��@G�@%@��@Ĝ@�u@bN@Q�@b@��@�P@|�@l�@+@�@
=@�y@�@��@ff@{@�@�T@��@@��@�@`B@?}@/@V@�/@�j@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�?}A�I�A�G�A�O�A�VA�S�A�VA�VA�VA�XA�bNA�dZA�dZA�ffA�ffA�hsA�hsA�bNA�ffA�hsA�hsA�dZA�bNA�bNA�`BA�hsA�jA�jA�v�AA+AA�A§�A°!A´9A�ĜA�ĜA�ƨA�A�A�A�A�A�ĜA�ĜA�ƨA�ȴA���A���A���A���A���A���A���A���A�A���A�{A��`A�ȴA��A��A���A�VA���A�7LA���A�I�A�=qA�K�A�l�A��mA�M�A��yA���A�?}A��DA���A��TA�=qA��-A�|�A��HA�ffA���A��
A��A�l�A��jA��hA��+A�ȴA�
=A�&�A�9XA�hsA�+A��!A��DA��#A���A�v�A�v�A���A�Q�A���A�=qA�v�A��hA���A���A��A��9A��`A�jA�VA���A���A~��A|~�A{�Ax��Av��Av�9Ar��Ao��Ao�Ao�An=qAk�mAj-Ag�;AdffAaƨAa�A_�mA^��A\��A[VAZ(�AYXAVQ�AR~�AP��AP�AN�jAL�yAJ�AH�AFJADȴAC��AAl�A?��A=�;A<��A;��A;C�A:jA85?A6�RA6bA5`BA4��A4��A3hsA1dZA0�!A/��A.�A.E�A-oA+�A+�A*(�A)"�A(��A'�wA'%A&bNA%x�A$z�A#��A"��A"9XA!ƨA ��AƨA
=AbNA  At�A��A��AE�A�AoA�A��A�A33A^5A�mAoA�+A5?AG�AZA�
AoA-A�#A|�A��A  A\)AȴA��A
=A
�A	XA�9AjAhsA�\A��A��A�#@�S�@��`@�ƨ@��w@�A�@���@��#@��#@��/@�dZ@���@�%@�=q@���@@�7@�t�@ꟾ@�5?@�@�
=@���@�C�@���@߅@�;d@��@�?}@� �@ڇ+@؛�@�ff@�j@��@ӍP@ҏ\@�hs@У�@�b@��@Ο�@�J@�X@̓u@˕�@ʟ�@�t�@�j@�|�@�M�@ɑh@�Ĝ@ȃ@�b@��;@�33@��@��@���@�5?@���@���@�"�@��R@�M�@���@��R@���@��w@�t�@��y@�v�@�J@�/@��@�O�@���@��m@��\@�?}@�r�@�  @�1@�9X@�ƨ@�O�@��u@�b@��;@���@��F@�1@��
@�~�@�J@�{@��@�5?@�/@�1@�;d@�~�@��@��@��@�I�@��P@�I�@�x�@��@�`B@���@�5?@��T@�O�@��@���@��@��h@���@�?}@�hs@�p�@��@���@�Z@���@��#@�?}@�7L@�7L@�p�@�X@��`@�hs@�{@�+@�p�@�\)@�1'@���@��^@���@�bN@�7L@���@�Q�@��@���@��@���@���@�C�@���@�33@���@��@�  @���@�dZ@���@�v�@�$�@�{@��@��T@�`B@�&�@�Z@�A�@��@��@��y@��^@�J@�M�@�^5@��@�@��-@�@��h@���@���@�S�@���@�ff@���@�V@��@�(�@�S�@�"�@�ff@��T@��^@�hs@�O�@���@��-@��D@���@�dZ@��@���@���@�Ĝ@�O�@���@��/@�%@�bN@�1'@�1@�1'@���@�@�&�@��@�|�@�S�@��@��@�J@�~�@��y@���@�^5@�@���@�x�@���@���@�bN@� �@��@��;@���@�|�@�\)@�33@�@��y@��!@�V@�J@���@���@���@��h@�`B@�7L@��@��@���@��@��D@�r�@�9X@�b@�1@���@���@�|�@�S�@�33@��@���@���@��!@��+@�n�@�M�@�J@��T@���@�@��h@�p�@�7L@�V@���@��`@���@���@�r�@�Z@�I�@� �@�  @��@+@~�@~v�@~@}p�@}`B@}/@|��@|z�@{�F@{S�@z��@z~�@y��@y��@yhs@x��@x��@x�u@x  @w�@wK�@w�@v��@v�@v��@vV@u�T@u�-@up�@t�/@t�@tZ@t9X@sƨ@sdZ@r��@rn�@r^5@rJ@q��@qG�@q7L@q�@q%@p�`@p��@pQ�@o��@o+@o�@n�y@nv�@m�T@m`B@l��@lZ@l9X@l9X@l�@k�m@kdZ@ko@j~�@i��@h�`@h�@hb@g�@g|�@g\)@f�y@f�R@f��@fff@e��@e`B@e�@d��@dj@c�F@c�@cS�@c33@c33@c@b�!@bM�@bM�@bJ@a�#@a�@`bN@_�w@_�P@^�@^ff@^$�@]��@]?}@]V@\Z@\9X@[t�@["�@Z��@Zn�@Y��@Yhs@Y�@X��@X1'@W�@W�w@W��@W+@V��@V�+@Vv�@V$�@U��@U`B@U?}@U?}@T��@T�D@T9X@S�
@SdZ@So@R�H@R��@R�\@R~�@R-@Q�^@Q��@Q��@Q�@P��@PĜ@Pr�@Pb@O�@O��@O�P@O��@Ol�@O
=@Nȴ@N�R@N��@N�+@N$�@M�-@M`B@MV@L�@L�/@L��@L�@LI�@K�m@K��@KdZ@K"�@J��@J^5@JJ@I�7@Ihs@IX@I7L@HĜ@H�@H��@Hb@Gl�@G
=@Fȴ@FV@F@E@E�h@E/@D�D@D1@D1@D1@Cƨ@C��@CdZ@C33@B�\@B=q@A�^@Ax�@AX@A�@@Ĝ@@�@@bN@@A�@@ �@@  @?�w@?��@?l�@?;d@?
=@>�@>��@>5?@=�@=@=��@=p�@=V@<��@<�D@<Z@;�
@;��@;"�@:�!@:�\@:M�@:J@9�#@9�7@9X@9G�@97L@8��@8r�@8Q�@8b@7�@7�;@7�;@7��@7+@6ȴ@6E�@6{@5�@5��@5`B@5V@4�j@4��@4Z@4�@3��@333@3"�@2�@2�!@2n�@2�@1�#@1��@1�7@1X@17L@1&�@0��@0�9@0�u@0r�@0A�@0 �@0  @/�;@/�w@/�w@/�@/|�@/l�@/+@/
=@.��@.E�@.@-@-O�@,�j@,j@,9X@+�
@+t�@+@*�!@*~�@*n�@*=q@*-@*�@)�@)��@)�7@)hs@)7L@)%@(�`@(Ĝ@(��@(�u@(bN@(b@'�;@'��@'�@'l�@';d@'
=@&ȴ@&v�@&ff@&$�@%�@%@%`B@$��@$��@$�@$��@$9X@$1@#ƨ@#�@#C�@#"�@"��@"�\@"n�@"=q@"�@!��@!��@!G�@ �`@ �9@ �@ bN@ A�@ b@�;@��@��@�w@��@K�@��@�+@v�@v�@V@{@��@��@p�@O�@?}@?}@�@V@��@�/@�j@��@9X@1@��@�
@��@t�@S�@"�@�@�\@M�@�@�#@��@X@��@Ĝ@bN@b@�;@�w@|�@;d@��@�@ȴ@v�@E�@{@�@��@��@�@O�@�@��@��@�@z�@I�@��@ƨ@ƨ@��@t�@C�@o@�H@�!@�\@~�@^5@=q@�@J@�@��@G�@%@��@Ĝ@�u@bN@Q�@b@��@�P@|�@l�@+@�@
=@�y@�@��@ff@{@�@�T@��@@��@�@`B@?}@/@V@�/@�j@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�HB
�NB
�ZB
�ZB
�ZB
�`B
�ZB
�ZB
�ZB
�TB
�ZB
�ZB
�TB
�NB
�NB
�NB
�NB
�TB
�TB
�TB
�fB
�yB
�B
�sB
�B
�B
��B
��B
��B  BB%B1B	7B
=BDBJBJBPBPBPBPBPBPBPBVBVBVBPBoB9XBXBm�B�+B��B��B�TB�fB%B�B'�B0!B6FB:^B-B%�B,B'�B)�B>wB@�B8RB/B)�B&�B#�B"�B$�B�B+B��B��BPBhBbBJBB��B��B�B�B�B�sB�/B�!B�\Bu�BXB!�B
�B
�;B
�mB
ÖB
�XB
��B
��B
�B
x�B
ffB
H�B
<jB
2-B
�B
uB	��B	�NB	�fB	�B	��B	�^B	�XB	�?B	��B	��B	�=B	p�B	_;B	ZB	Q�B	I�B	>wB	33B	+B	#�B	�B	  B�B�B�sB�5B��BǮB�jB�FB�3B�B��B��B��B��B��B��B��B�uB��B��B�{B�uB�oB�VB�JB�=B�=B�+B�1B�B�B� B�B� B�B}�B~�B}�B}�B{�B|�Bz�B{�Bz�Bx�Bx�Bx�Bw�Bv�Bw�Bv�Bw�Bu�Bu�Bu�Bu�Bt�Bt�Bu�Bs�Bu�Bu�Bv�Bx�Bz�By�Bz�Bz�Bz�Bz�B{�Bz�Bz�By�B{�Bx�Bw�Bv�Bv�Bt�Bs�Bp�Bl�B`BBK�B;dB7LB8RB>wBC�BL�BJ�BJ�BI�BH�BF�BF�BF�BG�BG�BF�BE�BE�BE�BF�BE�BE�BF�BI�BJ�BK�BM�BQ�BS�BT�BT�BS�BVBVBVBW
BXBXBYBXBYBYBYBZB\)BaHBl�By�B{�B|�B� B� B�B�B�B�B�+B�1B�7B�1B�DB�VB�\B�bB�oB��B��B��B��B��B��B��B��B�B�B�!B�!B�B�B�B�3B�FB�XB�dB�^B�RB�RB�qB��BBȴB��B��B��B�
B�)B�5B��B��B��B��B�B�B�B�B�HB�ZB�B��B��B��B	B	
=B	VB	VB	oB	�B	$�B	(�B	&�B	+B	2-B	6FB	.B	+B	(�B	'�B	%�B	#�B	#�B	$�B	(�B	)�B	)�B	0!B	5?B	>wB	Q�B	`BB	ffB	e`B	bNB	^5B	cTB	o�B	s�B	r�B	r�B	s�B	s�B	s�B	t�B	t�B	u�B	{�B	� B	�B	�B	�B	�7B	�JB	�VB	�bB	�hB	��B	��B	��B	��B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�B	�B	�B	�'B	�XB	�^B	�jB	��B	ǮB	ĜB	ȴB	ƨB	ƨB	ȴB	ɺB	ǮB	ÖB	ÖB	ÖB	ŢB	ǮB	ȴB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�TB	�TB	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
+B
+B
+B
1B
	7B
	7B
	7B

=B

=B
DB
JB
PB
PB
PB
PB
VB
VB
\B
\B
bB
hB
oB
oB
uB
uB
uB
uB
{B
{B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
(�B
)�B
)�B
)�B
)�B
)�B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
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
2-B
33B
33B
33B
33B
33B
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
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
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
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
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
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
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
T�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
XB
XB
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
[#B
[#B
[#B
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
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
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
bNB
bNB
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
dZB
e`B
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
gmB
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
iyB
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
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
�B
�B
��B
�B
��B
�B
�B
��B
�&B
�&B
�&B
�,B
�&B
�&B
�&B
� B
�&B
�&B
�B
�B
�B
�B
�B
�B
� B
��B
�B
�DB
�6B
�$B
�OB
�aB
�zB
��B
��B
��B�B�B�B	B
	BBBBBBBBBBB<B�B�B�B=B?HB[�Bo�B��B��B��B�`B�B+B�B)�B2B8RB<�B.B'B-�B)_B+�B?�BBB9�B0B*B($B$�B$ZB'B;B	7B��B�%B�B�B�B"B�B �B��B�|B�CB�B�QB�-B�|B��By�B]�B&LB
��B
�B
�B
�YB
�B
��B
�+B
��B
|6B
j�B
K^B
?.B
4TB
OB
9B	��B	��B	�0B	ڠB	�B	��B	��B	��B	�B	��B	��B	sB	`'B	[qB	SuB	K�B	@4B	4TB	,�B	'mB	1B	�B�B�UB�B��B��B��B��B��B��B�B��B��B��B�]B��B��B��B�,B�$B��B�B��B�aB�B�B�DB�B��B�RB�B�B�B��B��B��B~�B� B~�B~�B|�B}�B{dB}B{�By�ByrBy>BxlBw�By$BxBxlBv+Bv+BvFBv+Bu%Bu�Bv+Bt�BvFBvFBw�By�B{dBz�B{�B{0B{JB{�B|�B{�B{�Bz�B|�By�Bx�BwfBw2Bu�Bt�Bq�Bn�BcTBN"B<�B7�B8B>BBD3BMBJ�BKDBJ�BI�BG+BHBGEBHfBH�BG�BE�BE�BFtBG�BF�BF�BG�BJ=BJ�BK�BN�BR�BT�BVBV9BT�BVBV9BV�BWsBX_BX+BYKBXEBY1BYKBYeBZkB\)B`vBlBz*B|PB}"B�4B�B��B�'B�aB��B��B�B��B��B�xB�pB�BB�HB� B��B��B�4B��B��B��B��B�*B��B��B�;B��B��B��B�]B�3B��B�$B��B�dB��B�RB�<B�UB�[B�KB��B�[B�B�
B��BߤBԕB�TB�B�&B�B�B�1B�B�-B�B�B��B�jB�wB	�B	
=B	<B	"B	�B	=B	$�B	(�B	&fB	*�B	2-B	7�B	.�B	+�B	)DB	(�B	%�B	#�B	#nB	$�B	(�B	)�B	)DB	/B	4B	<�B	P�B	_�B	f�B	fLB	b�B	^B	b�B	o�B	s�B	r�B	raB	shB	shB	shB	t�B	tnB	u?B	{JB	}B	��B	��B	��B	�7B	�B	�<B	�.B	�B	�gB	�sB	�sB	��B	�@B	��B	��B	�qB	��B	�B	�B	�QB	�qB	��B	�vB	��B	��B	�$B	�B	��B	��B	��B	�vB	��B	�8B	�yB	�0B	��B	�B	��B	��B	��B	��B	��B	��B	�oB	�/B	��B	�6B	�UB	�	B	��B	��B	� B	��B	�MB	ȴB	ƎB	ƎB	ȀB	�XB	��B	��B	��B	ðB	ňB	ǮB	ȴB	�YB	�%B	�7B	бB	��B	��B	οB	ΥB	��B	��B	ϫB	��B	��B	ңB	��B	��B	ԯB	ԯB	��B	ԯB	��B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�-B	��B	��B	��B	� B	�:B	�,B	�B	�2B	�2B	�B	�8B	�8B	�>B	�>B	�DB	�0B	�KB	�KB	�KB	�6B	�QB	�WB	�CB	�CB	�CB	�}B	�iB	�OB	�OB	�UB	�oB	�vB	�|B	�B	�B	�B	�B	��B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B

#B

#B
B
B
B
B
�B
B
B
"B
(B
\B
HB
NB
TB
:B
&B
@B
@B
[B
FB
FB
,B
aB
MB
MB
MB
mB
SB
SB
?B
?B
$B
_B
_B
EB
KB
eB
B
�B
�B
�B
�B
qB
xB
xB
xB
dB
dB
~B
~B
�B
pB
pB
�B
�B
�B
 �B
!�B
!�B
!|B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
(�B
)�B
)�B
)�B
)�B
)�B
+�B
+�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
.�B
/�B
.�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
1B
0�B
1�B
2�B
3B
2�B
2�B
2�B
2�B
2�B
2�B
4B
4B
5B
4�B
4�B
4�B
5�B
5�B
5�B
6B
7B
7B
8B
8B
8B
8B
9$B
9	B
9$B
:B
:*B
:*B
;B
;0B
;0B
;0B
;0B
<B
<B
<B
<B
<B
<B
<6B
="B
="B
=<B
=VB
>(B
>BB
>BB
?.B
?.B
?.B
@4B
@4B
@4B
@4B
@4B
AUB
AUB
A;B
A;B
B[B
B'B
B[B
BAB
B[B
B[B
B[B
CGB
CGB
CGB
CaB
CaB
DgB
DMB
DgB
DMB
E�B
EmB
EmB
ESB
FYB
FtB
FtB
FYB
GzB
GzB
GzB
H�B
H�B
HfB
HfB
HfB
HfB
IlB
IlB
I�B
I�B
IlB
IRB
IlB
I�B
I�B
J�B
J�B
J�B
J�B
JrB
K�B
K�B
K�B
KxB
L~B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
NpB
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
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
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
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
Z�B
Z�B
Z�B
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
^B
]�B
^B
_B
_B
_B
^�B
_�B
_�B
_�B
aB
aB
`�B
aB
aB
`�B
aB
a�B
bB
bB
bB
a�B
a�B
a�B
c B
c B
c B
c B
cB
cB
dB
dB
d&B
dB
d&B
dB
dB
dB
eB
eB
eB
eB
eB
eB
e,B
eB
f2B
f2B
fB
f2B
g8B
g8B
gB
gB
gB
gB
h>B
h$B
h
B
h>B
h$B
iB
i*B
i*B
iDB
i*B
i*B
i*B
jKB
j0B
j0B
jB
jKB
j0B
k6B
kQB
kB
kQB
k6B
k6B
k6B
k6B
k1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.71(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202001240037102020012400371020200124003710202306231720142023062317201420230623172014202001250031232020012500312320200125003123  JA  ARFMdecpA19c                                                                20200119033723  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200118183835  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200118183838  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200118183838  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200118183839  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200118183839  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200118183839  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200118183839  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200118183839  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200118183840                      G�O�G�O�G�O�                JA  ARUP                                                                        20200118185359                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20200119153646  CV  JULD            G�O�G�O�F��3                JM  ARCAJMQC2.0                                                                 20200123153710  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200123153710  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200124153123  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082014  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031507                      G�O�G�O�G�O�                