CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-10-25T12:37:11Z creation;2019-10-25T12:37:17Z conversion to V3.1;2023-06-29T05:50:51Z update;     
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
resolution        =���   axis      Z        x  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  M\   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p,   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �d   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20191025123711  20230705031506  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_184                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @���� 1   @������ @6�'�/�W�b���+1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D?��D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DV��DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dgy�Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ Dۼ�D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D�3D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@ָRA\)A+\)AK\)Ak\)A��A��A��A��AŮAծA�A��B=qB
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
BZ�
Bb�
Bj�
Bs=qB{=qB�k�B�k�B�k�B�k�B�8RB�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C ��C��C��C��C��C
�\C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�g�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�g�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�D -qD �qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD	-qD	�qD
-qD
�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD -qD �qD!-qD!�qD"-qD"�qD#-qD#�qD$-qD$�qD%-qD%�qD&-qD&�qD'-qD'�qD(-qD(�qD)-qD)�qD*-qD*�qD+-qD+�qD,-qD,�qD--qD-�qD.-qD.�qD/-qD/�qD0-qD0�qD1-qD1�qD2-qD2�qD3-qD3�qD4-qD4�qD5-qD5�qD6-qD6�qD7-qD7�qD8-qD8�qD9-qD9�qD:-qD:�qD;-qD;�qD<-qD<�qD=-qD=�qD>-qD>�qD?-qD?�qD@'
D@�qDA-qDA�qDB-qDB�qDC-qDC�qDD-qDD�qDE-qDE�qDF-qDF�qDG-qDG�qDH-qDH�qDI-qDI�qDJ-qDJ�qDK-qDK�qDL-qDL�qDM-qDM�qDN-qDN�qDO-qDO�qDP-qDP�qDQ-qDQ�qDR-qDR�qDS-qDS�qDT-qDT�qDU-qDU�qDV-qDV�qDW'
DW�qDX-qDX�qDY-qDY�qDZ-qDZ�qD[-qD[�qD\-qD\�qD]-qD]�qD^-qD^�qD_-qD_�qD`-qD`�qDa-qDa�qDb-qDb�qDc-qDc�qDd-qDd�qDe-qDe�qDf-qDf�qDg'
Dg�
Dh-qDh�qDi-qDi�qDj-qDj�qDk3�Dk�qDl-qDl�qDm-qDm�qDn-qDn�qDo-qDo�qDp-qDp�qDq-qDq�qDr-qDr�qDs-qDs�qDt-qDt�qDu-qDu�qDv-qDv�qDw-qDw�qDx-qDx�qDy-qDy�qDz-qDz�qD{-qD{�qD|-qD|�qD}-qD}�qD~-qD~�qD-qD�qD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ӅD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D���D��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D�D�ָD��D�V�DÖ�D�ָD��D�V�DĖ�D�ָD��D�V�DŖ�D�ָD��D�V�DƖ�D�ָD��D�V�Dǖ�D�ָD��D�V�DȖ�D�ָD��D�V�Dɖ�D�ָD��D�V�Dʖ�D�ָD��D�V�D˖�D�ָD��D�V�D̖�D�ָD��D�V�D͖�D�ָD��D�V�DΖ�D�ָD��D�V�Dϖ�D�ָD��D�V�DЖ�D�ָD��D�V�Dі�D�ָD��D�V�DҖ�D�ָD��D�V�DӖ�D�ָD��D�V�DԖ�D�ָD��D�V�DՖ�D�ָD��D�V�D֖�D�ָD��D�V�Dז�D�ָD��D�V�Dؖ�D�ָD��D�V�Dٖ�D�ָD��D�V�Dږ�D�ָD��D�V�Dۖ�D�ӅD��D�V�Dܖ�D�ָD��D�V�Dݖ�D�ָD��D�V�Dޖ�D�ָD��D�V�Dߖ�D�ָD��D�V�D���D�ָD��D�V�D��D�ָD��D�V�DⓅD�ָD��D�V�D㖸D�ָD��D�V�D䖸D�ָD��D�V�D喸D�ָD��D�V�D斸D�ָD��D�V�D疸D�ָD��D�V�D薸D�ָD��D�V�D閸D�ָD��D�V�DꖸD�ָD��D�V�D떸D�ָD��D�V�D언D�ָD��D�V�D햸D�ָD��D�V�DD�ָD��D�V�DD�ָD��D�V�D�D�ָD��D�V�D�D�ָD��D�V�D�D�ָD��D�V�D�D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�Y�D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aҡ�Aҥ�Aҟ�AҰ!AҺ^AҼjAҼjA�A�ȴA�ȴA�ƨA�ƨA�ȴA�ƨA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A���A���A���A���AƇ+A��A�=qA²-A��FA��mA���A���A�l�A��A�/A��A��7A��!A���A��A���A��A��yA��A�?}A���A��A�dZA�^5A���A�VA���A�1A���A��A��#A�oA���A���A� �A���A�  A��+A�E�A��;A�A�A���A�  A�?}A��-A��^A�  A���A�?}A��HA��PA��A�1A�|�A�ȴA��A�A�M�A�l�A��A��A�O�A�A��FA�Q�A��A�hsA�ƨA��jA���A��/A��^A��wA��A��DA��uA�;dA�+A��A�E�A�A�TA~��A{�PAx�yAw�
Avz�At(�ArE�Ao�AmdZAk��Ai?}Ae��AcK�A`=qA]\)AZ~�AWAU�AS&�AO�#AM�AMK�AK��AI�
AGK�AEx�AB�HAAp�A@1A?�PA>��A>{A=?}A<��A<bNA;t�A:ZA9G�A7��A4z�A3�TA4A2jA/��A.�RA,��A*�9A)�A'%A&  A%�#A%�A%\)A$^5A"n�A �AhsA��A�A�PAbNA�hA�-A��AffA��A�A�9Ar�AJA`BA��A-A�RA1'A�PAM�A\)A��A�9Ar�A|�A
9XA	�A	�7A��AffA�A��A��Ap�A�A�;A��A��A1Ap�A �A �@��w@���@�E�@��@���@�bN@�o@��h@��+@�hs@�X@���@�I�@�?}@�Q�@�@�@�^5@�|�@�(�@�$�@��@�7@蛦@�~�@���@�+@�7@���@�ȴ@�ȴ@�-@��@���@�D@�P@�33@�@�@�D@�+@�V@��m@�^5@�%@�  @�"�@ٺ^@ם�@�Q�@���@�@д9@���@�{@�x�@�/@��@�V@���@̴9@� �@�|�@���@ʇ+@�{@�%@�z�@��@ǍP@��@�v�@���@�?}@�9X@���@��@��@��@�S�@���@�Ĝ@�7L@�G�@��@��/@��@��@���@��@��u@�ƨ@��@��h@�z�@��@�K�@�33@��@�+@�~�@�p�@���@�j@�Q�@�I�@�9X@�1'@� �@�  @��w@�|�@�\)@�C�@��@��y@��@��+@�{@��@��^@�p�@�?}@�7L@��@��@��@�33@�~�@�$�@�@�O�@�b@���@�(�@�Q�@�j@��@�33@��7@���@�G�@�7L@��@�Ĝ@�b@��\@��-@���@���@�I�@�1@���@�5?@��@�z�@�Z@���@���@�|�@�;d@�"�@�ff@�&�@�&�@���@�r�@�z�@�A�@��;@�l�@�33@��@���@�M�@���@��7@�x�@�`B@�G�@�%@��u@�(�@�b@�  @��@�l�@�K�@��@��@��\@�$�@��@�{@�{@��@�{@��-@�p�@�&�@���@��@�r�@�Q�@�A�@�(�@���@��m@���@���@���@���@��P@�+@���@���@�ȴ@���@�^5@��#@�x�@�`B@�7L@���@��@�z�@�r�@�bN@�9X@�1@��@��@���@���@�M�@��T@�@���@�&�@��@��/@���@��u@�z�@�j@�A�@�  @��m@���@�\)@�C�@��@�@��@��R@�V@�{@��@���@��^@���@��h@��@�p�@�V@��@���@��@�z�@�j@�I�@��@��@��@��@��R@�~�@�ff@�{@��#@��h@�x�@�hs@�`B@�/@��@�V@�%@���@��9@��D@�bN@�(�@�@~��@~ff@}�@}?}@|��@|�@{S�@{@z��@z��@zM�@y�#@y7L@x��@x�u@xbN@x �@w�@w;d@vȴ@v��@v�+@u@u�-@u@u?}@t�/@t�@t�@t�@t�j@tI�@s��@so@r�@r��@r-@q�#@q7L@q�@pĜ@pr�@p1'@oK�@n�y@n��@nV@n$�@n{@m@m/@l��@l��@l�D@l(�@k��@k�F@kS�@k"�@j�H@j�!@jM�@i��@h�`@h�@hb@g�P@g+@f�@f��@fV@e�@e`B@e/@d�/@d(�@ct�@c33@c@b��@b��@bM�@b�@a�#@ahs@a�@`��@`r�@` �@_�@_�;@_�w@_�P@_|�@_;d@^��@^�@^�R@^�+@^5?@^@]@]�@]O�@]/@\��@\�D@\I�@[�m@[��@[�@[C�@[o@Z�!@Z~�@Z=q@Y�@Y�7@YG�@Y%@XbN@X1'@Xb@W�@W�@W|�@W+@V�R@V5?@U�h@U/@T�/@T�@T(�@T1@S��@S33@R�H@R�!@R~�@R^5@RJ@QG�@Q�@P�9@PbN@PA�@O��@O��@O\)@O+@O
=@N��@N�@Nff@N@M��@M�-@Mp�@MV@L��@Lz�@L9X@K��@K33@J�H@J��@JM�@I�^@Ihs@I�@I%@H�`@H��@H�@HQ�@H �@G�@G�P@G�@F�y@Fv�@F5?@F$�@F@E�@E��@E`B@E/@D��@DI�@D�@C��@Ct�@Co@B��@B^5@BJ@A�^@A��@Ahs@@�`@@�@@ �@?�;@?��@?\)@?
=@>ȴ@>�+@>5?@>$�@>@=��@=@=p�@<�/@<�@<�D@<z�@<I�@;�m@;��@;S�@;o@:~�@:-@:J@9�@9��@9�7@9G�@8��@8Ĝ@8�@8Q�@8 �@8  @7��@7�P@7K�@7
=@6�@6ȴ@6��@6v�@6@5�T@5��@5@5�-@5�@5O�@4��@4�j@49X@3�
@3ƨ@3ƨ@3��@3t�@3dZ@333@3@2�H@2�\@2M�@1��@1��@1x�@1%@0Ĝ@0�9@0��@0�u@0�@0A�@/�@/�@/l�@/;d@/�@.��@.��@.E�@.$�@.@-�T@-��@-�-@-�@-?}@-V@,�j@,��@,Z@+��@+ƨ@+t�@+S�@+"�@*�H@*��@*��@*~�@*^5@*M�@)��@)��@)�#@)G�@(��@(�`@(Ĝ@(�u@(r�@(Q�@(1'@(  @'�;@'��@'�w@'K�@&��@&�R@&��@&ff@%�@%@%�h@%`B@%�@$��@$�@$z�@$Z@$(�@$1@#t�@#C�@#@"��@"^5@!��@!��@!x�@!hs@!X@!%@ �`@ �u@ bN@�@�P@l�@+@
=@�y@ȴ@v�@@��@��@�h@O�@/@/@��@�@�m@S�@@�@��@�\@�@��@hs@G�@G�@7L@&�@�@��@��@�@bN@1'@b@�@�;@�w@|�@\)@;d@�@�y@��@v�@�@��@@��@�@p�@/@�/@�j@j@I�@9X@�@��@�
@��@t�@33@@��@��@~�@n�@n�@n�@n�@M�@��@�^@�7@x�@hs@G�@G�@7L@&�@��@�9@�u@�u@r�@bN@bN@A�@1'@ �@b@�@��@�@l�@
=@�@ȴ@��@$�@��@�@O�@�@�/@�j@��@j@(�@�@�@1@�@dZ@33@
�@
��@
��@
~�@
n�@
M�@
�@
J@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aҡ�Aҥ�Aҟ�AҰ!AҺ^AҼjAҼjA�A�ȴA�ȴA�ƨA�ƨA�ȴA�ƨA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A���A���A���A���AƇ+A��A�=qA²-A��FA��mA���A���A�l�A��A�/A��A��7A��!A���A��A���A��A��yA��A�?}A���A��A�dZA�^5A���A�VA���A�1A���A��A��#A�oA���A���A� �A���A�  A��+A�E�A��;A�A�A���A�  A�?}A��-A��^A�  A���A�?}A��HA��PA��A�1A�|�A�ȴA��A�A�M�A�l�A��A��A�O�A�A��FA�Q�A��A�hsA�ƨA��jA���A��/A��^A��wA��A��DA��uA�;dA�+A��A�E�A�A�TA~��A{�PAx�yAw�
Avz�At(�ArE�Ao�AmdZAk��Ai?}Ae��AcK�A`=qA]\)AZ~�AWAU�AS&�AO�#AM�AMK�AK��AI�
AGK�AEx�AB�HAAp�A@1A?�PA>��A>{A=?}A<��A<bNA;t�A:ZA9G�A7��A4z�A3�TA4A2jA/��A.�RA,��A*�9A)�A'%A&  A%�#A%�A%\)A$^5A"n�A �AhsA��A�A�PAbNA�hA�-A��AffA��A�A�9Ar�AJA`BA��A-A�RA1'A�PAM�A\)A��A�9Ar�A|�A
9XA	�A	�7A��AffA�A��A��Ap�A�A�;A��A��A1Ap�A �A �@��w@���@�E�@��@���@�bN@�o@��h@��+@�hs@�X@���@�I�@�?}@�Q�@�@�@�^5@�|�@�(�@�$�@��@�7@蛦@�~�@���@�+@�7@���@�ȴ@�ȴ@�-@��@���@�D@�P@�33@�@�@�D@�+@�V@��m@�^5@�%@�  @�"�@ٺ^@ם�@�Q�@���@�@д9@���@�{@�x�@�/@��@�V@���@̴9@� �@�|�@���@ʇ+@�{@�%@�z�@��@ǍP@��@�v�@���@�?}@�9X@���@��@��@��@�S�@���@�Ĝ@�7L@�G�@��@��/@��@��@���@��@��u@�ƨ@��@��h@�z�@��@�K�@�33@��@�+@�~�@�p�@���@�j@�Q�@�I�@�9X@�1'@� �@�  @��w@�|�@�\)@�C�@��@��y@��@��+@�{@��@��^@�p�@�?}@�7L@��@��@��@�33@�~�@�$�@�@�O�@�b@���@�(�@�Q�@�j@��@�33@��7@���@�G�@�7L@��@�Ĝ@�b@��\@��-@���@���@�I�@�1@���@�5?@��@�z�@�Z@���@���@�|�@�;d@�"�@�ff@�&�@�&�@���@�r�@�z�@�A�@��;@�l�@�33@��@���@�M�@���@��7@�x�@�`B@�G�@�%@��u@�(�@�b@�  @��@�l�@�K�@��@��@��\@�$�@��@�{@�{@��@�{@��-@�p�@�&�@���@��@�r�@�Q�@�A�@�(�@���@��m@���@���@���@���@��P@�+@���@���@�ȴ@���@�^5@��#@�x�@�`B@�7L@���@��@�z�@�r�@�bN@�9X@�1@��@��@���@���@�M�@��T@�@���@�&�@��@��/@���@��u@�z�@�j@�A�@�  @��m@���@�\)@�C�@��@�@��@��R@�V@�{@��@���@��^@���@��h@��@�p�@�V@��@���@��@�z�@�j@�I�@��@��@��@��@��R@�~�@�ff@�{@��#@��h@�x�@�hs@�`B@�/@��@�V@�%@���@��9@��D@�bN@�(�@�@~��@~ff@}�@}?}@|��@|�@{S�@{@z��@z��@zM�@y�#@y7L@x��@x�u@xbN@x �@w�@w;d@vȴ@v��@v�+@u@u�-@u@u?}@t�/@t�@t�@t�@t�j@tI�@s��@so@r�@r��@r-@q�#@q7L@q�@pĜ@pr�@p1'@oK�@n�y@n��@nV@n$�@n{@m@m/@l��@l��@l�D@l(�@k��@k�F@kS�@k"�@j�H@j�!@jM�@i��@h�`@h�@hb@g�P@g+@f�@f��@fV@e�@e`B@e/@d�/@d(�@ct�@c33@c@b��@b��@bM�@b�@a�#@ahs@a�@`��@`r�@` �@_�@_�;@_�w@_�P@_|�@_;d@^��@^�@^�R@^�+@^5?@^@]@]�@]O�@]/@\��@\�D@\I�@[�m@[��@[�@[C�@[o@Z�!@Z~�@Z=q@Y�@Y�7@YG�@Y%@XbN@X1'@Xb@W�@W�@W|�@W+@V�R@V5?@U�h@U/@T�/@T�@T(�@T1@S��@S33@R�H@R�!@R~�@R^5@RJ@QG�@Q�@P�9@PbN@PA�@O��@O��@O\)@O+@O
=@N��@N�@Nff@N@M��@M�-@Mp�@MV@L��@Lz�@L9X@K��@K33@J�H@J��@JM�@I�^@Ihs@I�@I%@H�`@H��@H�@HQ�@H �@G�@G�P@G�@F�y@Fv�@F5?@F$�@F@E�@E��@E`B@E/@D��@DI�@D�@C��@Ct�@Co@B��@B^5@BJ@A�^@A��@Ahs@@�`@@�@@ �@?�;@?��@?\)@?
=@>ȴ@>�+@>5?@>$�@>@=��@=@=p�@<�/@<�@<�D@<z�@<I�@;�m@;��@;S�@;o@:~�@:-@:J@9�@9��@9�7@9G�@8��@8Ĝ@8�@8Q�@8 �@8  @7��@7�P@7K�@7
=@6�@6ȴ@6��@6v�@6@5�T@5��@5@5�-@5�@5O�@4��@4�j@49X@3�
@3ƨ@3ƨ@3��@3t�@3dZ@333@3@2�H@2�\@2M�@1��@1��@1x�@1%@0Ĝ@0�9@0��@0�u@0�@0A�@/�@/�@/l�@/;d@/�@.��@.��@.E�@.$�@.@-�T@-��@-�-@-�@-?}@-V@,�j@,��@,Z@+��@+ƨ@+t�@+S�@+"�@*�H@*��@*��@*~�@*^5@*M�@)��@)��@)�#@)G�@(��@(�`@(Ĝ@(�u@(r�@(Q�@(1'@(  @'�;@'��@'�w@'K�@&��@&�R@&��@&ff@%�@%@%�h@%`B@%�@$��@$�@$z�@$Z@$(�@$1@#t�@#C�@#@"��@"^5@!��@!��@!x�@!hs@!X@!%@ �`@ �u@ bN@�@�P@l�@+@
=@�y@ȴ@v�@@��@��@�h@O�@/@/@��@�@�m@S�@@�@��@�\@�@��@hs@G�@G�@7L@&�@�@��@��@�@bN@1'@b@�@�;@�w@|�@\)@;d@�@�y@��@v�@�@��@@��@�@p�@/@�/@�j@j@I�@9X@�@��@�
@��@t�@33@@��@��@~�@n�@n�@n�@n�@M�@��@�^@�7@x�@hs@G�@G�@7L@&�@��@�9@�u@�u@r�@bN@bN@A�@1'@ �@b@�@��@�@l�@
=@�@ȴ@��@$�@��@�@O�@�@�/@�j@��@j@(�@�@�@1@�@dZ@33@
�@
��@
��@
~�@
n�@
M�@
�@
J@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B.B.B.B-B.B.B.B-B-B.B.B.B.B.B.B.B.B-B.B.B.B.B.B.B.B/B0!B0!B1'B2-B2-B33B33B1'BR�B�bB�bB�oB�B�LBB�B�B�fB�BPB�B�B/B5?BB�BE�BK�BK�BO�BS�BS�BW
BYB]/B^5B^5BgmBhsBgmBhsBjBjBhsBjBe`BaHB_;B[#BZBW
BS�BL�BF�B<jB7LB1'B,B!�B�BoBPBB��B�B�fB�5B��BȴB�^B��B��B}�BdZBP�BD�B8RB(�BJB
��B
�fB
�
B
ƨB
�9B
��B
��B
�\B
}�B
k�B
cTB
\)B
T�B
I�B
>wB
0!B
�B
bB
%B	��B	�sB	��B	ŢB	�?B	��B	�JB	y�B	gmB	S�B	D�B	2-B	"�B	�B	B��B�B�B�mB�/B�B��BƨB��B�qB�jB�XB�FB�3B�!B�B��B��B�uBz�Bw�Bz�Bx�BbNBYBK�B=qB33B0!B2-BA�BZB\)B]/BYBT�BO�BF�B>wB9XB6FB6FBG�BM�BM�BM�BN�BO�BO�BO�BL�BK�BP�BN�BN�BP�BR�BR�BR�BQ�BXB]/BZBZBffBl�Bo�Bq�Bp�By�B~�Bv�Bl�BhsBjBq�Bm�BjBm�Bt�Bs�Bp�Bm�BffBdZBgmBe`Bq�B� Bw�BgmBp�Bo�Bl�Bk�BjBx�B�B�DB��BB��BÖBÖB�B��B	%B	hB	�B	�B	�B	!�B	 �B	�B	�B	�B	�B	PB	1B	B��B��B��B�B�B�B�mB�ZB�5B�#B�B��B��B��B�BB�TB�`B�fB�mB�yB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B��B�B�B�B�B�B	B	DB	PB	uB	uB	oB	oB	hB	uB	bB	VB	VB	PB	JB	JB	JB	PB	bB	uB	uB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	&�B	+B	1'B	49B	6FB	7LB	8RB	9XB	8RB	8RB	8RB	<jB	=qB	?}B	A�B	B�B	C�B	?}B	A�B	F�B	G�B	J�B	K�B	L�B	H�B	H�B	K�B	L�B	L�B	L�B	K�B	G�B	C�B	A�B	@�B	?}B	>wB	>wB	>wB	=qB	=qB	=qB	>wB	@�B	B�B	C�B	C�B	D�B	F�B	F�B	J�B	K�B	L�B	N�B	Q�B	S�B	VB	ZB	]/B	`BB	dZB	ffB	ffB	gmB	hsB	jB	o�B	r�B	s�B	t�B	x�B	z�B	{�B	}�B	�B	�%B	�7B	�DB	�JB	�VB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�3B	�9B	�LB	�RB	�^B	�^B	�^B	�dB	�wB	��B	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�TB	�TB	�TB	�TB	�TB	�TB	�`B	�`B	�fB	�mB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B

=B

=B

=B
DB
DB
DB
DB
JB
JB
JB
PB
PB
PB
VB
VB
\B
\B
\B
\B
bB
hB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
)�B
+B
+B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
/B
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
49B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
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
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
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
D�B
D�B
D�B
E�B
E�B
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
H�B
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
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
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
R�B
S�B
S�B
S�B
S�B
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
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
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
`BB
`BB
`BB
`BB
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
e`B
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
gmB
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
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
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
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B-�B-�B-�B,�B-�B-�B-�B,�B,�B-�B-�B-�B-�B-�B-�B-�B-�B,�B-�B-�B-�B-�B-�B-�B-�B.�B/�B/�B0�B2B2GB3�B5�B:DBabB��B� B��B��B�JB�+BܒB�wB�yB�?BB�B vB0�B7fBD�BH1BMBMjBQ�BU2BUgBXEB[	B^OB_pB`�Bh�Bh�Bh$Bi�Bk�Bk�Bi�Bk�BffBbNB_�B[�B[	BXEBT�BNpBHB=�B9$B2�B./B"�BEB&B�B�B�	B�B�
B�\B�{BʌB�PB��B��B��Bf�BQ�BE�B9�B,B�B
�B
�XB
�KB
ȀB
��B
�XB
��B
�B
�B
l�B
d&B
]dB
V�B
K�B
A�B
2�B
B
 B
�B	�*B	�QB	�mB	��B	�RB	��B	�\B	}qB	j�B	WYB	G�B	5%B	%zB	�B	B��B��B��B�KBߊBؓB�jB��B�B�B�VB�B��B��B�AB�OB�mB��B�9B{�Bx8B}B{JBd&B[=BNVB?cB5%B0�B2BA�BZ�B]�B_VB[	BW
BR�BH�B@4B:xB6�B6zBHKBNpBNVBN�BOBPBPHBP}BMjBL�BRTBO\BO�BR:BS�BS@BS&BRTBYB^OBZ�BZ7Bf�Bl�Bo�Bq�Bp�BzxB� BxlBmwBh�BkBrGBm�BkQBm�BuBtBqvBn�Bg�Bd�Bg�Be,Br�B�ABy�BhXBq�BpBl�Bk�Bi�Bw�B��B��B�tBB��B��B��B�YB��B	9B	B	mB	�B	�B	!�B	!-B	 'B	�B	�B	B	<B		7B	�B��B��B�?B�'B�B�qB��B��B�BۦBؓB��B�[B�B�B�B�,B�LB�RB�B�B�B�B��B��B�B�B��B�B�B�(B�wB��B��B�B��B�B�)B��B��B	 iB	
�B	6B	@B	[B	�B	�B	:B	B	�B	�B	B	�B	dB	0B	B	6B	.B	�B	�B	�B	FB	YB	EB	EB	EB	eB	eB	qB	�B	 �B	!|B	"�B	#�B	&�B	*�B	1'B	4B	6B	7B	8B	9	B	8B	88B	8�B	<�B	=�B	?cB	AoB	B�B	C�B	?cB	A B	F%B	G_B	J�B	L~B	M6B	HfB	H�B	KxB	L�B	L�B	MB	LJB	G�B	C�B	AoB	@iB	?cB	>�B	?B	>�B	=VB	=<B	=VB	>]B	@OB	BuB	C{B	C�B	D�B	FtB	F�B	JrB	KxB	L�B	N�B	Q�B	S�B	U�B	ZB	]/B	`BB	d@B	f2B	f2B	g8B	hsB	jB	o�B	raB	shB	t�B	x�B	z�B	{�B	}�B	��B	�B	�B	��B	��B	�B	�.B	�hB	�aB	�MB	�SB	�sB	�kB	�xB	�dB	�dB	��B	�vB	�vB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�/B	��B	��B	�B	�B	��B	�B	�*B	�*B	�*B	�0B	�]B	��B	āB	�mB	ƎB	ȚB	ʌB	ʦB	ˬB	ΥB	ϫB	ϫB	ϫB	ѝB	ѷB	��B	��B	өB	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	��B	��B	��B	��B	��B	�'B	�B	� B	�B	�B	�B	� B	�:B	�,B	�,B	�B	�RB	�8B	�$B	�XB	�KB	�kB	�6B	�WB	�WB	�=B	�WB	�)B	�CB	�]B	�cB	�OB	�B	�iB	��B	��B	�vB	�|B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
B
1B

	B
	�B
	�B

�B

�B
)B
B
B
B
B
B
B
B
B
"B
B
(B
BB
BB
bB
NB
4B
TB
:B
&B
@B
&B
@B
FB
,B
FB
{B
gB
2B
SB
SB
SB
SB
YB
YB
sB
sB
_B
eB
eB
QB
QB
QB
kB
kB
kB
WB
WB
WB
WB
WB
xB
xB
]B
xB
]B
]B
xB
dB
~B
�B
�B
�B
�B
�B
�B
�B
pB
�B
 vB
�B
 �B
!�B
!�B
!|B
!|B
!|B
!�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
*�B
)�B
*�B
*�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
-�B
-�B
.�B
.�B
.�B
.�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
0�B
1B
2B
1�B
2B
2�B
2�B
2�B
2�B
2�B
4B
3�B
4B
3�B
4B
4B
4B
5B
5%B
5�B
5�B
6B
5�B
6B
6B
7B
7B
8B
8B
8B
8B
9$B
9$B
9	B
9�B
:*B
:B
:B
:DB
;JB
;B
;B
;0B
;B
<6B
<B
<6B
="B
=VB
>(B
>BB
>BB
>(B
>(B
>(B
?.B
?.B
?HB
?HB
@4B
@4B
@OB
@OB
@4B
AUB
AUB
AUB
A;B
A;B
BAB
BAB
B'B
BAB
BAB
B[B
B[B
CGB
CaB
DgB
DMB
D3B
ESB
ESB
ESB
EmB
ESB
ESB
ESB
FYB
FtB
FYB
FtB
FtB
GzB
H�B
HfB
HKB
HfB
HfB
H�B
HfB
I�B
IlB
IlB
IlB
JrB
JrB
J�B
JrB
JrB
J�B
J�B
JrB
J�B
J�B
JrB
JrB
J�B
JrB
K�B
K�B
K�B
KxB
K�B
L~B
LdB
L�B
L~B
M�B
L~B
L�B
LdB
L~B
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
R�B
S�B
S�B
S�B
S�B
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
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
ZB
ZB
Z�B
[�B
[�B
[�B
[�B
\�B
\�B
^B
^B
]�B
]�B
]�B
]�B
^B
]�B
^�B
_B
^�B
^�B
^�B
^�B
^�B
_�B
_�B
`B
`B
_�B
_�B
aB
a-B
a�B
a�B
a�B
a�B
bB
a�B
a�B
cB
c B
cB
cB
cB
cB
d&B
d&B
d&B
d&B
d&B
e,B
e,B
e,B
d�B
eB
eB
d�B
eB
e,B
fB
f2B
fB
e�B
fB
e�B
e�B
fB
f2B
g8B
gB
gB
g8B
gB
gB
gB
gB
gB
gB
g8B
gB
h$B
h>B
h$B
h>B
i*B
i*B
iDB
jKB
j0B
jKB
j0B
kQB
kQB
k6B
k6B
k6B
l"B
l=B
lWB
lWB
lWB
mCB
mCB
mCB
mCB
nIB
nIB
nIB
nIB
n/B
n/B
n/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<jPj<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.71(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201910300035282019103000352820191030003528202306231718352023062317183520230623171835201911010044192019110100441920191101004419  JA  ARFMdecpA19c                                                                20191025213706  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191025123711  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191025123714  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191025123714  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191025123715  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191025123715  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191025123715  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191025123715  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191025123717  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191025123717                      G�O�G�O�G�O�                JA  ARUP                                                                        20191025125452                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20191025153600  CV  JULD            G�O�G�O�F�6�                JM  ARCAJMQC2.0                                                                 20191029153528  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191029153528  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191031154419  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081835  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031506                      G�O�G�O�G�O�                