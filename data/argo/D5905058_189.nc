CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-11-15T18:38:03Z creation;2019-11-15T18:38:08Z conversion to V3.1;2023-06-29T05:50:41Z update;     
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
_FillValue                 �  ]   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �\   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20191115183803  20230705031506  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_189                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @��&�T2 1   @��'����@7Y#��w��b�ڹ�Y�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D�|�D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@ָRA\)A+\)AK\)Ak\)A��A��A��A��AŮAծA�A��B�
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
BZ�
Bb�
Bj�
Bs=qBz�
B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B���B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B՞�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�NC�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�C�Z�D -qD �qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD	-qD	�qD
-qD
�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD-qD�qD -qD �qD!-qD!�qD"-qD"�qD#-qD#�qD$-qD$�qD%-qD%�qD&-qD&�qD'-qD'�qD(-qD(�qD)-qD)�qD*-qD*�qD+-qD+�qD,-qD,�qD--qD-�qD.-qD.�qD/-qD/�qD0-qD0�qD1-qD1�qD2-qD2�qD3-qD3�qD4-qD4�qD5-qD5�qD6-qD6�qD7-qD7�qD8-qD8�qD9-qD9�qD:-qD:�qD;-qD;�qD<-qD<�qD=-qD=�qD>-qD>�qD?-qD?�qD@-qD@�qDA-qDA�qDB-qDB�qDC-qDC�qDD-qDD�qDE-qDE�qDF-qDF�qDG-qDG�qDH-qDH�qDI-qDI�qDJ-qDJ�qDK-qDK�qDL-qDL�qDM-qDM�qDN-qDN�qDO-qDO�qDP-qDP�qDQ-qDQ�qDR-qDR�qDS-qDS�qDT-qDT�qDU-qDU�qDV-qDV�qDW-qDW�qDX-qDX�qDY-qDY�qDZ-qDZ�qD[-qD[�qD\-qD\�qD]-qD]�qD^-qD^�qD_-qD_�qD`-qD`�qDa-qDa�qDb-qDb�qDc-qDc�qDd-qDd�qDe-qDe�qDf-qDf�qDg-qDg�qDh-qDh�qDi-qDi�qDj-qDj�qDk-qDk�qDl-qDl�qDm-qDm�qDn-qDn�qDo-qDo��Dp-qDp�qDq-qDq�qDr-qDr�qDs-qDs�qDt-qDt�qDu-qDu�qDv-qDv�qDw-qDw�qDx-qDx�qDy-qDy�qDz-qDz�qD{-qD{�qD|-qD|�qD}-qD}�qD~-qD~�qD-qD�qD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�S�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D�D�ָD��D�V�DÖ�D�ָD��D�V�DĖ�D�ָD��D�V�DŖ�D�ָD��D�V�DƖ�D�ָD��D�V�Dǖ�D�ָD��D�V�DȖ�D�ָD��D�V�Dɖ�D�ָD��D�V�Dʖ�D�ָD��D�V�D˖�D�ָD��D�V�D̖�D�ָD��D�V�D͖�D�ָD��D�V�DΖ�D�ָD��D�V�Dϖ�D�ָD��D�V�DЖ�D�ָD��D�V�Dі�D�ָD��D�V�DҖ�D�ָD��D�V�DӖ�D�ָD��D�V�DԖ�D�ָD��D�V�DՖ�D�ָD��D�V�D֖�D�ָD��D�V�Dז�D�ָD��D�V�Dؖ�D�ָD��D�V�Dٖ�D�ָD��D�S�Dږ�D�ָD��D�V�Dۖ�D�ָD��D�V�Dܖ�D�ָD��D�V�Dݖ�D�ָD��D�V�Dޖ�D�ָD��D�V�Dߖ�D�ָD��D�V�D���D�ָD��D�V�DᓅD�ӅD��D�V�D▸D�ָD��D�V�D㖸D�ָD��D�V�D䖸D�ָD��D�V�D喸D�ӅD��D�V�D斸D�ָD��D�V�D瓅D�ָD��D�V�D薸D�ָD��D�V�D閸D�ָD��D�V�DꖸD�ָD��D�V�D떸D�ָD��D�V�D언D���D��D�V�D햸D�ָD��D�V�DD�ָD��D�V�DD�ָD��D�V�D�D�ָD��D�V�D�D�ָD��D�V�D�D�ָD��D�V�D�D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D�ָD��D�V�D���D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�C�A�A�A�C�A�I�A�I�A�K�A�O�A�Q�A�O�A�Q�A�S�A�S�A�VA�XA�\)A�^5A�^5A�bNA�bNA�`BA�`BA�`BA�dZA�l�A�r�AҋDAҥ�A҇+A�`BA�-A��#A���A�&�A�G�A�?}Aĝ�A�M�A�r�A�XA�1'A�A���A��TA��-A�K�A� �A���A��;A�t�A�ȴA�l�A�oA�ƨA���A�hsA���A��RA��A�+A��A��A���A��\A�/A�dZA�bA�?}A�p�A�l�A�~�A�{A��!A���A�(�A�G�A�A�M�A�;dA�G�A��FA��A�ĜA� �A��A���A�A��jA�bA���A�hsA��PA�JA�XA�%A�  A�M�A�O�A���A�t�A�v�A�t�A�A{��Ay�TAxz�Aw�7Av1'At9XArZAp^5An�RAn$�Al�Aj�jAi7LAf��Ad�A`��A\�HA\-AZ��AWx�AT  ARAP5?AM�
AKO�AIS�AF�HAD�9AA�FA?��A>9XA=/A<1'A:�`A8=qA5t�A4$�A3�A133A01A09XA/O�A-+A+�A*�`A*M�A)hsA($�A&��A&E�A%7LA$E�A#��A#A"��A!�^A!l�A �+Ap�A�9A(�A��A5?A�hA�A�yAbNA$�At�A��A�
A��A?}A�A�A��AG�A�HAVA��A33A��A7LA
�uA	��A	A	�Al�Ar�A�
AdZA��A�uA^5A$�Al�A�jA��A?}A ĜA @���@���@��@���@� �@��!@��-@���@��w@��@�E�@���@�`B@�dZ@��T@�I�@���@��@�-@睲@��@�j@�33@���@ް!@ݑh@�ff@ް!@�$�@��@�@���@ܴ9@۶F@�33@ٙ�@�V@�%@��@�Q�@�A�@�b@�l�@���@�v�@��@Դ9@ӥ�@�M�@�%@�bN@�\)@���@·+@��#@̋D@�+@�+@˝�@˅@�
=@�@ǍP@��@��@��@Ɵ�@�hs@�j@���@�|�@�K�@�@�G�@�Z@�C�@�\)@�S�@�o@�ȴ@�?}@�1@�t�@��@�^5@�`B@�z�@��@��y@��@���@�$�@���@��#@��#@���@�O�@��`@��9@��D@�A�@��F@�@�5?@��@��^@�5?@�v�@��7@�%@���@��`@���@��D@�j@�A�@��@�b@���@���@�l�@�;d@��@���@�ff@�-@���@��^@�/@��@�9X@��@�  @��w@���@���@���@�dZ@�
=@��\@�V@�-@�@�p�@�7L@�/@�&�@���@�Z@�(�@�|�@��@���@�ff@�E�@�J@�hs@���@�(�@���@��@��@�S�@��!@�V@�`B@�7L@�%@�?}@�V@���@�r�@�9X@�1@��w@�dZ@��@��!@�~�@�^5@�5?@��@���@���@���@�Ĝ@�&�@���@�Ĝ@�A�@���@�|�@�"�@�o@��y@��!@�ff@�5?@��-@�%@��/@��/@���@��@��w@��H@�~�@��@��-@��u@�9X@��w@��P@�+@���@��@���@��`@���@��@�r�@�Z@�1'@���@�|�@�@���@��+@�$�@��T@���@���@�@��@�&�@��@���@���@�z�@�z�@�z�@�r�@�9X@��@�  @��w@��@��@�ȴ@��\@�^5@�M�@��@��h@�?}@�V@��/@��j@�Z@��@�ƨ@��@�;d@���@��+@�V@�5?@��@���@�@��h@�x�@�`B@�hs@�G�@�&�@���@��@�1'@��@�ƨ@���@���@�|�@�K�@�o@��R@��\@�n�@�5?@�J@��T@���@��@�/@��@��@�j@�Z@�Q�@�I�@�A�@�(�@�@�P@l�@\)@;d@~��@~��@}@}�h@}`B@}?}@|�@|�@|1@{��@{S�@{o@z�!@z=q@y�@y�^@y&�@x�u@xb@x  @w��@wl�@w�@v��@vE�@v{@v@uO�@tj@tI�@t�@t1@sƨ@st�@sC�@so@r�!@r�@q�#@q�^@q��@qG�@q�@p�u@p  @o�@ol�@o+@o
=@nȴ@n��@nE�@m�T@m�@l�/@l��@l(�@l1@k��@k�
@k��@kS�@ko@j�H@j��@j~�@j-@i��@i��@i�@h��@hr�@g�;@g\)@g;d@g
=@fȴ@f�+@fff@f$�@eV@d�D@c�m@c�@c"�@b�@b�H@b��@b^5@a�#@aX@a7L@`��@`�u@`b@_�w@_|�@_K�@_
=@^�@^ȴ@^�R@^��@^5?@]�T@]@]��@]�h@]�h@]p�@\�/@\Z@[�m@[�@["�@Z^5@Z�@Y�#@YG�@X��@XĜ@X�@XQ�@X �@W�;@W�@W|�@W
=@V�R@V�+@Vff@V@U@U�@UV@Tj@TI�@T(�@T1@S�
@S��@R�@R��@R�\@RM�@RJ@Q��@Q7L@P�`@P��@PQ�@Pb@P  @P  @O�@O�w@O|�@OK�@O+@Nȴ@Nff@M��@MO�@L�@L��@L9X@Kƨ@K�@K@J~�@J^5@I��@IX@I�@H��@HĜ@H��@H�@H  @G|�@G;d@G�@F�@F��@F$�@E�T@E��@E��@Ep�@EO�@EV@Dz�@D9X@C��@C�F@C��@CS�@B��@B=q@BJ@A��@A��@A�7@A7L@@bN@?�w@?��@?K�@?
=@>�@>��@>ff@=@=O�@<��@<j@<1@;�m@;t�@;"�@;@:��@:�!@:~�@:n�@:n�@:^5@:-@9��@9hs@9G�@9&�@9%@8�9@8bN@8  @7�@7�w@7�P@7|�@7K�@7
=@6��@6@5�h@5O�@5V@4�j@4��@49X@3�m@3��@3�@3o@2��@2M�@2J@1��@1x�@1&�@0�`@0Ĝ@0��@0�@0bN@0Q�@0 �@/�;@/��@/|�@/\)@/;d@.��@.ȴ@.�R@.ff@.{@.@-�@-@-O�@-/@,�@,��@,��@,�D@,z�@,I�@,1@+�
@+�F@+dZ@+S�@*�@*�!@*^5@*=q@*J@)�^@)�7@)X@)%@(��@(r�@(A�@(  @'�;@'\)@'�@&�y@&5?@&@%�@%�-@%?}@%�@$��@$�/@$�j@$�j@$�@$�D@$Z@$�@#�m@#ƨ@#�@#S�@#33@#o@"n�@"=q@!��@!�^@!x�@!hs@!X@!�@ Ĝ@ �u@ A�@ 1'@  �@�;@�P@+@�@��@V@{@�@�T@�T@��@�h@O�@/@V@��@z�@(�@��@�
@��@��@dZ@"�@��@��@M�@J@�@�^@hs@&�@%@��@Ĝ@�u@Q�@1'@1'@1'@b@b@  @��@�P@K�@+@��@��@v�@ff@V@@�T@�h@`B@�@�@��@�j@�@Z@1@�m@�
@ƨ@��@t�@33@"�@o@�H@��@��@n�@^5@-@J@�@��@��@X@7L@7L@&�@�@��@Ĝ@��@bN@ �@  @�;@�@|�@l�@\)@;d@��@�@ff@E�@$�@��@�h@�@/@��@�/@�/@��@��@Z@1@��@�m@�
@�F@��@S�@"�@@
��@
��@
n�@
-@	�^@	��@	x�@	hs@	7L@	�@�`@��@Ĝ@�@b@  @b@ �@ �@  @|�@\)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�C�A�A�A�C�A�I�A�I�A�K�A�O�A�Q�A�O�A�Q�A�S�A�S�A�VA�XA�\)A�^5A�^5A�bNA�bNA�`BA�`BA�`BA�dZA�l�A�r�AҋDAҥ�A҇+A�`BA�-A��#A���A�&�A�G�A�?}Aĝ�A�M�A�r�A�XA�1'A�A���A��TA��-A�K�A� �A���A��;A�t�A�ȴA�l�A�oA�ƨA���A�hsA���A��RA��A�+A��A��A���A��\A�/A�dZA�bA�?}A�p�A�l�A�~�A�{A��!A���A�(�A�G�A�A�M�A�;dA�G�A��FA��A�ĜA� �A��A���A�A��jA�bA���A�hsA��PA�JA�XA�%A�  A�M�A�O�A���A�t�A�v�A�t�A�A{��Ay�TAxz�Aw�7Av1'At9XArZAp^5An�RAn$�Al�Aj�jAi7LAf��Ad�A`��A\�HA\-AZ��AWx�AT  ARAP5?AM�
AKO�AIS�AF�HAD�9AA�FA?��A>9XA=/A<1'A:�`A8=qA5t�A4$�A3�A133A01A09XA/O�A-+A+�A*�`A*M�A)hsA($�A&��A&E�A%7LA$E�A#��A#A"��A!�^A!l�A �+Ap�A�9A(�A��A5?A�hA�A�yAbNA$�At�A��A�
A��A?}A�A�A��AG�A�HAVA��A33A��A7LA
�uA	��A	A	�Al�Ar�A�
AdZA��A�uA^5A$�Al�A�jA��A?}A ĜA @���@���@��@���@� �@��!@��-@���@��w@��@�E�@���@�`B@�dZ@��T@�I�@���@��@�-@睲@��@�j@�33@���@ް!@ݑh@�ff@ް!@�$�@��@�@���@ܴ9@۶F@�33@ٙ�@�V@�%@��@�Q�@�A�@�b@�l�@���@�v�@��@Դ9@ӥ�@�M�@�%@�bN@�\)@���@·+@��#@̋D@�+@�+@˝�@˅@�
=@�@ǍP@��@��@��@Ɵ�@�hs@�j@���@�|�@�K�@�@�G�@�Z@�C�@�\)@�S�@�o@�ȴ@�?}@�1@�t�@��@�^5@�`B@�z�@��@��y@��@���@�$�@���@��#@��#@���@�O�@��`@��9@��D@�A�@��F@�@�5?@��@��^@�5?@�v�@��7@�%@���@��`@���@��D@�j@�A�@��@�b@���@���@�l�@�;d@��@���@�ff@�-@���@��^@�/@��@�9X@��@�  @��w@���@���@���@�dZ@�
=@��\@�V@�-@�@�p�@�7L@�/@�&�@���@�Z@�(�@�|�@��@���@�ff@�E�@�J@�hs@���@�(�@���@��@��@�S�@��!@�V@�`B@�7L@�%@�?}@�V@���@�r�@�9X@�1@��w@�dZ@��@��!@�~�@�^5@�5?@��@���@���@���@�Ĝ@�&�@���@�Ĝ@�A�@���@�|�@�"�@�o@��y@��!@�ff@�5?@��-@�%@��/@��/@���@��@��w@��H@�~�@��@��-@��u@�9X@��w@��P@�+@���@��@���@��`@���@��@�r�@�Z@�1'@���@�|�@�@���@��+@�$�@��T@���@���@�@��@�&�@��@���@���@�z�@�z�@�z�@�r�@�9X@��@�  @��w@��@��@�ȴ@��\@�^5@�M�@��@��h@�?}@�V@��/@��j@�Z@��@�ƨ@��@�;d@���@��+@�V@�5?@��@���@�@��h@�x�@�`B@�hs@�G�@�&�@���@��@�1'@��@�ƨ@���@���@�|�@�K�@�o@��R@��\@�n�@�5?@�J@��T@���@��@�/@��@��@�j@�Z@�Q�@�I�@�A�@�(�@�@�P@l�@\)@;d@~��@~��@}@}�h@}`B@}?}@|�@|�@|1@{��@{S�@{o@z�!@z=q@y�@y�^@y&�@x�u@xb@x  @w��@wl�@w�@v��@vE�@v{@v@uO�@tj@tI�@t�@t1@sƨ@st�@sC�@so@r�!@r�@q�#@q�^@q��@qG�@q�@p�u@p  @o�@ol�@o+@o
=@nȴ@n��@nE�@m�T@m�@l�/@l��@l(�@l1@k��@k�
@k��@kS�@ko@j�H@j��@j~�@j-@i��@i��@i�@h��@hr�@g�;@g\)@g;d@g
=@fȴ@f�+@fff@f$�@eV@d�D@c�m@c�@c"�@b�@b�H@b��@b^5@a�#@aX@a7L@`��@`�u@`b@_�w@_|�@_K�@_
=@^�@^ȴ@^�R@^��@^5?@]�T@]@]��@]�h@]�h@]p�@\�/@\Z@[�m@[�@["�@Z^5@Z�@Y�#@YG�@X��@XĜ@X�@XQ�@X �@W�;@W�@W|�@W
=@V�R@V�+@Vff@V@U@U�@UV@Tj@TI�@T(�@T1@S�
@S��@R�@R��@R�\@RM�@RJ@Q��@Q7L@P�`@P��@PQ�@Pb@P  @P  @O�@O�w@O|�@OK�@O+@Nȴ@Nff@M��@MO�@L�@L��@L9X@Kƨ@K�@K@J~�@J^5@I��@IX@I�@H��@HĜ@H��@H�@H  @G|�@G;d@G�@F�@F��@F$�@E�T@E��@E��@Ep�@EO�@EV@Dz�@D9X@C��@C�F@C��@CS�@B��@B=q@BJ@A��@A��@A�7@A7L@@bN@?�w@?��@?K�@?
=@>�@>��@>ff@=@=O�@<��@<j@<1@;�m@;t�@;"�@;@:��@:�!@:~�@:n�@:n�@:^5@:-@9��@9hs@9G�@9&�@9%@8�9@8bN@8  @7�@7�w@7�P@7|�@7K�@7
=@6��@6@5�h@5O�@5V@4�j@4��@49X@3�m@3��@3�@3o@2��@2M�@2J@1��@1x�@1&�@0�`@0Ĝ@0��@0�@0bN@0Q�@0 �@/�;@/��@/|�@/\)@/;d@.��@.ȴ@.�R@.ff@.{@.@-�@-@-O�@-/@,�@,��@,��@,�D@,z�@,I�@,1@+�
@+�F@+dZ@+S�@*�@*�!@*^5@*=q@*J@)�^@)�7@)X@)%@(��@(r�@(A�@(  @'�;@'\)@'�@&�y@&5?@&@%�@%�-@%?}@%�@$��@$�/@$�j@$�j@$�@$�D@$Z@$�@#�m@#ƨ@#�@#S�@#33@#o@"n�@"=q@!��@!�^@!x�@!hs@!X@!�@ Ĝ@ �u@ A�@ 1'@  �@�;@�P@+@�@��@V@{@�@�T@�T@��@�h@O�@/@V@��@z�@(�@��@�
@��@��@dZ@"�@��@��@M�@J@�@�^@hs@&�@%@��@Ĝ@�u@Q�@1'@1'@1'@b@b@  @��@�P@K�@+@��@��@v�@ff@V@@�T@�h@`B@�@�@��@�j@�@Z@1@�m@�
@ƨ@��@t�@33@"�@o@�H@��@��@n�@^5@-@J@�@��@��@X@7L@7L@&�@�@��@Ĝ@��@bN@ �@  @�;@�@|�@l�@\)@;d@��@�@ff@E�@$�@��@�h@�@/@��@�/@�/@��@��@Z@1@��@�m@�
@�F@��@S�@"�@@
��@
��@
n�@
-@	�^@	��@	x�@	hs@	7L@	�@�`@��@Ĝ@�@b@  @b@ �@ �@  @|�@\)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
m�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
o�B
n�B
m�B
n�B
m�B
n�B
p�B
s�B
|�B
��B
��B
��B
�B
�?B
��B�BO�Bo�Bu�Bv�B�B�BiyB(�BB
��B
��B
��B
�BB
�B
ɺB
�B,B"�B�B,B��B��B!�B7LBL�BC�BVBXB]/B\)B\)B`BB_;B\)BXBW
BK�BF�BB�B8RB&�B!�B�B�BbBB��B�B�B�/B��BĜB�qB��B��By�BcTBD�B'�B�BuB%B
�`B
��B
�B
�hB
r�B
^5B
D�B
-B
�B
oB

=B
B	�B	�`B	�B	��B	ÖB	�dB	��B	��B	�1B	p�B	[#B	5?B	/B	!�B	
=B�B�5B��BĜB�?B��B��B��B�PB�7B�B�%B~�Bz�Bv�Bp�BhsBdZBbNB`BBhsBhsB]/BT�BYB^5B`BBbNB_;B_;B`BBbNBaHB`BBaHB_;B`BBbNB`BBcTBffBhsBgmBe`BffBaHB^5B^5B[#BYBVBT�BM�BL�BI�BH�BD�BE�BI�BH�BF�BE�B?}B<jB9XB:^B8RB1'B.B0!B2-B33B2-B2-B2-B6FB8RB<jB9XB7LB5?B33B33B49B49B33B2-B1'B0!B.B/B(�B'�B(�B+B+B/B1'B5?B7LB8RB6FB8RB8RB6FB5?B8RBC�BJ�BI�BM�BT�BR�BT�BVBYBYBW
BYB`BBbNBffBm�Bp�Bt�Bu�Bv�Bx�By�Bx�Bz�B{�B� B�B�B�B�+B�7B�JB�\B�uB�{B��B��B��B��B��B��B��B��B�B�B�B�'B�!B�B�B�!B�?B�dBBÖBɺB��B��B�B�B�
B�)B�HB�ZB�sB�B�B��B��B	  B	B	B	B		7B	JB	PB	{B	�B	�B	�B	"�B	,B	,B	.B	0!B	1'B	1'B	1'B	2-B	49B	6FB	7LB	9XB	<jB	=qB	?}B	C�B	D�B	E�B	F�B	G�B	H�B	J�B	L�B	N�B	O�B	O�B	Q�B	T�B	ZB	^5B	`BB	cTB	e`B	gmB	jB	n�B	p�B	q�B	q�B	q�B	u�B	v�B	w�B	x�B	y�B	|�B	}�B	~�B	� B	�B	�B	�+B	�7B	�DB	�JB	�VB	�\B	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�3B	�FB	�RB	�RB	�XB	�^B	�^B	�^B	�^B	�dB	�jB	�jB	�qB	�wB	��B	��B	��B	B	B	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�/B	�;B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
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
%B
%B
%B
%B
+B
+B
1B
	7B
	7B
	7B

=B

=B

=B

=B

=B
DB
JB
JB
PB
PB
PB
PB
VB
VB
VB
\B
\B
\B
\B
bB
bB
hB
hB
oB
oB
uB
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
,B
-B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
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
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
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
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
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
K�B
K�B
L�B
L�B
L�B
L�B
M�B
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
P�B
P�B
P�B
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
YB
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
\)B
\)B
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
`BB
`BB
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
aHB
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
m�B
m�B
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
q�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
m]B
lWB
lWB
lWB
lWB
lWB
lWB
lWB
lWB
lWB
lWB
lWB
lWB
m]B
m]B
m]B
m]B
oiB
ncB
m]B
ncB
m]B
ncB
pUB
shB
|�B
��B
�B
��B
��B
��B
ںB$&BV9BvFBz�B|B�B��Bo�B-wBSB
��B
��B
��B
��B
��B
��B
�B.IB$�B�B*�B��B��B!�B8�BN"BC�BV�BYeB^B]B^Bb�B`�B^BZBX�BL�BG�BEB;B(�B"�BB�B BSB��B�[B��BޞB��B�?B� B�KB��B}VBg8BG_B)yB�BB	�B
�B
רB
��B
��B
u�B
b4B
G�B
/B
)B
�B
�B
B	��B	�mB	چB	ˬB	�B	��B	�B	��B	�^B	t�B	^�B	6�B	1[B	%zB	�B�B��BҽB�zB��B��B��B��B��B��B�YB�zB��B~By�BrBi�BfBc:B`vBi�Bj�B^�BV9BY�B_;Ba�BcnB`'B`\B`�Bb�Ba�BaHBbNB_�BaHBcTB`�Bc�Bf�Bi�Bh$BfLBh>Ba�B^�B^�B[�BZBWYBVmBN�BM�BK)BI�BEBF?BJrBIRBG�BFYB@ B<�B9�B;JB:B2-B.�B0�B2�B3MB2GB2|B2�B6�B9$B=B9�B8B6+B3MB3hB49B4�B3�B2�B1�B0�B/�B0UB)*B(>B)�B+�B+�B/OB1�B5�B8�B9	B7B9>B9>B7�B5�B7�BCGBJ�BIRBNVBUMBSBUMBV9BY�BY1BV�BX�B`vBb4BffBm�Bp�Bt�BvFBv�By>Bz^ByXB{B|6B�B�'B�{B��B��B��B��B�\B��B�2B��B��B�B��B��B�B�B��B�B�B��B�vB�oB�iB��B�B�?B�B�-B��BɺB��B�(B�yB�SB�sB�CB�B�ZB�sB�wB�oB�tB��B	  B	B	�B	B		B	dB	�B	�B	B	qB	CB	"�B	,"B	+�B	-�B	/�B	1B	0�B	1B	2B	4B	5�B	72B	9$B	<PB	=VB	?cB	CaB	DgB	EmB	F�B	G�B	H�B	J�B	L�B	N�B	O�B	O�B	Q�B	T�B	Y�B	^B	`BB	c:B	eFB	g8B	jB	ncB	poB	q[B	q�B	q�B	u�B	v�B	w�B	x�B	y�B	|�B	}�B	~�B	�B	�-B	�B	�B	��B	�DB	�JB	�pB	�\B	��B	�.B	� B	�2B	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�$B	��B	�ZB	�`B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�)B	��B	�B	��B	�eB	�B	��B	��B	�=B	��B	��B	��B	��B	��B	��B	� B	��B	��B	��B	�B	� B	� B	��B	�B	��B	��B	��B	��B	�B	�3B	�+B	�B	�B	�	B	��B	�B	�B	�*B	�B	�6B	�PB	�<B	�]B	�iB	�iB	�oB	�[B	B	ĜB	āB	�mB	ňB	ƎB	ǮB	ʌB	̳B	ΥB	��B	��B	��B	��B	յB	��B	��B	�B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�@B	�B	�2B	�B	�2B	�LB	�RB	�RB	�$B	�DB	�DB	�0B	�KB	�eB	�QB	�kB	�B	�cB	�IB	�iB	�OB	�OB	�iB	�OB	�UB	�UB	�[B	�[B	�[B	�vB	�vB	�B	�hB	�nB	�nB	�nB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B

	B

	B

	B

	B

#B
)B
�B
0B
B
B
B
B
"B
"B
B
(B
(B
(B
(B
.B
HB
B
4B
:B
:B
@B
@B
@B
@B
&B
[B
{B
MB
MB
SB
SB
YB
YB
?B
YB
_B
_B
KB
KB
eB
kB
QB
kB
kB
WB
qB
]B
xB
]B
�B
dB
�B
�B
OB
jB
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
pB
�B
�B
�B
�B
 vB
 vB
 vB
 �B
 vB
!|B
!|B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
*�B
*�B
*�B
+�B
,�B
+�B
,�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
/�B
/�B
/�B
0�B
0�B
1�B
1�B
1�B
2�B
2�B
2�B
3B
4B
4B
3�B
5B
5B
5%B
6B
5�B
5�B
6�B
6�B
72B
72B
88B
8B
8B
9$B
9	B
9	B
9	B
9$B
:*B
:*B
;JB
;B
;B
<6B
<B
<6B
<B
<B
<B
=B
=B
="B
=<B
=VB
>(B
>BB
>(B
>(B
>(B
>(B
?.B
@4B
@OB
@OB
@OB
@4B
AUB
AoB
AoB
BuB
B[B
B[B
CGB
CaB
C{B
DgB
DMB
DMB
EmB
ESB
FtB
FYB
FYB
FtB
G_B
GzB
G_B
GzB
HfB
H�B
H�B
HfB
HfB
HfB
H�B
I�B
IlB
IlB
IlB
IlB
I�B
J�B
JXB
J�B
JrB
J�B
KxB
KxB
KxB
KxB
K^B
K�B
L~B
L~B
L�B
L�B
M�B
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
P�B
P�B
P�B
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
X�B
X�B
X�B
X�B
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
[�B
[�B
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
]�B
]�B
^B
^B
^B
]�B
^�B
^�B
^�B
^�B
_B
`B
_�B
_�B
_�B
`B
`�B
`�B
`�B
`�B
aB
aB
`�B
`�B
a�B
a�B
bB
a�B
bB
a�B
c B
c B
cB
cB
cB
cB
c B
dB
d&B
c�B
dB
dB
eB
eB
d�B
e,B
e,B
eB
e,B
f2B
f2B
f2B
f2B
f2B
fB
f2B
f2B
gB
gB
gB
gB
g8B
gB
gB
gB
h$B
h>B
h>B
h$B
i*B
iB
i*B
iDB
i*B
iDB
i*B
jKB
j0B
jKB
kQB
kQB
k6B
kQB
l=B
l=B
lWB
lWB
lWB
mCB
mCB
m)B
m)B
m]B
mCB
nIB
nIB
nIB
nIB
oOB
oOB
oOB
oOB
oOB
oiB
pUB
pUB
pUB
poB
q[B
q[B
q[B
qvB
q[B
qAB
q[B
q[B
r|B
raB
qvB
ra11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.71(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201911210038032019112100380320191121003803202306231719002023062317190020230623171900201911220029542019112200295420191122002954  JA  ARFMdecpA19c                                                                20191116033715  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191115183803  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191115183805  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191115183805  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191115183806  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191115183806  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191115183806  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191115183806  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191115183807  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191115183808                      G�O�G�O�G�O�                JA  ARUP                                                                        20191115185419                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20191116153436  CV  JULD            G�O�G�O�F�a8                JM  ARCAJMQC2.0                                                                 20191120153803  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191120153803  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191121152954  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081900  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031506                      G�O�G�O�G�O�                