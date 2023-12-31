CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-07T00:35:43Z creation;2018-02-07T00:35:47Z conversion to V3.1;2019-12-19T07:45:48Z update;     
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
_FillValue                 �  IP   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �,   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �t   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ސ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20180207003543  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_208                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�Js�"�1   @�Jt��O�@3zڹ�Y��d]I�^1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @9��@�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�jA�jA�n�A�l�A�l�A�r�A�v�A�p�A�p�A�t�A�|�A�|�AɁAɅAɍPAɕ�Aɩ�A���A�(�A��A��;A��/A��A���Aɰ!Aɕ�A�p�A�oA��mAȋDA��yA��A���A���AǅA��A���A�+A��AžwAŃA�hsA�I�A� �A�%Aĺ^Aĩ�AĴ9A�n�A���A�-APA�-A���A��A��^A��\A�VA��
A�hsA��#A�K�A��HA�r�A��A�A�z�A��A�r�A��mA���A��+A�1'A�K�A�5?A�E�A�`BA�oA�p�A���A���A�S�A�&�A���A���A���A��uA�p�A�VA���A��FA��;A�5?A��mA��uA��A�z�A���A��wA�{A��mA�n�A���A��A��A�z�A���A�ĜA�%A��A��7A��RA�{A�K�A�{A�dZA��Ay�Au��As�^Ap�AmAhVAg|�Ae��Ac�wAcC�Ac
=Ab  A^�yA]�A]%AZ�`AYAW�#AV��ATbNASC�AQ�mAP�AOO�AM?}AJ�AI�AG`BAF9XAD1'ABz�AAK�A?�A=�;A:��A9��A8�A5K�A3��A1��A.��A-�A-�A,��A,(�A*�yA)�-A(�\A'ƨA&VA$�uA$�A"�yA!�A!VA A��AXA1'A��A��A�-AQ�A"�A(�A%A(�AĜA�AS�A%AbAJA��AO�A
��A
��A	��A��At�A+AjAhsA��A�A=qA��A7LA ^5@��\@��9@��;@���@��P@���@�h@�K�@홚@�I�@�ƨ@�^5@�D@�b@�@�M�@���@�@�u@��T@�p�@���@��@�G�@ە�@���@٩�@�p�@�/@�(�@��@�-@���@պ^@�j@��@���@��@��
@��@�^5@���@��@�l�@�@�n�@�@�G�@�7L@�/@ȣ�@��
@Ɵ�@�V@�/@ÍP@�+@��H@�{@��/@�A�@�b@��
@��P@�+@��\@�?}@�I�@�l�@�o@���@��+@�=q@�J@���@��@��@��@���@���@���@�@�@��@�&�@���@�Q�@��@�t�@�\)@��@�-@��@�@��@���@��w@��P@��!@���@�%@��@��@�Z@���@���@�+@�o@�o@�+@�+@�=q@�v�@�
=@���@�ff@���@�z�@�bN@�r�@��@�r�@�b@�ƨ@��@�K�@���@�E�@���@��@�O�@��j@�1'@���@�l�@�t�@�dZ@�;d@�|�@��w@��w@��@��w@��@�t�@�o@�J@��7@�/@���@���@���@��^@�@��@�`B@�&�@�Ĝ@���@�j@�Q�@�z�@���@��@�1@�1@��m@��@�l�@��@��@�n�@���@�C�@�dZ@�
=@��@��R@�E�@�hs@��`@��`@��9@�bN@�I�@�9X@��@��;@��w@��F@���@��@�;d@�o@���@��H@��R@��\@�ff@�$�@�J@���@���@��j@���@���@�r�@�r�@�Q�@�1@���@���@�dZ@��@���@�V@��T@���@���@��D@�Q�@���@��P@��P@��P@���@��@��@�~�@�ff@�-@�-@��#@���@�O�@��@��9@�z�@�b@��@���@��m@��F@�t�@�+@�
=@��H@��@�ȴ@���@�v�@�V@�=q@��@��^@�`B@�O�@�G�@�7L@�/@��@���@��`@���@��j@��@��@�Q�@�9X@��@���@�\)@�\)@�C�@���@���@�^5@�5?@�{@���@��@�p�@�`B@�G�@�%@��/@���@�j@�b@���@�ƨ@���@�t�@�C�@��@�ff@�E�@�-@�5?@�{@��#@���@���@���@��^@���@�?}@���@���@�r�@�  @�ƨ@�|�@���@�ff@��@�G�@�/@��@�bN@�1'@�@|�@l�@~ȴ@~ȴ@~E�@}/@|�D@|I�@|9X@{�@{C�@z�\@x�`@x  @w\)@u�@u�@u`B@u?}@u�@t�@t�D@s�m@sdZ@so@r�@r-@p�`@p�@p �@o�;@o��@o�w@o�@n��@n5?@m�@mO�@l��@l�@l�D@lZ@l�@k�@j�\@j-@j�@jJ@i�#@ix�@i�@hbN@g�@gl�@gK�@g;d@g+@g
=@fv�@f5?@e�@e?}@e�@d�/@d��@d�@c�m@c�
@c�
@c��@c"�@b�H@b��@b�!@b�@a�^@a��@a�^@a��@aX@`��@`��@`�9@`r�@`1'@_�;@_�@^�+@^{@]p�@]V@\��@\��@\j@\9X@\9X@\(�@\�@[��@[ƨ@[��@[��@[t�@[dZ@[33@[o@Z�H@Z�\@Z=q@Y��@Yx�@YX@Y7L@Y&�@Y�@X�9@W�;@W|�@W\)@WK�@W+@VV@U�@U�@U/@UV@T�j@Tz�@T1@St�@S33@R�\@R=q@Q��@Q7L@P�9@P�@PbN@PQ�@Pb@O�@O\)@O�@O�@O
=@N�y@Nff@N5?@M��@M`B@MV@L�j@Lj@L9X@K��@KS�@J�!@I��@IG�@H��@H�@H1'@G��@F�R@FE�@E�@E�-@E�h@E�@Ep�@E`B@EO�@EO�@E?}@EV@DI�@C�F@B�@B�!@B��@Bn�@B�@A�#@A�^@A��@Ax�@Ahs@@��@@�9@@��@@b@?�;@?�@?|�@?;d@?
=@>��@>v�@>$�@=�@=�-@=�@<��@<��@<�@<�D@<z�@<(�@;�m@;��@;S�@;S�@;"�@:��@:�\@:^5@9��@9�#@9�^@9��@9x�@9&�@9%@8��@8��@8�9@8r�@8A�@8b@7�;@7�w@7|�@6��@6�R@6v�@6ff@65?@5�T@5�@5?}@4��@4j@4I�@4(�@3��@3��@3S�@2�H@2��@2~�@2M�@2�@1�#@1�^@17L@0�`@0��@0Ĝ@0��@0r�@0b@/�;@/\)@/;d@.��@.�R@.V@-�@-�-@-�h@-V@,�@,Z@,�@+ƨ@+dZ@+33@+o@*�@*�H@*��@*�\@*~�@)�@)�7@)X@)7L@(�`@(�u@(Q�@(1'@(  @'�;@'��@'|�@&�@&��@&ff@&E�@&{@&@%�@%��@%�-@%�h@%O�@%V@$�j@$Z@$9X@$1@#�F@#33@"�@"��@"=q@!�@!�7@!�@!%@ ��@ ��@ A�@   @��@l�@\)@;d@
=@��@�y@�R@V@��@p�@�@�j@�@z�@(�@�@"�@@�H@M�@��@�^@�^@��@G�@%@Ĝ@r�@ �@�@�;@�w@��@��@�P@l�@+@
=@�y@�R@v�@v�@ff@ff@E�@�@@��@p�@V@��@I�@9X@9X@(�@��@�F@��@�@dZ@33@o@@��@��@��@n�@M�@-@��@hs@�@�@%@%@��@��@�`@��@��@��@��@�@ �@�;@\)@
=@
=@
=@��@��@��@ȴ@��@v�@E�@$�@�T@�T@@@��@/@�j@z�@�@�m@�m@�
@�
@�
@�F@o@
n�@
^5@
^5@
M�@
=q@
-@
=q@
-@
�@
�@
J@	��@	��@	��@	�#@	�#@	��@	��@	�^@	��@	x�@	7L@�u@bN@Q�@A�@b@��@|�@l�@K�@K�@;d@��@{@@�@@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�jA�jA�n�A�l�A�l�A�r�A�v�A�p�A�p�A�t�A�|�A�|�AɁAɅAɍPAɕ�Aɩ�A���A�(�A��A��;A��/A��A���Aɰ!Aɕ�A�p�A�oA��mAȋDA��yA��A���A���AǅA��A���A�+A��AžwAŃA�hsA�I�A� �A�%Aĺ^Aĩ�AĴ9A�n�A���A�-APA�-A���A��A��^A��\A�VA��
A�hsA��#A�K�A��HA�r�A��A�A�z�A��A�r�A��mA���A��+A�1'A�K�A�5?A�E�A�`BA�oA�p�A���A���A�S�A�&�A���A���A���A��uA�p�A�VA���A��FA��;A�5?A��mA��uA��A�z�A���A��wA�{A��mA�n�A���A��A��A�z�A���A�ĜA�%A��A��7A��RA�{A�K�A�{A�dZA��Ay�Au��As�^Ap�AmAhVAg|�Ae��Ac�wAcC�Ac
=Ab  A^�yA]�A]%AZ�`AYAW�#AV��ATbNASC�AQ�mAP�AOO�AM?}AJ�AI�AG`BAF9XAD1'ABz�AAK�A?�A=�;A:��A9��A8�A5K�A3��A1��A.��A-�A-�A,��A,(�A*�yA)�-A(�\A'ƨA&VA$�uA$�A"�yA!�A!VA A��AXA1'A��A��A�-AQ�A"�A(�A%A(�AĜA�AS�A%AbAJA��AO�A
��A
��A	��A��At�A+AjAhsA��A�A=qA��A7LA ^5@��\@��9@��;@���@��P@���@�h@�K�@홚@�I�@�ƨ@�^5@�D@�b@�@�M�@���@�@�u@��T@�p�@���@��@�G�@ە�@���@٩�@�p�@�/@�(�@��@�-@���@պ^@�j@��@���@��@��
@��@�^5@���@��@�l�@�@�n�@�@�G�@�7L@�/@ȣ�@��
@Ɵ�@�V@�/@ÍP@�+@��H@�{@��/@�A�@�b@��
@��P@�+@��\@�?}@�I�@�l�@�o@���@��+@�=q@�J@���@��@��@��@���@���@���@�@�@��@�&�@���@�Q�@��@�t�@�\)@��@�-@��@�@��@���@��w@��P@��!@���@�%@��@��@�Z@���@���@�+@�o@�o@�+@�+@�=q@�v�@�
=@���@�ff@���@�z�@�bN@�r�@��@�r�@�b@�ƨ@��@�K�@���@�E�@���@��@�O�@��j@�1'@���@�l�@�t�@�dZ@�;d@�|�@��w@��w@��@��w@��@�t�@�o@�J@��7@�/@���@���@���@��^@�@��@�`B@�&�@�Ĝ@���@�j@�Q�@�z�@���@��@�1@�1@��m@��@�l�@��@��@�n�@���@�C�@�dZ@�
=@��@��R@�E�@�hs@��`@��`@��9@�bN@�I�@�9X@��@��;@��w@��F@���@��@�;d@�o@���@��H@��R@��\@�ff@�$�@�J@���@���@��j@���@���@�r�@�r�@�Q�@�1@���@���@�dZ@��@���@�V@��T@���@���@��D@�Q�@���@��P@��P@��P@���@��@��@�~�@�ff@�-@�-@��#@���@�O�@��@��9@�z�@�b@��@���@��m@��F@�t�@�+@�
=@��H@��@�ȴ@���@�v�@�V@�=q@��@��^@�`B@�O�@�G�@�7L@�/@��@���@��`@���@��j@��@��@�Q�@�9X@��@���@�\)@�\)@�C�@���@���@�^5@�5?@�{@���@��@�p�@�`B@�G�@�%@��/@���@�j@�b@���@�ƨ@���@�t�@�C�@��@�ff@�E�@�-@�5?@�{@��#@���@���@���@��^@���@�?}@���@���@�r�@�  @�ƨ@�|�@���@�ff@��@�G�@�/@��@�bN@�1'@�@|�@l�@~ȴ@~ȴ@~E�@}/@|�D@|I�@|9X@{�@{C�@z�\@x�`@x  @w\)@u�@u�@u`B@u?}@u�@t�@t�D@s�m@sdZ@so@r�@r-@p�`@p�@p �@o�;@o��@o�w@o�@n��@n5?@m�@mO�@l��@l�@l�D@lZ@l�@k�@j�\@j-@j�@jJ@i�#@ix�@i�@hbN@g�@gl�@gK�@g;d@g+@g
=@fv�@f5?@e�@e?}@e�@d�/@d��@d�@c�m@c�
@c�
@c��@c"�@b�H@b��@b�!@b�@a�^@a��@a�^@a��@aX@`��@`��@`�9@`r�@`1'@_�;@_�@^�+@^{@]p�@]V@\��@\��@\j@\9X@\9X@\(�@\�@[��@[ƨ@[��@[��@[t�@[dZ@[33@[o@Z�H@Z�\@Z=q@Y��@Yx�@YX@Y7L@Y&�@Y�@X�9@W�;@W|�@W\)@WK�@W+@VV@U�@U�@U/@UV@T�j@Tz�@T1@St�@S33@R�\@R=q@Q��@Q7L@P�9@P�@PbN@PQ�@Pb@O�@O\)@O�@O�@O
=@N�y@Nff@N5?@M��@M`B@MV@L�j@Lj@L9X@K��@KS�@J�!@I��@IG�@H��@H�@H1'@G��@F�R@FE�@E�@E�-@E�h@E�@Ep�@E`B@EO�@EO�@E?}@EV@DI�@C�F@B�@B�!@B��@Bn�@B�@A�#@A�^@A��@Ax�@Ahs@@��@@�9@@��@@b@?�;@?�@?|�@?;d@?
=@>��@>v�@>$�@=�@=�-@=�@<��@<��@<�@<�D@<z�@<(�@;�m@;��@;S�@;S�@;"�@:��@:�\@:^5@9��@9�#@9�^@9��@9x�@9&�@9%@8��@8��@8�9@8r�@8A�@8b@7�;@7�w@7|�@6��@6�R@6v�@6ff@65?@5�T@5�@5?}@4��@4j@4I�@4(�@3��@3��@3S�@2�H@2��@2~�@2M�@2�@1�#@1�^@17L@0�`@0��@0Ĝ@0��@0r�@0b@/�;@/\)@/;d@.��@.�R@.V@-�@-�-@-�h@-V@,�@,Z@,�@+ƨ@+dZ@+33@+o@*�@*�H@*��@*�\@*~�@)�@)�7@)X@)7L@(�`@(�u@(Q�@(1'@(  @'�;@'��@'|�@&�@&��@&ff@&E�@&{@&@%�@%��@%�-@%�h@%O�@%V@$�j@$Z@$9X@$1@#�F@#33@"�@"��@"=q@!�@!�7@!�@!%@ ��@ ��@ A�@   @��@l�@\)@;d@
=@��@�y@�R@V@��@p�@�@�j@�@z�@(�@�@"�@@�H@M�@��@�^@�^@��@G�@%@Ĝ@r�@ �@�@�;@�w@��@��@�P@l�@+@
=@�y@�R@v�@v�@ff@ff@E�@�@@��@p�@V@��@I�@9X@9X@(�@��@�F@��@�@dZ@33@o@@��@��@��@n�@M�@-@��@hs@�@�@%@%@��@��@�`@��@��@��@��@�@ �@�;@\)@
=@
=@
=@��@��@��@ȴ@��@v�@E�@$�@�T@�T@@@��@/@�j@z�@�@�m@�m@�
@�
@�
@�F@o@
n�@
^5@
^5@
M�@
=q@
-@
=q@
-@
�@
�@
J@	��@	��@	��@	�#@	�#@	��@	��@	�^@	��@	x�@	7L@�u@bN@Q�@A�@b@��@|�@l�@K�@K�@;d@��@{@@�@@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
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
��B
��B
��B
��B
��BBPB%�BW
BjBp�By�Bz�B|�B�%B�=B�=B�{B��B�{B�3BĜBǮBĜB�)B��B��BPBuB�B�B�B�B#�B%�B5?B?}B<jB5?B;dB9XBC�BI�BK�BI�BG�BC�B?}BJ�BM�BM�BS�B]/BhsBs�Bs�Bs�Bm�BhsBhsBiyB`BBW
BQ�BT�BA�B33BVBM�B)�BPB\B�B$�B33B#�BhB��B�B�5B�dB�}B��B��B�'B��B�{B�JBv�B�B� B]/B<jB�B�B
��B
�)B
ǮB
��B
{�B
v�B
u�B
gmB
N�B
/B
%�B	��B	ÖB	��B	�B	��B	iyB	�\B	�B	s�B	~�B	|�B	n�B	T�B	XB	\)B	D�B	<jB	7LB	.B	"�B	!�B	!�B	�B	DB��B�`B��B�yB�ZB�
B��B��BĜB�?B��B�B��B�7B�uB�%B|�B�DB��B�oB�=B�+B|�B�B� Bw�Bp�B|�Bv�Bt�Bo�Bm�Be`BaHB^5B[#BYBQ�BM�BR�BP�BL�BN�BG�BM�BJ�B9XBO�B_;B\)BT�BT�BXBJ�BA�BS�BW
BP�BJ�BL�BI�BL�BW
BVBO�BO�BM�BR�BI�B@�B@�B<jBG�BN�BT�BZBXBVB`BB^5B\)B^5B\)BW
BK�BVBO�BW
B]/BXBZBjBiyBhsBdZBdZBhsBm�Bk�Be`Be`Bl�Bp�Bp�Bt�Bw�Bs�Bx�B}�B�B�B�B�B�7B�1B�B�B� B�7B�B�B�uB�uB�hB�hB��B��B��B��B��B��B��B��B��B�-B�FB�RB�^B�wB�qB��B�jBƨBɺBǮB��B��B�B�B�/B�HB�ZB�yB�sB�B�B�B��B��B��B��B��B��B��B��B��B	%B	\B	bB	\B	hB	{B	�B	�B	�B	�B	�B	%�B	33B	2-B	5?B	0!B	7LB	;dB	=qB	D�B	F�B	G�B	K�B	L�B	L�B	L�B	J�B	P�B	VB	ZB	YB	\)B	`BB	ffB	l�B	l�B	m�B	w�B	z�B	{�B	|�B	~�B	�B	�B	� B	{�B	|�B	~�B	�B	�B	�1B	�\B	�\B	�JB	�PB	�DB	�=B	�PB	�bB	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�B	�B	�3B	�^B	�^B	�^B	�wB	�qB	�dB	�XB	��B	ŢB	ŢB	ŢB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�/B	�/B	�;B	�;B	�;B	�BB	�BB	�HB	�;B	�;B	�BB	�BB	�TB	�TB	�fB	�yB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
  B
  B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
B
%B
B
1B
	7B
+B
DB
JB
JB
DB

=B
DB
DB
DB

=B
PB
JB
PB
PB
JB
DB
	7B
PB
VB
\B
VB
VB
hB
uB
oB
oB
hB
bB
hB
uB
hB
hB
uB
uB
hB
uB
�B
uB
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
!�B
%�B
%�B
%�B
$�B
#�B
"�B
"�B
"�B
"�B
 �B
�B
$�B
&�B
'�B
(�B
'�B
&�B
'�B
'�B
&�B
(�B
(�B
(�B
)�B
(�B
(�B
'�B
&�B
)�B
+B
,B
+B
)�B
)�B
(�B
)�B
,B
-B
-B
-B
,B
+B
+B
,B
+B
-B
-B
-B
-B
.B
/B
/B
.B
-B
.B
/B
.B
-B
/B
0!B
1'B
0!B
/B
/B
0!B
0!B
/B
/B
/B
-B
/B
0!B
0!B
2-B
49B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
49B
49B
49B
5?B
6FB
6FB
6FB
6FB
49B
33B
5?B
7LB
6FB
6FB
49B
5?B
6FB
6FB
7LB
6FB
7LB
6FB
6FB
7LB
7LB
8RB
7LB
9XB
9XB
;dB
<jB
<jB
;dB
;dB
;dB
<jB
>wB
=qB
=qB
;dB
=qB
<jB
=qB
=qB
>wB
>wB
?}B
>wB
=qB
=qB
>wB
?}B
A�B
A�B
A�B
A�B
@�B
C�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
E�B
D�B
B�B
C�B
D�B
F�B
H�B
G�B
G�B
H�B
I�B
I�B
H�B
H�B
G�B
I�B
I�B
H�B
I�B
I�B
J�B
I�B
J�B
I�B
J�B
J�B
J�B
J�B
J�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
L�B
M�B
N�B
M�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
O�B
P�B
P�B
P�B
P�B
P�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
VB
VB
VB
VB
VB
T�B
VB
XB
XB
XB
W
B
VB
XB
W
B
XB
XB
XB
XB
XB
YB
YB
XB
ZB
XB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
[#B
[#B
]/B
]/B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
]/B
\)B
_;B
_;B
`BB
`BB
aHB
aHB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
`BB
aHB
bNB
aHB
bNB
bNB
bNB
dZB
dZB
cTB
cTB
dZB
e`B
dZB
ffB
ffB
ffB
ffB
ffB
e`B
e`B
dZB
gmB
gmB
gmB
iyB
hsB
hsB
gmB
iyB
jB
jB
iyB
jB
k�B
l�B
l�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
o�B
o�B
o�B
n�B
n�B
p�B
q�B
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
q�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
r�B
r�B
s�B
s�B
t�B
v�B
v�B
v�B
v�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
v�B
v�B
v�B
u�B
u�B
v�B
w�B
x�B
y�B
x�B
x�B
x�B
w�B
v�B
v�B
z�B
z�B
z�B
z�B
z�B
{�B
z�B
z�B
{�B
z�B
z�B
{�B
{�B
z�B
{�B
{�B
{�B
z�B
z�B
z�B
z�B
y�B
{�B
|�B
|�B
{�B
{�B
{�B
}�B
}�B
}�B
|�B
z�B
{�B
~�B
~�B
~�B
~�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
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
��B
��B
��B
��B
��B�BB%�BWsBjBp�By�Bz�B}<B�YB��B�B��B�SB��B�BāB��B��B�B�fB�B�B�BB�BB 'B$&B&�B5tB?�B=<B6�B<�B:�BDMBJ#BK�BJ	BHBDMB@�BK�BOBOBT�B^Bi*BtBt�Bt�Bn�BiyBiBi�Ba|BY1BT�BW�BE�B7�BW�BPHB/�B,B�B"�B'RB5?B'B{B��B�qB��B��B��B�AB�UB�aB�zB��B�Bz�B��B�BbBB[BdB�BaB
��B
�B
�B
�oB
z�B
w�B
i�B
R B
33B
(sB	�6B	��B	�vB	� B	��B	oB	��B	�B	vFB	�B	}�B	poB	XyB	Y�B	]dB	G+B	>(B	9�B	/�B	%zB	#nB	#nB	+B	�B	�B�*B�B�B�2BٚB� BЗB��B�B��B��B�&B�~B��B�RB�B��B��B�uB�xB��B~�B��B�UBy�Br�B}�BxlBvBp�BoBg8BcTB_�B]/BZ�BTaBO�BT{BRTBNpBP.BI�BN�BLJB<PBP�B_VB\�BU�BU�BX�BLdBC�BT�BW�BR BL0BNBKxBNpBW�BW
BQhBQNBO(BS�BK^BB[BB�B>�BI7BO�BU�BZ�BY1BW?B`�B^�B\�B^�B\�BW�BM�BV�BQ�BW�B]�BYKB[#Bj�Bi�Bh�Be,Be,BiBm�BlBffBf�Bm]Bq[Bq�ButBxRBt�ByrB~wB�oB��B��B�mB�RB�fB��B��B��B��B�B�9B��B��B� B�TB�	B��B�B�B�!B�IB��B��B��B�|B�zB��B��B��B��B��B�qB��B�	B�1B�B�VB�9B�kB�~B�B��B��B��B��B�B�!B�B�	B�2B�`B�zB�0B��B��B�}B	tB	BB	�B	�B	�B	�B	�B	�B	�B	�B	;B	%�B	2�B	2|B	5�B	1AB	7fB	;B	=�B	D�B	F�B	G�B	K�B	MB	MB	M6B	K^B	QB	VSB	ZkB	Y�B	\�B	`�B	f�B	l�B	l�B	m�B	w�B	z�B	{�B	|�B	~�B	�B	�UB	�iB	|�B	}VB	HB	�B	�B	�B	��B	�\B	�~B	��B	�xB	��B	�jB	��B	��B	��B	��B	�,B	�
B	�B	�0B	�B	�_B	�DB	�)B	�IB	�B	�*B	�^B	��B	��B	��B	��B	��B	��B	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�&B	�2B	�FB	уB	�+B	�=B	�B	�IB	�!B	�VB	�pB	�vB	�\B	�|B	ߊB	ߊB	��B	�B	�B	�B	�B	��B	��B	�B	�B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	�B	��B	�B	��B	��B	�B	��B	�B	�B	�B	�B
 B
 B
 B
 B
 B
 B	�.B	�.B	�.B
B
-B
3B
3B
3B
3B
3B
3B
9B
B
3B
3B
3B
GB
MB
9B
EB
EB
mB
YB
mB
KB
	RB
zB
^B
dB
dB
^B

rB
xB
xB
xB

�B
jB
~B
jB
jB
~B
�B
	�B
jB
pB
\B
pB
�B
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
 �B
B
 B
B
=B
B
 B
5B
!�B
%�B
%�B
%�B
$�B
$B
#B
#B
"�B
"�B
!B
;B
$�B
'B
(
B
)B
(
B
'8B
($B
($B
'B
)B
)B
)B
*B
)*B
)*B
(>B
'8B
*B
+B
,B
+B
*0B
*B
)DB
*0B
,"B
-)B
-)B
-)B
,"B
+6B
+B
,"B
+6B
-)B
-)B
-)B
-CB
.IB
/B
/5B
./B
-CB
.IB
/5B
./B
-CB
/5B
0!B
1'B
0UB
/5B
/5B
0;B
0;B
/5B
/B
/OB
-]B
/OB
0UB
0UB
2GB
49B
3MB
3MB
4TB
5?B
5?B
5ZB
5ZB
5?B
5?B
5?B
5ZB
5ZB
5ZB
5ZB
5ZB
4TB
4TB
4TB
5ZB
6`B
6`B
6`B
6`B
4TB
3�B
5ZB
7LB
6`B
6`B
4�B
5tB
6zB
6`B
7�B
6`B
7fB
6zB
6zB
7fB
7�B
8lB
7�B
9�B
9�B
;dB
<�B
<�B
;B
;B
;�B
<jB
>wB
=�B
=�B
;�B
=�B
<�B
=�B
=�B
>�B
>�B
?�B
>�B
=�B
=�B
>�B
?�B
A�B
A�B
A�B
A�B
@�B
C�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
E�B
D�B
B�B
C�B
D�B
F�B
H�B
G�B
G�B
H�B
I�B
I�B
H�B
H�B
G�B
I�B
I�B
H�B
I�B
I�B
J�B
I�B
J�B
I�B
J�B
J�B
J�B
J�B
KB
MB
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
MB
M�B
N�B
M�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q B
O�B
Q B
Q B
Q B
Q B
Q B
PB
Q B
RB
RB
RB
RB
RB
RB
R B
SB
TB
TB
TB
TB
TB
T,B
UB
VB
VB
VB
VB
VB
U2B
VB
XB
X+B
X+B
W$B
V9B
X+B
WYB
X+B
X+B
X+B
XEB
XEB
Y1B
Y1B
XEB
ZB
XEB
Z7B
Z7B
[=B
[=B
\CB
\CB
\)B
\CB
\CB
\CB
[qB
[WB
]IB
]IB
\CB
]dB
]IB
^jB
^OB
^OB
^OB
]dB
\CB
_VB
_VB
`BB
`\B
aHB
aHB
`\B
`\B
`vB
`\B
`vB
`\B
`\B
abB
abB
abB
`vB
abB
bhB
a|B
bNB
b�B
b�B
dtB
dtB
cTB
cnB
dZB
ezB
d�B
ffB
f�B
f�B
ffB
ffB
ezB
e�B
d�B
g�B
g�B
g�B
i�B
h�B
h�B
g�B
i�B
j�B
j�B
i�B
j�B
k�B
l�B
l�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
o�B
o�B
o�B
n�B
n�B
p�B
q�B
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
q�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
r�B
r�B
s�B
s�B
t�B
v�B
v�B
v�B
v�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
v�B
v�B
v�B
u�B
u�B
v�B
w�B
x�B
y�B
x�B
x�B
x�B
w�B
v�B
v�B
z�B
z�B
z�B
z�B
z�B
{�B
z�B
z�B
{�B
z�B
z�B
{�B
{�B
z�B
{�B
{�B
{�B
z�B
z�B
z�B
{B
z*B
|B
|�B
}B
{�B
|B
|B
}�B
~B
}�B
}B
{B
|B
~�B
.B
B
B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802110033482018021100334820180211003348201806221325582018062213255820180622132558201804050729342018040507293420180405072934  JA  ARFMdecpA19c                                                                20180207093519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180207003543  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180207003545  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180207003545  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180207003546  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180207003546  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180207003546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180207003546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180207003546  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180207003547                      G�O�G�O�G�O�                JA  ARUP                                                                        20180207005639                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180207154132  CV  JULD            G�O�G�O�F�S�                JM  ARGQJMQC2.0                                                                 20180207154132  CV  JULD_LOCATION   G�O�G�O�F�S�                JM  ARGQJMQC2.0                                                                 20180207154132  CV  LATITUDE        G�O�G�O�A��/                JM  ARGQJMQC2.0                                                                 20180207154132  CV  LONGITUDE       G�O�G�O��"�m                JM  ARCAJMQC2.0                                                                 20180210153348  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180210153348  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222934  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042558  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                