CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-14T00:35:43Z creation;2018-04-14T00:35:45Z conversion to V3.1;2019-12-23T06:23:37Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180414003543  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               5A   JA  I2_0675_053                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�Z��z 1   @�Z�W:� @6�-V�b�oiDg81   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DIy�DI��DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�<�D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@Tz�@�=q@�=qA�A%�AE�Ae�A��\A��\A��\A��\A\Aҏ\A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B���B���B���B��
B���B���B���B���B���B���B���B���B���B���B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�Ck�CQ�CQ�CQ�CQ�C k�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�DJDJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dx{Dx�{Dy{Dy�{Dz{Dz�{D{{D{�{D|{D|�{D}{D}�{D~{D~�{D{D�{D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��
D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D=D��=D�
=D�J=DÊ=D��=D�
=D�J=DĊ=D��=D�
=D�J=DŊ=D��=D�
=D�J=DƊ=D��=D�
=D�J=DǊ=D��=D�
=D�J=DȊ=D��=D�
=D�J=DɊ=D��=D�
=D�J=Dʊ=D��=D�
=D�J=Dˊ=D��=D�
=D�G
D̊=D��=D�
=D�J=D͊=D��=D�
=D�J=DΊ=D��=D�
=D�J=Dϊ=D��=D�
=D�J=DЊ=D��=D�
=D�J=Dъ=D��=D�
=D�J=DҊ=D��=D�
=D�J=Dӊ=D��=D�
=D�J=DԊ=D��=D�
=D�J=DՊ=D��=D�
=D�J=D֊=D��=D�
=D�J=D׊=D��=D�
=D�J=D؊=D��=D�
=D�J=Dي=D��=D�
=D�J=Dڊ=D��=D�
=D�J=Dۊ=D��=D�
=D�J=D܊=D��=D�
=D�J=D݊=D��=D�
=D�J=Dފ=D��=D�
=D�J=Dߊ=D��=D�
=D�J=D��=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D��=D��
D�
D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
D�J=D�=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��qD�q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A�ƨA��wA��A��uA��\A��7A��A�r�A�hsA�`BA�Q�A�;dA��^A�VA��A���A��DA�(�A��A��!A��7A�r�A�M�A��A��A���A���A��uA�hsA�+A��hA�\)A� �A���A���A���A��A�oA�r�A�n�A���A�p�A�l�A�\)A�A�33A��mA�ZA���A�+A���A���A��A���A��7A�`BA�;dA�VA��`A�|�A���A���A��A��A�1'A�VA���A��;A��A�VA���A�G�A���A��wA���A���A��7A�ĜA�x�A���A���A��#A��A��
A�r�A��A�33A���A�
=A�t�A�ĜA��7A�l�A��+A�A�hAoA~�\A}/Az�9Av��At5?AqƨApffAm�wAh��Ae�Ab(�A]�FAZ-AV�uAT�9ARĜAQ+AP5?AL��AGp�AE"�ADA�ACl�AB�+AA+A?��A@$�A@5?A>  A<�`A<�9A<M�A;`BA:�DA:Q�A9�A8�jA8JA7\)A6��A4�\A3��A1�TA0�A/|�A-%A+��A*ffA)�FA(n�A&��A&��A&E�A& �A$��A$5?A#�#A"�`A!t�A!/A�
A/A�PA�/AQ�A?}A��A|�A�AG�AAv�A�^AS�A��A`BA�9AA�A�AffA��AA�/A�An�A=qA�
A
jA	XA��A�-A�A�TAG�AVAZAt�A�A �`@�|�@��!@�7L@��@��m@��@�A�@�&�@�I�@�1@�C�@���@�z�@���@�o@�^@�j@�@�R@�hs@�I�@�@�+@�J@���@�dZ@ܣ�@�$�@ؓu@���@�V@��/@ӥ�@�ȴ@щ7@�j@��
@�;d@���@���@Ώ\@��@�O�@��/@�bN@���@�"�@ʗ�@�V@ə�@�I�@���@�dZ@��y@��@��/@�j@��@�+@§�@�@�v�@�hs@��@��R@�x�@��@��@���@��!@���@��-@�1@��P@�
=@��!@���@���@��P@�@��^@�V@�I�@���@�t�@�+@�
=@��@���@�~�@���@�Ĝ@���@�S�@�5?@���@��h@��-@��T@���@��@�hs@�V@�Ĝ@�z�@�1'@��@���@��P@��
@�1@�(�@�b@�  @��F@�@��@�&�@���@��j@��j@��D@��@�
=@��@���@��T@��7@�%@�r�@�A�@��
@��F@�l�@�@���@���@�5?@���@�&�@��9@�A�@���@��;@���@���@��w@��w@�33@���@�~�@�^5@�$�@�$�@��@��@�X@���@�9X@�b@���@���@��@�C�@�;d@�;d@�+@�@��R@��+@�=q@�$�@�J@��-@���@���@��h@�hs@�G�@���@���@��`@���@��9@��@�1'@�(�@�(�@��@�ƨ@��P@�S�@�;d@�33@��@���@��+@�M�@�$�@�@��T@��h@�hs@�X@�?}@�7L@���@��/@��j@��D@�1'@��@�1@���@��m@�|�@�+@�+@�"�@�+@��@���@��@���@���@�~�@�n�@�~�@�M�@��@�x�@�7L@��@��@���@�Ĝ@���@�Z@�  @��P@�t�@�l�@�C�@�@��@���@�v�@���@���@�x�@�7L@�Ĝ@�r�@�A�@� �@�1@���@���@��@�K�@�@���@��+@�~�@�n�@�@��@��T@��-@��@�%@��9@��9@��u@�Q�@��m@�ƨ@��@��@�+@�@��R@���@���@��+@�M�@�$�@�{@�J@��@��^@�x�@�&�@��@���@��@��`@��/@�Ĝ@��j@���@��D@�9X@�  @�  @�  @�@�@�;@��@�P@K�@�@
=@~�y@~ȴ@}/@|1@{ƨ@{�@{dZ@{C�@{33@z�H@z�!@z~�@z=q@y�@y�7@yhs@yX@yG�@y&�@y%@x��@x�9@x�9@x�@xbN@xQ�@xQ�@x1'@x  @w|�@wK�@w
=@v�@v��@vE�@u�@u�-@u�h@u`B@t�j@t�@tZ@s�
@s"�@r�\@r~�@q��@q7L@p��@p�u@o�P@n�R@n��@n{@m�@mO�@m?}@m/@l�j@lZ@l1@k�m@kC�@j�H@j~�@i�@i��@i�7@i7L@h��@h�`@h�9@h��@h1'@g�;@g�;@g�@g;d@g�@g
=@f�@fV@e�h@eV@d��@d�@cƨ@ct�@cdZ@cS�@co@b�@b�H@b�\@bn�@b=q@a��@a��@`��@_��@_\)@_;d@^��@^��@^V@]�@\��@\�D@\z�@\z�@\Z@\�@[ƨ@[S�@["�@Z�@Z��@Z�\@Y��@YG�@Y%@X�u@XbN@X  @W�@Wl�@V�@Vv�@V@U�@UV@T�j@TZ@T1@S�
@S��@St�@S"�@R��@RM�@Q��@Qx�@Q%@P��@PbN@P �@O��@O�P@O+@N�@N�+@NE�@N5?@M�T@M�-@M�@MV@L�/@Lz�@K�
@KdZ@Kt�@KS�@K@J�\@JJ@I��@I�7@I�@H��@H�@H �@Gl�@G;d@G+@F��@FE�@F@E@E�@EO�@E�@D��@D�@DZ@C�@B�@B��@B�@A�^@Ahs@AX@AX@A7L@@�`@@�9@@�u@@Q�@@1'@?�;@?�@?|�@?\)@?K�@>�y@>��@>��@>�+@>v�@>E�@>$�@>@=�T@=�h@=O�@=/@=�@<�@<�D@<I�@;�m@;��@;t�@;dZ@;C�@:��@:^5@:^5@:^5@:=q@9��@9x�@9hs@9X@9&�@8�u@8r�@8bN@81'@8  @7��@7�w@7��@7�P@7K�@7+@6�y@6��@6v�@65?@5��@5�h@5�@5O�@4��@4�/@4��@4�j@4�@4�@4j@4�@3ƨ@3t�@3o@2��@2�\@2^5@2J@1��@1�#@1��@1��@1�^@1��@1�7@1x�@0��@0��@0�@0bN@01'@/l�@.ȴ@.E�@-�-@-�@,�D@,I�@,1@+ƨ@+��@+�@+t�@+t�@+dZ@+33@+@*~�@*=q@*J@)�^@)X@)G�@)7L@)�@(�`@(Ĝ@(�@(1'@'�;@'��@'+@&��@&ȴ@&V@&@%@%p�@%O�@%�@$�@$9X@$(�@$�@$1@$1@#��@#��@#o@"�H@"��@"M�@"�@!�^@!��@!x�@!X@!&�@ ��@ Ĝ@ �9@ ��@ �@ A�@   @��@�w@�P@;d@�@�@ȴ@��@5?@�T@@`B@?}@/@��@�D@9X@(�@��@�F@t�@C�@C�@33@�@~�@^5@=q@-@�@�#@��@X@�@%@Ĝ@�@A�@  @�@��@��@|�@l�@\)@�@�R@��@��@V@{@�@��@��@@�-@��@�h@`B@O�@O�@O�@O�@O�@?}@/@�@j@9X@9X@��@��@t�@C�@C�@33@"�@"�@o@@��@~�@�@�7@X@&�@%@��@�9@�9@�u@r�@Q�@ �@�;@�P@l�@K�@+@
=@��@�@��@�+@ff@V@E�@{@@@�T@��@�-@�h@p�@p�@`B@`B@?}@�@�/@�/@�@z�@I�@(�@(�@�@(�@�@��@��@�@dZ@C�@@
�H@
�H@
��@
�!@
^5@
=q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A�ƨA��wA��A��uA��\A��7A��A�r�A�hsA�`BA�Q�A�;dA��^A�VA��A���A��DA�(�A��A��!A��7A�r�A�M�A��A��A���A���A��uA�hsA�+A��hA�\)A� �A���A���A���A��A�oA�r�A�n�A���A�p�A�l�A�\)A�A�33A��mA�ZA���A�+A���A���A��A���A��7A�`BA�;dA�VA��`A�|�A���A���A��A��A�1'A�VA���A��;A��A�VA���A�G�A���A��wA���A���A��7A�ĜA�x�A���A���A��#A��A��
A�r�A��A�33A���A�
=A�t�A�ĜA��7A�l�A��+A�A�hAoA~�\A}/Az�9Av��At5?AqƨApffAm�wAh��Ae�Ab(�A]�FAZ-AV�uAT�9ARĜAQ+AP5?AL��AGp�AE"�ADA�ACl�AB�+AA+A?��A@$�A@5?A>  A<�`A<�9A<M�A;`BA:�DA:Q�A9�A8�jA8JA7\)A6��A4�\A3��A1�TA0�A/|�A-%A+��A*ffA)�FA(n�A&��A&��A&E�A& �A$��A$5?A#�#A"�`A!t�A!/A�
A/A�PA�/AQ�A?}A��A|�A�AG�AAv�A�^AS�A��A`BA�9AA�A�AffA��AA�/A�An�A=qA�
A
jA	XA��A�-A�A�TAG�AVAZAt�A�A �`@�|�@��!@�7L@��@��m@��@�A�@�&�@�I�@�1@�C�@���@�z�@���@�o@�^@�j@�@�R@�hs@�I�@�@�+@�J@���@�dZ@ܣ�@�$�@ؓu@���@�V@��/@ӥ�@�ȴ@щ7@�j@��
@�;d@���@���@Ώ\@��@�O�@��/@�bN@���@�"�@ʗ�@�V@ə�@�I�@���@�dZ@��y@��@��/@�j@��@�+@§�@�@�v�@�hs@��@��R@�x�@��@��@���@��!@���@��-@�1@��P@�
=@��!@���@���@��P@�@��^@�V@�I�@���@�t�@�+@�
=@��@���@�~�@���@�Ĝ@���@�S�@�5?@���@��h@��-@��T@���@��@�hs@�V@�Ĝ@�z�@�1'@��@���@��P@��
@�1@�(�@�b@�  @��F@�@��@�&�@���@��j@��j@��D@��@�
=@��@���@��T@��7@�%@�r�@�A�@��
@��F@�l�@�@���@���@�5?@���@�&�@��9@�A�@���@��;@���@���@��w@��w@�33@���@�~�@�^5@�$�@�$�@��@��@�X@���@�9X@�b@���@���@��@�C�@�;d@�;d@�+@�@��R@��+@�=q@�$�@�J@��-@���@���@��h@�hs@�G�@���@���@��`@���@��9@��@�1'@�(�@�(�@��@�ƨ@��P@�S�@�;d@�33@��@���@��+@�M�@�$�@�@��T@��h@�hs@�X@�?}@�7L@���@��/@��j@��D@�1'@��@�1@���@��m@�|�@�+@�+@�"�@�+@��@���@��@���@���@�~�@�n�@�~�@�M�@��@�x�@�7L@��@��@���@�Ĝ@���@�Z@�  @��P@�t�@�l�@�C�@�@��@���@�v�@���@���@�x�@�7L@�Ĝ@�r�@�A�@� �@�1@���@���@��@�K�@�@���@��+@�~�@�n�@�@��@��T@��-@��@�%@��9@��9@��u@�Q�@��m@�ƨ@��@��@�+@�@��R@���@���@��+@�M�@�$�@�{@�J@��@��^@�x�@�&�@��@���@��@��`@��/@�Ĝ@��j@���@��D@�9X@�  @�  @�  @�@�@�;@��@�P@K�@�@
=@~�y@~ȴ@}/@|1@{ƨ@{�@{dZ@{C�@{33@z�H@z�!@z~�@z=q@y�@y�7@yhs@yX@yG�@y&�@y%@x��@x�9@x�9@x�@xbN@xQ�@xQ�@x1'@x  @w|�@wK�@w
=@v�@v��@vE�@u�@u�-@u�h@u`B@t�j@t�@tZ@s�
@s"�@r�\@r~�@q��@q7L@p��@p�u@o�P@n�R@n��@n{@m�@mO�@m?}@m/@l�j@lZ@l1@k�m@kC�@j�H@j~�@i�@i��@i�7@i7L@h��@h�`@h�9@h��@h1'@g�;@g�;@g�@g;d@g�@g
=@f�@fV@e�h@eV@d��@d�@cƨ@ct�@cdZ@cS�@co@b�@b�H@b�\@bn�@b=q@a��@a��@`��@_��@_\)@_;d@^��@^��@^V@]�@\��@\�D@\z�@\z�@\Z@\�@[ƨ@[S�@["�@Z�@Z��@Z�\@Y��@YG�@Y%@X�u@XbN@X  @W�@Wl�@V�@Vv�@V@U�@UV@T�j@TZ@T1@S�
@S��@St�@S"�@R��@RM�@Q��@Qx�@Q%@P��@PbN@P �@O��@O�P@O+@N�@N�+@NE�@N5?@M�T@M�-@M�@MV@L�/@Lz�@K�
@KdZ@Kt�@KS�@K@J�\@JJ@I��@I�7@I�@H��@H�@H �@Gl�@G;d@G+@F��@FE�@F@E@E�@EO�@E�@D��@D�@DZ@C�@B�@B��@B�@A�^@Ahs@AX@AX@A7L@@�`@@�9@@�u@@Q�@@1'@?�;@?�@?|�@?\)@?K�@>�y@>��@>��@>�+@>v�@>E�@>$�@>@=�T@=�h@=O�@=/@=�@<�@<�D@<I�@;�m@;��@;t�@;dZ@;C�@:��@:^5@:^5@:^5@:=q@9��@9x�@9hs@9X@9&�@8�u@8r�@8bN@81'@8  @7��@7�w@7��@7�P@7K�@7+@6�y@6��@6v�@65?@5��@5�h@5�@5O�@4��@4�/@4��@4�j@4�@4�@4j@4�@3ƨ@3t�@3o@2��@2�\@2^5@2J@1��@1�#@1��@1��@1�^@1��@1�7@1x�@0��@0��@0�@0bN@01'@/l�@.ȴ@.E�@-�-@-�@,�D@,I�@,1@+ƨ@+��@+�@+t�@+t�@+dZ@+33@+@*~�@*=q@*J@)�^@)X@)G�@)7L@)�@(�`@(Ĝ@(�@(1'@'�;@'��@'+@&��@&ȴ@&V@&@%@%p�@%O�@%�@$�@$9X@$(�@$�@$1@$1@#��@#��@#o@"�H@"��@"M�@"�@!�^@!��@!x�@!X@!&�@ ��@ Ĝ@ �9@ ��@ �@ A�@   @��@�w@�P@;d@�@�@ȴ@��@5?@�T@@`B@?}@/@��@�D@9X@(�@��@�F@t�@C�@C�@33@�@~�@^5@=q@-@�@�#@��@X@�@%@Ĝ@�@A�@  @�@��@��@|�@l�@\)@�@�R@��@��@V@{@�@��@��@@�-@��@�h@`B@O�@O�@O�@O�@O�@?}@/@�@j@9X@9X@��@��@t�@C�@C�@33@"�@"�@o@@��@~�@�@�7@X@&�@%@��@�9@�9@�u@r�@Q�@ �@�;@�P@l�@K�@+@
=@��@�@��@�+@ff@V@E�@{@@@�T@��@�-@�h@p�@p�@`B@`B@?}@�@�/@�/@�@z�@I�@(�@(�@�@(�@�@��@��@�@dZ@C�@@
�H@
�H@
��@
�!@
^5@
=q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��BŢB�B�B;dB>wBE�BI�BH�BJ�BK�BM�BP�BT�BVBW
BZB\)B]/B]/BgmBjB|�B�VB�hB��B��B�Bw�By�B�+B�1B�+B�=B�JB�B|�Bw�Bn�B_;B=qB-B(�B%�B$�B#�B �B�B�B\B��B�B�sB�5B�)B�#B�#B��BĜB�B�bB�Bs�BaHBM�B9XB)�B{B
��B
��B
�3B
�uB
n�B
YB
N�B
E�B
>wB
33B
(�B
�B
bB	��B	�B	�yB	�`B	�HB	�)B	��B	�jB	��B	�VB	y�B	k�B	T�B	)�B	JB��B��B�^B��B��B��B�=B�Bs�B[#BR�BS�BR�BR�BR�BXB_;BiyBx�B}�B}�B�B�B}�B{�Bz�Bx�Bw�Bu�Bt�Bq�Bo�Bm�Bk�BiyBe`BaHB]/BZB[#BYBVBVBT�BVBS�BS�BW
BS�BP�BP�BK�BF�BB�BC�BA�B@�BB�BA�B>wB=qB=qB@�BC�BE�BD�BE�BD�BA�B<jB<jB:^B9XB9XB9XB9XB:^B;dB<jB<jB;dB9XB8RB7LB7LB6FB5?B33B/B-B,B,B+B+B)�B/B0!B/B/B/B.B,B+B'�B'�B'�B)�B)�B(�B(�B(�B'�B&�B&�B$�B&�B(�B(�B(�B(�B)�B+B-B0!B33B49B5?B49B49B49B6FB8RB:^B;dB<jB?}B@�B@�BC�BF�BG�BI�BJ�BO�BT�BW
BXB]/B_;B_;B`BBe`BffBjBjBm�Bo�Bn�Bm�Bm�Bm�Bn�Bo�Bp�Bq�Bu�Bu�By�B}�B~�B�B�+B�7B�=B�DB�DB�JB�JB�JB�\B�oB��B��B��B��B��B��B��B��B�B�B�!B�9B�LB�XB�XB�wB��BƨBɺB��B��B��B��B�B�#B�5B�HB�`B�B�B�B��B��B��B��B	B	%B	DB	PB	hB	hB	{B	�B	�B	�B	�B	�B	#�B	'�B	.B	0!B	1'B	33B	5?B	8RB	<jB	?}B	B�B	C�B	E�B	H�B	I�B	I�B	M�B	P�B	T�B	VB	VB	W
B	XB	\)B	_;B	_;B	_;B	_;B	aHB	e`B	gmB	k�B	m�B	n�B	s�B	u�B	u�B	v�B	x�B	z�B	|�B	}�B	~�B	� B	�B	�B	�+B	�1B	�1B	�7B	�JB	�VB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�-B	�3B	�3B	�3B	�9B	�FB	�LB	�RB	�XB	�XB	�dB	�jB	�qB	�qB	�wB	�wB	�}B	��B	��B	��B	B	B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�;B	�HB	�NB	�TB	�ZB	�`B	�`B	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
1B
	7B
	7B

=B

=B

=B
DB

=B
DB
DB
JB
PB
PB
VB
PB
PB
VB
\B
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
uB
uB
uB
{B
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
-B
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
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
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
A�B
A�B
A�B
B�B
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
D�B
D�B
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
H�B
I�B
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
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
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
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
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
bNB
bNB
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
iyB
iyB
iyB
iyB
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
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
r�B
s�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�yB�yB�B�yB�B�B�B�B�B�B�B�B�B�yBňByB�B;JB>]BE�BI�BH�BJ�BK�BM�BP�BT�BU�BV�BZB\B]B]BgRBjeB|�B�<B�NB��B�sB��Bw�By�B�B�B�B�#B�0B��B|�Bw�Bn}B_!B=VB,�B(�B%�B$�B#�B �B�BBBB��B��B�XB�B��B�	B�	B��BāB��B�HB��Bs�Ba-BM�B9>B)�BaB
��B
��B
�B
�[B
ncB
X�B
N�B
EmB
>]B
3B
(�B
�B
HB	��B	�|B	�_B	�FB	�B	�B	��B	�PB	��B	�<B	y�B	kkB	T�B	)�B	B��B��B�DB��B��B�sB�#B��Bs�B[	BR�BS�BR�BR�BR�BW�B_Bi_Bx�B}�B}�B��B��B}�B{�Bz�Bx�Bw�Bu�Bt�BqvBo�BmwBkQBi_BeFBa-B]BZB[	BX�BU�BU�BT�BU�BS�BS�BV�BS�BP�BP�BK�BF�BBuBC{BAoB@OBB[BAoB>]B=<B=VB@iBC{BE�BDgBEmBD�BAoB<6B<6B:*B9$B9$B9>B9>B:*B;JB<6B<6B;JB9>B8B7B7B6+B5B3B/ B,�B+�B+�B*�B*�B)�B.�B0B/ B/ B.�B-�B+�B*�B'�B'�B'�B)�B)�B(�B(�B(�B'�B&�B&�B$�B&�B(�B(�B(�B(�B)�B*�B,�B0B3B4B5B4B4B4B6B88B:DB;0B<PB?HB@OB@OBC{BF�BGzBI�BJ�BO�BT�BV�BW�B\�B_B_!B`'Be,Bf2BjeBjKBm]Bo�BncBm]Bm]Bm]BncBo�Bp�BqvBu�Bu�By�B}�B~�B��B�B�B�	B�)B�)B�0B�B�B�(B�:B�MB�gB�qB��B��B��B��B��B��B��B��B�B�2B�$B�$B�BB�UBƎBɆBҽB��B��B��B��B��B�B�B�FB�KB�QB�B��B��B��B��B	�B	B	B	6B	NB	4B	aB	YB	_B	kB	~B	�B	#�B	'�B	-�B	0B	1B	3B	5%B	88B	<6B	?cB	BuB	CaB	EmB	H�B	I�B	I�B	M�B	P�B	T�B	U�B	U�B	V�B	W�B	[�B	_!B	_!B	_B	_B	a-B	e,B	gRB	kQB	m]B	ncB	s�B	u�B	u�B	v�B	x�B	z�B	|�B	}�B	~�B	�B	��B	��B	�B	��B	�B	�B	�B	�<B	�.B	�TB	�@B	�gB	�_B	��B	�qB	��B	�qB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�+B	�B	�8B	�$B	�$B	�JB	�PB	�<B	�<B	�BB	�BB	�HB	�iB	�UB	�oB	�[B	�[B	�[B	�{B	�gB	�mB	�mB	ňB	�mB	�tB	ȚB	ɆB	ɆB	ˬB	̘B	οB	��B	ҽB	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�B	�B	� B	�&B	�,B	�,B	�8B	�8B	�>B	�XB	�_B	�KB	�kB	�]B	�]B	�}B	�iB	�iB	�iB	�oB	�B	�B	�vB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
B
�B
�B
B
	B
	B

	B

	B

	B
)B

	B
)B
)B
B
6B
B
"B
6B
B
"B
BB
HB
HB
.B
NB
NB
NB
4B
NB
4B
:B
:B
[B
@B
[B
FB
@B
FB
aB
aB
FB
FB
gB
MB
gB
MB
mB
SB
SB
mB
mB
YB
_B
_B
yB
eB
�B
kB
�B
�B
�B
�B
kB
qB
�B
�B
qB
xB
xB
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
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
+�B
+�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
.�B
.�B
/ B
/�B
/�B
/�B
/�B
/�B
1B
1B
0�B
1�B
1�B
1�B
1�B
1�B
3B
4B
4B
4B
5%B
5%B
5B
5B
6B
6B
6+B
7B
7B
7B
88B
8B
8B
8B
88B
8B
9$B
:DB
:*B
:*B
;0B
;JB
<6B
<6B
<6B
<6B
<PB
<PB
<6B
<PB
<6B
=<B
=<B
=<B
=<B
=VB
>]B
>BB
>BB
>BB
>]B
>]B
?cB
?HB
?HB
?cB
?HB
?cB
?HB
@iB
@OB
@iB
AoB
AUB
AoB
AUB
AUB
B[B
B[B
BuB
B[B
BuB
C{B
CaB
CaB
C{B
D�B
DgB
DgB
D�B
E�B
E�B
E�B
EmB
EmB
E�B
FtB
FtB
FtB
FtB
FtB
GzB
G�B
GzB
GzB
H�B
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
J�B
J�B
J�B
K�B
K�B
K�B
K�B
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
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
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
W�B
X�B
X�B
X�B
X�B
ZB
Y�B
Y�B
ZB
ZB
[	B
Z�B
[	B
Z�B
[	B
Z�B
[	B
[�B
[�B
[�B
[�B
\�B
]B
\�B
]B
]B
\�B
^B
^B
^B
^B
^B
^B
_B
_B
_!B
`B
`B
a-B
a-B
a-B
aB
a-B
a-B
b4B
bB
b4B
bB
bB
b4B
c B
c B
c:B
c B
c B
d@B
d@B
d&B
d&B
d&B
eFB
e,B
eFB
eFB
eFB
e,B
fLB
f2B
f2B
fLB
gRB
gRB
g8B
gRB
g8B
g8B
g8B
gRB
gRB
g8B
g8B
g8B
gRB
g8B
g8B
gRB
hXB
hXB
h>B
hXB
hXB
h>B
i_B
iDB
i_B
i_B
jKB
jeB
jeB
jKB
jeB
kkB
kkB
kkB
kQB
kQB
kkB
kkB
lqB
lWB
lWB
lqB
m]B
mwB
m]B
m]B
mwB
m]B
mwB
m]B
m]B
m]B
ncB
n}B
n}B
ncB
n}B
n}B
n}B
n}B
o�B
oiB
o�B
oiB
oiB
poB
poB
p�B
poB
poB
poB
p�B
p�B
poB
q�B
qvB
q�B
qvB
r|B
r�B
r|B
r�B
r|B
s�B
r|B
r|B
r|B
s�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.32(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804190045532018041900455320180419004553201804261732022018042617320220180426173202JA  ARFMdecpA19c                                                                20180414093516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180414003543  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180414003544  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180414003544  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180414003545  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180414003545  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180414003545  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180414003545  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180414003545  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180414003545                      G�O�G�O�G�O�                JA  ARUP                                                                        20180414005634                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180414154400  CV  JULD            G�O�G�O�F�׮                JM  ARCAJMQC2.0                                                                 20180418154553  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180418154553  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180426083202  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                