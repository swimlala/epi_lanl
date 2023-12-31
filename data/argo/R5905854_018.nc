CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:47:36Z creation;2022-06-04T17:47:36Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604174736  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�ѬB^И1   @�Ѭ��b�@-�������c�-V1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�ffB���B�  B���B�  B�ffB�  B�  B�  B�  B�  B�ffB�  B�33B�ffB�  B���B�  B�  C   C  C  C  C�C
33C�fC�fC  C  C  C  C  C  C  C�fC   C"  C$  C&�C(33C)��C,  C-�fC/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D���D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�C3Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�C3D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�s3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�ffB���B�  B���B�  B�ffB�  B�  B�  B�  B�  B�ffB�  B�33B�ffB�  B���B�  B�  C   C  C  C  C�C
33C�fC�fC  C  C  C  C  C  C  C�fC   C"  C$  C&�C(33C)��C,  C-�fC/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D���D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�C3Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�C3D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�s3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�L0A�LdA�MjA�OBA�O�A�P�A�Q�A�RTA�S[A�T,A�T,A�V9A�V�A�WsA�XEA�X�A�YA�Z�A�\�A�[WA�[�A�\�A�]/A�Q�A��cA���A���A��A��A��WAܰ�A��tA�QA�h�A֜�AՅA���A�l�A�_AӾBA�/�A�@OA�GA�SA��AA�+A�� A�(�A�6�A�{�A��MA��?A�ZA���A�� A�ȴA�tTA�bA��gA���A�d&A��	A��nA���A�
�A�(�A�`BA���A��}A�c A�t�A�x�A�Y�A�<jA�^�A��BA�~�A��A�]�A��A��fA��A}Z�Ay��Aq)_An~�Ai��Ad��A^o�AY�{ASѷAO�XAM}�AJ��AH�~AE�AADgA>�oA<��A<,=A;�A:ߤA:+�A8�~A3�xA/y�A-��A-��A-֡A.��A.��A.��A-��A,��A,�A+uA*c�A)� A'��A&�A$U2A#�1A#@OA#�A"�"A"�EA!��A҉A��A�oA��A}VA��Am]A�$A\�A:�AخA@OA�AcA{JAl�A(�A�A5?A-A�A	lAN<AjA�A�A~�A�A:�Aa�A�A5�A�AخA��A[�AVmA-�A�'A��A)�A��As�A�nA��A��A�1A�fA��A
�A
h�A	�?A	HA�,Ax�A<�A�A��A�)A��A,�A9XA��Al�A�AaAuAN<A�DA��Ag8AFtA ��A ��A J#@���@�F�@��@��q@�o @��@��g@�}V@���@��@�9X@���@���@�4@�c�@��@��@��@��@�_�@�-@��T@�@���@��@�Ĝ@�:*@��W@�n@�S@�X�@�e@��p@�0U@�o@��3@��@���@�a@���@��Z@ߑh@�F�@�@ީ�@�&�@�y�@ܚ�@�0U@�5�@�<�@ٲ-@�'�@ع�@�c @��@�Y�@�  @��N@չ�@�|@���@�W�@��@ө�@�K�@��@�ی@��@�IR@��,@�>B@���@�=�@��@Ίr@�ݘ@͜@�iD@��@��@�;d@��@ʗ�@�R�@��3@�1�@ȕ@�?@ǥ@ƶ�@�q�@�PH@��W@ż�@ŶF@�_p@Ļ�@�g8@�C-@�6@�'R@�O@��@��@�@�!-@��	@�~(@�#:@��T@���@�qv@�4�@���@�_�@��@��$@�33@��1@�@�@��Q@���@�m]@�A�@��@�y>@��o@���@��@���@�PH@��D@�u�@�=@��@�	@���@�X�@�*0@��P@���@�)�@���@�IR@��/@���@��@���@�G�@���@�A�@���@��}@�S&@��c@��@��$@���@���@�V@�v`@�&@�@���@��}@�g�@��@��4@�H@�@���@���@�rG@�N<@�+@��m@��\@�C�@�@�x@��W@��X@�33@���@�~�@�  @��X@�rG@�@��!@�z@�V@��"@��@��@��j@��b@���@�Q@��@���@��@�O@�(�@��@��@�oi@��j@���@��"@���@�q@�4@��D@��>@���@�^�@�8�@��c@�d�@���@���@�_p@��@���@�q�@��@���@���@�X@��c@�z�@�Ft@�0U@���@���@��&@��@��t@��	@��@��D@�z@�p;@�h�@��@��h@� \@��M@��I@��@���@���@��4@�Y�@�8�@�(�@�+@��M@���@�7�@��&@���@��0@�}�@�!-@��P@�ں@��O@���@��b@��r@�h�@�B[@��;@���@�G�@� \@���@�m�@�1'@��@�_@��#@���@�|@�W?@�2a@��@��p@���@�{�@�Xy@�7@�S&@�&�@��"@��?@�h
@�u@���@��6@�@���@�]�@��+@�B[@��@���@��Q@���@��X@��7@�'�@�;@���@�6�@�	�@��@��3@��~@�K�@���@���@��o@�l"@�:*@�!@���@���@���@��@�Ĝ@��6@�w�@�U2@�9X@�!�@��Q@���@�`B@�^�@�Q�@�/�@�
=@��@���@�C-@�(�@�	@��z@�o�@��@��@�8�@�{@�W@��@iD@Y@~-@|ی@|��@|_@|~@{��@{dZ@{�@z��@z~�@z�@y��@y�~@ya�@y5�@y@x�@vߤ@vkQ@vW�@u�D@u[W@u�@t]d@s�@s��@s˒@s�@s�w@s��@s�4@r�<@rV@q��@p��@p7�@o�}@odZ@nYK@m��@m�S@m!�@l��@l,=@kJ#@j�L@j��@j{�@j�@i��@i�d@i�3@i^�@h�[@h2�@g��@g�@g��@gn/@gO@gS@fz@f�@ew2@d��@c��@c(@b�'@bW�@a��@a`B@`�U@`�@_U�@^�}@^6�@]��@]X@\Ĝ@\�@[��@[_p@Z�@Zq�@Y�@X��@X��@XU2@XH@X  @W�w@W�@@WMj@Vȴ@VZ�@U�.@U��@U5�@U%@T�?@T7�@S��@S4�@R�B@ROv@Q}�@Qq@P�@P�4@P�@PA�@O��@Os@O>�@O
=@N�y@N��@N�1@NL0@M��@L�@L��@LZ@K�
@Kn/@J��@J��@I�@IA @I�@H��@HbN@HQ�@H9X@G�r@G��@Gx@F�@F��@F}V@F($@E�H@EG�@D�f@D~(@D>B@D~@C�+@C�A@Cƨ@C|�@C+@B�m@B6�@B �@A�)@A�@AN<@A�@@�@@�@@�@@I�@?�r@?�$@?n/@>�"@>��@>v�@>L0@=��@=4@=@<�	@<�/@<�@<V�@<A�@<@<�@;�+@;�@;�@:��@:B[@:�@:e@:4@9��@9��@9��@9�@9=�@8�@8��@8oi@8G@7��@7s@7�@6��@6^5@6#:@5��@5f�@5<6@5q@4�K@4�Y@4 �@3v`@3C�@36z@3)_@3o@2��@2@�@1�@1Q�@0�P@0�j@0�@0q@0m�@0c�@09X@/�@/��@/U�@/(@.�,@.�h@.�r@.E�@.5?@.O@-�=@-Q�@- \@-�@,�K@,Ĝ@,b@+E9@+�@+�@*�@*z@*d�@*Z�@*J@)}�@)Q�@)a�@)B�@)#�@)�@(�	@(��@(��@(z�@(Ft@(1'@(7@(�@'�w@'j�@'P�@&��@&�,@&�m@&��@&�\@&Z�@%�@%�7@%2a@$z�@$Z@$C-@$b@#�]@#�@#�K@#��@#\)@#
=@"�x@"($@"
�@!�.@!��@!�@!k�@ �f@ ��@ [�@ D�@ @ݘ@�@Z�@C�@�"@�@�X@��@�r@Ta@	@�H@IR@#�@+@�@��@�@�p@��@��@Q�@�@�&@��@�P@�@qv@@O@�@ i@��@�h@~�@d�@:*@�@�h@Dg@4@ \@�@�@�@��@�4@I�@'R@!@�&@��@��@��@�{@U�@�@��@q�@6�@�@@�@�n@�@o @f�@T�@-w@	l@�@�[@��@|�@PH@"h@�@��@��@x@X�@H�@33@�@�2@�!@�A@d�@^5@Q@J�@1�@@��@ϫ@�@Y�@�@�@�[@�O@��@g8@"h@�]@�Q@�0@�@P�@o@�c@�@��@�@��@xl@YK@4@�@��@��@��@�"@}�@m]@`B@=�@�f@��@��@�D@�.@�.@��@�Y@��@�@j@`�@A�@@�@�]@�g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�L0A�LdA�MjA�OBA�O�A�P�A�Q�A�RTA�S[A�T,A�T,A�V9A�V�A�WsA�XEA�X�A�YA�Z�A�\�A�[WA�[�A�\�A�]/A�Q�A��cA���A���A��A��A��WAܰ�A��tA�QA�h�A֜�AՅA���A�l�A�_AӾBA�/�A�@OA�GA�SA��AA�+A�� A�(�A�6�A�{�A��MA��?A�ZA���A�� A�ȴA�tTA�bA��gA���A�d&A��	A��nA���A�
�A�(�A�`BA���A��}A�c A�t�A�x�A�Y�A�<jA�^�A��BA�~�A��A�]�A��A��fA��A}Z�Ay��Aq)_An~�Ai��Ad��A^o�AY�{ASѷAO�XAM}�AJ��AH�~AE�AADgA>�oA<��A<,=A;�A:ߤA:+�A8�~A3�xA/y�A-��A-��A-֡A.��A.��A.��A-��A,��A,�A+uA*c�A)� A'��A&�A$U2A#�1A#@OA#�A"�"A"�EA!��A҉A��A�oA��A}VA��Am]A�$A\�A:�AخA@OA�AcA{JAl�A(�A�A5?A-A�A	lAN<AjA�A�A~�A�A:�Aa�A�A5�A�AخA��A[�AVmA-�A�'A��A)�A��As�A�nA��A��A�1A�fA��A
�A
h�A	�?A	HA�,Ax�A<�A�A��A�)A��A,�A9XA��Al�A�AaAuAN<A�DA��Ag8AFtA ��A ��A J#@���@�F�@��@��q@�o @��@��g@�}V@���@��@�9X@���@���@�4@�c�@��@��@��@��@�_�@�-@��T@�@���@��@�Ĝ@�:*@��W@�n@�S@�X�@�e@��p@�0U@�o@��3@��@���@�a@���@��Z@ߑh@�F�@�@ީ�@�&�@�y�@ܚ�@�0U@�5�@�<�@ٲ-@�'�@ع�@�c @��@�Y�@�  @��N@չ�@�|@���@�W�@��@ө�@�K�@��@�ی@��@�IR@��,@�>B@���@�=�@��@Ίr@�ݘ@͜@�iD@��@��@�;d@��@ʗ�@�R�@��3@�1�@ȕ@�?@ǥ@ƶ�@�q�@�PH@��W@ż�@ŶF@�_p@Ļ�@�g8@�C-@�6@�'R@�O@��@��@�@�!-@��	@�~(@�#:@��T@���@�qv@�4�@���@�_�@��@��$@�33@��1@�@�@��Q@���@�m]@�A�@��@�y>@��o@���@��@���@�PH@��D@�u�@�=@��@�	@���@�X�@�*0@��P@���@�)�@���@�IR@��/@���@��@���@�G�@���@�A�@���@��}@�S&@��c@��@��$@���@���@�V@�v`@�&@�@���@��}@�g�@��@��4@�H@�@���@���@�rG@�N<@�+@��m@��\@�C�@�@�x@��W@��X@�33@���@�~�@�  @��X@�rG@�@��!@�z@�V@��"@��@��@��j@��b@���@�Q@��@���@��@�O@�(�@��@��@�oi@��j@���@��"@���@�q@�4@��D@��>@���@�^�@�8�@��c@�d�@���@���@�_p@��@���@�q�@��@���@���@�X@��c@�z�@�Ft@�0U@���@���@��&@��@��t@��	@��@��D@�z@�p;@�h�@��@��h@� \@��M@��I@��@���@���@��4@�Y�@�8�@�(�@�+@��M@���@�7�@��&@���@��0@�}�@�!-@��P@�ں@��O@���@��b@��r@�h�@�B[@��;@���@�G�@� \@���@�m�@�1'@��@�_@��#@���@�|@�W?@�2a@��@��p@���@�{�@�Xy@�7@�S&@�&�@��"@��?@�h
@�u@���@��6@�@���@�]�@��+@�B[@��@���@��Q@���@��X@��7@�'�@�;@���@�6�@�	�@��@��3@��~@�K�@���@���@��o@�l"@�:*@�!@���@���@���@��@�Ĝ@��6@�w�@�U2@�9X@�!�@��Q@���@�`B@�^�@�Q�@�/�@�
=@��@���@�C-@�(�@�	@��z@�o�@��@��@�8�@�{@�W@��@iD@Y@~-@|ی@|��@|_@|~@{��@{dZ@{�@z��@z~�@z�@y��@y�~@ya�@y5�@y@x�@vߤ@vkQ@vW�@u�D@u[W@u�@t]d@s�@s��@s˒@s�@s�w@s��@s�4@r�<@rV@q��@p��@p7�@o�}@odZ@nYK@m��@m�S@m!�@l��@l,=@kJ#@j�L@j��@j{�@j�@i��@i�d@i�3@i^�@h�[@h2�@g��@g�@g��@gn/@gO@gS@fz@f�@ew2@d��@c��@c(@b�'@bW�@a��@a`B@`�U@`�@_U�@^�}@^6�@]��@]X@\Ĝ@\�@[��@[_p@Z�@Zq�@Y�@X��@X��@XU2@XH@X  @W�w@W�@@WMj@Vȴ@VZ�@U�.@U��@U5�@U%@T�?@T7�@S��@S4�@R�B@ROv@Q}�@Qq@P�@P�4@P�@PA�@O��@Os@O>�@O
=@N�y@N��@N�1@NL0@M��@L�@L��@LZ@K�
@Kn/@J��@J��@I�@IA @I�@H��@HbN@HQ�@H9X@G�r@G��@Gx@F�@F��@F}V@F($@E�H@EG�@D�f@D~(@D>B@D~@C�+@C�A@Cƨ@C|�@C+@B�m@B6�@B �@A�)@A�@AN<@A�@@�@@�@@�@@I�@?�r@?�$@?n/@>�"@>��@>v�@>L0@=��@=4@=@<�	@<�/@<�@<V�@<A�@<@<�@;�+@;�@;�@:��@:B[@:�@:e@:4@9��@9��@9��@9�@9=�@8�@8��@8oi@8G@7��@7s@7�@6��@6^5@6#:@5��@5f�@5<6@5q@4�K@4�Y@4 �@3v`@3C�@36z@3)_@3o@2��@2@�@1�@1Q�@0�P@0�j@0�@0q@0m�@0c�@09X@/�@/��@/U�@/(@.�,@.�h@.�r@.E�@.5?@.O@-�=@-Q�@- \@-�@,�K@,Ĝ@,b@+E9@+�@+�@*�@*z@*d�@*Z�@*J@)}�@)Q�@)a�@)B�@)#�@)�@(�	@(��@(��@(z�@(Ft@(1'@(7@(�@'�w@'j�@'P�@&��@&�,@&�m@&��@&�\@&Z�@%�@%�7@%2a@$z�@$Z@$C-@$b@#�]@#�@#�K@#��@#\)@#
=@"�x@"($@"
�@!�.@!��@!�@!k�@ �f@ ��@ [�@ D�@ @ݘ@�@Z�@C�@�"@�@�X@��@�r@Ta@	@�H@IR@#�@+@�@��@�@�p@��@��@Q�@�@�&@��@�P@�@qv@@O@�@ i@��@�h@~�@d�@:*@�@�h@Dg@4@ \@�@�@�@��@�4@I�@'R@!@�&@��@��@��@�{@U�@�@��@q�@6�@�@@�@�n@�@o @f�@T�@-w@	l@�@�[@��@|�@PH@"h@�@��@��@x@X�@H�@33@�@�2@�!@�A@d�@^5@Q@J�@1�@@��@ϫ@�@Y�@�@�@�[@�O@��@g8@"h@�]@�Q@�0@�@P�@o@�c@�@��@�@��@xl@YK@4@�@��@��@��@�"@}�@m]@`B@=�@�f@��@��@�D@�.@�.@��@�Y@��@�@j@`�@A�@@�@�]@�g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B�B��B�B��B�B�B�B��B��B�B�B�B�B�B�mB��B�B�mB�SB�SB�?B�B��B	+B	>�B	ncB	��B	�_B	~wB	FB	=�B	"4B	B	�B	fB	�B�(B�wB�B��B	%�B	4B	9$B	Q�B	l�B	p!B	z�B	��B
6�B
`BB
shB
�B
��B
~BB
x�B
e`B
OBB
YB
cTB
�B
�vB
��B
�tB
��B
}�B
|6B
�B
'�B
MB
HB
�VB
��B
�B
��B
�=B
{�B
Q4B
'mB
�B	�B	��B	�B	��B	~B	\�B	9�B	�B��B߾B�"B�B��B�B�B�ZB�B�=B�;B��B��B�%BˬB��B��B�;B�'B��B��B��B��B��B�mB��B��B��B�*B�yB�B�!B��B�;B�B�B�cB�B��B��B�4B��B��B�B�iB�LB�B�]B	B	B	�B	]B	#:B	+B	.�B	7B	@ B	I7B	NVB	R�B	\�B	xB	.B	�B	�B	��B	�-B	��B	�'B	��B	��B	��B	��B	��B	�qB	�iB	��B	��B	�ZB	��B	��B	��B	�2B	��B	��B	�B	��B	�qB	�"B	�6B	��B	�B	��B	��B	��B	��B	��B	�[B	�gB	��B	ʌB	ȚB	��B	�fB	�rB	��B	�xB	˒B	�0B	�dB	��B	�B	�"B	�B	�hB	�HB	�bB	�uB	��B	ңB	ѝB	�hB	��B	�oB	ҽB	�&B	�B	� B	ЗB	�bB	� B	ѷB	�hB	� B	� B	�bB	�}B	�}B	��B	ЗB	�}B	��B	��B	��B	ϑB	� B	�B	бB	�.B	�.B	��B	бB	ѝB	�@B	յB	ՁB	֡B	ևB	ּB	�mB	�gB	�2B	�B	�2B	՛B	ՁB	յB	�B	�B	�B	�yB	ؓB	��B	�KB	ڠB	یB	ۦB	یB	�CB	�)B	��B	�B	�/B	�~B	�5B	�jB	��B	޸B	�!B	߾B	�pB	�!B	�!B	��B	�pB	�pB	�pB	߾B	�B	�B	��B	�-B	�bB	��B	�vB	�BB	��B	��B	�BB	�B	�B	�B	��B	��B	��B	��B	�B	�bB	�4B	�B	�B	�nB	�nB	�B	�B	�B	��B	�B	�tB	��B	��B	�B	�B	�2B	�B	��B	�B	�B	�B	�
B	�B	��B	�_B	��B	��B	�KB	�B	��B	��B	�B	��B	��B	�)B	�CB	��B	�B	��B	��B	�B	�iB	�UB	�B	�'B	��B	�aB	�B	�B	�hB	�B	��B	��B	��B	��B	��B	�tB	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	�	B	��B	�*B	�JB	�dB	��B	��B	��B	��B	��B	�B	��B	��B	�VB	�B	��B	�"B	��B	�(B	�(B	�(B	��B	��B	��B
 4B
 �B
UB
;B
�B
�B
�B
�B
�B
B
�B
�B
3B
MB
MB
B
?B
tB
tB
tB
+B
B
B
_B
fB
�B
	B
	�B

XB

�B
DB
�B
�B
�B
0B
�B
6B
PB
PB
�B
�B
�B
�B
�B
�B
�B
�B
pB
<B
�B
�B
�B
.B
HB
�B
�B
B
 B
 B
�B
�B
B
B
uB
[B
�B
aB
{B
�B
MB
2B
B
�B
B

B

B

B

B
?B

B

B
�B

B
�B
B
EB
EB
�B
�B
1B
eB
B
KB
�B
7B
�B
�B
�B
�B
�B
WB
WB
qB
qB
�B
)B
�B
IB
�B
�B
�B
�B
B
5B
5B
�B
;B
�B
!-B
!bB
!�B
!-B
!B
!bB
!|B
!�B
"NB
#�B
$�B
$�B
%B
%`B
%zB
'B
'8B
'mB
'mB
'mB
'�B
'�B
($B
(sB
(sB
(�B
(�B
)*B
)�B
*�B
+QB
+6B
+6B
+B
+�B
-B
-�B
.�B
/B
.IB
-�B
-�B
-�B
.B
-�B
.cB
.cB
/�B
0�B
0�B
0�B
0�B
1'B
1AB
1[B
1[B
1�B
2-B
2-B
3B
3�B
3�B
3MB
3�B
3�B
3�B
3�B
3�B
4B
49B
4�B
4�B
5B
5B
5%B
5?B
5%B
5tB
6+B
5�B
6�B
6�B
6�B
6�B
7LB
7�B
88B
8�B
8�B
9$B
9�B
:�B
:�B
:xB
:�B
:�B
:�B
:�B
:�B
;0B
;�B
<�B
<�B
=B
="B
=<B
=<B
=VB
=�B
=�B
=�B
>(B
>�B
>�B
>�B
?B
?�B
@�B
@�B
AB
AoB
A�B
A�B
B'B
B�B
C-B
C�B
D�B
E�B
F%B
G�B
IB
IRB
I�B
J#B
J	B
J=B
J�B
JXB
J�B
J�B
K)B
K^B
K�B
K�B
K�B
K�B
K�B
L~B
L~B
L�B
L�B
MjB
M�B
NB
NB
NB
NpB
N�B
N�B
OB
O(B
OBB
OBB
O\B
O\B
P.B
P�B
P�B
P�B
QNB
Q�B
RB
Q�B
R�B
R�B
R�B
S@B
S[B
SuB
SuB
S�B
S�B
S�B
TaB
TFB
T{B
T�B
T�B
U2B
U�B
U�B
VB
VB
VB
VB
VB
VmB
V�B
V�B
WYB
WsB
WYB
WsB
W�B
X+B
X+B
XB
X_B
X_B
X�B
YB
YB
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[WB
[WB
[�B
[qB
[WB
[�B
\)B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]B
]IB
]�B
]�B
]�B
^B
^OB
^jB
^�B
_B
_�B
_�B
`BB
`BB
`vB
`vB
`�B
`�B
a�B
a|B
a�B
a�B
a|B
a|B
a�B
bB
bNB
cnB
cnB
c�B
c�B
d@B
dtB
d�B
eB
d�B
e,B
eB
ezB
ezB
e�B
f2B
fLB
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
iB
iyB
i_B
iyB
iyB
i�B
i�B
jKB
j0B
jB
i�B
j0B
jB
i�B
j0B
jB
j�B
j�B
j�B
j�B
jB
j�B
jB
jeB
jeB
jB
jB
j�B
k6B
kkB
kkB
k�B
l"B
l=B
lWB
lWB
lWB
lqB
lqB
l�B
l�B
m]B
m�B
m�B
m�B
m�B
m�B
n/B
n}B
o B
o B
oB
o5B
o�B
o�B
pB
p;B
p�B
p�B
p�B
p�B
qB
qAB
q[B
q�B
q�B
rB
rB
rB
r-B
r-B
rGB
rGB
raB
r|B
r�B
sB
s�B
s�B
s�B
tB
tB
tTB
t9B
tnB
t�B
t�B
t�B
uB
u%B
u�B
vFB
v�B
v�B
v�B
v�B
wB
wLB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
xB
x8B
x8B
xlB
x�B
x�B
x�B
y	B
y	B
y$B
y>B
y>B
yrB
y�B
y�B
y�B
y�B
y�B
z*B
z^B
zDB
z*B
z^B
zxB
z�B
z�B
z�B
{0B
{�B
{�B
|B
|B
|B
|B
|B
|6B
|6B
|PB
|�B
|�B
|�B
|�B
|�B
|�B
}B
|�B
}B
}<B
}<B
}qB
}qB
}VB
}�B
}�B
}qB
}�B
}�B
}�B
}�B
~BB
~wB
~wB
~]B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
.B
�B
�B
�B
�B
� B
��B
��B
��B
�B
� B
��B
��B
��B
�'B
�AB
�uB
�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B�B��B�B��B�B�B�B��B��B�B�B�B�B�B�mB��B�B�mB�SB�SB�?B�B��B	+B	>�B	ncB	��B	�_B	~wB	FB	=�B	"4B	B	�B	fB	�B�(B�wB�B��B	%�B	4B	9$B	Q�B	l�B	p!B	z�B	��B
6�B
`BB
shB
�B
��B
~BB
x�B
e`B
OBB
YB
cTB
�B
�vB
��B
�tB
��B
}�B
|6B
�B
'�B
MB
HB
�VB
��B
�B
��B
�=B
{�B
Q4B
'mB
�B	�B	��B	�B	��B	~B	\�B	9�B	�B��B߾B�"B�B��B�B�B�ZB�B�=B�;B��B��B�%BˬB��B��B�;B�'B��B��B��B��B��B�mB��B��B��B�*B�yB�B�!B��B�;B�B�B�cB�B��B��B�4B��B��B�B�iB�LB�B�]B	B	B	�B	]B	#:B	+B	.�B	7B	@ B	I7B	NVB	R�B	\�B	xB	.B	�B	�B	��B	�-B	��B	�'B	��B	��B	��B	��B	��B	�qB	�iB	��B	��B	�ZB	��B	��B	��B	�2B	��B	��B	�B	��B	�qB	�"B	�6B	��B	�B	��B	��B	��B	��B	��B	�[B	�gB	��B	ʌB	ȚB	��B	�fB	�rB	��B	�xB	˒B	�0B	�dB	��B	�B	�"B	�B	�hB	�HB	�bB	�uB	��B	ңB	ѝB	�hB	��B	�oB	ҽB	�&B	�B	� B	ЗB	�bB	� B	ѷB	�hB	� B	� B	�bB	�}B	�}B	��B	ЗB	�}B	��B	��B	��B	ϑB	� B	�B	бB	�.B	�.B	��B	бB	ѝB	�@B	յB	ՁB	֡B	ևB	ּB	�mB	�gB	�2B	�B	�2B	՛B	ՁB	յB	�B	�B	�B	�yB	ؓB	��B	�KB	ڠB	یB	ۦB	یB	�CB	�)B	��B	�B	�/B	�~B	�5B	�jB	��B	޸B	�!B	߾B	�pB	�!B	�!B	��B	�pB	�pB	�pB	߾B	�B	�B	��B	�-B	�bB	��B	�vB	�BB	��B	��B	�BB	�B	�B	�B	��B	��B	��B	��B	�B	�bB	�4B	�B	�B	�nB	�nB	�B	�B	�B	��B	�B	�tB	��B	��B	�B	�B	�2B	�B	��B	�B	�B	�B	�
B	�B	��B	�_B	��B	��B	�KB	�B	��B	��B	�B	��B	��B	�)B	�CB	��B	�B	��B	��B	�B	�iB	�UB	�B	�'B	��B	�aB	�B	�B	�hB	�B	��B	��B	��B	��B	��B	�tB	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	�	B	��B	�*B	�JB	�dB	��B	��B	��B	��B	��B	�B	��B	��B	�VB	�B	��B	�"B	��B	�(B	�(B	�(B	��B	��B	��B
 4B
 �B
UB
;B
�B
�B
�B
�B
�B
B
�B
�B
3B
MB
MB
B
?B
tB
tB
tB
+B
B
B
_B
fB
�B
	B
	�B

XB

�B
DB
�B
�B
�B
0B
�B
6B
PB
PB
�B
�B
�B
�B
�B
�B
�B
�B
pB
<B
�B
�B
�B
.B
HB
�B
�B
B
 B
 B
�B
�B
B
B
uB
[B
�B
aB
{B
�B
MB
2B
B
�B
B

B

B

B

B
?B

B

B
�B

B
�B
B
EB
EB
�B
�B
1B
eB
B
KB
�B
7B
�B
�B
�B
�B
�B
WB
WB
qB
qB
�B
)B
�B
IB
�B
�B
�B
�B
B
5B
5B
�B
;B
�B
!-B
!bB
!�B
!-B
!B
!bB
!|B
!�B
"NB
#�B
$�B
$�B
%B
%`B
%zB
'B
'8B
'mB
'mB
'mB
'�B
'�B
($B
(sB
(sB
(�B
(�B
)*B
)�B
*�B
+QB
+6B
+6B
+B
+�B
-B
-�B
.�B
/B
.IB
-�B
-�B
-�B
.B
-�B
.cB
.cB
/�B
0�B
0�B
0�B
0�B
1'B
1AB
1[B
1[B
1�B
2-B
2-B
3B
3�B
3�B
3MB
3�B
3�B
3�B
3�B
3�B
4B
49B
4�B
4�B
5B
5B
5%B
5?B
5%B
5tB
6+B
5�B
6�B
6�B
6�B
6�B
7LB
7�B
88B
8�B
8�B
9$B
9�B
:�B
:�B
:xB
:�B
:�B
:�B
:�B
:�B
;0B
;�B
<�B
<�B
=B
="B
=<B
=<B
=VB
=�B
=�B
=�B
>(B
>�B
>�B
>�B
?B
?�B
@�B
@�B
AB
AoB
A�B
A�B
B'B
B�B
C-B
C�B
D�B
E�B
F%B
G�B
IB
IRB
I�B
J#B
J	B
J=B
J�B
JXB
J�B
J�B
K)B
K^B
K�B
K�B
K�B
K�B
K�B
L~B
L~B
L�B
L�B
MjB
M�B
NB
NB
NB
NpB
N�B
N�B
OB
O(B
OBB
OBB
O\B
O\B
P.B
P�B
P�B
P�B
QNB
Q�B
RB
Q�B
R�B
R�B
R�B
S@B
S[B
SuB
SuB
S�B
S�B
S�B
TaB
TFB
T{B
T�B
T�B
U2B
U�B
U�B
VB
VB
VB
VB
VB
VmB
V�B
V�B
WYB
WsB
WYB
WsB
W�B
X+B
X+B
XB
X_B
X_B
X�B
YB
YB
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[WB
[WB
[�B
[qB
[WB
[�B
\)B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]B
]IB
]�B
]�B
]�B
^B
^OB
^jB
^�B
_B
_�B
_�B
`BB
`BB
`vB
`vB
`�B
`�B
a�B
a|B
a�B
a�B
a|B
a|B
a�B
bB
bNB
cnB
cnB
c�B
c�B
d@B
dtB
d�B
eB
d�B
e,B
eB
ezB
ezB
e�B
f2B
fLB
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
iB
iyB
i_B
iyB
iyB
i�B
i�B
jKB
j0B
jB
i�B
j0B
jB
i�B
j0B
jB
j�B
j�B
j�B
j�B
jB
j�B
jB
jeB
jeB
jB
jB
j�B
k6B
kkB
kkB
k�B
l"B
l=B
lWB
lWB
lWB
lqB
lqB
l�B
l�B
m]B
m�B
m�B
m�B
m�B
m�B
n/B
n}B
o B
o B
oB
o5B
o�B
o�B
pB
p;B
p�B
p�B
p�B
p�B
qB
qAB
q[B
q�B
q�B
rB
rB
rB
r-B
r-B
rGB
rGB
raB
r|B
r�B
sB
s�B
s�B
s�B
tB
tB
tTB
t9B
tnB
t�B
t�B
t�B
uB
u%B
u�B
vFB
v�B
v�B
v�B
v�B
wB
wLB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
xB
x8B
x8B
xlB
x�B
x�B
x�B
y	B
y	B
y$B
y>B
y>B
yrB
y�B
y�B
y�B
y�B
y�B
z*B
z^B
zDB
z*B
z^B
zxB
z�B
z�B
z�B
{0B
{�B
{�B
|B
|B
|B
|B
|B
|6B
|6B
|PB
|�B
|�B
|�B
|�B
|�B
|�B
}B
|�B
}B
}<B
}<B
}qB
}qB
}VB
}�B
}�B
}qB
}�B
}�B
}�B
}�B
~BB
~wB
~wB
~]B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
.B
�B
�B
�B
�B
� B
��B
��B
��B
�B
� B
��B
��B
��B
�'B
�AB
�uB
�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104942  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174736  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174736  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174736                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024744  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024744  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                