CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:21:54Z creation;2022-06-04T19:21:54Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
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
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192154  20220610151509  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               @A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�FN�Q�1   @�FN}�u1@/@�n���cz�x���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B��B��B  B*ffB.  B8��B>  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�ffB���B���B�  B�  B�  B�  B�ffB�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C�C�3C��C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C833C:  C<  C>  C@  CB  CC�fCE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~�fD  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D��3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B��B��B  B*ffB.  B8��B>  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�ffB���B���B�  B�  B�  B�  B�ffB�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C�C�3C��C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C833C:  C<  C>  C@  CB  CC�fCE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~�fD  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D��3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A��A��
A���A���A���A��"A��A��8A���A���A��A���A���A��}A�ɺA�y>A�	A���A�8RA��KAѶzA��A�p�A��BA� �A˨�A�c A��A�A��A��A���A�H�A��A�!�A�҉A�9�A�w�A�� A��jA��A��A��nA��A���A���A�ȴA�s�A��A�hA�LdA�JXA��rA�IA��-A��A�_A�A���A��8A��VA�O�A��&A��fA�oA�@�A�5tA���A~�PAy��AvRTAt0�Ao�Aj��Af��Ab#:A`�aA^��AZ�OAT=�AR��AP��AOM�AN�MAN�nAL��AI��AG��AE�AB��A@�|A=�'A<��A>��A>��A=�A7�A4iDA2�.A1[WA0<6A-ĜA+��A(�A"�A!��A"��A%Q�A&D�A&y>A&tTA&2�A%��A%�9A%ȴA%��A%�-A%DgA%.IA$��A$>BA#bNA"��A"��A"XA!�fA!\�A!o�A!��A!�A!�)A!��A!!�A�2AA��A��A�<A��A U�A!A ��A ��A�AA+A�hA��Af�A�A�A��A~Ax�A�A|A{A�A�A�rA?}A�)A\�AȴAA$�Ap;AxAbNA�_A�IA�3A(AA<�AT�A+kA�)AݘA�_A(�A�}A�eAy�A�.A��AA�4AqA[WAB�A33A)_A�rA��A��A�A�Au�A	A҉AzASA
v`A
XA	�cA	��A	Z�A��A��An/A��A��AaA&�A�jA�AS&A��A��An/AA�A_AK�A4�A�A�hA�Aa�A ��A ��A �@�w�@�IR@�֡@��h@�Dg@�j�@��@��@�IR@��@�hs@���@��"@���@�b�@�@�J�@�q@�o @���@땁@�e@�	@�\)@��U@�H@�u�@�{�@��6@�@��@�s�@�1'@�h�@�e�@�_�@�%�@�%F@���@�1�@�U�@ބ�@�
�@�<6@��`@ܓu@���@�8�@�q�@��@���@وf@�X@�-w@�7L@��@ؠ�@�V�@�%�@��r@׿H@�\�@�@�[�@ա�@�S&@�C@��H@��@ԟ�@�k�@��`@�2�@ёh@ЯO@��@�/�@κ�@�Z@� �@�}�@�V�@�l�@�.I@���@���@ɪ�@��@�*�@�1�@�}V@��@Ŋ	@�{@�H�@��	@·�@§@��@���@��m@�f�@�F@��@��h@�D�@��@���@�B�@���@�$�@��@�Q�@��@��!@��@��d@��@���@�$@���@�v`@�Dg@��@���@���@�?@��@��@�x�@�\�@�Mj@��@���@�3�@��@��@���@���@�e,@���@�x@�}�@�e�@�_p@�S�@��m@��@��H@��X@�v`@�?}@��E@��1@���@�p;@�V�@�:*@��
@��@��{@�@O@�+@�Ɇ@�n�@�ݘ@���@���@���@�Dg@��2@��O@�Z�@�x�@��@�Ĝ@���@���@�|�@�3�@�� @��@��@�c�@���@��@�h
@�8�@���@�H�@��@���@���@��@�x@��@�i�@�~@��@���@�J#@�*0@��<@�kQ@�GE@��Z@���@��7@�m]@�7L@��v@�h�@��]@���@�C�@��@��@��o@�"h@��@���@�Mj@��@�1�@��@��P@�_p@�4�@��@��@���@���@�v�@��@���@�y�@�F�@��@��]@���@��9@���@�Z�@�6�@�($@��@��@���@���@���@�f�@�-w@��@��h@�v�@�*�@���@�m]@�/@��@��$@�l"@�C-@���@���@�s�@�X@�Dg@�,�@��2@���@�e�@�,=@��Q@��4@�7L@�4@�+@��@��2@��)@���@�tT@�'R@��@��@��~@�F@��@��@��@���@��@�tT@�z�@�M@���@��
@�zx@�J�@��@��@�~(@�1�@�1@��@�p�@�/�@��@���@��s@���@��&@���@���@���@�p�@�;d@�C@��@���@���@�C-@�"h@��@���@��	@��@�;�@�&�@�@��@�� @���@��=@�f�@�@��'@��4@��b@�kQ@��@��g@���@�iD@�B�@�ی@��<@��@���@�|�@�c�@�A@P�@�@~�@~�r@}��@|��@|M@{��@{��@{e�@{o@z��@z^5@z �@y�t@y��@y=�@x�v@x�U@xq@x@w��@w@O@vTa@u�@u?}@u%@t�[@tFt@t�@s��@s��@se�@sE9@rW�@q�@q8�@p��@p֡@pm�@p%�@o�g@n�@nz@n?@n{@m�@me,@m	l@l��@l�_@l��@l|�@lC-@l�@k��@ke�@kJ#@k.I@jYK@jH�@j&�@i�@i��@iQ�@h�@h��@g��@g�$@gF�@f�2@f�X@f��@fYK@f;�@e��@es�@e-w@d֡@d�_@d1'@cخ@c��@c��@b�8@bv�@a�=@a*0@`��@`r�@_�@_j�@^�B@^��@^3�@]�@]�@]J�@\�U@\�@[��@[H�@[(@Zq�@Z�@Y�@YT�@X��@Xu�@W�&@Wa@WS�@WC�@W)_@V�\@U�@T��@T6@S��@S8@R��@Rv�@RC�@R �@Q�H@Q�=@Q \@P[�@O�A@O�*@O��@O!-@O@N�B@M�T@Mc@MO�@M�@L��@LA�@K��@K8@J��@J��@J1�@I�n@I!�@Hr�@H	�@G�F@F�H@FV@F4@Ej@D��@Dc�@DS�@D<�@D�@D�@C�m@C��@C>�@Bں@Bxl@A�n@A*0@@e�@@ �@@@@�@?�g@?P�@>�@>��@>($@=��@=��@=��@=0�@<�@<@<*�@<G@;��@;g�@;dZ@;_p@;C�@;�@:�B@:�\@9��@9G�@8�/@8�O@8�@87�@7�A@7��@7v`@7U�@7O@7=@78@76z@78@7/�@7�@6�M@6�@6͟@6�'@6\�@5�)@5�t@5�S@5|@5J�@5�@4�U@4�@4�.@4�Y@3�]@3�K@3�@3t�@3@2�r@2H�@2�@1�^@1�S@12a@0�@0�Y@0D�@/�@/�w@/t�@.��@.ff@.Ta@.�@-ԕ@-�~@-hs@-&�@,�@,u�@,N�@,�@+�&@+�q@+X�@*� @*;�@*?@)��@)��@)s�@)%@(��@(N�@(!@(�@(  @'�m@'�@'H�@'+@'�@'(@&��@%��@%�@%��@%�7@%\�@%F@%<6@%+�@$��@$g8@$/�@#�m@#�w@#Mj@"�@"��@";�@!�d@!J�@!#�@ ��@ �9@ u�@ M@�Q@�F@��@��@X�@+@�@��@c @+k@�3@Vm@��@�e@<�@�m@�@��@j�@J#@�@�F@$�@�@�)@�H@�~@|@o @a�@@ی@tT@N�@:�@1@�@�V@�f@{J@]�@�@�2@�<@��@q�@Z�@Ov@H�@;�@!�@��@��@��@�-@�@c@o @hs@8�@�|@��@��@�.@Xy@~@�@�Q@�@o�@@�@�,@��@~�@?@�@��@��@��@�=@�"@o @+@�@��@��@_@*�@	�@�]@��@�@�@��@��@��@�{@dZ@F�@�@
=@@�@��@u%@;�@
�@�N@��@�h@rG@Q�@V@�/@w�@bN@U2@M@S�@U211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A��A��
A���A���A���A��"A��A��8A���A���A��A���A���A��}A�ɺA�y>A�	A���A�8RA��KAѶzA��A�p�A��BA� �A˨�A�c A��A�A��A��A���A�H�A��A�!�A�҉A�9�A�w�A�� A��jA��A��A��nA��A���A���A�ȴA�s�A��A�hA�LdA�JXA��rA�IA��-A��A�_A�A���A��8A��VA�O�A��&A��fA�oA�@�A�5tA���A~�PAy��AvRTAt0�Ao�Aj��Af��Ab#:A`�aA^��AZ�OAT=�AR��AP��AOM�AN�MAN�nAL��AI��AG��AE�AB��A@�|A=�'A<��A>��A>��A=�A7�A4iDA2�.A1[WA0<6A-ĜA+��A(�A"�A!��A"��A%Q�A&D�A&y>A&tTA&2�A%��A%�9A%ȴA%��A%�-A%DgA%.IA$��A$>BA#bNA"��A"��A"XA!�fA!\�A!o�A!��A!�A!�)A!��A!!�A�2AA��A��A�<A��A U�A!A ��A ��A�AA+A�hA��Af�A�A�A��A~Ax�A�A|A{A�A�A�rA?}A�)A\�AȴAA$�Ap;AxAbNA�_A�IA�3A(AA<�AT�A+kA�)AݘA�_A(�A�}A�eAy�A�.A��AA�4AqA[WAB�A33A)_A�rA��A��A�A�Au�A	A҉AzASA
v`A
XA	�cA	��A	Z�A��A��An/A��A��AaA&�A�jA�AS&A��A��An/AA�A_AK�A4�A�A�hA�Aa�A ��A ��A �@�w�@�IR@�֡@��h@�Dg@�j�@��@��@�IR@��@�hs@���@��"@���@�b�@�@�J�@�q@�o @���@땁@�e@�	@�\)@��U@�H@�u�@�{�@��6@�@��@�s�@�1'@�h�@�e�@�_�@�%�@�%F@���@�1�@�U�@ބ�@�
�@�<6@��`@ܓu@���@�8�@�q�@��@���@وf@�X@�-w@�7L@��@ؠ�@�V�@�%�@��r@׿H@�\�@�@�[�@ա�@�S&@�C@��H@��@ԟ�@�k�@��`@�2�@ёh@ЯO@��@�/�@κ�@�Z@� �@�}�@�V�@�l�@�.I@���@���@ɪ�@��@�*�@�1�@�}V@��@Ŋ	@�{@�H�@��	@·�@§@��@���@��m@�f�@�F@��@��h@�D�@��@���@�B�@���@�$�@��@�Q�@��@��!@��@��d@��@���@�$@���@�v`@�Dg@��@���@���@�?@��@��@�x�@�\�@�Mj@��@���@�3�@��@��@���@���@�e,@���@�x@�}�@�e�@�_p@�S�@��m@��@��H@��X@�v`@�?}@��E@��1@���@�p;@�V�@�:*@��
@��@��{@�@O@�+@�Ɇ@�n�@�ݘ@���@���@���@�Dg@��2@��O@�Z�@�x�@��@�Ĝ@���@���@�|�@�3�@�� @��@��@�c�@���@��@�h
@�8�@���@�H�@��@���@���@��@�x@��@�i�@�~@��@���@�J#@�*0@��<@�kQ@�GE@��Z@���@��7@�m]@�7L@��v@�h�@��]@���@�C�@��@��@��o@�"h@��@���@�Mj@��@�1�@��@��P@�_p@�4�@��@��@���@���@�v�@��@���@�y�@�F�@��@��]@���@��9@���@�Z�@�6�@�($@��@��@���@���@���@�f�@�-w@��@��h@�v�@�*�@���@�m]@�/@��@��$@�l"@�C-@���@���@�s�@�X@�Dg@�,�@��2@���@�e�@�,=@��Q@��4@�7L@�4@�+@��@��2@��)@���@�tT@�'R@��@��@��~@�F@��@��@��@���@��@�tT@�z�@�M@���@��
@�zx@�J�@��@��@�~(@�1�@�1@��@�p�@�/�@��@���@��s@���@��&@���@���@���@�p�@�;d@�C@��@���@���@�C-@�"h@��@���@��	@��@�;�@�&�@�@��@�� @���@��=@�f�@�@��'@��4@��b@�kQ@��@��g@���@�iD@�B�@�ی@��<@��@���@�|�@�c�@�A@P�@�@~�@~�r@}��@|��@|M@{��@{��@{e�@{o@z��@z^5@z �@y�t@y��@y=�@x�v@x�U@xq@x@w��@w@O@vTa@u�@u?}@u%@t�[@tFt@t�@s��@s��@se�@sE9@rW�@q�@q8�@p��@p֡@pm�@p%�@o�g@n�@nz@n?@n{@m�@me,@m	l@l��@l�_@l��@l|�@lC-@l�@k��@ke�@kJ#@k.I@jYK@jH�@j&�@i�@i��@iQ�@h�@h��@g��@g�$@gF�@f�2@f�X@f��@fYK@f;�@e��@es�@e-w@d֡@d�_@d1'@cخ@c��@c��@b�8@bv�@a�=@a*0@`��@`r�@_�@_j�@^�B@^��@^3�@]�@]�@]J�@\�U@\�@[��@[H�@[(@Zq�@Z�@Y�@YT�@X��@Xu�@W�&@Wa@WS�@WC�@W)_@V�\@U�@T��@T6@S��@S8@R��@Rv�@RC�@R �@Q�H@Q�=@Q \@P[�@O�A@O�*@O��@O!-@O@N�B@M�T@Mc@MO�@M�@L��@LA�@K��@K8@J��@J��@J1�@I�n@I!�@Hr�@H	�@G�F@F�H@FV@F4@Ej@D��@Dc�@DS�@D<�@D�@D�@C�m@C��@C>�@Bں@Bxl@A�n@A*0@@e�@@ �@@@@�@?�g@?P�@>�@>��@>($@=��@=��@=��@=0�@<�@<@<*�@<G@;��@;g�@;dZ@;_p@;C�@;�@:�B@:�\@9��@9G�@8�/@8�O@8�@87�@7�A@7��@7v`@7U�@7O@7=@78@76z@78@7/�@7�@6�M@6�@6͟@6�'@6\�@5�)@5�t@5�S@5|@5J�@5�@4�U@4�@4�.@4�Y@3�]@3�K@3�@3t�@3@2�r@2H�@2�@1�^@1�S@12a@0�@0�Y@0D�@/�@/�w@/t�@.��@.ff@.Ta@.�@-ԕ@-�~@-hs@-&�@,�@,u�@,N�@,�@+�&@+�q@+X�@*� @*;�@*?@)��@)��@)s�@)%@(��@(N�@(!@(�@(  @'�m@'�@'H�@'+@'�@'(@&��@%��@%�@%��@%�7@%\�@%F@%<6@%+�@$��@$g8@$/�@#�m@#�w@#Mj@"�@"��@";�@!�d@!J�@!#�@ ��@ �9@ u�@ M@�Q@�F@��@��@X�@+@�@��@c @+k@�3@Vm@��@�e@<�@�m@�@��@j�@J#@�@�F@$�@�@�)@�H@�~@|@o @a�@@ی@tT@N�@:�@1@�@�V@�f@{J@]�@�@�2@�<@��@q�@Z�@Ov@H�@;�@!�@��@��@��@�-@�@c@o @hs@8�@�|@��@��@�.@Xy@~@�@�Q@�@o�@@�@�,@��@~�@?@�@��@��@��@�=@�"@o @+@�@��@��@_@*�@	�@�]@��@�@�@��@��@��@�{@dZ@F�@�@
=@@�@��@u%@;�@
�@�N@��@�h@rG@Q�@V@�/@w�@bN@U2@M@S�@U211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B�nB��B��B��B��B��B�B��B��B��B�hB� B�ZB�B�KB�B*0B	�BB
�B
#B
j0B
�YB
��B
�0B
��B
�KB
��B
�B
��B
��B
�B
�4B
��B�B-)B*�B=�B?}B:�B�B�BaB�B�B
�LB
�CB
��BEB�B B~B �B
��B
�B
�eB
��B
�
B
�B
l�B
Y�B
O�B
AoB
)�B
�B	�B	��B	οB	�B	��B	��B	yXB	c�B	NVB	@ B	1�B	,B	$�B	�B		B	UB	�B	�B	�B	!B	�B	�B	�B	�B�B�B�uB�B	�B	-�B	6zB	dB	"�B	,WB	H�B	P}B	AUB	<B	/5B	B	oB	'B	n�B	� B	�B	�eB	�cB	�hB	�<B	��B	�B	�2B	�B	��B	�mB	�B	�@B	�B	�B	��B	�tB	�tB	��B	�B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
;�B
SuB
U2B
R�B
K�B
EmB
4�B
*eB
(XB
&�B
!�B
B
�B
B
hB
�B
�B
hB
6B
?B
'B
 �B	��B	��B	�}B	��B
mB
!B
'mB
(�B
2�B
4�B
9$B
D�B
EmB
G�B
J	B
J	B
H�B
HB
G�B
G�B
G+B
F�B
F?B
E�B
E�B
B�B
@4B
@ B
?�B
@4B
@�B
BAB
A�B
@B
=�B
>wB
>�B
A�B
>�B
>(B
<�B
;B
8�B
7B
4nB
1�B
1B
0�B
0!B
0�B
1AB
1[B
0!B
.B
,�B
)�B
'�B
$ZB
#nB
#TB
#nB
%zB
&2B
(XB
)�B
'�B
$�B
"B
"hB
#�B
 BB
KB
�B
B
4B
	lB
�B
�B
�B
~B
�B
�B
~B
�B
]B
B
�B
�B
MB
oB
�B
KB
�B
�B
KB
EB
B
_B
B
B
B
 �B
 iB
 �B
gB
B
�B
+B
�B
YB
�B
�B
?B
SB
B
�B
�B
�B
�B
�B
GB
-B
aB
aB
gB
?B
	�B
�B
�B
�B
pB
B
\B
�B
�B
\B
B
�B
�B
<B
B
�B
B
B
�B
�B
6B
B
6B
�B
�B
�B
jB
�B
PB
�B
B
PB
jB
B
xB
B
�B
B
-B
B
B
AB
�B
[B
uB
�B
GB
�B
�B
�B
�B
�B
�B
AB
B
�B
�B
�B
aB
{B
�B
�B
�B
�B
aB
B
GB
aB
�B
�B
3B
�B
MB
�B
�B
�B
�B
�B
�B
B
mB
SB
�B
mB
mB
�B
B
�B
�B
�B
_B
�B
�B
�B
�B
�B
�B
	�B
	RB
	lB
	RB
	�B
	7B
	RB
	B
	B
	B
	RB
	�B

=B
DB
)B
DB
DB
�B
0B
�B
dB
"B
"B
�B
�B
�B
�B
�B
�B
�B
�B
"B
pB
VB
�B
�B
\B
\B
vB
\B
�B
�B
BB
�B
.B
HB
}B
�B
4B
4B
 B
�B
�B
[B
FB
�B
�B
2B
MB
�B
�B
�B
�B
�B
B
SB
�B
�B
�B
_B
�B
B
B
B
B
�B
WB
WB
#B
#B
=B
�B
B
B
CB
xB
�B
�B
�B
�B
/B
IB
dB
dB
B
B
B
B
OB
jB
B
5B
OB
5B
�B
�B
�B
�B
�B
�B
�B
 BB
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!HB
!-B
!bB
!�B
!�B
!�B
!�B
"B
"B
"�B
#B
#�B
$ZB
$B
#�B
#B
"�B
!�B
!�B
!bB
!bB
!�B
!�B
!�B
$@B
$@B
# B
"�B
!�B
!�B
"B
!�B
#B
#B
#�B
$B
$�B
$�B
%B
%B
%zB
&�B
'B
'B
'B
'�B
)B
)_B
)yB
)�B
*�B
+B
+B
+QB
+�B
,qB
,WB
,"B
,WB
,�B
-B
-CB
-]B
-�B
-�B
.�B
/B
/B
.�B
/B
/OB
/5B
/iB
/iB
/�B
0�B
0�B
0�B
0�B
0�B
1�B
0�B
0oB
0�B
0�B
0�B
0oB
0�B
0�B
1AB
1�B
2-B
2GB
2aB
2�B
3MB
3MB
3�B
4B
3�B
3�B
3�B
4B
4TB
5B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
6zB
6�B
6�B
6�B
7fB
7LB
7�B
8�B
9$B
9>B
9XB
9XB
9�B
9�B
9�B
:B
:B
:*B
:*B
:xB
:xB
:xB
:xB
:xB
;B
;0B
;B
;dB
;JB
;�B
;�B
<PB
<�B
<�B
=B
="B
="B
=VB
=qB
=�B
>B
=�B
=�B
>B
>(B
>�B
>�B
>�B
>wB
?cB
@�B
A;B
A;B
@�B
@�B
A�B
B�B
C{B
CaB
C�B
C�B
C�B
D�B
D�B
D�B
D�B
C�B
C{B
D�B
E9B
E�B
FYB
F�B
GB
G_B
G�B
G�B
G�B
G�B
HKB
H�B
I7B
I7B
I�B
I�B
J#B
JXB
JrB
J�B
J�B
J�B
K)B
K�B
K�B
L0B
LB
LdB
L�B
L�B
M�B
M�B
M�B
M�B
N<B
N�B
N�B
O\B
O�B
O�B
O�B
PHB
P�B
QB
QB
QNB
R B
RTB
RoB
R�B
S[B
S�B
S�B
S�B
S�B
S�B
TB
T,B
T�B
T�B
UB
VB
VB
V�B
V�B
V�B
V�B
V�B
WYB
WsB
W�B
W�B
W�B
X+B
XB
XyB
X�B
Y�B
Y1B
YeB
ZB
Z7B
ZB
ZB
ZB
Z7B
ZQB
ZkB
Z�B
[	B
[=B
[WB
[qB
[�B
\)B
\]B
\�B
\�B
\�B
]B
\�B
]/B
\�B
\�B
]/B
]IB
]dB
]IB
]IB
]�B
^B
^5B
^B
^B
^5B
^�B
^�B
_B
_!B
_!B
_�B
_�B
_�B
`B
`\B
`�B
`�B
a-B
abB
a|B
a�B
a�B
b4B
b�B
b�B
b�B
b�B
cTB
c�B
cTB
cTB
c�B
c�B
c�B
d&B
d�B
d�B
e,B
eFB
e`B
ezB
e�B
ffB
f�B
f�B
f�B
f�B
f�B
gRB
g�B
g�B
h$B
h
B
h
B
h
B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
jB
j0B
j0B
j0B
jB
j�B
j�B
j�B
kB
kB
kkB
kkB
k�B
l"B
l�B
l�B
l�B
m)B
m]B
mwB
m�B
nB
m�B
m�B
nB
nIB
n}B
n�B
n�B
oB
o5B
o�B
o�B
p!B
p;B
p�B
p�B
qB
qB
qAB
qAB
q�B
q�B
rGB
raB
raB
r�B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
tB
t9B
tnB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u?B
uZB
utB
u�B
utB
u�B
u�B
u�B
u�B
vB
vB
v+B
v+B
v`B
v`B
v�B
v�B
v�B
wB
w2B
wfB
w�B
w�B
w�B
w�B
xRB
x�B
x�B
x�B
y$B
y$B
yrB
yrB
y�B
y�B
y�B
zB
zB
zB
zxB
z�B
z�B
z�B
z�B
{0B
{JB
{B
{B
{B
{B
{dB
{�B
{�B
|B
|B
|6B
|jB
|jB
|PB
|jB
|�B
|�B
}<B
}qB
}�B
}�B
}�B
}�B
~B
~(B
~wB
~�B
~�B
B
B
~�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B�nB��B��B��B��B��B�B��B��B��B�hB� B�ZB�B�KB�B*0B	�BB
�B
#B
j0B
�YB
��B
�0B
��B
�KB
��B
�B
��B
��B
�B
�4B
��B�B-)B*�B=�B?}B:�B�B�BaB�B�B
�LB
�CB
��BEB�B B~B �B
��B
�B
�eB
��B
�
B
�B
l�B
Y�B
O�B
AoB
)�B
�B	�B	��B	οB	�B	��B	��B	yXB	c�B	NVB	@ B	1�B	,B	$�B	�B		B	UB	�B	�B	�B	!B	�B	�B	�B	�B�B�B�uB�B	�B	-�B	6zB	dB	"�B	,WB	H�B	P}B	AUB	<B	/5B	B	oB	'B	n�B	� B	�B	�eB	�cB	�hB	�<B	��B	�B	�2B	�B	��B	�mB	�B	�@B	�B	�B	��B	�tB	�tB	��B	�B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
;�B
SuB
U2B
R�B
K�B
EmB
4�B
*eB
(XB
&�B
!�B
B
�B
B
hB
�B
�B
hB
6B
?B
'B
 �B	��B	��B	�}B	��B
mB
!B
'mB
(�B
2�B
4�B
9$B
D�B
EmB
G�B
J	B
J	B
H�B
HB
G�B
G�B
G+B
F�B
F?B
E�B
E�B
B�B
@4B
@ B
?�B
@4B
@�B
BAB
A�B
@B
=�B
>wB
>�B
A�B
>�B
>(B
<�B
;B
8�B
7B
4nB
1�B
1B
0�B
0!B
0�B
1AB
1[B
0!B
.B
,�B
)�B
'�B
$ZB
#nB
#TB
#nB
%zB
&2B
(XB
)�B
'�B
$�B
"B
"hB
#�B
 BB
KB
�B
B
4B
	lB
�B
�B
�B
~B
�B
�B
~B
�B
]B
B
�B
�B
MB
oB
�B
KB
�B
�B
KB
EB
B
_B
B
B
B
 �B
 iB
 �B
gB
B
�B
+B
�B
YB
�B
�B
?B
SB
B
�B
�B
�B
�B
�B
GB
-B
aB
aB
gB
?B
	�B
�B
�B
�B
pB
B
\B
�B
�B
\B
B
�B
�B
<B
B
�B
B
B
�B
�B
6B
B
6B
�B
�B
�B
jB
�B
PB
�B
B
PB
jB
B
xB
B
�B
B
-B
B
B
AB
�B
[B
uB
�B
GB
�B
�B
�B
�B
�B
�B
AB
B
�B
�B
�B
aB
{B
�B
�B
�B
�B
aB
B
GB
aB
�B
�B
3B
�B
MB
�B
�B
�B
�B
�B
�B
B
mB
SB
�B
mB
mB
�B
B
�B
�B
�B
_B
�B
�B
�B
�B
�B
�B
	�B
	RB
	lB
	RB
	�B
	7B
	RB
	B
	B
	B
	RB
	�B

=B
DB
)B
DB
DB
�B
0B
�B
dB
"B
"B
�B
�B
�B
�B
�B
�B
�B
�B
"B
pB
VB
�B
�B
\B
\B
vB
\B
�B
�B
BB
�B
.B
HB
}B
�B
4B
4B
 B
�B
�B
[B
FB
�B
�B
2B
MB
�B
�B
�B
�B
�B
B
SB
�B
�B
�B
_B
�B
B
B
B
B
�B
WB
WB
#B
#B
=B
�B
B
B
CB
xB
�B
�B
�B
�B
/B
IB
dB
dB
B
B
B
B
OB
jB
B
5B
OB
5B
�B
�B
�B
�B
�B
�B
�B
 BB
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!HB
!-B
!bB
!�B
!�B
!�B
!�B
"B
"B
"�B
#B
#�B
$ZB
$B
#�B
#B
"�B
!�B
!�B
!bB
!bB
!�B
!�B
!�B
$@B
$@B
# B
"�B
!�B
!�B
"B
!�B
#B
#B
#�B
$B
$�B
$�B
%B
%B
%zB
&�B
'B
'B
'B
'�B
)B
)_B
)yB
)�B
*�B
+B
+B
+QB
+�B
,qB
,WB
,"B
,WB
,�B
-B
-CB
-]B
-�B
-�B
.�B
/B
/B
.�B
/B
/OB
/5B
/iB
/iB
/�B
0�B
0�B
0�B
0�B
0�B
1�B
0�B
0oB
0�B
0�B
0�B
0oB
0�B
0�B
1AB
1�B
2-B
2GB
2aB
2�B
3MB
3MB
3�B
4B
3�B
3�B
3�B
4B
4TB
5B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
6zB
6�B
6�B
6�B
7fB
7LB
7�B
8�B
9$B
9>B
9XB
9XB
9�B
9�B
9�B
:B
:B
:*B
:*B
:xB
:xB
:xB
:xB
:xB
;B
;0B
;B
;dB
;JB
;�B
;�B
<PB
<�B
<�B
=B
="B
="B
=VB
=qB
=�B
>B
=�B
=�B
>B
>(B
>�B
>�B
>�B
>wB
?cB
@�B
A;B
A;B
@�B
@�B
A�B
B�B
C{B
CaB
C�B
C�B
C�B
D�B
D�B
D�B
D�B
C�B
C{B
D�B
E9B
E�B
FYB
F�B
GB
G_B
G�B
G�B
G�B
G�B
HKB
H�B
I7B
I7B
I�B
I�B
J#B
JXB
JrB
J�B
J�B
J�B
K)B
K�B
K�B
L0B
LB
LdB
L�B
L�B
M�B
M�B
M�B
M�B
N<B
N�B
N�B
O\B
O�B
O�B
O�B
PHB
P�B
QB
QB
QNB
R B
RTB
RoB
R�B
S[B
S�B
S�B
S�B
S�B
S�B
TB
T,B
T�B
T�B
UB
VB
VB
V�B
V�B
V�B
V�B
V�B
WYB
WsB
W�B
W�B
W�B
X+B
XB
XyB
X�B
Y�B
Y1B
YeB
ZB
Z7B
ZB
ZB
ZB
Z7B
ZQB
ZkB
Z�B
[	B
[=B
[WB
[qB
[�B
\)B
\]B
\�B
\�B
\�B
]B
\�B
]/B
\�B
\�B
]/B
]IB
]dB
]IB
]IB
]�B
^B
^5B
^B
^B
^5B
^�B
^�B
_B
_!B
_!B
_�B
_�B
_�B
`B
`\B
`�B
`�B
a-B
abB
a|B
a�B
a�B
b4B
b�B
b�B
b�B
b�B
cTB
c�B
cTB
cTB
c�B
c�B
c�B
d&B
d�B
d�B
e,B
eFB
e`B
ezB
e�B
ffB
f�B
f�B
f�B
f�B
f�B
gRB
g�B
g�B
h$B
h
B
h
B
h
B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
jB
j0B
j0B
j0B
jB
j�B
j�B
j�B
kB
kB
kkB
kkB
k�B
l"B
l�B
l�B
l�B
m)B
m]B
mwB
m�B
nB
m�B
m�B
nB
nIB
n}B
n�B
n�B
oB
o5B
o�B
o�B
p!B
p;B
p�B
p�B
qB
qB
qAB
qAB
q�B
q�B
rGB
raB
raB
r�B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
tB
t9B
tnB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u?B
uZB
utB
u�B
utB
u�B
u�B
u�B
u�B
vB
vB
v+B
v+B
v`B
v`B
v�B
v�B
v�B
wB
w2B
wfB
w�B
w�B
w�B
w�B
xRB
x�B
x�B
x�B
y$B
y$B
yrB
yrB
y�B
y�B
y�B
zB
zB
zB
zxB
z�B
z�B
z�B
z�B
{0B
{JB
{B
{B
{B
{B
{dB
{�B
{�B
|B
|B
|6B
|jB
|jB
|PB
|jB
|�B
|�B
}<B
}qB
}�B
}�B
}�B
}�B
~B
~(B
~wB
~�B
~�B
B
B
~�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105240  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192154  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192154  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192154                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042205  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042205  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151509                      G�O�G�O�G�O�                