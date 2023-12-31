CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:50:25Z creation;2022-06-04T17:50:25Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604175025  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               !A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��k�UUU1   @��lTò�@/O\(��cfffff1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A��A   A@  A^ffA�  A���A�  A�  A�  A���A���A�  B ��B��B��B  B��B(  B0  B8  BC33BE��BP  BX  B`  Bh  BpffBy33B33B�  B�  B�  B�  B�33B�ffB�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN��CO�3CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DP� DQ  DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy�fDzfDz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@�  @�  A��A   A@  A^ffA�  A���A�  A�  A�  A���A���A�  B ��B��B��B  B��B(  B0  B8  BC33BE��BP  BX  B`  Bh  BpffBy33B33B�  B�  B�  B�  B�33B�ffB�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN��CO�3CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DP� DQ  DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy�fDzfDz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�	�A��A��A�kA�1A�(�A�/�A�1[A�1�A�2�A�.A�0!A�-�A�-wA�.A�*0A�2-A�3�A�:�A�:�A�0�A�-A��A�}VA��A��aAȹ�A��aAć�Aß�A�L�A��yA�!A�E�A�U�A��A�5�A��A�poA�Z�A���A��eA�1�A�@�A��A�-�A��A�r|A�A�VA�{A��0A�6A�+A���A�~�A�JXA��A��KA�!-A�|PA���A��[A���A��MA�$�A�d&A��A��A�� A��QA��A�  A��A��A�6�A��!A�-�A�,=A|.�As��An{JAl�aAk�uAi&�Ac�A_��A]H�AZ��AW�	AR��AP��ANm]AMVAL��AJ֡AH�>AG5�AD�OAC�uAB��AA��A?FA>1�A= iA:d�A9�PA8��A6��A5�AA4 �A/g8A(�eA%�[A$�9A$��A$6zA"ںA"n�A"�]A#g�A#��A#�A$x�A$�NA$F�A#�^A#FA"�.A"�QA"��A"A!�~A!?�A �TA [�A�	AFtA�A�?A�7A$tAL0A0�A��AxlA�CA4A��A~�A{�AϫA�AE�A	�Ap�A�1A�'A�sA�DA
�AA�A�A�RA�+A($A/�A��A`�A5?A+A��Ag8A�A��AA�A�_AM�AYA�&AXA�A
HA	�gA	��A�A��A{JAJA��A \A��A��AT�A�A.IA��A��Ak�A��AR�A��Ar�A�"A��A�A �A �oA 3�@�<6@�Ft@���@�@O@���@��4@��@�u%@� �@���@�)_@�\�@�`B@�8�@��@���@���@�C�@��@�e�@�@��.@�+@�R�@��@�}V@�4@���@�_@��@���@���@�X@趮@�2�@�-@�j�@��E@�w�@��W@�F@�c @㸻@��@�L@�n�@�	@�J�@��@���@�u�@�h�@�7�@�o @��p@ޚ�@މ�@�?�@��@�iD@��|@ܖ�@��@�\�@ڪe@�Xy@�A�@٥@�@@��[@؋D@�oi@�!�@ׄM@�1'@�s�@��@�>B@��@�IR@�j@�9X@�@���@�qv@�҉@Ќ@�7�@�ϫ@�(�@Η�@�bN@�<�@��N@�X�@�0�@��@�V@�7�@�~@���@��@�K^@ɄM@�bN@Ʊ�@�A�@�,=@�&�@���@�c@� \@��p@�*�@Þ�@�n/@¹$@�m�@��W@�A @��@�:*@��V@�(@���@�&�@��=@�t�@�6z@��@���@�ff@���@��@���@��d@���@��H@�ں@���@�7�@��@��0@���@��@�GE@�#:@�(�@�!@�Dg@���@�I�@��r@��*@�L�@���@���@�Q@���@�'�@��@���@�~�@�9X@��@���@���@�>�@��L@�D�@�	@���@�L�@�P�@�A @���@�+k@��$@�RT@�@��@��@�s�@�R�@�3�@���@���@�iD@�q@��<@�d�@��o@��@�rG@�%F@��@��M@���@�-@��@�X�@���@�tT@��@��]@���@�ƨ@�t�@�C@�q�@�
�@��@�C�@�S@�ی@�|�@�6@��@�j�@��@�1�@��@�GE@���@�_p@��M@��h@���@���@��@�!�@��K@��@�I�@���@���@�E9@�(@���@�:*@���@��H@��:@�O@��@���@�c�@�H�@�8�@�O@��@�ƨ@��h@��@���@�A�@�#:@��@�b@��@�@�b@��N@�v`@�(�@��u@�GE@�7@�@���@�Dg@�%@�֡@��U@���@�w�@�Z@�H�@��@�!�@�#:@��@���@��:@�a@��@��<@���@�Ft@��
@���@��~@���@�^�@�=@��@���@��@�h�@�!�@��@�ϫ@���@��~@�o�@�O@�)_@�@��@���@�y>@�S�@�@�@�2�@��@��{@�k�@�B�@��@��4@�R�@�<�@�5?@�,=@��@��@��@��:@�e�@�G�@�4@�@��@��@��b@�g8@�Ta@�Ta@�A�@�=q@�3�@�	@��m@���@���@���@�P�@��@��s@���@���@���@��@��@�/�@��@��]@��@���@��'@�s@�a@�&�@��K@��m@���@���@�q@�C�@�.�@��;@��4@�G�@��@��@���@�Ft@�f@�@~H�@}�o@}��@}N<@|�@|b@{��@{C�@z��@z��@y��@y��@y!�@x�4@x�@w��@w�{@wE9@v��@v0U@u��@t��@t7�@s�K@sF�@r�X@r{�@q��@q<6@p�?@pb@o��@n�h@nH�@n�@m�z@m|@m0�@m%@l�j@lN�@k�@kt�@j�m@j^5@j@�@j0U@j4@i�H@h�5@hm�@h7�@g��@gZ�@f�@f\�@f!�@e��@e�z@e\�@d��@d�@dS�@d(�@c� @cP�@b�@b��@bJ@a��@aVm@`��@`��@`l"@`%�@_�r@_9�@^��@^�b@^@]��@]��@]s�@]?}@]�@\�[@\�@[�@[S@Z��@Z��@ZH�@Z�@Y�@Y<6@Y	l@X��@W�m@We�@V�@V��@VOv@V	@U��@U�'@UVm@U \@T�@T��@T~(@T6@S��@S�@@S@O@R�@R($@Q�>@Q�@Q��@Q^�@Q	l@P�$@P?�@O��@O_p@O�@N��@NR�@NC�@NJ@M��@MY�@M%@Ly>@K��@K��@Jں@J~�@JO@Ik�@I+@H�$@G��@Ga@GC@F��@Fu%@F\�@E�@E}�@Ee,@E;@D�@D�?@Doi@D/�@C�@C4�@B�H@B�r@A��@A�S@A��@AL�@A!�@@Ɇ@@Q�@?�A@?��@?o@>��@>c @>C�@>#:@=�@=�M@=8�@<ѷ@;�m@;�*@;H�@;Y@:͟@:l�@9�@9�S@9�M@9|@9k�@9q@8��@8�u@8h�@81'@7��@7��@7\)@7�@6�@6��@6M�@6;�@6	@5�#@5�S@5(�@4Ɇ@4��@4@3��@3�P@2��@2�L@2l�@2�@1�@1��@1%@0�)@0��@0��@0�_@0c�@/خ@/��@/�@/�4@.��@.kQ@.!�@-�@-�C@-|@-X@-+@,��@,C-@,M@+��@+�0@+��@+dZ@*��@*͟@*�x@*}V@*ff@*!�@)��@)��@)��@)a�@)7L@)V@(�o@(!@'�@'�@'�@'l�@'�@&��@&��@&�\@&Q@&3�@%�.@%�@%�@%+�@$��@$ѷ@$��@$q@$PH@$'R@#�W@#��@#n/@#E9@#&@#�@"�s@"h
@":*@"e@!�>@!�n@!p�@!�@ ��@ ��@ g8@ /�@ݘ@��@��@��@Z�@�@��@u%@\�@YK@:*@u@��@��@N<@V@�5@�I@A�@M@�r@��@1�@�8@��@��@�\@z@C�@�@[W@?}@�@h�@$@��@y�@.I@�@��@��@R�@@�@)�@��@7L@�@��@bN@,=@7@��@��@K�@�@�B@��@p;@J�@�.@�3@rG@Y�@&�@��@��@�I@�Y@`�@*�@��@�@x@Z�@K�@>�@o@�@�@�@kQ@L0@5?@#:@4@��@�>@�@�@��@��@Y�@#�@�[@�O@�.@[�@/�@�@�@��@Mj@�@
�@
��@
�@
z@
R�@
6�@
�@	�9@	��@	��@	u�@	B�@��@��@�e@�@q@?�@~@�@��@�*11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�	�A��A��A�kA�1A�(�A�/�A�1[A�1�A�2�A�.A�0!A�-�A�-wA�.A�*0A�2-A�3�A�:�A�:�A�0�A�-A��A�}VA��A��aAȹ�A��aAć�Aß�A�L�A��yA�!A�E�A�U�A��A�5�A��A�poA�Z�A���A��eA�1�A�@�A��A�-�A��A�r|A�A�VA�{A��0A�6A�+A���A�~�A�JXA��A��KA�!-A�|PA���A��[A���A��MA�$�A�d&A��A��A�� A��QA��A�  A��A��A�6�A��!A�-�A�,=A|.�As��An{JAl�aAk�uAi&�Ac�A_��A]H�AZ��AW�	AR��AP��ANm]AMVAL��AJ֡AH�>AG5�AD�OAC�uAB��AA��A?FA>1�A= iA:d�A9�PA8��A6��A5�AA4 �A/g8A(�eA%�[A$�9A$��A$6zA"ںA"n�A"�]A#g�A#��A#�A$x�A$�NA$F�A#�^A#FA"�.A"�QA"��A"A!�~A!?�A �TA [�A�	AFtA�A�?A�7A$tAL0A0�A��AxlA�CA4A��A~�A{�AϫA�AE�A	�Ap�A�1A�'A�sA�DA
�AA�A�A�RA�+A($A/�A��A`�A5?A+A��Ag8A�A��AA�A�_AM�AYA�&AXA�A
HA	�gA	��A�A��A{JAJA��A \A��A��AT�A�A.IA��A��Ak�A��AR�A��Ar�A�"A��A�A �A �oA 3�@�<6@�Ft@���@�@O@���@��4@��@�u%@� �@���@�)_@�\�@�`B@�8�@��@���@���@�C�@��@�e�@�@��.@�+@�R�@��@�}V@�4@���@�_@��@���@���@�X@趮@�2�@�-@�j�@��E@�w�@��W@�F@�c @㸻@��@�L@�n�@�	@�J�@��@���@�u�@�h�@�7�@�o @��p@ޚ�@މ�@�?�@��@�iD@��|@ܖ�@��@�\�@ڪe@�Xy@�A�@٥@�@@��[@؋D@�oi@�!�@ׄM@�1'@�s�@��@�>B@��@�IR@�j@�9X@�@���@�qv@�҉@Ќ@�7�@�ϫ@�(�@Η�@�bN@�<�@��N@�X�@�0�@��@�V@�7�@�~@���@��@�K^@ɄM@�bN@Ʊ�@�A�@�,=@�&�@���@�c@� \@��p@�*�@Þ�@�n/@¹$@�m�@��W@�A @��@�:*@��V@�(@���@�&�@��=@�t�@�6z@��@���@�ff@���@��@���@��d@���@��H@�ں@���@�7�@��@��0@���@��@�GE@�#:@�(�@�!@�Dg@���@�I�@��r@��*@�L�@���@���@�Q@���@�'�@��@���@�~�@�9X@��@���@���@�>�@��L@�D�@�	@���@�L�@�P�@�A @���@�+k@��$@�RT@�@��@��@�s�@�R�@�3�@���@���@�iD@�q@��<@�d�@��o@��@�rG@�%F@��@��M@���@�-@��@�X�@���@�tT@��@��]@���@�ƨ@�t�@�C@�q�@�
�@��@�C�@�S@�ی@�|�@�6@��@�j�@��@�1�@��@�GE@���@�_p@��M@��h@���@���@��@�!�@��K@��@�I�@���@���@�E9@�(@���@�:*@���@��H@��:@�O@��@���@�c�@�H�@�8�@�O@��@�ƨ@��h@��@���@�A�@�#:@��@�b@��@�@�b@��N@�v`@�(�@��u@�GE@�7@�@���@�Dg@�%@�֡@��U@���@�w�@�Z@�H�@��@�!�@�#:@��@���@��:@�a@��@��<@���@�Ft@��
@���@��~@���@�^�@�=@��@���@��@�h�@�!�@��@�ϫ@���@��~@�o�@�O@�)_@�@��@���@�y>@�S�@�@�@�2�@��@��{@�k�@�B�@��@��4@�R�@�<�@�5?@�,=@��@��@��@��:@�e�@�G�@�4@�@��@��@��b@�g8@�Ta@�Ta@�A�@�=q@�3�@�	@��m@���@���@���@�P�@��@��s@���@���@���@��@��@�/�@��@��]@��@���@��'@�s@�a@�&�@��K@��m@���@���@�q@�C�@�.�@��;@��4@�G�@��@��@���@�Ft@�f@�@~H�@}�o@}��@}N<@|�@|b@{��@{C�@z��@z��@y��@y��@y!�@x�4@x�@w��@w�{@wE9@v��@v0U@u��@t��@t7�@s�K@sF�@r�X@r{�@q��@q<6@p�?@pb@o��@n�h@nH�@n�@m�z@m|@m0�@m%@l�j@lN�@k�@kt�@j�m@j^5@j@�@j0U@j4@i�H@h�5@hm�@h7�@g��@gZ�@f�@f\�@f!�@e��@e�z@e\�@d��@d�@dS�@d(�@c� @cP�@b�@b��@bJ@a��@aVm@`��@`��@`l"@`%�@_�r@_9�@^��@^�b@^@]��@]��@]s�@]?}@]�@\�[@\�@[�@[S@Z��@Z��@ZH�@Z�@Y�@Y<6@Y	l@X��@W�m@We�@V�@V��@VOv@V	@U��@U�'@UVm@U \@T�@T��@T~(@T6@S��@S�@@S@O@R�@R($@Q�>@Q�@Q��@Q^�@Q	l@P�$@P?�@O��@O_p@O�@N��@NR�@NC�@NJ@M��@MY�@M%@Ly>@K��@K��@Jں@J~�@JO@Ik�@I+@H�$@G��@Ga@GC@F��@Fu%@F\�@E�@E}�@Ee,@E;@D�@D�?@Doi@D/�@C�@C4�@B�H@B�r@A��@A�S@A��@AL�@A!�@@Ɇ@@Q�@?�A@?��@?o@>��@>c @>C�@>#:@=�@=�M@=8�@<ѷ@;�m@;�*@;H�@;Y@:͟@:l�@9�@9�S@9�M@9|@9k�@9q@8��@8�u@8h�@81'@7��@7��@7\)@7�@6�@6��@6M�@6;�@6	@5�#@5�S@5(�@4Ɇ@4��@4@3��@3�P@2��@2�L@2l�@2�@1�@1��@1%@0�)@0��@0��@0�_@0c�@/خ@/��@/�@/�4@.��@.kQ@.!�@-�@-�C@-|@-X@-+@,��@,C-@,M@+��@+�0@+��@+dZ@*��@*͟@*�x@*}V@*ff@*!�@)��@)��@)��@)a�@)7L@)V@(�o@(!@'�@'�@'�@'l�@'�@&��@&��@&�\@&Q@&3�@%�.@%�@%�@%+�@$��@$ѷ@$��@$q@$PH@$'R@#�W@#��@#n/@#E9@#&@#�@"�s@"h
@":*@"e@!�>@!�n@!p�@!�@ ��@ ��@ g8@ /�@ݘ@��@��@��@Z�@�@��@u%@\�@YK@:*@u@��@��@N<@V@�5@�I@A�@M@�r@��@1�@�8@��@��@�\@z@C�@�@[W@?}@�@h�@$@��@y�@.I@�@��@��@R�@@�@)�@��@7L@�@��@bN@,=@7@��@��@K�@�@�B@��@p;@J�@�.@�3@rG@Y�@&�@��@��@�I@�Y@`�@*�@��@�@x@Z�@K�@>�@o@�@�@�@kQ@L0@5?@#:@4@��@�>@�@�@��@��@Y�@#�@�[@�O@�.@[�@/�@�@�@��@Mj@�@
�@
��@
�@
z@
R�@
6�@
�@	�9@	��@	��@	u�@	B�@��@��@�e@�@q@?�@~@�@��@�*11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�XB�XB�>B�$B�	B��B��B��B��B�>B��B�rB��B��B��B�>B�B��B��B�2B��B	VB	��B	��B
�B
rBKBBYB�B{B�B,=B?BK^B]/B�B�zB�7B�4B��B��B�B֡B��B��B�lB�B��B�#B�1B��B��B�B��B��B9B�B�B�2B�{B�B�qB�@B�B}qBU2B1[BB
�@B
ϫB
�B
�=B
�B
�PB
uZB
RoB
2B	�%B	�+B	��B	� B	w�B	f�B	D�B	*�B	�B	�B	 �B�B�B�5B��B�'B�B�
B��B�B��B	�B	bB	pB	 �B	 �B	0B	49B	;0B	GB	J�B	E�B	3�B	�B	B	zB	)B	)�B	$@B	-B	:^B	G�B	WsB	\]B	l�B	|�B	�JB	��B	�=B	�B	�B	��B	��B	��B	�B	��B	�KB	��B	ňB	�B	��B	�%B	�%B	��B	�SB	��B	�(B	��B	̈́B	�B	�B	��B	�5B	�B	�!B	��B	��B	�B	�B	��B	�^B
�B
dB
B
B	�LB	�AB	�B	��B	�B	�gB	՛B	ּB	�mB	��B	�+B	خB	��B	�WB	�!B	�B	��B	��B	�HB	��B	ߊB	�OB	�dB	ݲB	�dB	�OB	�dB	��B	ںB	��B	�QB	�_B	خB	��B	�_B	��B	ٴB	�KB	ڠB	�7B	ܒB	�B	�~B	�/B	�WB	��B	�SB	��B	ԕB	��B	�B	��B	��B	��B	�2B	ՁB	��B	�?B	�?B	�$B	�$B	یB	��B	چB	ڠB	ںB	�#B	��B	�#B	�=B	��B	��B	�qB	�qB	�WB	�)B	��B	�B	�-B	� B	�B	�4B	�B	�B	�B	�zB	��B	��B	�B	�4B	��B	��B	�TB	�B	�B	��B	�B	�B	��B	�B	�B	�B	�B	�TB	��B	�nB	��B	�B	� B	�B	�TB	�B	�B	�ZB	�tB	��B	��B	��B	�FB	�@B	�B	��B	�B	��B	��B	��B	�B	�tB	�B	�B	�zB	��B	�zB	�B	�B	�RB	�8B	�B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�B	�B	�B	�B	��B	��B	� B	�B	�UB	�;B	��B	��B	�aB	�3B	��B	�?B	��B	��B	�2B	�B	��B	��B	��B	��B	�2B	�B	�RB	��B	��B	��B	��B	�fB	�LB	��B	�fB	��B	��B	��B	�B	��B	��B	�B	��B	�*B	�B	�DB	��B	��B	�8B	�RB	�$B	��B	��B	�B	�jB	�6B	��B	��B	�B	��B	�VB	�B	��B	�B	��B
 OB
 iB
 OB
 �B
 �B	��B
 iB
;B
'B
�B
 �B	��B
 4B
 �B
�B
�B
�B
-B
{B
{B
�B
�B
�B
B
B
SB
�B
�B
%B
B
+B
�B
�B
B
�B
�B
B
�B
1B
�B
�B
�B
zB
YB
tB
�B

�B
�B
�B
^B

�B

�B

�B
�B
VB
�B
�B
^B
B
�B
�B
<B
�B
�B
�B
BB
�B
�B
B
BB
�B
bB
�B
4B
�B
�B
 B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
,B
�B
�B
�B
2B
gB
MB
�B
�B
�B
�B
?B
�B

B
�B
?B
B
B
�B
�B
�B
_B
�B
�B
B
�B
WB
�B
WB
�B
�B
qB
#B
�B
]B
=B
qB
�B
IB
OB
jB
�B
�B
�B
B
�B
B
B
�B
�B
�B
�B
B
�B
�B
�B
�B
 'B
 'B
 BB
 vB
 �B
!bB
!�B
!�B
"B
"�B
"�B
# B
#:B
#�B
$B
$tB
$�B
$�B
$�B
$�B
%,B
%B
%`B
&fB
&�B
'B
&�B
'�B
'�B
'�B
(�B
)DB
)_B
*B
+B
+B
+B
+�B
,�B
,�B
-CB
-�B
-�B
-�B
-�B
-�B
.IB
.IB
.�B
.�B
.�B
/ B
/iB
/iB
/�B
0oB
0�B
0�B
1AB
1�B
1�B
1�B
2GB
2GB
2�B
3�B
2-B
1�B
1�B
1�B
1�B
2GB
3B
3hB
3�B
3�B
4B
4�B
5B
5B
5%B
5�B
5�B
6FB
6`B
6zB
6�B
7LB
7�B
8lB
9$B
9rB
9�B
9�B
9�B
:DB
:xB
:�B
:�B
;�B
<jB
<�B
<�B
<�B
=B
=<B
="B
="B
=VB
=�B
=�B
>(B
>BB
>BB
>(B
>(B
>(B
>�B
>�B
>�B
?.B
?}B
?�B
?�B
?�B
?�B
?�B
@4B
@iB
@�B
@�B
@iB
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B'B
BAB
B'B
B'B
BAB
B�B
BAB
BAB
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
CaB
C�B
DB
C�B
DB
DB
D�B
D�B
D�B
EB
E�B
F%B
F�B
G+B
GEB
G�B
G�B
G�B
HKB
HKB
HfB
H�B
H�B
I7B
I�B
J	B
JrB
K)B
K)B
J�B
J�B
KDB
K^B
K�B
K�B
L0B
L�B
L�B
MjB
M�B
M�B
M�B
N"B
N<B
NVB
NVB
N�B
N�B
N�B
OvB
OvB
O�B
PHB
PbB
P�B
QB
QhB
Q�B
Q�B
RB
Q�B
R:B
RoB
RTB
R�B
R�B
R�B
S&B
SB
R�B
S&B
SuB
S�B
T{B
T�B
T�B
U2B
U2B
U�B
U�B
U�B
U�B
VSB
V�B
W
B
W
B
W
B
W?B
WYB
W�B
W�B
XEB
XB
X_B
X+B
XyB
X�B
X�B
X�B
X�B
X�B
X�B
YKB
Y�B
ZB
ZB
Y�B
Z�B
ZkB
Z�B
Z�B
Z�B
[=B
[#B
[#B
[=B
[qB
[�B
[�B
[�B
[�B
\)B
\�B
\�B
\�B
]IB
]~B
]�B
]�B
]�B
^OB
^�B
^�B
^�B
^�B
^�B
_B
_B
_B
_!B
_�B
`B
`BB
`\B
`vB
`vB
`�B
`�B
aHB
a�B
a�B
a�B
b4B
b4B
b�B
cB
cB
c B
c:B
cTB
c�B
c�B
c�B
c�B
dZB
dZB
dZB
d�B
d�B
eB
e,B
e`B
ezB
e�B
e�B
e�B
f2B
ffB
f�B
f�B
f�B
f�B
gRB
g�B
g�B
g�B
g�B
h
B
h$B
hXB
h�B
h�B
h�B
h�B
h�B
h�B
i*B
i*B
i_B
i�B
jeB
jeB
jB
j�B
kB
j�B
kB
k�B
k�B
k�B
k�B
l"B
l=B
l�B
l�B
l�B
l�B
l�B
mB
mCB
mwB
m�B
m�B
m�B
n/B
n}B
n}B
n}B
o B
oiB
o�B
o�B
o�B
o�B
o�B
p!B
p�B
p�B
p�B
qAB
q�B
q�B
r-B
r|B
r�B
r�B
sB
shB
s�B
s�B
s�B
t9B
t�B
t�B
t�B
uZB
utB
utB
u�B
u�B
v+B
vzB
v�B
v�B
v�B
v�B
wfB
w�B
w�B
xB
x8B
xRB
x�B
x�B
x�B
x�B
y$B
yXB
y�B
y�B
y�B
y�B
y�B
z*B
z*B
z^B
z^B
z�B
z�B
z�B
{B
{B
{B
{0B
{0B
{0B
{JB
{B
{�B
{�B
|jB
|jB
|�B
|�B
|�B
|�B
}B
}qB
}�B
~(B
~]B
~wB
~�B
~�B
~�B
~�B
~�B
.B
HB
}B
�B
�B
�4B
�OB
��B
��B
��B
��B
��B
�B
�oB
�o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�XB�XB�>B�$B�	B��B��B��B��B�>B��B�rB��B��B��B�>B�B��B��B�2B��B	VB	��B	��B
�B
rBKBBYB�B{B�B,=B?BK^B]/B�B�zB�7B�4B��B��B�B֡B��B��B�lB�B��B�#B�1B��B��B�B��B��B9B�B�B�2B�{B�B�qB�@B�B}qBU2B1[BB
�@B
ϫB
�B
�=B
�B
�PB
uZB
RoB
2B	�%B	�+B	��B	� B	w�B	f�B	D�B	*�B	�B	�B	 �B�B�B�5B��B�'B�B�
B��B�B��B	�B	bB	pB	 �B	 �B	0B	49B	;0B	GB	J�B	E�B	3�B	�B	B	zB	)B	)�B	$@B	-B	:^B	G�B	WsB	\]B	l�B	|�B	�JB	��B	�=B	�B	�B	��B	��B	��B	�B	��B	�KB	��B	ňB	�B	��B	�%B	�%B	��B	�SB	��B	�(B	��B	̈́B	�B	�B	��B	�5B	�B	�!B	��B	��B	�B	�B	��B	�^B
�B
dB
B
B	�LB	�AB	�B	��B	�B	�gB	՛B	ּB	�mB	��B	�+B	خB	��B	�WB	�!B	�B	��B	��B	�HB	��B	ߊB	�OB	�dB	ݲB	�dB	�OB	�dB	��B	ںB	��B	�QB	�_B	خB	��B	�_B	��B	ٴB	�KB	ڠB	�7B	ܒB	�B	�~B	�/B	�WB	��B	�SB	��B	ԕB	��B	�B	��B	��B	��B	�2B	ՁB	��B	�?B	�?B	�$B	�$B	یB	��B	چB	ڠB	ںB	�#B	��B	�#B	�=B	��B	��B	�qB	�qB	�WB	�)B	��B	�B	�-B	� B	�B	�4B	�B	�B	�B	�zB	��B	��B	�B	�4B	��B	��B	�TB	�B	�B	��B	�B	�B	��B	�B	�B	�B	�B	�TB	��B	�nB	��B	�B	� B	�B	�TB	�B	�B	�ZB	�tB	��B	��B	��B	�FB	�@B	�B	��B	�B	��B	��B	��B	�B	�tB	�B	�B	�zB	��B	�zB	�B	�B	�RB	�8B	�B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�B	�B	�B	�B	��B	��B	� B	�B	�UB	�;B	��B	��B	�aB	�3B	��B	�?B	��B	��B	�2B	�B	��B	��B	��B	��B	�2B	�B	�RB	��B	��B	��B	��B	�fB	�LB	��B	�fB	��B	��B	��B	�B	��B	��B	�B	��B	�*B	�B	�DB	��B	��B	�8B	�RB	�$B	��B	��B	�B	�jB	�6B	��B	��B	�B	��B	�VB	�B	��B	�B	��B
 OB
 iB
 OB
 �B
 �B	��B
 iB
;B
'B
�B
 �B	��B
 4B
 �B
�B
�B
�B
-B
{B
{B
�B
�B
�B
B
B
SB
�B
�B
%B
B
+B
�B
�B
B
�B
�B
B
�B
1B
�B
�B
�B
zB
YB
tB
�B

�B
�B
�B
^B

�B

�B

�B
�B
VB
�B
�B
^B
B
�B
�B
<B
�B
�B
�B
BB
�B
�B
B
BB
�B
bB
�B
4B
�B
�B
 B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
,B
�B
�B
�B
2B
gB
MB
�B
�B
�B
�B
?B
�B

B
�B
?B
B
B
�B
�B
�B
_B
�B
�B
B
�B
WB
�B
WB
�B
�B
qB
#B
�B
]B
=B
qB
�B
IB
OB
jB
�B
�B
�B
B
�B
B
B
�B
�B
�B
�B
B
�B
�B
�B
�B
 'B
 'B
 BB
 vB
 �B
!bB
!�B
!�B
"B
"�B
"�B
# B
#:B
#�B
$B
$tB
$�B
$�B
$�B
$�B
%,B
%B
%`B
&fB
&�B
'B
&�B
'�B
'�B
'�B
(�B
)DB
)_B
*B
+B
+B
+B
+�B
,�B
,�B
-CB
-�B
-�B
-�B
-�B
-�B
.IB
.IB
.�B
.�B
.�B
/ B
/iB
/iB
/�B
0oB
0�B
0�B
1AB
1�B
1�B
1�B
2GB
2GB
2�B
3�B
2-B
1�B
1�B
1�B
1�B
2GB
3B
3hB
3�B
3�B
4B
4�B
5B
5B
5%B
5�B
5�B
6FB
6`B
6zB
6�B
7LB
7�B
8lB
9$B
9rB
9�B
9�B
9�B
:DB
:xB
:�B
:�B
;�B
<jB
<�B
<�B
<�B
=B
=<B
="B
="B
=VB
=�B
=�B
>(B
>BB
>BB
>(B
>(B
>(B
>�B
>�B
>�B
?.B
?}B
?�B
?�B
?�B
?�B
?�B
@4B
@iB
@�B
@�B
@iB
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B'B
BAB
B'B
B'B
BAB
B�B
BAB
BAB
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
CaB
C�B
DB
C�B
DB
DB
D�B
D�B
D�B
EB
E�B
F%B
F�B
G+B
GEB
G�B
G�B
G�B
HKB
HKB
HfB
H�B
H�B
I7B
I�B
J	B
JrB
K)B
K)B
J�B
J�B
KDB
K^B
K�B
K�B
L0B
L�B
L�B
MjB
M�B
M�B
M�B
N"B
N<B
NVB
NVB
N�B
N�B
N�B
OvB
OvB
O�B
PHB
PbB
P�B
QB
QhB
Q�B
Q�B
RB
Q�B
R:B
RoB
RTB
R�B
R�B
R�B
S&B
SB
R�B
S&B
SuB
S�B
T{B
T�B
T�B
U2B
U2B
U�B
U�B
U�B
U�B
VSB
V�B
W
B
W
B
W
B
W?B
WYB
W�B
W�B
XEB
XB
X_B
X+B
XyB
X�B
X�B
X�B
X�B
X�B
X�B
YKB
Y�B
ZB
ZB
Y�B
Z�B
ZkB
Z�B
Z�B
Z�B
[=B
[#B
[#B
[=B
[qB
[�B
[�B
[�B
[�B
\)B
\�B
\�B
\�B
]IB
]~B
]�B
]�B
]�B
^OB
^�B
^�B
^�B
^�B
^�B
_B
_B
_B
_!B
_�B
`B
`BB
`\B
`vB
`vB
`�B
`�B
aHB
a�B
a�B
a�B
b4B
b4B
b�B
cB
cB
c B
c:B
cTB
c�B
c�B
c�B
c�B
dZB
dZB
dZB
d�B
d�B
eB
e,B
e`B
ezB
e�B
e�B
e�B
f2B
ffB
f�B
f�B
f�B
f�B
gRB
g�B
g�B
g�B
g�B
h
B
h$B
hXB
h�B
h�B
h�B
h�B
h�B
h�B
i*B
i*B
i_B
i�B
jeB
jeB
jB
j�B
kB
j�B
kB
k�B
k�B
k�B
k�B
l"B
l=B
l�B
l�B
l�B
l�B
l�B
mB
mCB
mwB
m�B
m�B
m�B
n/B
n}B
n}B
n}B
o B
oiB
o�B
o�B
o�B
o�B
o�B
p!B
p�B
p�B
p�B
qAB
q�B
q�B
r-B
r|B
r�B
r�B
sB
shB
s�B
s�B
s�B
t9B
t�B
t�B
t�B
uZB
utB
utB
u�B
u�B
v+B
vzB
v�B
v�B
v�B
v�B
wfB
w�B
w�B
xB
x8B
xRB
x�B
x�B
x�B
x�B
y$B
yXB
y�B
y�B
y�B
y�B
y�B
z*B
z*B
z^B
z^B
z�B
z�B
z�B
{B
{B
{B
{0B
{0B
{0B
{JB
{B
{�B
{�B
|jB
|jB
|�B
|�B
|�B
|�B
}B
}qB
}�B
~(B
~]B
~wB
~�B
~�B
~�B
~�B
~�B
.B
HB
}B
�B
�B
�4B
�OB
��B
��B
��B
��B
��B
�B
�oB
�o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104948  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175025  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175025  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175025                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025033  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025033  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                