CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:48:01Z creation;2022-06-04T17:48:01Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174801  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�ִ"R��1   @�ִ�4Vy@.�bM���c�j~��#1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  Aa��A���A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh��Bq33BvffB��B�  B�ffB�  B�  B�  B���B�  B�  B���B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,33C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Db��Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D��3D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ D�|�D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@�  @�  A   A   A@  Aa��A���A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh��Bq33BvffB��B�  B�ffB�  B�  B�  B���B�  B�  B���B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,33C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Db��Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D��3D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ D�|�D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�aA�A��AऩA�AࡖA�-A���A��A��A���A��!A���A���A���A���A��VA��	A���A��YA�cA�o5A��/AݻdA��.A�� A��#A���Aܼ�A܁;A�>wA�VmA�*eA��A�QNA�0�A�ӏAȘ_AŲ�Aă�A¨$A��vA�qAA���A���A�#�A��A���A�A�S�A��MA�A��A�c�A���A�;0A�D�A�<jA��qA��A���A��:A���A��uA���A� 4A�GzA�S�A��A���A��YA�FA���A��GA���A��A��A�R�A�a�A���A��SA��uA���A�K�A���A��A}͟AzCAv�'Aq
�An�AkTaAh�,Ag*�Ac�vA`ԕA]�AST�AP�AN��AL�AHA�AF�AD��A@��A?�A<��A:��A8��A7�9A4�A1��A0�EA.^�A-\)A,��A+��A+��A+A*��A*�&A*�A*�CA*qA*MA)��A'�*A%�A%>BA$یA$/A"��A !�AH�A�BA9XA�A��A�Ah�A�AA<6APHA�uA@�A��AS�A�OAaA2�A�cA�=AVmAM�AQ�A]dAf�Ak�A;�A�"A�
Al"A�A��A��AjA��A��A��A5?A��A��A�`A�A�A��A�AK^AoA�8A�HA�}AU�A
ϫA
�FA
H�A
!�A	��A	'RA�QAqvA�Aw�AGEA1'A�A_pA�~A�Ar�A��A��AzxA^�AC-A=�A%FAخAc�A�"A}�A;dACA ��A �A E�@�A�@���@�4n@�G@�f�@���@��@�	l@�s�@��[@��4@��k@�1'@��D@�M�@�� @��@��N@�Ov@��@�p�@��@��m@�a@��2@�Z�@���@�y>@�+k@�!�@��@�p�@��/@�Z�@��^@�.@�*�@�!�@�4@�m�@��@�C-@��@�8@��K@��@���@���@�j�@��@��]@�bN@�  @���@�^@�+@�C�@ߟV@�'�@�w�@�&@��@���@�_@� �@ٗ�@�N<@��@؜x@�N�@�M@׊	@��@�(�@��>@բ�@�7L@Ԑ.@�[�@�C-@��@��a@�u�@��@ҧ@��@�_p@�Dg@���@�c�@��@ϔ�@�?}@��@�7@��d@͜�@�\�@�҉@�D�@ˎ�@�&@���@�ԕ@�e,@��8@�~�@�~@��N@Ǉ�@��@Ʊ�@�_�@���@Œ:@��@�N�@�s�@�;@�E9@���@±�@�Z�@��K@�+@��@�"�@��)@�PH@�	@��C@�-w@�֡@��r@�]d@�Q@�$@�$@��9@��P@�~(@�"h@��A@��@��@���@��@��6@�~(@�m�@�H�@�o @�1�@��@�w�@��j@�g�@�[W@�B�@��@���@�n�@��@��H@��@���@��@��[@�@@��@�($@���@���@�J#@�ی@���@�<�@�@��n@�@���@�q@�$�@���@��@��#@��F@��4@��@���@�-�@���@��@�o @���@��@��w@��7@�S@��@�-@���@�\�@�Dg@��@��E@��I@�C�@�6@�1�@�1'@�e@��@��w@��@��@�m�@�(�@��@@�/@��5@��@�0U@���@�f�@���@��}@�C-@��o@�˒@�@O@�ߤ@���@���@�h
@��@���@��@�A�@��"@���@��@���@�y�@�5�@�Ɇ@���@�J�@���@�Mj@���@��4@��Y@�V@�� @�w2@�Y�@�1�@��@� �@��@��@���@�B�@��@���@��@�h
@�'R@��@��@�w2@�;d@��|@��j@���@�\�@�R�@��.@��@�Vm@��@��M@��y@���@�!@��@��Z@��@���@���@�O@�*0@��@���@�V�@�!@���@��
@��@��[@��f@�qv@�@O@�	l@��@��_@�&�@��@��j@��~@�N<@���@���@�;�@��@���@�o @�!-@���@��+@�YK@�'R@�� @�p�@�6z@���@�ȴ@���@�u%@�6�@��Z@�@�|�@�F�@�	l@���@�Ĝ@��1@�-�@�!�@��#@��"@�RT@��@���@�Ta@�:�@�M@�&@j�@~��@~_@}^�@}�@|��@|9X@{��@{v`@{'�@z�B@zl�@z.�@z�@y��@y��@yj@y&�@y�@x�f@x�@x	�@w�;@w�6@wO@v�"@v��@u��@u7L@t]d@s��@s�*@sP�@r��@rff@r@�@r@q��@q��@q4@qV@p�K@p�@p4n@o��@o�g@o��@o
=@n�}@nz@n=q@n�@m�#@m��@mV@lw�@k�@@kC@j��@j��@j�F@j�@i�"@i?}@h��@hV�@h:�@h*�@g��@g;d@g�@f��@f��@f@�@f�@e�9@eB�@d�5@d�@c�
@c�V@c�	@c|�@c,�@b�,@bW�@b��@b_@`�@`�@_��@_s@_�@^�m@^J�@]��@]�@]e,@\��@\9X@[�q@[/�@Z��@Z)�@Y�@Y��@YX@Y;@X��@X|�@XC-@X@W�K@Wqv@W
=@V� @VTa@VGE@V#:@U��@U7L@T��@TC-@T�@S�V@Se�@S�@R��@R�+@R)�@R �@Q�@Q�@Q�@QQ�@P�[@P!@O�6@O�@@O��@ORT@N�y@Nq�@M�@M�M@Me,@MG�@Lm�@K��@K�@@KH�@J��@J�\@J�@I��@I@I�=@Ihs@I+�@H��@H֡@H`�@H@G�0@G�@F�@F�b@Fh
@F�@F
�@E�D@E�T@EF@D�[@DH@D~@Dx@C� @C��@C{J@C>�@Bߤ@B_�@A��@A��@A�n@AV@@�	@@�	@@��@@��@@��@@�@@�.@@Xy@@9X@@@?��@?��@?+@>��@>z@>_@=�=@=G�@=!�@<�@<�4@<m�@;��@;a@:��@:^5@:�@9��@9�@9�S@9j@95�@9	l@8�f@8֡@8�p@8�e@8�@8�o@8�@7{J@7K�@7!-@6�8@6��@6i�@6R�@6:*@6�@5�N@5S&@4��@4'R@3�@2�"@2�@2͟@2͟@2��@2�'@2��@2��@2R�@2Q@2@�@25?@2�@1�~@0�Y@0/�@/��@/��@/b�@/"�@.��@.Ta@-�o@-�C@-�"@-G�@-#�@,�@,�D@,~@+��@+�@@+U�@*��@*d�@*L0@)��@)��@)5�@)@(�	@(�?@(��@(w�@(c�@(1'@(	�@'��@'1�@&�8@&ߤ@&�@&� @&s�@&3�@&J@&�@%�Z@%�@%Y�@%?}@%%F@$�	@$��@$V�@$1@#�}@#�[@#��@#X�@"�]@"��@"�6@"�b@"�1@"��@"s�@"C�@"�@!�@!��@!\�@!@@!�@ ��@ �5@ �@ ی@ Ɇ@ ��@ [�@ ?�@ G@��@��@qv@A�@�@�@�@� @�A@3�@e@�)@��@@c@B�@�@�@��@�.@�@u�@Z@,=@��@�@6z@�"@�@҉@�!@��@YK@)�@��@��@��@m]@IR@2a@��@��@:�@�w@��@y�@H�@,�@�y@��@n�@8�@$�@�@��@��@O�@A @!�@q@�@�v@�@�Y@PH@/�@�]@�@��@�P@W?@�@��@�+@i�@GE@_@��@�)@�@��@�d@�H@��@��@<6@�@;@�K@��@�@u�@g8@?�@�@�g@��@X�@(@�c@��@��@z@Z�@e@�j@��@�"111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�aA�A��AऩA�AࡖA�-A���A��A��A���A��!A���A���A���A���A��VA��	A���A��YA�cA�o5A��/AݻdA��.A�� A��#A���Aܼ�A܁;A�>wA�VmA�*eA��A�QNA�0�A�ӏAȘ_AŲ�Aă�A¨$A��vA�qAA���A���A�#�A��A���A�A�S�A��MA�A��A�c�A���A�;0A�D�A�<jA��qA��A���A��:A���A��uA���A� 4A�GzA�S�A��A���A��YA�FA���A��GA���A��A��A�R�A�a�A���A��SA��uA���A�K�A���A��A}͟AzCAv�'Aq
�An�AkTaAh�,Ag*�Ac�vA`ԕA]�AST�AP�AN��AL�AHA�AF�AD��A@��A?�A<��A:��A8��A7�9A4�A1��A0�EA.^�A-\)A,��A+��A+��A+A*��A*�&A*�A*�CA*qA*MA)��A'�*A%�A%>BA$یA$/A"��A !�AH�A�BA9XA�A��A�Ah�A�AA<6APHA�uA@�A��AS�A�OAaA2�A�cA�=AVmAM�AQ�A]dAf�Ak�A;�A�"A�
Al"A�A��A��AjA��A��A��A5?A��A��A�`A�A�A��A�AK^AoA�8A�HA�}AU�A
ϫA
�FA
H�A
!�A	��A	'RA�QAqvA�Aw�AGEA1'A�A_pA�~A�Ar�A��A��AzxA^�AC-A=�A%FAخAc�A�"A}�A;dACA ��A �A E�@�A�@���@�4n@�G@�f�@���@��@�	l@�s�@��[@��4@��k@�1'@��D@�M�@�� @��@��N@�Ov@��@�p�@��@��m@�a@��2@�Z�@���@�y>@�+k@�!�@��@�p�@��/@�Z�@��^@�.@�*�@�!�@�4@�m�@��@�C-@��@�8@��K@��@���@���@�j�@��@��]@�bN@�  @���@�^@�+@�C�@ߟV@�'�@�w�@�&@��@���@�_@� �@ٗ�@�N<@��@؜x@�N�@�M@׊	@��@�(�@��>@բ�@�7L@Ԑ.@�[�@�C-@��@��a@�u�@��@ҧ@��@�_p@�Dg@���@�c�@��@ϔ�@�?}@��@�7@��d@͜�@�\�@�҉@�D�@ˎ�@�&@���@�ԕ@�e,@��8@�~�@�~@��N@Ǉ�@��@Ʊ�@�_�@���@Œ:@��@�N�@�s�@�;@�E9@���@±�@�Z�@��K@�+@��@�"�@��)@�PH@�	@��C@�-w@�֡@��r@�]d@�Q@�$@�$@��9@��P@�~(@�"h@��A@��@��@���@��@��6@�~(@�m�@�H�@�o @�1�@��@�w�@��j@�g�@�[W@�B�@��@���@�n�@��@��H@��@���@��@��[@�@@��@�($@���@���@�J#@�ی@���@�<�@�@��n@�@���@�q@�$�@���@��@��#@��F@��4@��@���@�-�@���@��@�o @���@��@��w@��7@�S@��@�-@���@�\�@�Dg@��@��E@��I@�C�@�6@�1�@�1'@�e@��@��w@��@��@�m�@�(�@��@@�/@��5@��@�0U@���@�f�@���@��}@�C-@��o@�˒@�@O@�ߤ@���@���@�h
@��@���@��@�A�@��"@���@��@���@�y�@�5�@�Ɇ@���@�J�@���@�Mj@���@��4@��Y@�V@�� @�w2@�Y�@�1�@��@� �@��@��@���@�B�@��@���@��@�h
@�'R@��@��@�w2@�;d@��|@��j@���@�\�@�R�@��.@��@�Vm@��@��M@��y@���@�!@��@��Z@��@���@���@�O@�*0@��@���@�V�@�!@���@��
@��@��[@��f@�qv@�@O@�	l@��@��_@�&�@��@��j@��~@�N<@���@���@�;�@��@���@�o @�!-@���@��+@�YK@�'R@�� @�p�@�6z@���@�ȴ@���@�u%@�6�@��Z@�@�|�@�F�@�	l@���@�Ĝ@��1@�-�@�!�@��#@��"@�RT@��@���@�Ta@�:�@�M@�&@j�@~��@~_@}^�@}�@|��@|9X@{��@{v`@{'�@z�B@zl�@z.�@z�@y��@y��@yj@y&�@y�@x�f@x�@x	�@w�;@w�6@wO@v�"@v��@u��@u7L@t]d@s��@s�*@sP�@r��@rff@r@�@r@q��@q��@q4@qV@p�K@p�@p4n@o��@o�g@o��@o
=@n�}@nz@n=q@n�@m�#@m��@mV@lw�@k�@@kC@j��@j��@j�F@j�@i�"@i?}@h��@hV�@h:�@h*�@g��@g;d@g�@f��@f��@f@�@f�@e�9@eB�@d�5@d�@c�
@c�V@c�	@c|�@c,�@b�,@bW�@b��@b_@`�@`�@_��@_s@_�@^�m@^J�@]��@]�@]e,@\��@\9X@[�q@[/�@Z��@Z)�@Y�@Y��@YX@Y;@X��@X|�@XC-@X@W�K@Wqv@W
=@V� @VTa@VGE@V#:@U��@U7L@T��@TC-@T�@S�V@Se�@S�@R��@R�+@R)�@R �@Q�@Q�@Q�@QQ�@P�[@P!@O�6@O�@@O��@ORT@N�y@Nq�@M�@M�M@Me,@MG�@Lm�@K��@K�@@KH�@J��@J�\@J�@I��@I@I�=@Ihs@I+�@H��@H֡@H`�@H@G�0@G�@F�@F�b@Fh
@F�@F
�@E�D@E�T@EF@D�[@DH@D~@Dx@C� @C��@C{J@C>�@Bߤ@B_�@A��@A��@A�n@AV@@�	@@�	@@��@@��@@��@@�@@�.@@Xy@@9X@@@?��@?��@?+@>��@>z@>_@=�=@=G�@=!�@<�@<�4@<m�@;��@;a@:��@:^5@:�@9��@9�@9�S@9j@95�@9	l@8�f@8֡@8�p@8�e@8�@8�o@8�@7{J@7K�@7!-@6�8@6��@6i�@6R�@6:*@6�@5�N@5S&@4��@4'R@3�@2�"@2�@2͟@2͟@2��@2�'@2��@2��@2R�@2Q@2@�@25?@2�@1�~@0�Y@0/�@/��@/��@/b�@/"�@.��@.Ta@-�o@-�C@-�"@-G�@-#�@,�@,�D@,~@+��@+�@@+U�@*��@*d�@*L0@)��@)��@)5�@)@(�	@(�?@(��@(w�@(c�@(1'@(	�@'��@'1�@&�8@&ߤ@&�@&� @&s�@&3�@&J@&�@%�Z@%�@%Y�@%?}@%%F@$�	@$��@$V�@$1@#�}@#�[@#��@#X�@"�]@"��@"�6@"�b@"�1@"��@"s�@"C�@"�@!�@!��@!\�@!@@!�@ ��@ �5@ �@ ی@ Ɇ@ ��@ [�@ ?�@ G@��@��@qv@A�@�@�@�@� @�A@3�@e@�)@��@@c@B�@�@�@��@�.@�@u�@Z@,=@��@�@6z@�"@�@҉@�!@��@YK@)�@��@��@��@m]@IR@2a@��@��@:�@�w@��@y�@H�@,�@�y@��@n�@8�@$�@�@��@��@O�@A @!�@q@�@�v@�@�Y@PH@/�@�]@�@��@�P@W?@�@��@�+@i�@GE@_@��@�)@�@��@�d@�H@��@��@<6@�@;@�K@��@�@u�@g8@?�@�@�g@��@X�@(@�c@��@��@z@Z�@e@�j@��@�"111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	zB	zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	1B	fB	�B	B	W�B	b�B	a�B	dB	h>B	p�B	uB	z�B	�)B	��B
:�B
wfB
Q�B
0B
)�B
G�B
X�B
�4B
�B
��B
��B
�B
�B
�B�BjBC{B[WBbBv�B��B�}B�(B�gB�%BʦBѷB�~B��B��B��B�B�VB�UB��B��B�sB��Bm�BgRB`'BH�B4nB�B
��B
�qB
�B
��B
vB
gB
I�B
%�B
�B
 B	�B	�yB	�FB	��B	�rB	s�B	dtB	H�B	0�B	�B�B�nBݲB�sB��B�(B�jB�B��B��B�6B��B��B�UB��B��B�xB��B�rB�MB�B	�B	/B	0!B	G�B	NVB	dtB	vzB	�'B	r�B	p�B	{�B	��B	~�B	x�B	n�B	i�B	i�B	r�B	sMB	poB	p�B	p!B	��B	��B	�1B	ĶB	�}B	��B	�QB	�B	�~B	��B	�@B	��B	��B	�B	��B	��B	�aB	�%B	��B	�zB	��B	�B	�'B	�[B	�B	��B	��B	�JB	�0B	��B	��B	�-B	ĜB	�tB	�AB	ªB	��B	�B	�B	�B	��B	˒B	�PB	̈́B	͟B	�jB	�6B	�B	̳B	��B	��B	̘B	�PB	ϑB	��B	��B	� B	ΥB	�dB	�B	ΥB	�.B	бB	�NB	уB	�NB	�hB	��B	�hB	��B	бB	бB	�}B	�B	�oB	� B	� B	҉B	�B	��B	�hB	ЗB	ϑB	ЗB	ѷB	��B	ѝB	�&B	ٴB	ߤB	�'B	�'B	��B	��B	�-B	�\B	�B	��B	�'B	�vB	ߊB	�jB	�IB	��B	�ZB	��B	�KB	��B	��B	�mB	�B	�TB	��B	�B	�B	�B	��B	�`B	�B	�LB	�2B	�LB	�sB	�sB	�B	�>B	�XB	�>B	�B	�B	�B	��B	�B	�B	�=B	�B	�B	�B	�@B	�TB	�TB	�B	�:B	�tB	�&B	�B	�@B	��B	��B	��B	�B	�@B	�,B	��B	�2B	�2B	�B	�8B	�2B	�fB	��B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�wB	�B	�)B	�B	��B	�=B	�wB	�}B	��B	�B	�!B	��B	�OB	��B	�AB	�B	�GB	��B	�vB	��B	�AB	�;B	�B	��B	�IB	�B	��B	�B	��B	�AB	�oB	��B	�B	��B	�AB	�B	�aB	�B	�|B	�B	�aB	�B	�hB	�9B	�TB	�aB	�B	�B	��B	�B	�B	�B	�hB	�aB	�B	��B	�B	��B	��B	�tB	��B	��B	�%B	�B	�%B	�ZB	�tB	��B	�+B	�FB	��B	��B	�B	�B	��B	��B	��B	�+B	��B	�LB	�fB	�2B	�2B	�2B	��B	�lB	�	B	��B	�B	��B	�B	��B	�qB	��B	�B
 OB
 B
�B
B
�B
�B
 �B
 B
�B
AB
AB
[B
�B
�B
oB
�B
�B
�B
[B
uB
�B
uB
�B
�B
tB
�B
YB
+B
+B
_B
�B
�B
1B
�B
	B
	7B
	RB
	�B
	�B
	�B
	�B

rB

�B

�B
B
^B
�B
JB
dB
�B
6B
�B
�B
�B
�B
�B
vB
vB
�B
B
�B
 B
4B
4B
�B
�B
&B
[B
[B
,B
{B
,B
aB
�B
B
�B
�B
�B
B
9B
SB
�B
�B
�B
$B
$B
�B
�B
�B
yB
�B
B
B
B
�B
�B
	B
	B
	B
	B
=B
qB
�B
�B
�B
�B
xB
�B
�B
�B
�B
B
IB
IB
�B
�B
B
�B
VB
�B
�B
 vB
 �B
!�B
"�B
"�B
#B
#B
"�B
#nB
$B
$�B
$�B
%B
%�B
%�B
&2B
&�B
&�B
'B
'8B
'�B
(�B
)�B
*KB
*�B
+QB
+QB
+6B
+QB
,"B
+�B
+�B
+�B
,=B
,�B
-B
,�B
-B
-CB
-]B
-�B
.}B
/B
/�B
/�B
0!B
0UB
0�B
0�B
1AB
1vB
1�B
1�B
1�B
2�B
2�B
3B
3�B
3�B
3�B
4nB
5ZB
5tB
5?B
5%B
4�B
5ZB
5�B
6B
6B
6�B
7�B
8RB
8�B
8�B
9	B
9	B
9	B
9	B
9$B
9$B
9$B
9>B
9rB
9rB
9XB
9rB
9�B
9�B
9�B
9�B
9�B
9�B
:*B
:*B
:^B
:�B
;B
;B
;B
;�B
<B
<B
<6B
<6B
<6B
<�B
<�B
<�B
="B
=B
=B
=B
=VB
=<B
="B
=�B
=�B
>(B
>�B
>�B
>�B
?.B
?cB
@�B
@ B
AB
A�B
A B
@�B
@�B
AB
AB
AUB
A�B
A�B
A�B
A�B
B'B
B�B
B�B
C-B
C�B
DMB
D�B
D�B
E�B
G+B
GzB
GzB
G�B
HB
HB
HfB
H�B
H�B
IB
IB
IB
IlB
IlB
I�B
J=B
J�B
J�B
J�B
J�B
KB
K)B
K�B
K�B
K�B
K�B
K�B
K�B
LJB
L�B
MB
L�B
MB
MB
MjB
M�B
NB
NVB
N<B
NB
N�B
OBB
OBB
OBB
O�B
O�B
PB
P.B
P.B
PHB
PHB
P�B
P�B
P�B
Q4B
QNB
QhB
RB
R B
R:B
R:B
RoB
RTB
RTB
RTB
R�B
SB
S@B
S&B
S@B
S�B
S�B
S�B
S�B
TB
T�B
T�B
T�B
T�B
UMB
UMB
UMB
UMB
UMB
UgB
U�B
U�B
U�B
U�B
U�B
U�B
U�B
VmB
V�B
V�B
W
B
WsB
WsB
WsB
W�B
W�B
W�B
XEB
X_B
X�B
Y1B
YeB
YeB
YB
Y�B
Y�B
ZB
Z7B
Z7B
Z7B
ZQB
ZQB
ZQB
ZQB
Z�B
[#B
[=B
[=B
[WB
[�B
[�B
[�B
[�B
[�B
[�B
\)B
\CB
\�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^B
^B
^B
^B
^B
]�B
^�B
_VB
_�B
_�B
_�B
_�B
`'B
`BB
`�B
`�B
`�B
aB
aHB
aHB
aHB
a�B
b4B
bhB
b�B
b�B
c�B
c�B
c�B
c�B
d@B
dtB
d�B
d�B
d�B
d�B
eB
e,B
ezB
e�B
ezB
e�B
e�B
fB
fB
fLB
f�B
g�B
g�B
g�B
g�B
h$B
hXB
hXB
h>B
hsB
h�B
h�B
h�B
h�B
iB
i*B
iDB
iDB
iDB
iDB
iDB
i_B
i�B
i�B
i�B
i�B
jKB
jeB
j�B
kB
kB
k6B
kQB
kQB
kQB
k�B
k�B
l"B
l"B
lqB
l�B
l�B
mB
m)B
mCB
m�B
m�B
m�B
m�B
n/B
m�B
nIB
ncB
ncB
n�B
n�B
o B
o5B
oOB
oiB
oiB
o�B
o�B
o�B
pB
p;B
p�B
p�B
p�B
p�B
p�B
qB
q'B
qAB
qvB
q�B
q�B
q�B
q�B
q�B
r-B
raB
r�B
sMB
shB
shB
s�B
s�B
s�B
tB
tB
tTB
tTB
tnB
t�B
t�B
u%B
u%B
u?B
u?B
u?B
uZB
u�B
u�B
u�B
u�B
v+B
v+B
v`B
v�B
v�B
v�B
w2B
w�B
wfB
w�B
w�B
w�B
xB
xB
xB
xB
xB
xRB
xRB
x�B
x�B
x�B
x�B
y$B
y>B
yXB
yXB
y�B
y�B
y�B
y�B
z*B
zxB
z�B
z�B
z�B
z�B
{B
{B
{�B
|B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	zB	zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	1B	fB	�B	B	W�B	b�B	a�B	dB	h>B	p�B	uB	z�B	�)B	��B
:�B
wfB
Q�B
0B
)�B
G�B
X�B
�4B
�B
��B
��B
�B
�B
�B�BjBC{B[WBbBv�B��B�}B�(B�gB�%BʦBѷB�~B��B��B��B�B�VB�UB��B��B�sB��Bm�BgRB`'BH�B4nB�B
��B
�qB
�B
��B
vB
gB
I�B
%�B
�B
 B	�B	�yB	�FB	��B	�rB	s�B	dtB	H�B	0�B	�B�B�nBݲB�sB��B�(B�jB�B��B��B�6B��B��B�UB��B��B�xB��B�rB�MB�B	�B	/B	0!B	G�B	NVB	dtB	vzB	�'B	r�B	p�B	{�B	��B	~�B	x�B	n�B	i�B	i�B	r�B	sMB	poB	p�B	p!B	��B	��B	�1B	ĶB	�}B	��B	�QB	�B	�~B	��B	�@B	��B	��B	�B	��B	��B	�aB	�%B	��B	�zB	��B	�B	�'B	�[B	�B	��B	��B	�JB	�0B	��B	��B	�-B	ĜB	�tB	�AB	ªB	��B	�B	�B	�B	��B	˒B	�PB	̈́B	͟B	�jB	�6B	�B	̳B	��B	��B	̘B	�PB	ϑB	��B	��B	� B	ΥB	�dB	�B	ΥB	�.B	бB	�NB	уB	�NB	�hB	��B	�hB	��B	бB	бB	�}B	�B	�oB	� B	� B	҉B	�B	��B	�hB	ЗB	ϑB	ЗB	ѷB	��B	ѝB	�&B	ٴB	ߤB	�'B	�'B	��B	��B	�-B	�\B	�B	��B	�'B	�vB	ߊB	�jB	�IB	��B	�ZB	��B	�KB	��B	��B	�mB	�B	�TB	��B	�B	�B	�B	��B	�`B	�B	�LB	�2B	�LB	�sB	�sB	�B	�>B	�XB	�>B	�B	�B	�B	��B	�B	�B	�=B	�B	�B	�B	�@B	�TB	�TB	�B	�:B	�tB	�&B	�B	�@B	��B	��B	��B	�B	�@B	�,B	��B	�2B	�2B	�B	�8B	�2B	�fB	��B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�wB	�B	�)B	�B	��B	�=B	�wB	�}B	��B	�B	�!B	��B	�OB	��B	�AB	�B	�GB	��B	�vB	��B	�AB	�;B	�B	��B	�IB	�B	��B	�B	��B	�AB	�oB	��B	�B	��B	�AB	�B	�aB	�B	�|B	�B	�aB	�B	�hB	�9B	�TB	�aB	�B	�B	��B	�B	�B	�B	�hB	�aB	�B	��B	�B	��B	��B	�tB	��B	��B	�%B	�B	�%B	�ZB	�tB	��B	�+B	�FB	��B	��B	�B	�B	��B	��B	��B	�+B	��B	�LB	�fB	�2B	�2B	�2B	��B	�lB	�	B	��B	�B	��B	�B	��B	�qB	��B	�B
 OB
 B
�B
B
�B
�B
 �B
 B
�B
AB
AB
[B
�B
�B
oB
�B
�B
�B
[B
uB
�B
uB
�B
�B
tB
�B
YB
+B
+B
_B
�B
�B
1B
�B
	B
	7B
	RB
	�B
	�B
	�B
	�B

rB

�B

�B
B
^B
�B
JB
dB
�B
6B
�B
�B
�B
�B
�B
vB
vB
�B
B
�B
 B
4B
4B
�B
�B
&B
[B
[B
,B
{B
,B
aB
�B
B
�B
�B
�B
B
9B
SB
�B
�B
�B
$B
$B
�B
�B
�B
yB
�B
B
B
B
�B
�B
	B
	B
	B
	B
=B
qB
�B
�B
�B
�B
xB
�B
�B
�B
�B
B
IB
IB
�B
�B
B
�B
VB
�B
�B
 vB
 �B
!�B
"�B
"�B
#B
#B
"�B
#nB
$B
$�B
$�B
%B
%�B
%�B
&2B
&�B
&�B
'B
'8B
'�B
(�B
)�B
*KB
*�B
+QB
+QB
+6B
+QB
,"B
+�B
+�B
+�B
,=B
,�B
-B
,�B
-B
-CB
-]B
-�B
.}B
/B
/�B
/�B
0!B
0UB
0�B
0�B
1AB
1vB
1�B
1�B
1�B
2�B
2�B
3B
3�B
3�B
3�B
4nB
5ZB
5tB
5?B
5%B
4�B
5ZB
5�B
6B
6B
6�B
7�B
8RB
8�B
8�B
9	B
9	B
9	B
9	B
9$B
9$B
9$B
9>B
9rB
9rB
9XB
9rB
9�B
9�B
9�B
9�B
9�B
9�B
:*B
:*B
:^B
:�B
;B
;B
;B
;�B
<B
<B
<6B
<6B
<6B
<�B
<�B
<�B
="B
=B
=B
=B
=VB
=<B
="B
=�B
=�B
>(B
>�B
>�B
>�B
?.B
?cB
@�B
@ B
AB
A�B
A B
@�B
@�B
AB
AB
AUB
A�B
A�B
A�B
A�B
B'B
B�B
B�B
C-B
C�B
DMB
D�B
D�B
E�B
G+B
GzB
GzB
G�B
HB
HB
HfB
H�B
H�B
IB
IB
IB
IlB
IlB
I�B
J=B
J�B
J�B
J�B
J�B
KB
K)B
K�B
K�B
K�B
K�B
K�B
K�B
LJB
L�B
MB
L�B
MB
MB
MjB
M�B
NB
NVB
N<B
NB
N�B
OBB
OBB
OBB
O�B
O�B
PB
P.B
P.B
PHB
PHB
P�B
P�B
P�B
Q4B
QNB
QhB
RB
R B
R:B
R:B
RoB
RTB
RTB
RTB
R�B
SB
S@B
S&B
S@B
S�B
S�B
S�B
S�B
TB
T�B
T�B
T�B
T�B
UMB
UMB
UMB
UMB
UMB
UgB
U�B
U�B
U�B
U�B
U�B
U�B
U�B
VmB
V�B
V�B
W
B
WsB
WsB
WsB
W�B
W�B
W�B
XEB
X_B
X�B
Y1B
YeB
YeB
YB
Y�B
Y�B
ZB
Z7B
Z7B
Z7B
ZQB
ZQB
ZQB
ZQB
Z�B
[#B
[=B
[=B
[WB
[�B
[�B
[�B
[�B
[�B
[�B
\)B
\CB
\�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^B
^B
^B
^B
^B
]�B
^�B
_VB
_�B
_�B
_�B
_�B
`'B
`BB
`�B
`�B
`�B
aB
aHB
aHB
aHB
a�B
b4B
bhB
b�B
b�B
c�B
c�B
c�B
c�B
d@B
dtB
d�B
d�B
d�B
d�B
eB
e,B
ezB
e�B
ezB
e�B
e�B
fB
fB
fLB
f�B
g�B
g�B
g�B
g�B
h$B
hXB
hXB
h>B
hsB
h�B
h�B
h�B
h�B
iB
i*B
iDB
iDB
iDB
iDB
iDB
i_B
i�B
i�B
i�B
i�B
jKB
jeB
j�B
kB
kB
k6B
kQB
kQB
kQB
k�B
k�B
l"B
l"B
lqB
l�B
l�B
mB
m)B
mCB
m�B
m�B
m�B
m�B
n/B
m�B
nIB
ncB
ncB
n�B
n�B
o B
o5B
oOB
oiB
oiB
o�B
o�B
o�B
pB
p;B
p�B
p�B
p�B
p�B
p�B
qB
q'B
qAB
qvB
q�B
q�B
q�B
q�B
q�B
r-B
raB
r�B
sMB
shB
shB
s�B
s�B
s�B
tB
tB
tTB
tTB
tnB
t�B
t�B
u%B
u%B
u?B
u?B
u?B
uZB
u�B
u�B
u�B
u�B
v+B
v+B
v`B
v�B
v�B
v�B
w2B
w�B
wfB
w�B
w�B
w�B
xB
xB
xB
xB
xB
xRB
xRB
x�B
x�B
x�B
x�B
y$B
y>B
yXB
yXB
y�B
y�B
y�B
y�B
z*B
zxB
z�B
z�B
z�B
z�B
{B
{B
{�B
|B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104943  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174801  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174801  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174801                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024809  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024809  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                