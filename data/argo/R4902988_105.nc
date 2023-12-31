CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-08-26T09:41:33Z creation;2022-08-26T09:41:33Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220826094133  20220826095832  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               iA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @���b	�Z1   @��ζ͎�@:8Q���d �\)1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   AA��A`  A~ffA�  A�  A�33A�  A���A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BHffBP  BX  B`  Bh  Bp  BxffB�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz�C|  C}�fC�  C�  C��3C�  C�  C��3C�  C��3C��3C�  C��C��C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C��3C��C��C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  D   D �fD  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D�fD  D� DfD� D  D� D  D� DfD�fD  Dy�D��Dy�D  D� D  D� D  D� D  Dy�D  D�fD  Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJy�DK  DK� DL  DL�fDMfDM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DRy�DR��DSy�DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\fD\� D]fD]�fD^  D^� D^��D_� D`  D`� Da  Da�fDbfDb� Db��Dcy�Dd  Dd� DefDe� Df  Df� Df��Dg� Dh  Dh� Di  Di� Di��Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dx� Dy  Dy� Dz  Dz� D{  D{� D{��D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�C3D�� D�� D�  D�<�D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�3D�@ D D�� D�  D�@ DÀ D�� D�  D�@ Dă3D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D���D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ Dμ�D���D�<�D�|�D�� D�  D�@ DЀ D�� D�  D�@ Dр D��3D�3D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D��3D�  D�@ DՀ Dռ�D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�3D�@ Dـ D�� D�  D�<�Dڀ D�� D�  D�@ D�|�Dۼ�D�  D�@ D܃3D�� D�  D�@ D݀ D�� D�3D�C3Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D��3D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�  A   A   AA��A`  A~ffA�  A�  A�33A�  A���A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BHffBP  BX  B`  Bh  Bp  BxffB�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz�C|  C}�fC�  C�  C��3C�  C�  C��3C�  C��3C��3C�  C��C��C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C��3C��C��C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  D   D �fD  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D�fD  D� DfD� D  D� D  D� DfD�fD  Dy�D��Dy�D  D� D  D� D  D� D  Dy�D  D�fD  Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJy�DK  DK� DL  DL�fDMfDM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DRy�DR��DSy�DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\fD\� D]fD]�fD^  D^� D^��D_� D`  D`� Da  Da�fDbfDb� Db��Dcy�Dd  Dd� DefDe� Df  Df� Df��Dg� Dh  Dh� Di  Di� Di��Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dx� Dy  Dy� Dz  Dz� D{  D{� D{��D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�C3D�� D�� D�  D�<�D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�3D�@ D D�� D�  D�@ DÀ D�� D�  D�@ Dă3D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D���D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ Dμ�D���D�<�D�|�D�� D�  D�@ DЀ D�� D�  D�@ Dр D��3D�3D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D��3D�  D�@ DՀ Dռ�D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�3D�@ Dـ D�� D�  D�<�Dڀ D�� D�  D�@ D�|�Dۼ�D�  D�@ D܃3D�� D�  D�@ D݀ D�� D�3D�C3Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D��3D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�� A��A��A���A��xA��	A��[AڶzAڋ�A�xA��A��pA�gAئ�A���Aד�A�@�A�EmA��AՅ�A�2�A�d�A���AɶFA��A�S�A�q�A�H�A���A��A��A��vA��{A��'A�3�A��tA��1A���A�7A��A�~(A���A���A�YA��2A�X�A���A�"�A���A���A�бA�P�A��mA�0�A�A���A�K�A���A�6FA��^A���A�/�A��A�A�@OA��RA�<A��fA��A���A��FA���A�]/A� �A��(A�ܒA��'A�*�A��A��xA���A���A�h
A�1�A���A���A��=A��^A��A���A��KA�g�A�+kA� 4A�Y�A��A��1A��A~�UA~�nA}��A{V�Ay-Av��Au$tArYKAoF�Ai�yAhTaAf��Ae�0Aea|AdB�Ab��Aa�A^�A]��A]�A[e�AZ�PAY��AYq�AY.�AX�XAXAWGEAV�AS�AR�AR2aAP�AP��APaAN\�AL��AKo�AJ�MAI)_AGp�AEo AC�PAA��A@��A>ںA>i�A=�A=|A<��A;��A8�hA8!A7��A7h
A7�A64�A5J�A4S�A3�<A2��A2xA1��A1%FA/��A/_A.�$A-��A-�A,�6A,��A,�hA+��A+��A*&A'��A&�<A&�A%�MA$�/A#y�A"jA!��A!�A E�A�-A�A��A:�A��Ap�A@OAU�A�2A��A�SAf�A�.A3�A^�A�A($A��AFA�A��ArGAc A�AP�A��A($AϫA�{A��AZ�A
��A
��A
y�A
Z�A	�]A	�A��A�A�A�rAB�A�]A�uA	At�A�jA��A Xy@�U2@��@���@���@�1�@�a�@�Ɇ@�&@�PH@��)@�c�@�_@��@�>�@�q�@�hs@� i@䒣@�bN@�$@��@��@⟾@��@��@�>B@؟�@�,�@��@֖�@�Ta@�@�=@ԟ�@�C@�b@�h�@��@ʒ�@��@ȯO@ȹ�@ȴ9@�y�@�Vm@�h�@�@���@���@��@���@��@��f@���@���@�=q@���@���@�(@�C�@��>@���@��@��@�4�@���@�W?@��$@��g@��<@���@��@�.�@��@�A�@��k@�C�@���@�)_@�^�@��&@���@��3@�/@�_�@�o@���@��h@��4@�ѷ@�i�@�#:@���@���@��K@�q@��e@�M@�ݘ@��0@��V@���@�e�@�O@�'�@��@���@��@�\�@�&�@�x@��@�O�@�+�@���@�� @���@�!-@���@�)_@���@���@�c�@���@�r�@��D@��k@�^�@��s@�_@��@��3@��@�e@��5@���@�c@�j�@�e,@�a�@�c�@��@���@���@��@��@�<6@��@�@@���@�)�@���@��S@���@�Y�@���@��@��@��P@�RT@��<@�{�@�A�@��r@��'@��@��@���@��/@��e@�z�@�PH@�!�@��@��Z@���@��@���@�6z@��@�q@�B[@�ݘ@��P@��O@��.@�/�@��t@���@�l�@�[W@�=�@��@��h@���@�y>@�4@��@;d@~��@~Ta@|�4@|<�@|1'@|(�@|@|@|1@|G@{��@{�f@z��@y��@x�P@x�P@y@x��@x<�@x �@w�]@w�A@wݘ@w�g@w�6@w��@wE9@w>�@w/�@v�@vxl@u��@u \@t�u@s˒@r��@r�x@r��@rh
@rZ�@r;�@q��@q�d@q5�@p~(@p?�@p>B@p6@p<�@p?�@p�@o�r@o�@o�@o��@ov`@oP�@oJ#@o6z@n͟@n+k@m�@m��@m��@mp�@mN<@l�[@l��@l��@ll"@l�@lG@k��@k�w@kJ#@k+@k@j�X@i�Z@hZ@g��@g|�@ga@g4�@f��@f�b@f^5@e�.@e�@e�@e[W@e<6@e�@d�/@d��@d�@de�@d~@cP�@b?@a�@`�v@_�@^;�@]�@]�C@]�@]e,@\�$@\C-@\>B@\<�@\A�@\/�@\�@[�W@[�K@[��@[�q@[C�@Z�@Xی@Xm�@XC-@X(�@W�w@Wx@W�@V�@V�\@V�A@V�A@VQ@U�^@U��@U�~@Uc�@T�E@T��@T7�@S�P@S@O@Rں@R1�@Q�@QO�@Q�@Q;@Poi@O�m@O�:@O=@N�M@Me,@L�@Kn/@J��@J��@Js�@J�@I��@IG�@I(�@I#�@I+@I�@IV@I�@H��@H�f@H�v@H[�@HI�@HK^@H<�@H�@G�}@G��@Gl�@GF�@G@O@GK�@G&@G i@G@F�H@F��@F��@Fff@FQ@FJ�@F.�@E��@E��@E��@D��@Dh�@DD�@DM@C�F@CO@B��@B{�@BYK@BGE@B)�@Be@B�@B�@B�@B �@A��@A�o@A�@A��@A��@A�@A�N@A�n@A�7@AS&@@��@?��@?�[@?��@?b�@?F�@?,�@?C@>�M@>�@>��@>�@>�+@>Ta@>C�@=ԕ@=Dg@=+�@<�|@<�	@<Ĝ@<h�@<2�@;خ@;�V@;\)@;�@:�,@:�x@:}V@:ff@:C�@:�@9s�@8�p@82�@7��@7��@7qv@7Mj@74�@7�@6��@6�,@6�L@6n�@6_�@6C�@66�@6�@5�@5�@5w2@5�@4֡@4�j@4~(@4l"@4Xy@4N�@4"h@3�m@3�@@3(@2�@2B[@1��@1�@0�_@0"h@/� @.ȴ@.xl@.q�@.i�@.L0@-�T@-o @-[W@-*0@,��@,@+�@+�6@+��@+O@+C@*�H@*i�@)�9@)c@(�@(1@'��@'\)@&ں@&��@&@�@%�)@%�d@%�=@%p�@%J�@%5�@$��@$�[@$�U@$��@$e�@$7@#��@#��@#�	@#qv@#,�@#�@"�8@"�@"�H@"҉@"�<@"{�@!�@!�3@!e,@!L�@!0�@!0�@!#�@ �@ �j@ ��@ Xy@�]@˒@�$@F�@)_@"�@C@�@�@Y@�@�@�+@�@�r@h
@�@�@��@��@��@��@L�@-w@�v@�r@ƨ@�*@,�@��@��@�X@� @n�@#:@�@��@x�@+�@�v@�4@�Y@h�@N�@*�@��@�m@�m@��@�a@�k@��@|�@l�@>�@ں@O@�@u@ԕ@��@+�@�@�[@�u@bN@1'@�r@��@��@=@@��@�\@�\@�F@i�@($@O@#:@�@�D@�@�#@�@��@ϫ@�@��@��@��@�X@�7@c@2a@�O@|�@H@6@-�@'R@�]@��@�w@��@W?@��@��@�R@�6@s�@V@B[@+k@@��@5�@�@��@�)@��@�@l"@]d@*�@x@��@�$@F�@
��@
�@
�@
�@
�L@
�L@
��@
�r@
L0@
8�@
1�@
1�@
�@
	@	�d@	�~@	e,@��@�@��@�.@��@C-@%�@��@�@��@��@�@@�f@~�@e�@1�@�@��@GE@@�@�@��@�@��@��@�@s�@`B@O�@;@��@��@�@�f@�e@m�@Ft@1'@~@@��@��@RT@�M@�@�@@�n@O�@*0@@@;@ ��@ �P@ ��@�@�@ �`@ ѷ@ �?@ ��@ �@ �I@ �I@ �@ ~(@ oi@ h�@ q@ g8@ j@ l"@ l"111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�� A��A��A���A��xA��	A��[AڶzAڋ�A�xA��A��pA�gAئ�A���Aד�A�@�A�EmA��AՅ�A�2�A�d�A���AɶFA��A�S�A�q�A�H�A���A��A��A��vA��{A��'A�3�A��tA��1A���A�7A��A�~(A���A���A�YA��2A�X�A���A�"�A���A���A�бA�P�A��mA�0�A�A���A�K�A���A�6FA��^A���A�/�A��A�A�@OA��RA�<A��fA��A���A��FA���A�]/A� �A��(A�ܒA��'A�*�A��A��xA���A���A�h
A�1�A���A���A��=A��^A��A���A��KA�g�A�+kA� 4A�Y�A��A��1A��A~�UA~�nA}��A{V�Ay-Av��Au$tArYKAoF�Ai�yAhTaAf��Ae�0Aea|AdB�Ab��Aa�A^�A]��A]�A[e�AZ�PAY��AYq�AY.�AX�XAXAWGEAV�AS�AR�AR2aAP�AP��APaAN\�AL��AKo�AJ�MAI)_AGp�AEo AC�PAA��A@��A>ںA>i�A=�A=|A<��A;��A8�hA8!A7��A7h
A7�A64�A5J�A4S�A3�<A2��A2xA1��A1%FA/��A/_A.�$A-��A-�A,�6A,��A,�hA+��A+��A*&A'��A&�<A&�A%�MA$�/A#y�A"jA!��A!�A E�A�-A�A��A:�A��Ap�A@OAU�A�2A��A�SAf�A�.A3�A^�A�A($A��AFA�A��ArGAc A�AP�A��A($AϫA�{A��AZ�A
��A
��A
y�A
Z�A	�]A	�A��A�A�A�rAB�A�]A�uA	At�A�jA��A Xy@�U2@��@���@���@�1�@�a�@�Ɇ@�&@�PH@��)@�c�@�_@��@�>�@�q�@�hs@� i@䒣@�bN@�$@��@��@⟾@��@��@�>B@؟�@�,�@��@֖�@�Ta@�@�=@ԟ�@�C@�b@�h�@��@ʒ�@��@ȯO@ȹ�@ȴ9@�y�@�Vm@�h�@�@���@���@��@���@��@��f@���@���@�=q@���@���@�(@�C�@��>@���@��@��@�4�@���@�W?@��$@��g@��<@���@��@�.�@��@�A�@��k@�C�@���@�)_@�^�@��&@���@��3@�/@�_�@�o@���@��h@��4@�ѷ@�i�@�#:@���@���@��K@�q@��e@�M@�ݘ@��0@��V@���@�e�@�O@�'�@��@���@��@�\�@�&�@�x@��@�O�@�+�@���@�� @���@�!-@���@�)_@���@���@�c�@���@�r�@��D@��k@�^�@��s@�_@��@��3@��@�e@��5@���@�c@�j�@�e,@�a�@�c�@��@���@���@��@��@�<6@��@�@@���@�)�@���@��S@���@�Y�@���@��@��@��P@�RT@��<@�{�@�A�@��r@��'@��@��@���@��/@��e@�z�@�PH@�!�@��@��Z@���@��@���@�6z@��@�q@�B[@�ݘ@��P@��O@��.@�/�@��t@���@�l�@�[W@�=�@��@��h@���@�y>@�4@��@;d@~��@~Ta@|�4@|<�@|1'@|(�@|@|@|1@|G@{��@{�f@z��@y��@x�P@x�P@y@x��@x<�@x �@w�]@w�A@wݘ@w�g@w�6@w��@wE9@w>�@w/�@v�@vxl@u��@u \@t�u@s˒@r��@r�x@r��@rh
@rZ�@r;�@q��@q�d@q5�@p~(@p?�@p>B@p6@p<�@p?�@p�@o�r@o�@o�@o��@ov`@oP�@oJ#@o6z@n͟@n+k@m�@m��@m��@mp�@mN<@l�[@l��@l��@ll"@l�@lG@k��@k�w@kJ#@k+@k@j�X@i�Z@hZ@g��@g|�@ga@g4�@f��@f�b@f^5@e�.@e�@e�@e[W@e<6@e�@d�/@d��@d�@de�@d~@cP�@b?@a�@`�v@_�@^;�@]�@]�C@]�@]e,@\�$@\C-@\>B@\<�@\A�@\/�@\�@[�W@[�K@[��@[�q@[C�@Z�@Xی@Xm�@XC-@X(�@W�w@Wx@W�@V�@V�\@V�A@V�A@VQ@U�^@U��@U�~@Uc�@T�E@T��@T7�@S�P@S@O@Rں@R1�@Q�@QO�@Q�@Q;@Poi@O�m@O�:@O=@N�M@Me,@L�@Kn/@J��@J��@Js�@J�@I��@IG�@I(�@I#�@I+@I�@IV@I�@H��@H�f@H�v@H[�@HI�@HK^@H<�@H�@G�}@G��@Gl�@GF�@G@O@GK�@G&@G i@G@F�H@F��@F��@Fff@FQ@FJ�@F.�@E��@E��@E��@D��@Dh�@DD�@DM@C�F@CO@B��@B{�@BYK@BGE@B)�@Be@B�@B�@B�@B �@A��@A�o@A�@A��@A��@A�@A�N@A�n@A�7@AS&@@��@?��@?�[@?��@?b�@?F�@?,�@?C@>�M@>�@>��@>�@>�+@>Ta@>C�@=ԕ@=Dg@=+�@<�|@<�	@<Ĝ@<h�@<2�@;خ@;�V@;\)@;�@:�,@:�x@:}V@:ff@:C�@:�@9s�@8�p@82�@7��@7��@7qv@7Mj@74�@7�@6��@6�,@6�L@6n�@6_�@6C�@66�@6�@5�@5�@5w2@5�@4֡@4�j@4~(@4l"@4Xy@4N�@4"h@3�m@3�@@3(@2�@2B[@1��@1�@0�_@0"h@/� @.ȴ@.xl@.q�@.i�@.L0@-�T@-o @-[W@-*0@,��@,@+�@+�6@+��@+O@+C@*�H@*i�@)�9@)c@(�@(1@'��@'\)@&ں@&��@&@�@%�)@%�d@%�=@%p�@%J�@%5�@$��@$�[@$�U@$��@$e�@$7@#��@#��@#�	@#qv@#,�@#�@"�8@"�@"�H@"҉@"�<@"{�@!�@!�3@!e,@!L�@!0�@!0�@!#�@ �@ �j@ ��@ Xy@�]@˒@�$@F�@)_@"�@C@�@�@Y@�@�@�+@�@�r@h
@�@�@��@��@��@��@L�@-w@�v@�r@ƨ@�*@,�@��@��@�X@� @n�@#:@�@��@x�@+�@�v@�4@�Y@h�@N�@*�@��@�m@�m@��@�a@�k@��@|�@l�@>�@ں@O@�@u@ԕ@��@+�@�@�[@�u@bN@1'@�r@��@��@=@@��@�\@�\@�F@i�@($@O@#:@�@�D@�@�#@�@��@ϫ@�@��@��@��@�X@�7@c@2a@�O@|�@H@6@-�@'R@�]@��@�w@��@W?@��@��@�R@�6@s�@V@B[@+k@@��@5�@�@��@�)@��@�@l"@]d@*�@x@��@�$@F�@
��@
�@
�@
�@
�L@
�L@
��@
�r@
L0@
8�@
1�@
1�@
�@
	@	�d@	�~@	e,@��@�@��@�.@��@C-@%�@��@�@��@��@�@@�f@~�@e�@1�@�@��@GE@@�@�@��@�@��@��@�@s�@`B@O�@;@��@��@�@�f@�e@m�@Ft@1'@~@@��@��@RT@�M@�@�@@�n@O�@*0@@@;@ ��@ �P@ ��@�@�@ �`@ ѷ@ �?@ ��@ �@ �I@ �I@ �@ ~(@ oi@ h�@ q@ g8@ j@ l"@ l"111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B+QB+�B+6B+kB+6B*�B(>B&�B$�B!�BOByB B+B��B��B��B��B�B�B��B�9BڠB�gB��B��B�FB��B��B��B��B�AB�OBl�Bg�Bc:BaHB`'BZ�BW�BTaBT�BH�BD�B;�B6�B2�B-�B vB�B�B�]B�fB��B�gB�B�#B�[B�RB��B�9B�;Bz�By	BR�BC�B49B)�B(sB'8B&�B$tB"�B�B)BB�B�B�B�BЗB�jB�RB�YBªB��B��B��B�B�)B{�BtBp!Bl�Bc�BQ�B@OB$�BYB�BbB�B
�cB
�bB
�{B
��B
��B
�JB
|�B
v�B
n�B
l�B
gB
[�B
U�B
C�B
<�B
:B
2B
,B
(
B
%�B
%FB
#�B
 �B
dB
�B
_B
3B
�B	�^B	�lB	��B	�B	��B	�CB	��B	��B	�_B	�jB	�	B	�B	��B	��B	�B	�B	��B	��B	�-B	�$B	��B	��B	�B	�<B	��B	��B	��B	�B	|�B	x�B	v�B	uZB	qvB	nIB	nB	h�B	g�B	e�B	eB	dtB	b�B	`vB	_�B	YeB	SuB	Q B	P.B	N"B	I7B	D�B	CB	@�B	>BB	<�B	:�B	7�B	7�B	5�B	5?B	2�B	/�B	-B	,B	+kB	)�B	&�B	$&B	OB	1B	B	�B	�B	2B	MB	:B	hB	hB	hB	�B	(B	�B	�B	0B	
�B		�B	�B	�B	�B	B	%B	�B	�B	�B	B	 OB��B��B�B�B�DB��B�rB�2B�B�TB�B�B�B��B�5B��B�eB�_B��B�B�yB��B�QB�kB�6B�QB�B�B��B�0B�wB��B�2B�fB�B�B�mB�sB�B�DB�
B�>B�:B�vB��B�B�@B��B�B�eB�}B�B�}B��B�B�MB�aB�tB�B��B��B��B�<B��B�]B�]B��B�XB�	B��B�]B�}B	�B	�B	fB	�B	YB	�B	�B	3B	�B	
	B	NB	�B	�B	aB	IB	 BB	"�B	$�B	%�B	)DB	/iB	3�B	7B	:B	;�B	=�B	>�B	?}B	CGB	H�B	J�B	MB	O�B	P�B	Q4B	Q�B	RoB	S�B	TFB	U�B	V�B	ZQB	[�B	\CB	^B	_VB	`�B	d@B	d@B	f�B	gmB	j�B	o�B	z*B	�B	��B	��B	��B	��B	��B	��B	�GB	�B	��B	��B	�B	��B	��B	��B	�B	��B	�B	��B	�$B	�sB	��B	�B	��B	�5B	�!B	�hB	�zB	�^B	�$B	�RB	��B	�2B	��B	�2B	��B	��B	��B	� B	��B	�SB	�dB	�\B	҉B	�SB	ۦB	�4B	�@B	�FB	�fB	�B	�B	�yB	�KB	��B	�B	��B	�B	��B	�!B	�B	��B
 4B
 �B
mB
�B
%B
KB
�B
PB
B
�B
vB
�B
,B
SB
?B
WB
/B
;B
!�B
#TB
*KB
,=B
-)B
-�B
/OB
/iB
/�B
0B
0�B
2�B
8�B
:�B
>(B
?}B
AB
ESB
IB
I�B
JXB
J�B
KDB
K�B
LJB
N�B
Q4B
QNB
QhB
R:B
TaB
V�B
XB
Y�B
[�B
_�B
b�B
c�B
d&B
dtB
d�B
e�B
fLB
h�B
n�B
q[B
q�B
raB
raB
r�B
s�B
s�B
tB
uB
u�B
v`B
vzB
vzB
v�B
yXB
}B
~�B
}B
�B
�iB
�B
��B
��B
�GB
�{B
��B
�B
�9B
�B
��B
��B
�KB
��B
��B
�B
�4B
��B
�@B
�B
�MB
��B
��B
��B
��B
�KB
��B
��B
�7B
��B
��B
��B
�#B
��B
��B
��B
�!B
�|B
�B
��B
�B
�B
�6B
��B
�wB
��B
��B
��B
��B
��B
�OB
�OB
��B
��B
��B
��B
��B
��B
��B
�	B
�$B
��B
��B
��B
�B
��B
�B
��B
��B
�UB
��B
��B
�B
ÖB
āB
�mB
�_B
��B
ȴB
�=B
��B
�0B
��B
��B
�VB
�\B
�B
�}B
��B
�,B
�mB
�$B
�B
�B
�EB
��B
ٚB
��B
�qB
یB
��B
��B
�)B
ܒB
ܬB
��B
�dB
�!B
ߊB
�\B
�B
�B
�HB
�B
�NB
�B
�B
�B
� B
�B
�nB
�B
��B
�B
�`B
�zB
�zB
��B
�B
�B
�B
��B
�B
�DB
�yB
�KB
�B
�"B
��B
��B
�)B
�wB
�B
�B
��B
��B
��B
��B
�B
�B
�/B
�/B
�/B
�IB
�B
��B
�B
��B
��B
�aB
�B
��B
�3B
�hB
�B
��B
�B
�9B
�nB
��B
�B
�B
�B
�2B
�fB
��B
��B
�RB
��B
�>B
�B
�xB
��B
��B
�B
�jB
��B
��B
��B
�<B
�BB
��B �BBoB�BBABuB�B�BGB�B�BB3B�B�B�BmB%B�B�B�B+BEB_B�B�BKB	RB	�B
rBxBB�BjBB�BBBB.B�B�B�B�B�B�B�B�BFB�B�B2B�B�B$B+BKBBQB	B�BBxB�B�B/BdBdB�BBBjB�B!B�B�B�B B vB �B �B �B �B �B �B!HB"NB"hB#B# B#TB#:B#TB#�B#�B$B$tB$�B%,B%zB%�B&B&2B&2B&2B&2B&2B&LB'B'B'B'B'RB'�B(
B(XB(XB(XB(sB(�B(�B)DB*�B*�B*�B+�B,=B,"B,B,qB,�B,�B-wB-�B-�B.IB.�B/ B/B/OB/�B/�B/�B0B0B/�B0;B0oB0oB0�B0�B0�B1[B2aB2GB2GB2�B2�B3hB3�B3�B4B4nB4�B5B5tB5tB5�B6B6zB6�B6�B6�B6�B7�B7fB7fB7�B7�B7�B7�B7�B7�B7�B8B8B8B8B8B8lB8RB8�B9rB9�B:B:*B:B:*B:^B:xB:�B:�B;dB;�B<B<B<B<�B<�B<�B<�B<�B=<B>BB>]B>]B>�B>�B?.B?.B?.B?�B?�B@ B@iB@�BA;BAoBA�BA�BA�BA�BA�BA�BB[BB[BB[BB[BB�BB�BB�BC-BCGBC�BDMBDgBDgBDgBD�BD�BEBE9BE�BE�BE�BE�BE�BE�BF?BF�BF�BG�BG_BG�BG�BHBHfBHfBHfBH�BH�BH�BIlBIlBIlBIlBIlBI�BJ=BJXBJrBJ�BJ�BJ�BK)BK�BL0BL�BM�BM�BM�BNVBN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BOBBOBBOBBOBBO\BO\BOvBOvBOvBO�BO�BO�BO�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B+QB+�B+6B+kB+6B*�B(>B&�B$�B!�BOByB B+B��B��B��B��B�B�B��B�9BڠB�gB��B��B�FB��B��B��B��B�AB�OBl�Bg�Bc:BaHB`'BZ�BW�BTaBT�BH�BD�B;�B6�B2�B-�B vB�B�B�]B�fB��B�gB�B�#B�[B�RB��B�9B�;Bz�By	BR�BC�B49B)�B(sB'8B&�B$tB"�B�B)BB�B�B�B�BЗB�jB�RB�YBªB��B��B��B�B�)B{�BtBp!Bl�Bc�BQ�B@OB$�BYB�BbB�B
�cB
�bB
�{B
��B
��B
�JB
|�B
v�B
n�B
l�B
gB
[�B
U�B
C�B
<�B
:B
2B
,B
(
B
%�B
%FB
#�B
 �B
dB
�B
_B
3B
�B	�^B	�lB	��B	�B	��B	�CB	��B	��B	�_B	�jB	�	B	�B	��B	��B	�B	�B	��B	��B	�-B	�$B	��B	��B	�B	�<B	��B	��B	��B	�B	|�B	x�B	v�B	uZB	qvB	nIB	nB	h�B	g�B	e�B	eB	dtB	b�B	`vB	_�B	YeB	SuB	Q B	P.B	N"B	I7B	D�B	CB	@�B	>BB	<�B	:�B	7�B	7�B	5�B	5?B	2�B	/�B	-B	,B	+kB	)�B	&�B	$&B	OB	1B	B	�B	�B	2B	MB	:B	hB	hB	hB	�B	(B	�B	�B	0B	
�B		�B	�B	�B	�B	B	%B	�B	�B	�B	B	 OB��B��B�B�B�DB��B�rB�2B�B�TB�B�B�B��B�5B��B�eB�_B��B�B�yB��B�QB�kB�6B�QB�B�B��B�0B�wB��B�2B�fB�B�B�mB�sB�B�DB�
B�>B�:B�vB��B�B�@B��B�B�eB�}B�B�}B��B�B�MB�aB�tB�B��B��B��B�<B��B�]B�]B��B�XB�	B��B�]B�}B	�B	�B	fB	�B	YB	�B	�B	3B	�B	
	B	NB	�B	�B	aB	IB	 BB	"�B	$�B	%�B	)DB	/iB	3�B	7B	:B	;�B	=�B	>�B	?}B	CGB	H�B	J�B	MB	O�B	P�B	Q4B	Q�B	RoB	S�B	TFB	U�B	V�B	ZQB	[�B	\CB	^B	_VB	`�B	d@B	d@B	f�B	gmB	j�B	o�B	z*B	�B	��B	��B	��B	��B	��B	��B	�GB	�B	��B	��B	�B	��B	��B	��B	�B	��B	�B	��B	�$B	�sB	��B	�B	��B	�5B	�!B	�hB	�zB	�^B	�$B	�RB	��B	�2B	��B	�2B	��B	��B	��B	� B	��B	�SB	�dB	�\B	҉B	�SB	ۦB	�4B	�@B	�FB	�fB	�B	�B	�yB	�KB	��B	�B	��B	�B	��B	�!B	�B	��B
 4B
 �B
mB
�B
%B
KB
�B
PB
B
�B
vB
�B
,B
SB
?B
WB
/B
;B
!�B
#TB
*KB
,=B
-)B
-�B
/OB
/iB
/�B
0B
0�B
2�B
8�B
:�B
>(B
?}B
AB
ESB
IB
I�B
JXB
J�B
KDB
K�B
LJB
N�B
Q4B
QNB
QhB
R:B
TaB
V�B
XB
Y�B
[�B
_�B
b�B
c�B
d&B
dtB
d�B
e�B
fLB
h�B
n�B
q[B
q�B
raB
raB
r�B
s�B
s�B
tB
uB
u�B
v`B
vzB
vzB
v�B
yXB
}B
~�B
}B
�B
�iB
�B
��B
��B
�GB
�{B
��B
�B
�9B
�B
��B
��B
�KB
��B
��B
�B
�4B
��B
�@B
�B
�MB
��B
��B
��B
��B
�KB
��B
��B
�7B
��B
��B
��B
�#B
��B
��B
��B
�!B
�|B
�B
��B
�B
�B
�6B
��B
�wB
��B
��B
��B
��B
��B
�OB
�OB
��B
��B
��B
��B
��B
��B
��B
�	B
�$B
��B
��B
��B
�B
��B
�B
��B
��B
�UB
��B
��B
�B
ÖB
āB
�mB
�_B
��B
ȴB
�=B
��B
�0B
��B
��B
�VB
�\B
�B
�}B
��B
�,B
�mB
�$B
�B
�B
�EB
��B
ٚB
��B
�qB
یB
��B
��B
�)B
ܒB
ܬB
��B
�dB
�!B
ߊB
�\B
�B
�B
�HB
�B
�NB
�B
�B
�B
� B
�B
�nB
�B
��B
�B
�`B
�zB
�zB
��B
�B
�B
�B
��B
�B
�DB
�yB
�KB
�B
�"B
��B
��B
�)B
�wB
�B
�B
��B
��B
��B
��B
�B
�B
�/B
�/B
�/B
�IB
�B
��B
�B
��B
��B
�aB
�B
��B
�3B
�hB
�B
��B
�B
�9B
�nB
��B
�B
�B
�B
�2B
�fB
��B
��B
�RB
��B
�>B
�B
�xB
��B
��B
�B
�jB
��B
��B
��B
�<B
�BB
��B �BBoB�BBABuB�B�BGB�B�BB3B�B�B�BmB%B�B�B�B+BEB_B�B�BKB	RB	�B
rBxBB�BjBB�BBBB.B�B�B�B�B�B�B�B�BFB�B�B2B�B�B$B+BKBBQB	B�BBxB�B�B/BdBdB�BBBjB�B!B�B�B�B B vB �B �B �B �B �B �B!HB"NB"hB#B# B#TB#:B#TB#�B#�B$B$tB$�B%,B%zB%�B&B&2B&2B&2B&2B&2B&LB'B'B'B'B'RB'�B(
B(XB(XB(XB(sB(�B(�B)DB*�B*�B*�B+�B,=B,"B,B,qB,�B,�B-wB-�B-�B.IB.�B/ B/B/OB/�B/�B/�B0B0B/�B0;B0oB0oB0�B0�B0�B1[B2aB2GB2GB2�B2�B3hB3�B3�B4B4nB4�B5B5tB5tB5�B6B6zB6�B6�B6�B6�B7�B7fB7fB7�B7�B7�B7�B7�B7�B7�B8B8B8B8B8B8lB8RB8�B9rB9�B:B:*B:B:*B:^B:xB:�B:�B;dB;�B<B<B<B<�B<�B<�B<�B<�B=<B>BB>]B>]B>�B>�B?.B?.B?.B?�B?�B@ B@iB@�BA;BAoBA�BA�BA�BA�BA�BA�BB[BB[BB[BB[BB�BB�BB�BC-BCGBC�BDMBDgBDgBDgBD�BD�BEBE9BE�BE�BE�BE�BE�BE�BF?BF�BF�BG�BG_BG�BG�BHBHfBHfBHfBH�BH�BH�BIlBIlBIlBIlBIlBI�BJ=BJXBJrBJ�BJ�BJ�BK)BK�BL0BL�BM�BM�BM�BNVBN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BOBBOBBOBBOBBO\BO\BOvBOvBOvBO�BO�BO�BO�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220826094131  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220826094133  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220826094133  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220826094133                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220826184137  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220826184137  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20220826095832                      G�O�G�O�G�O�                