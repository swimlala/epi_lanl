CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:12:03Z creation;2022-06-04T19:12:03Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ͱ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20220604191203  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ع}����1   @ع~u\@0����o�d-1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BZffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C L�C� C  C  C  C
  C  C  C  C  C  C  C  C  C33C  C�fC!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN�CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�&fC��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.fD.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�C3Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BZffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C L�C� C  C  C  C
  C  C  C  C  C  C  C  C  C33C  C�fC!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN�CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�&fC��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.fD.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�C3Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��,AΘ�A��A͝�A͇�A�~�A�r�A�j�A�a�A�\�A�V9A�QA�J�A�E9A�=�A�9XA��A��cA��5A��A���A���A̹�Ą�Å�A��A���AǢhA���A�4nA�wfA�_Aĥ�A�aHA��aAÀ�A�GA�یA%A�~(A�=qA�JA��IA��oA��JA���A�ffA�\A�o�A��HA�_A��,A��A�hsA��A��A��(A��A�g8A��=A�*�A���A���A���A��A���A�hsA��\A�b�A��oA�ϫA�O�A�?}A��A�9XA�m�A���A��A��PA�F�A�0�A�{A�uZA�@�A�S[A��=A���A���A�ܒA�GA��AA���A��dA�?�A��fA��A�M6A�33A�\�A�0UA��,A��A|v�AzcAw��At��Ar�AoخAl�,Aj��Ah˒Ab�A_�EA_A[�AXc�AVXyAU4AQ�AN?AL,�AI��AG��AFz�AE�9AD�AC�RAC:*A@�0A>4nA="hA9��A6��A6bNA6-A6(A5��A5��A62aA5�?A4]�A07�A*ffA)�3A)S&A'�A$�PA#h�A"JA!�fA �A��AcA�A��A�A
�Au�A��A�An�A�A�"A"hA	�A��A%A�"AA A�A��A�A��A��A\�A�.A��A�AT�A�A<6A�A�}AxA)�A�A&A��AF�A��A�SA'RA�}A�?A{�A@OA��A
�IA	�#A	��A	\�A��AxAqA��A[WA�IAo A�qA9XA�A��A��A�A�FAo�A ֡@��@�S&@���@��#@���@��@��@��@��v@��q@�[W@�.I@�V@�ߤ@��b@�Z@�4@��@�\)@�F@�R�@��r@�@�c@�Y�@���@��3@�[W@�K�@��@���@�]�@�
=@��@��@��@�]d@�C@�^�@�1�@��@��@��A@�($@�]�@���@��U@�u%@�!�@��8@�^5@�@�\�@��,@��@�e�@�@�͟@ݛ=@ە�@�H�@��@ڵ�@�خ@�n�@��@�Z�@���@֣�@�!@Վ�@Թ$@��3@�6z@�ѷ@�y>@��o@��@�H�@���@�x@��@��,@Ι1@�!�@ͼ@�Y@̵@̇�@�M@�4@�ݘ@˼@�~�@�#�@��M@ʾ@�e@���@�33@��M@�v�@� �@ǨX@�+�@Ƭ@�Ft@�ϫ@�m]@�<6@��@�
=@ĺ�@�ff@�)�@ý�@Â�@�j�@�\�@�'�@x@�+k@��a@��k@�k�@��@���@�M@��@��@���@��v@���@���@�X@�2a@��[@���@�GE@��{@��@���@�<�@��Z@���@�P�@��M@��E@���@�>B@�ԕ@�A�@�\�@���@��;@�a@��O@�u@���@�Dg@��H@��D@�U2@�PH@�I�@�	@���@���@�y�@���@��@�.�@��@��@��@��}@��-@��X@�n�@�V@�:�@�	�@���@�_p@�B�@���@�6@��r@���@�,�@��Q@�33@��P@���@�R�@�)�@�U�@��@�z@�_@�?�@�($@��@��@��@�k�@�Vm@�E9@�&@��|@�&�@���@�!�@��@��1@�C-@��@�L�@�&�@���@�͟@���@�h
@�@�@��t@��@���@��@�{�@�;�@�b@��+@�خ@���@�33@���@��$@��O@��L@���@�� @�*�@��-@��X@���@�.I@��)@�^5@��@��d@��'@�?}@���@��F@�,=@��)@�خ@��K@��:@��@�;d@��Y@�_@�e@��@�s�@�@@��@�S�@�˒@���@�u�@�P�@���@���@���@�u%@��@��g@�dZ@���@�l�@�:*@�-@��@���@�ی@�YK@�
�@���@��@�͟@���@��@�V@�E�@�C-@�-�@���@�`B@���@�� @��@�E9@�B�@�+@���@���@���@�͟@��<@��@��r@�y>@�]d@��@��.@��D@��m@���@�8@�֡@���@�d�@�PH@�C�@�4@���@���@��X@��V@��	@�~�@�O@�/@��@��@��c@��)@�n�@�~@��.@���@��@��@��}@��H@��@���@�e,@��H@���@���@��1@��Y@�I�@��@�
@�@�@~�F@~!�@}�@|�/@|l"@|�@{��@{�6@{�4@{�@z�'@z{�@z#:@y�@yo @x�j@w�]@w��@wX�@v�X@v�x@v�@u&�@t~(@t�@s��@so�@s$t@r�2@r�@rM�@q�@qQ�@p��@o�@oX�@o"�@n�y@nv�@m��@m!�@l�z@lV�@l~@k�m@kMj@j�,@j+k@i��@ihs@i;@htT@h:�@g|�@f��@f^5@ek�@e+�@d��@d��@dtT@d_@d:�@c˒@cMj@b�@b�B@bxl@a�-@`�5@`h�@`@_��@_��@__p@^��@^
�@]�C@]zx@]X@]F@\��@\��@\oi@\�@[�Q@[qv@Z�@Zh
@Y�X@Y<6@Xی@X�I@Xr�@Xq@Xl"@XD�@X<�@W��@W�:@Wy�@W@O@WY@V�2@V�}@Vd�@V?@V4@U�>@U[W@UA @U0�@U�@T��@T��@T��@Te�@T1'@S��@S_p@R��@R{@Q�.@Q��@Q:�@P�|@P�@P��@P��@P�D@P��@PbN@P7�@P@O��@O�
@O�@@O�f@OdZ@OJ#@N��@N�@M+�@L�v@L��@L�z@K�@K&@KS@K(@J��@I�T@Iϫ@I�@I�M@H��@H��@H  @G�K@G��@G/�@F�2@F@�@E�.@E�o@E��@E�"@D�4@D�@C��@Ca@CK�@B�]@B�1@BYK@B!�@A��@A8�@A�@@֡@@��@@%�@?��@?��@?O@?,�@>�@>W�@>@=�=@=/@<�@<4n@;��@;��@;��@;dZ@;8@:��@:^5@9��@9��@9!�@8�4@7�r@7�[@7��@7~�@7g�@6�H@6��@6z@63�@5�Z@5��@5`B@4��@4��@4�D@4oi@4M@3�@3E9@3@2�M@2{�@2e@1��@1j@17L@0�@0`�@0:�@/�r@/�Q@/�@/�@/X�@/33@.�y@.��@.C�@-�@-�z@-��@-�S@-IR@,�E@,�4@,�@,c�@,I�@,*�@,�@,@+�]@+ݘ@+�@+��@+|�@+]�@+6z@+.I@+�@*�@*�@*~�@*p;@*^5@*-@)��@)ϫ@)��@)j@)(�@(��@(��@(C-@(�@'��@'��@'�@'��@'|�@'s@'j�@'33@'�@'�@&��@&��@&z@&0U@%�@%�T@%��@%`B@%<6@%q@$Ĝ@$��@$h�@$?�@#�r@#��@#g�@#O@#6z@#�@"��@"� @"d�@"\�@"?@"_@!�T@!��@!c@!Vm@!�@ ��@ �@ ��@ oi@ Ft@��@�$@_p@�@��@�c@��@_�@($@�@��@��@�@�@�S@u�@B�@;@�	@�5@ѷ@��@z�@Xy@4n@(�@ݘ@�F@��@~�@O@(@�s@��@� @@�@�@��@��@��@��@8�@�@��@w�@H@�@�f@8@�@�"@�B@~�@C�@e@	@	@��@�d@�t@��@w2@<6@��@|�@g8@PH@$@�@��@;d@Y@(@�@�6@�\@z@^5@;�@4@�@@��@�@k�@�@�f@�@�j@��@�Y@bN@1'@!@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��,AΘ�A��A͝�A͇�A�~�A�r�A�j�A�a�A�\�A�V9A�QA�J�A�E9A�=�A�9XA��A��cA��5A��A���A���A̹�Ą�Å�A��A���AǢhA���A�4nA�wfA�_Aĥ�A�aHA��aAÀ�A�GA�یA%A�~(A�=qA�JA��IA��oA��JA���A�ffA�\A�o�A��HA�_A��,A��A�hsA��A��A��(A��A�g8A��=A�*�A���A���A���A��A���A�hsA��\A�b�A��oA�ϫA�O�A�?}A��A�9XA�m�A���A��A��PA�F�A�0�A�{A�uZA�@�A�S[A��=A���A���A�ܒA�GA��AA���A��dA�?�A��fA��A�M6A�33A�\�A�0UA��,A��A|v�AzcAw��At��Ar�AoخAl�,Aj��Ah˒Ab�A_�EA_A[�AXc�AVXyAU4AQ�AN?AL,�AI��AG��AFz�AE�9AD�AC�RAC:*A@�0A>4nA="hA9��A6��A6bNA6-A6(A5��A5��A62aA5�?A4]�A07�A*ffA)�3A)S&A'�A$�PA#h�A"JA!�fA �A��AcA�A��A�A
�Au�A��A�An�A�A�"A"hA	�A��A%A�"AA A�A��A�A��A��A\�A�.A��A�AT�A�A<6A�A�}AxA)�A�A&A��AF�A��A�SA'RA�}A�?A{�A@OA��A
�IA	�#A	��A	\�A��AxAqA��A[WA�IAo A�qA9XA�A��A��A�A�FAo�A ֡@��@�S&@���@��#@���@��@��@��@��v@��q@�[W@�.I@�V@�ߤ@��b@�Z@�4@��@�\)@�F@�R�@��r@�@�c@�Y�@���@��3@�[W@�K�@��@���@�]�@�
=@��@��@��@�]d@�C@�^�@�1�@��@��@��A@�($@�]�@���@��U@�u%@�!�@��8@�^5@�@�\�@��,@��@�e�@�@�͟@ݛ=@ە�@�H�@��@ڵ�@�خ@�n�@��@�Z�@���@֣�@�!@Վ�@Թ$@��3@�6z@�ѷ@�y>@��o@��@�H�@���@�x@��@��,@Ι1@�!�@ͼ@�Y@̵@̇�@�M@�4@�ݘ@˼@�~�@�#�@��M@ʾ@�e@���@�33@��M@�v�@� �@ǨX@�+�@Ƭ@�Ft@�ϫ@�m]@�<6@��@�
=@ĺ�@�ff@�)�@ý�@Â�@�j�@�\�@�'�@x@�+k@��a@��k@�k�@��@���@�M@��@��@���@��v@���@���@�X@�2a@��[@���@�GE@��{@��@���@�<�@��Z@���@�P�@��M@��E@���@�>B@�ԕ@�A�@�\�@���@��;@�a@��O@�u@���@�Dg@��H@��D@�U2@�PH@�I�@�	@���@���@�y�@���@��@�.�@��@��@��@��}@��-@��X@�n�@�V@�:�@�	�@���@�_p@�B�@���@�6@��r@���@�,�@��Q@�33@��P@���@�R�@�)�@�U�@��@�z@�_@�?�@�($@��@��@��@�k�@�Vm@�E9@�&@��|@�&�@���@�!�@��@��1@�C-@��@�L�@�&�@���@�͟@���@�h
@�@�@��t@��@���@��@�{�@�;�@�b@��+@�خ@���@�33@���@��$@��O@��L@���@�� @�*�@��-@��X@���@�.I@��)@�^5@��@��d@��'@�?}@���@��F@�,=@��)@�خ@��K@��:@��@�;d@��Y@�_@�e@��@�s�@�@@��@�S�@�˒@���@�u�@�P�@���@���@���@�u%@��@��g@�dZ@���@�l�@�:*@�-@��@���@�ی@�YK@�
�@���@��@�͟@���@��@�V@�E�@�C-@�-�@���@�`B@���@�� @��@�E9@�B�@�+@���@���@���@�͟@��<@��@��r@�y>@�]d@��@��.@��D@��m@���@�8@�֡@���@�d�@�PH@�C�@�4@���@���@��X@��V@��	@�~�@�O@�/@��@��@��c@��)@�n�@�~@��.@���@��@��@��}@��H@��@���@�e,@��H@���@���@��1@��Y@�I�@��@�
@�@�@~�F@~!�@}�@|�/@|l"@|�@{��@{�6@{�4@{�@z�'@z{�@z#:@y�@yo @x�j@w�]@w��@wX�@v�X@v�x@v�@u&�@t~(@t�@s��@so�@s$t@r�2@r�@rM�@q�@qQ�@p��@o�@oX�@o"�@n�y@nv�@m��@m!�@l�z@lV�@l~@k�m@kMj@j�,@j+k@i��@ihs@i;@htT@h:�@g|�@f��@f^5@ek�@e+�@d��@d��@dtT@d_@d:�@c˒@cMj@b�@b�B@bxl@a�-@`�5@`h�@`@_��@_��@__p@^��@^
�@]�C@]zx@]X@]F@\��@\��@\oi@\�@[�Q@[qv@Z�@Zh
@Y�X@Y<6@Xی@X�I@Xr�@Xq@Xl"@XD�@X<�@W��@W�:@Wy�@W@O@WY@V�2@V�}@Vd�@V?@V4@U�>@U[W@UA @U0�@U�@T��@T��@T��@Te�@T1'@S��@S_p@R��@R{@Q�.@Q��@Q:�@P�|@P�@P��@P��@P�D@P��@PbN@P7�@P@O��@O�
@O�@@O�f@OdZ@OJ#@N��@N�@M+�@L�v@L��@L�z@K�@K&@KS@K(@J��@I�T@Iϫ@I�@I�M@H��@H��@H  @G�K@G��@G/�@F�2@F@�@E�.@E�o@E��@E�"@D�4@D�@C��@Ca@CK�@B�]@B�1@BYK@B!�@A��@A8�@A�@@֡@@��@@%�@?��@?��@?O@?,�@>�@>W�@>@=�=@=/@<�@<4n@;��@;��@;��@;dZ@;8@:��@:^5@9��@9��@9!�@8�4@7�r@7�[@7��@7~�@7g�@6�H@6��@6z@63�@5�Z@5��@5`B@4��@4��@4�D@4oi@4M@3�@3E9@3@2�M@2{�@2e@1��@1j@17L@0�@0`�@0:�@/�r@/�Q@/�@/�@/X�@/33@.�y@.��@.C�@-�@-�z@-��@-�S@-IR@,�E@,�4@,�@,c�@,I�@,*�@,�@,@+�]@+ݘ@+�@+��@+|�@+]�@+6z@+.I@+�@*�@*�@*~�@*p;@*^5@*-@)��@)ϫ@)��@)j@)(�@(��@(��@(C-@(�@'��@'��@'�@'��@'|�@'s@'j�@'33@'�@'�@&��@&��@&z@&0U@%�@%�T@%��@%`B@%<6@%q@$Ĝ@$��@$h�@$?�@#�r@#��@#g�@#O@#6z@#�@"��@"� @"d�@"\�@"?@"_@!�T@!��@!c@!Vm@!�@ ��@ �@ ��@ oi@ Ft@��@�$@_p@�@��@�c@��@_�@($@�@��@��@�@�@�S@u�@B�@;@�	@�5@ѷ@��@z�@Xy@4n@(�@ݘ@�F@��@~�@O@(@�s@��@� @@�@�@��@��@��@��@8�@�@��@w�@H@�@�f@8@�@�"@�B@~�@C�@e@	@	@��@�d@�t@��@w2@<6@��@|�@g8@PH@$@�@��@;d@Y@(@�@�6@�\@z@^5@;�@4@�@@��@�@k�@�@�f@�@�j@��@�Y@bN@1'@!@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	^�B	^B	\�B	\�B	\]B	[�B	\xB	\�B	\�B	]B	]/B	]�B	^B	^�B	`BB	b4B	iB	r-B	yrB	�aB	�4B	�B	��B	�0B	��B	��B	�MB	�uB
�B
��B
��B
�<B
� B
�B
�mB
��B
�tB
�>B�B^BBIB.�B4�B<�B?�BA�BL�BZBg�B��B��B��B��B�"B��B��B�[B�\B��B��B��BǔB̘B�B�B �B�BKB!bB#�B#�B"4BCB�B��B�B��B�hB�_B�B�gB�OB�B��B��Bs�Be�BE�B�B
	B
��B
�B
��B
�2B
��B
��B
��B
��B
q�B
m�B
e�B
N�B
�B
�B	��B	�SB	��B	�VB	�B	��B	��B	kB	TB	N�B	>wB	,�B	�B	�B	�B�B�B� B��B��B��B�\B�BB��B��B��B�mB��B֡B�xB�NB�2B�B	MB	,WB	6�B	D3B	*B	�B	�B	�B	�B	�B	gB	OB	!-B	(�B	,qB	&�B	%FB	(�B	/�B	C�B	J�B	G_B	Z�B	jB	r�B	v�B	�uB	��B	��B	�RB	��B	��B	�UB	��B	�B	�B	�5B	��B	��B	��B	�mB	��B	��B	��B	�	B	��B	��B	ӏB	��B	ǔB	�.B	��B	�$B	�gB	�NB	��B	�EB	�B	�NB	�bB	�IB	��B	�CB	��B	ݘB	�dB	�B	��B	�B	�B	�bB	�pB	߾B	�HB	�NB	�nB	�B	�tB	�B	�!B	��B	ٴB	�B	߾B	�OB	�B	�B	�tB	�`B	�B	�B	�B	��B	�B	�B	�ZB	��B	�B	��B	�
B	�B	�
B	��B	�B	�B	�mB	�B	��B	��B	�mB	��B	��B	�mB	�B	�RB	��B	��B	�B	��B	�B	�tB	�&B	�ZB	�B	� B	��B	��B	�B	�B	��B	ߊB	�BB	��B	�B	�B	�B	�B	��B	�B	�#B	�7B	�=B	�B	ܬB	ڠB	�B	ٴB	��B	�yB	خB	�EB	׍B	��B	��B	�$B	�$B	֡B	֡B	��B	��B	�+B	�yB	ؓB	ؓB	��B	�eB	��B	��B	��B	�QB	��B	ٴB	�B	��B	ڠB	یB	��B	��B	��B	� B	��B	�B	��B	�B	��B	�B	�&B	�B	�B	�zB	�B	�zB	��B	�B	�2B	�B	�B	�B	��B	�B	�XB	�sB	�$B	�>B	�XB	��B	�$B	��B	�yB	�eB	�B	��B	�"B	�kB	�B	��B	��B	��B	�kB	��B	�B	�WB	�WB	��B	�}B	�B	��B	�B	��B	�B	��B	�B	�B	� B	�B	�B	�5B	�B	�OB	�B	��B	�B	�B	�'B	�B	�B	��B	�B	�-B	�B	�?B	��B	�FB	�`B	�FB	��B	�B	�RB	��B	��B	�>B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�dB	��B	��B	�B	��B	�qB	�qB	��B	��B	�qB	��B	��B	��B	�B	��B	�B	��B	��B	�cB
 4B
 �B
 �B
UB
B
B
�B
�B
�B
aB
�B
�B
�B
B
�B
?B
tB
+B
zB
�B
�B
�B
B
fB
	7B
	B
	B
	B
	B
	B

=B

�B

�B

�B
�B
�B
JB
�B
�B
B
�B
<B
pB
BB
�B
�B
�B
HB
.B
bB
4B
�B
�B
�B
[B
�B
�B
?B
1B
B
7B
kB
�B
=B
�B
CB
�B
/B
~B
B
jB
!B
;B
pB
�B
�B
B
5B
�B
dB
OB
jB
�B
!B
!B
B
�B
;B
�B
B
OB
/B
�B
�B
�B
pB
�B
 �B
!-B
!bB
!bB
!�B
!�B
!�B
"B
"�B
"�B
#:B
#nB
#B
#TB
#nB
#�B
$&B
#�B
$@B
$�B
$�B
$�B
$�B
%`B
%�B
&LB
&�B
&fB
&�B
&�B
'RB
($B
(sB
(sB
(sB
(sB
(sB
(�B
(�B
)B
(�B
)_B
*eB
*�B
*�B
*�B
*�B
*�B
+�B
+�B
,B
,"B
,B
+�B
,WB
,�B
,�B
-B
-wB
-wB
-�B
.IB
.cB
.}B
.}B
.�B
.}B
/�B
/�B
/�B
0UB
0�B
0�B
1'B
1[B
1[B
1�B
2aB
2aB
2�B
33B
33B
3hB
3�B
4TB
4nB
5?B
5%B
5%B
5?B
5�B
6B
6�B
6�B
7LB
7LB
7LB
7�B
7�B
8B
8lB
8lB
9	B
9�B
:B
:�B
:�B
;�B
<�B
=B
<�B
<�B
=�B
>(B
>�B
?HB
?cB
?B
?B
>�B
?B
?B
?�B
@ B
@�B
@�B
@�B
@OB
@OB
@OB
@OB
@�B
A�B
A�B
A�B
A�B
A�B
A�B
BuB
B�B
CGB
B�B
B�B
CB
C�B
C�B
C�B
C�B
D3B
D�B
E9B
EB
EB
EB
EB
EmB
FtB
F�B
F�B
F�B
GB
GEB
G+B
GB
G+B
GEB
GzB
GzB
GzB
G�B
G�B
HB
G�B
HKB
HB
HKB
H�B
H�B
H�B
H�B
H�B
I7B
IlB
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J	B
J	B
J=B
J�B
JrB
KxB
K�B
LB
K�B
KxB
K�B
LB
L�B
LdB
MB
N<B
N�B
O(B
O(B
O�B
OvB
OvB
O\B
OvB
O�B
O\B
O�B
O�B
PB
PbB
P�B
QhB
Q�B
Q�B
RB
R:B
RoB
R B
SB
SuB
S�B
S�B
S�B
T,B
TFB
TaB
T�B
T�B
T�B
U2B
UgB
U�B
VB
V�B
V�B
W?B
W?B
WsB
WsB
WsB
XEB
XEB
X�B
X_B
XyB
X�B
YeB
YB
Y�B
Y�B
Y�B
ZQB
ZQB
ZkB
Z�B
Z�B
Z�B
[WB
[�B
[qB
[�B
[�B
[�B
[�B
\]B
\]B
\]B
\�B
]~B
]�B
^B
^B
^jB
^�B
^�B
_!B
_;B
_;B
_pB
_�B
_pB
_�B
_�B
_�B
`'B
`'B
`'B
`'B
`�B
a|B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
b4B
bNB
bhB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
cB
c:B
cnB
cnB
c�B
c�B
c�B
c�B
d&B
dZB
dtB
dtB
d�B
d�B
d�B
d�B
eB
d�B
eFB
eFB
e,B
eB
e,B
eFB
e�B
e�B
e�B
e�B
f2B
fLB
ffB
f�B
f�B
f�B
f�B
gB
g�B
g�B
g�B
g�B
g�B
h$B
hsB
hsB
hsB
h�B
h�B
h�B
h�B
i*B
iDB
iyB
i�B
i�B
i�B
i�B
jB
j�B
j�B
kB
kQB
kQB
kQB
k�B
k�B
lB
l"B
lB
l=B
l"B
l=B
lqB
lqB
l�B
l�B
l�B
l�B
mB
m]B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
nIB
n}B
n�B
n�B
n�B
o5B
oOB
oiB
o�B
o�B
o�B
pB
p;B
pUB
p�B
p�B
p�B
q[B
q�B
q�B
q�B
q�B
rGB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s3B
s�B
tB
tB
tB
tB
s�B
t�B
t�B
t�B
t�B
uB
uZB
utB
utB
u�B
u�B
u�B
vB
vFB
v`B
vFB
v�B
v�B
v�B
wB
wB
wLB
wLB
w�B
w�B
w�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	^�B	^B	\�B	\�B	\]B	[�B	\xB	\�B	\�B	]B	]/B	]�B	^B	^�B	`BB	b4B	iB	r-B	yrB	�aB	�4B	�B	��B	�0B	��B	��B	�MB	�uB
�B
��B
��B
�<B
� B
�B
�mB
��B
�tB
�>B�B^BBIB.�B4�B<�B?�BA�BL�BZBg�B��B��B��B��B�"B��B��B�[B�\B��B��B��BǔB̘B�B�B �B�BKB!bB#�B#�B"4BCB�B��B�B��B�hB�_B�B�gB�OB�B��B��Bs�Be�BE�B�B
	B
��B
�B
��B
�2B
��B
��B
��B
��B
q�B
m�B
e�B
N�B
�B
�B	��B	�SB	��B	�VB	�B	��B	��B	kB	TB	N�B	>wB	,�B	�B	�B	�B�B�B� B��B��B��B�\B�BB��B��B��B�mB��B֡B�xB�NB�2B�B	MB	,WB	6�B	D3B	*B	�B	�B	�B	�B	�B	gB	OB	!-B	(�B	,qB	&�B	%FB	(�B	/�B	C�B	J�B	G_B	Z�B	jB	r�B	v�B	�uB	��B	��B	�RB	��B	��B	�UB	��B	�B	�B	�5B	��B	��B	��B	�mB	��B	��B	��B	�	B	��B	��B	ӏB	��B	ǔB	�.B	��B	�$B	�gB	�NB	��B	�EB	�B	�NB	�bB	�IB	��B	�CB	��B	ݘB	�dB	�B	��B	�B	�B	�bB	�pB	߾B	�HB	�NB	�nB	�B	�tB	�B	�!B	��B	ٴB	�B	߾B	�OB	�B	�B	�tB	�`B	�B	�B	�B	��B	�B	�B	�ZB	��B	�B	��B	�
B	�B	�
B	��B	�B	�B	�mB	�B	��B	��B	�mB	��B	��B	�mB	�B	�RB	��B	��B	�B	��B	�B	�tB	�&B	�ZB	�B	� B	��B	��B	�B	�B	��B	ߊB	�BB	��B	�B	�B	�B	�B	��B	�B	�#B	�7B	�=B	�B	ܬB	ڠB	�B	ٴB	��B	�yB	خB	�EB	׍B	��B	��B	�$B	�$B	֡B	֡B	��B	��B	�+B	�yB	ؓB	ؓB	��B	�eB	��B	��B	��B	�QB	��B	ٴB	�B	��B	ڠB	یB	��B	��B	��B	� B	��B	�B	��B	�B	��B	�B	�&B	�B	�B	�zB	�B	�zB	��B	�B	�2B	�B	�B	�B	��B	�B	�XB	�sB	�$B	�>B	�XB	��B	�$B	��B	�yB	�eB	�B	��B	�"B	�kB	�B	��B	��B	��B	�kB	��B	�B	�WB	�WB	��B	�}B	�B	��B	�B	��B	�B	��B	�B	�B	� B	�B	�B	�5B	�B	�OB	�B	��B	�B	�B	�'B	�B	�B	��B	�B	�-B	�B	�?B	��B	�FB	�`B	�FB	��B	�B	�RB	��B	��B	�>B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�dB	��B	��B	�B	��B	�qB	�qB	��B	��B	�qB	��B	��B	��B	�B	��B	�B	��B	��B	�cB
 4B
 �B
 �B
UB
B
B
�B
�B
�B
aB
�B
�B
�B
B
�B
?B
tB
+B
zB
�B
�B
�B
B
fB
	7B
	B
	B
	B
	B
	B

=B

�B

�B

�B
�B
�B
JB
�B
�B
B
�B
<B
pB
BB
�B
�B
�B
HB
.B
bB
4B
�B
�B
�B
[B
�B
�B
?B
1B
B
7B
kB
�B
=B
�B
CB
�B
/B
~B
B
jB
!B
;B
pB
�B
�B
B
5B
�B
dB
OB
jB
�B
!B
!B
B
�B
;B
�B
B
OB
/B
�B
�B
�B
pB
�B
 �B
!-B
!bB
!bB
!�B
!�B
!�B
"B
"�B
"�B
#:B
#nB
#B
#TB
#nB
#�B
$&B
#�B
$@B
$�B
$�B
$�B
$�B
%`B
%�B
&LB
&�B
&fB
&�B
&�B
'RB
($B
(sB
(sB
(sB
(sB
(sB
(�B
(�B
)B
(�B
)_B
*eB
*�B
*�B
*�B
*�B
*�B
+�B
+�B
,B
,"B
,B
+�B
,WB
,�B
,�B
-B
-wB
-wB
-�B
.IB
.cB
.}B
.}B
.�B
.}B
/�B
/�B
/�B
0UB
0�B
0�B
1'B
1[B
1[B
1�B
2aB
2aB
2�B
33B
33B
3hB
3�B
4TB
4nB
5?B
5%B
5%B
5?B
5�B
6B
6�B
6�B
7LB
7LB
7LB
7�B
7�B
8B
8lB
8lB
9	B
9�B
:B
:�B
:�B
;�B
<�B
=B
<�B
<�B
=�B
>(B
>�B
?HB
?cB
?B
?B
>�B
?B
?B
?�B
@ B
@�B
@�B
@�B
@OB
@OB
@OB
@OB
@�B
A�B
A�B
A�B
A�B
A�B
A�B
BuB
B�B
CGB
B�B
B�B
CB
C�B
C�B
C�B
C�B
D3B
D�B
E9B
EB
EB
EB
EB
EmB
FtB
F�B
F�B
F�B
GB
GEB
G+B
GB
G+B
GEB
GzB
GzB
GzB
G�B
G�B
HB
G�B
HKB
HB
HKB
H�B
H�B
H�B
H�B
H�B
I7B
IlB
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J	B
J	B
J=B
J�B
JrB
KxB
K�B
LB
K�B
KxB
K�B
LB
L�B
LdB
MB
N<B
N�B
O(B
O(B
O�B
OvB
OvB
O\B
OvB
O�B
O\B
O�B
O�B
PB
PbB
P�B
QhB
Q�B
Q�B
RB
R:B
RoB
R B
SB
SuB
S�B
S�B
S�B
T,B
TFB
TaB
T�B
T�B
T�B
U2B
UgB
U�B
VB
V�B
V�B
W?B
W?B
WsB
WsB
WsB
XEB
XEB
X�B
X_B
XyB
X�B
YeB
YB
Y�B
Y�B
Y�B
ZQB
ZQB
ZkB
Z�B
Z�B
Z�B
[WB
[�B
[qB
[�B
[�B
[�B
[�B
\]B
\]B
\]B
\�B
]~B
]�B
^B
^B
^jB
^�B
^�B
_!B
_;B
_;B
_pB
_�B
_pB
_�B
_�B
_�B
`'B
`'B
`'B
`'B
`�B
a|B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
b4B
bNB
bhB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
cB
c:B
cnB
cnB
c�B
c�B
c�B
c�B
d&B
dZB
dtB
dtB
d�B
d�B
d�B
d�B
eB
d�B
eFB
eFB
e,B
eB
e,B
eFB
e�B
e�B
e�B
e�B
f2B
fLB
ffB
f�B
f�B
f�B
f�B
gB
g�B
g�B
g�B
g�B
g�B
h$B
hsB
hsB
hsB
h�B
h�B
h�B
h�B
i*B
iDB
iyB
i�B
i�B
i�B
i�B
jB
j�B
j�B
kB
kQB
kQB
kQB
k�B
k�B
lB
l"B
lB
l=B
l"B
l=B
lqB
lqB
l�B
l�B
l�B
l�B
mB
m]B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
nIB
n}B
n�B
n�B
n�B
o5B
oOB
oiB
o�B
o�B
o�B
pB
p;B
pUB
p�B
p�B
p�B
q[B
q�B
q�B
q�B
q�B
rGB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s3B
s�B
tB
tB
tB
tB
s�B
t�B
t�B
t�B
t�B
uB
uZB
utB
utB
u�B
u�B
u�B
vB
vFB
v`B
vFB
v�B
v�B
v�B
wB
wB
wLB
wLB
w�B
w�B
w�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105227  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191203  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191203  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191203                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041211  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041211  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                