CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-11-22T10:02:13Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \\   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ܜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ߜ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �,   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �<   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �D   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20221122100213  20221122100213  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @���|�:�1   @���""-�@*��1'�d�Ƨ1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�ffB�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DxfDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�ffB�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DxfDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��A��#A��;A��;A��TA��`A��mA��mA��yA��A��A��A��A��A��A�  A�  A�  A�  A���A��HA���AپwAُ\A�z�A�t�A�p�A�l�A�ffA�^5A�Q�A�I�A��HAΰ!A�O�A��
AƧ�A��#AĬA���A��#A��A���A�(�A���A�"�A��yA�bNA�JA�K�A��A�\)A���A��yA���A���A�{A���A�G�A�G�A���A��`A�ƨA�G�A���A�9XA�p�A��\A���A��FA��A�1'A�ZA��A�O�A�XA���A�|�A��DA��A�z�A�A��uA�=qA~��Ax�Av�Ap=qAgC�AcO�A^�AZ��AX�AV^5AS��APbNAL�AK\)AJ-AI%AH�AC�7A@r�A?�A?�A;C�A9+A8�A6��A6bA5O�A3��A/�
A-l�A.M�A.��A,ffA*��A*A*��A*bNA)&�A(-A(~�A(�\A'��A't�A'p�A'O�A'A&E�A$5?A"VA!C�A!�A �RAA�AĜA �AdZA5?A��A7LA��Av�A�A��AG�A �A�A�A�^A�jAp�A�A+A�A��A�A�FA�A��A��A=qAƨA��A�A
�!A	ƨA	A��Av�A-AhsA�A��A��AVAbAoA��A��AbNAVAI�A-A��A�AƨA�PAt�Ap�Al�A+A�/A�\AI�AA�A=qA9XA �A��A�PA �uA �@���@���@��-@�Q�@��
@�l�@�
=@���@���@�-@���@��/@���@�
=@�ff@���@�x�@�&�@��j@�I�@�ƨ@�  @��@�"�@�{@�p�@��@�  @�t�@�w@�F@�w@�;d@�=q@�h@�X@�V@�Z@��@�C�@��@���@��@��@�j@�z�@�l�@��H@�\@�J@�9X@��@�\)@��@��y@◍@�=q@��@��@���@�;d@��`@�9X@�1'@�(�@���@�l�@֟�@��@��@�I�@ӕ�@�+@ҏ\@���@���@Ѳ-@�hs@�1'@�S�@Η�@��@��T@�`B@�V@���@�bN@�9X@˝�@�
=@ʗ�@ʇ+@�~�@ɺ^@�x�@ȼj@ǅ@�^5@�x�@���@�Z@�\)@��@���@���@+@�M�@�-@��@�p�@�?}@�Ĝ@�Q�@�1@��
@�+@�^5@���@�`B@���@��D@�r�@��;@���@��@�t�@�C�@���@���@��@�%@��/@��j@�j@�(�@���@��F@���@�@��7@��`@��@�j@��
@��@�
=@���@�ȴ@�ȴ@�ȴ@��\@�n�@�^5@�^5@�V@�E�@�-@�@���@�O�@���@��9@��@��y@��!@�^5@��@�`B@�%@���@��D@�Q�@��
@�t�@�@�ff@�^5@�ff@�J@�O�@�V@���@��u@� �@��m@�t�@�33@�"�@�@���@�^5@��@���@�V@��D@�z�@�1@��P@�l�@�K�@�
=@���@��@�@�x�@�?}@��@���@��j@��@���@�l�@��!@�~�@�M�@��h@���@�Ĝ@��9@��u@��@�Z@�  @�33@�o@��H@��\@�E�@��@��7@�&�@���@��w@�33@�
=@�n�@�5?@��@�J@��#@���@��-@�p�@�X@�O�@�%@��u@�  @�t�@��@���@�~�@�^5@�-@���@��@���@���@�hs@�?}@��@��/@��D@�Q�@�9X@��m@��F@��F@���@�K�@��@���@�^5@�-@���@�@��^@�hs@���@��@���@�z�@�  @��P@�l�@�\)@�33@�@���@��R@�ff@�-@���@���@�x�@�hs@�`B@�X@��@��9@��9@��@��u@�9X@�ƨ@�|�@�"�@�@��y@��R@�^5@�-@��-@�V@��u@�bN@�1@�|�@�l�@�dZ@�;d@�"�@�o@���@���@���@�V@�@���@���@���@��@�?}@�7L@�&�@��/@��9@��@��@;d@~�y@~��@~5?@}��@}p�@}V@|�D@|Z@|Z@|Z@|I�@{��@{o@{@z�\@y�#@yx�@y7L@x��@x�`@xĜ@xb@w�@v��@v{@u�@t��@t��@t��@t�D@tz�@s�F@st�@s@r�H@r��@r��@rM�@q��@q�#@q��@qG�@pr�@pbN@pA�@p �@o��@oK�@o;d@o
=@nȴ@nff@m�@m?}@l�/@lz�@l�D@lI�@k��@k�@kdZ@kdZ@k"�@j�!@j�\@i��@ihs@i7L@h�`@h�@g�;@g|�@g�@f�R@fV@f5?@f{@e@e?}@e/@eV@dI�@c��@c"�@b��@b~�@b�@a��@a��@aX@`��@`�@_�@_K�@_
=@^�R@]��@\��@\I�@[��@[ƨ@[t�@["�@[@Z��@Z��@Z-@Y�7@YX@X��@X�9@X1'@W�@W\)@V�R@V{@U��@UO�@T��@T��@S�
@SdZ@S33@So@R�H@R��@R~�@RJ@Qx�@Q%@Pr�@P  @O�w@O�w@O;d@N�@N$�@L�@LI�@L�@L1@K��@J��@J~�@I��@I&�@HĜ@H�u@Hr�@HA�@G�w@G\)@G;d@G+@F�@Fff@F@E��@E?}@D�j@D�D@C�m@Ct�@Co@B��@B�\@B^5@B-@BJ@A��@Ax�@A%@@r�@@ �@?��@?\)@?�@?
=@>��@>�y@>�y@>�+@=�-@<��@<Z@<9X@;�m@;dZ@;C�@;33@;o@:�@:��@:n�@:M�@:�@9��@9��@9�7@9G�@97L@97L@9�@8��@8r�@81'@81'@8 �@7�@7�w@7�P@7\)@7
=@6�@6�+@6E�@5@5p�@5�@4�/@4�D@3��@3t�@2�H@2n�@2�@1hs@17L@0�`@0r�@0 �@0  @/�;@/��@/|�@/|�@/;d@/�@/
=@.�y@.�+@.5?@-�@-��@-@-��@-�h@-p�@-`B@-�@,�j@,9X@+�m@+�
@+�F@+33@*��@*=q@)��@)��@)�^@)�^@)��@)x�@)X@)X@)7L@)%@)%@(��@(Ĝ@(�@(Q�@'�@'|�@'K�@'+@'
=@&�@&ȴ@&v�@&5?@%�@%@%�h@%O�@%V@$�/@$��@$Z@$9X@#�
@#��@#��@#dZ@#C�@#C�@#33@#"�@#@"�H@"~�@"M�@"�@!��@!x�@!hs@!G�@!�@ �`@ �u@ bN@ Q�@ 1'@   @��@��@��@�@�P@;d@��@�@�R@ff@{@��@?}@��@�j@z�@9X@�@��@�
@�F@�@33@�H@��@�!@^5@=q@�@�@��@��@�^@��@hs@7L@�@��@Ĝ@�9@�u@bN@A�@1'@1'@ �@�w@\)@
=@�y@ȴ@�+@V@$�@{@��@�@p�@`B@O�@/@��@�@z�@z�@I�@�@1@�m@��@S�@o@�@��@�!@~�@n�@^5@J@�^@G�@�@%@��@�`@��@Ĝ@Ĝ@�9@��@�@r�@r�@bN@Q�@1'@b@  @�;@�;@�;@�w@l�@�@��@��@��@��@�+@v�@5?@{@�@��@�-@�h@?}@/@��@�/@��@Z@(�@�@�@�@��@�m@�F@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A��A��#A��;A��;A��TA��`A��mA��mA��yA��A��A��A��A��A��A�  A�  A�  A�  A���A��HA���AپwAُ\A�z�A�t�A�p�A�l�A�ffA�^5A�Q�A�I�A��HAΰ!A�O�A��
AƧ�A��#AĬA���A��#A��A���A�(�A���A�"�A��yA�bNA�JA�K�A��A�\)A���A��yA���A���A�{A���A�G�A�G�A���A��`A�ƨA�G�A���A�9XA�p�A��\A���A��FA��A�1'A�ZA��A�O�A�XA���A�|�A��DA��A�z�A�A��uA�=qA~��Ax�Av�Ap=qAgC�AcO�A^�AZ��AX�AV^5AS��APbNAL�AK\)AJ-AI%AH�AC�7A@r�A?�A?�A;C�A9+A8�A6��A6bA5O�A3��A/�
A-l�A.M�A.��A,ffA*��A*A*��A*bNA)&�A(-A(~�A(�\A'��A't�A'p�A'O�A'A&E�A$5?A"VA!C�A!�A �RAA�AĜA �AdZA5?A��A7LA��Av�A�A��AG�A �A�A�A�^A�jAp�A�A+A�A��A�A�FA�A��A��A=qAƨA��A�A
�!A	ƨA	A��Av�A-AhsA�A��A��AVAbAoA��A��AbNAVAI�A-A��A�AƨA�PAt�Ap�Al�A+A�/A�\AI�AA�A=qA9XA �A��A�PA �uA �@���@���@��-@�Q�@��
@�l�@�
=@���@���@�-@���@��/@���@�
=@�ff@���@�x�@�&�@��j@�I�@�ƨ@�  @��@�"�@�{@�p�@��@�  @�t�@�w@�F@�w@�;d@�=q@�h@�X@�V@�Z@��@�C�@��@���@��@��@�j@�z�@�l�@��H@�\@�J@�9X@��@�\)@��@��y@◍@�=q@��@��@���@�;d@��`@�9X@�1'@�(�@���@�l�@֟�@��@��@�I�@ӕ�@�+@ҏ\@���@���@Ѳ-@�hs@�1'@�S�@Η�@��@��T@�`B@�V@���@�bN@�9X@˝�@�
=@ʗ�@ʇ+@�~�@ɺ^@�x�@ȼj@ǅ@�^5@�x�@���@�Z@�\)@��@���@���@+@�M�@�-@��@�p�@�?}@�Ĝ@�Q�@�1@��
@�+@�^5@���@�`B@���@��D@�r�@��;@���@��@�t�@�C�@���@���@��@�%@��/@��j@�j@�(�@���@��F@���@�@��7@��`@��@�j@��
@��@�
=@���@�ȴ@�ȴ@�ȴ@��\@�n�@�^5@�^5@�V@�E�@�-@�@���@�O�@���@��9@��@��y@��!@�^5@��@�`B@�%@���@��D@�Q�@��
@�t�@�@�ff@�^5@�ff@�J@�O�@�V@���@��u@� �@��m@�t�@�33@�"�@�@���@�^5@��@���@�V@��D@�z�@�1@��P@�l�@�K�@�
=@���@��@�@�x�@�?}@��@���@��j@��@���@�l�@��!@�~�@�M�@��h@���@�Ĝ@��9@��u@��@�Z@�  @�33@�o@��H@��\@�E�@��@��7@�&�@���@��w@�33@�
=@�n�@�5?@��@�J@��#@���@��-@�p�@�X@�O�@�%@��u@�  @�t�@��@���@�~�@�^5@�-@���@��@���@���@�hs@�?}@��@��/@��D@�Q�@�9X@��m@��F@��F@���@�K�@��@���@�^5@�-@���@�@��^@�hs@���@��@���@�z�@�  @��P@�l�@�\)@�33@�@���@��R@�ff@�-@���@���@�x�@�hs@�`B@�X@��@��9@��9@��@��u@�9X@�ƨ@�|�@�"�@�@��y@��R@�^5@�-@��-@�V@��u@�bN@�1@�|�@�l�@�dZ@�;d@�"�@�o@���@���@���@�V@�@���@���@���@��@�?}@�7L@�&�@��/@��9@��@��@;d@~�y@~��@~5?@}��@}p�@}V@|�D@|Z@|Z@|Z@|I�@{��@{o@{@z�\@y�#@yx�@y7L@x��@x�`@xĜ@xb@w�@v��@v{@u�@t��@t��@t��@t�D@tz�@s�F@st�@s@r�H@r��@r��@rM�@q��@q�#@q��@qG�@pr�@pbN@pA�@p �@o��@oK�@o;d@o
=@nȴ@nff@m�@m?}@l�/@lz�@l�D@lI�@k��@k�@kdZ@kdZ@k"�@j�!@j�\@i��@ihs@i7L@h�`@h�@g�;@g|�@g�@f�R@fV@f5?@f{@e@e?}@e/@eV@dI�@c��@c"�@b��@b~�@b�@a��@a��@aX@`��@`�@_�@_K�@_
=@^�R@]��@\��@\I�@[��@[ƨ@[t�@["�@[@Z��@Z��@Z-@Y�7@YX@X��@X�9@X1'@W�@W\)@V�R@V{@U��@UO�@T��@T��@S�
@SdZ@S33@So@R�H@R��@R~�@RJ@Qx�@Q%@Pr�@P  @O�w@O�w@O;d@N�@N$�@L�@LI�@L�@L1@K��@J��@J~�@I��@I&�@HĜ@H�u@Hr�@HA�@G�w@G\)@G;d@G+@F�@Fff@F@E��@E?}@D�j@D�D@C�m@Ct�@Co@B��@B�\@B^5@B-@BJ@A��@Ax�@A%@@r�@@ �@?��@?\)@?�@?
=@>��@>�y@>�y@>�+@=�-@<��@<Z@<9X@;�m@;dZ@;C�@;33@;o@:�@:��@:n�@:M�@:�@9��@9��@9�7@9G�@97L@97L@9�@8��@8r�@81'@81'@8 �@7�@7�w@7�P@7\)@7
=@6�@6�+@6E�@5@5p�@5�@4�/@4�D@3��@3t�@2�H@2n�@2�@1hs@17L@0�`@0r�@0 �@0  @/�;@/��@/|�@/|�@/;d@/�@/
=@.�y@.�+@.5?@-�@-��@-@-��@-�h@-p�@-`B@-�@,�j@,9X@+�m@+�
@+�F@+33@*��@*=q@)��@)��@)�^@)�^@)��@)x�@)X@)X@)7L@)%@)%@(��@(Ĝ@(�@(Q�@'�@'|�@'K�@'+@'
=@&�@&ȴ@&v�@&5?@%�@%@%�h@%O�@%V@$�/@$��@$Z@$9X@#�
@#��@#��@#dZ@#C�@#C�@#33@#"�@#@"�H@"~�@"M�@"�@!��@!x�@!hs@!G�@!�@ �`@ �u@ bN@ Q�@ 1'@   @��@��@��@�@�P@;d@��@�@�R@ff@{@��@?}@��@�j@z�@9X@�@��@�
@�F@�@33@�H@��@�!@^5@=q@�@�@��@��@�^@��@hs@7L@�@��@Ĝ@�9@�u@bN@A�@1'@1'@ �@�w@\)@
=@�y@ȴ@�+@V@$�@{@��@�@p�@`B@O�@/@��@�@z�@z�@I�@�@1@�m@��@S�@o@�@��@�!@~�@n�@^5@J@�^@G�@�@%@��@�`@��@Ĝ@Ĝ@�9@��@�@r�@r�@bN@Q�@1'@b@  @�;@�;@�;@�w@l�@�@��@��@��@��@�+@v�@5?@{@�@��@�-@�h@?}@/@��@�/@��@Z@(�@�@�@�@��@�m@�F@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
?}B
?}B
?}B
@�B
A�B
C�B
C�B
C�B
C�B
D�B
D�B
C�B
E�B
D�B
C�B
G�B
P�B
S�B
W
B
XB
`BB
q�B
z�B
� B
� B
~�B
� B
� B
� B
~�B
}�B
x�B
��BJB^5B�=B��B�ZB�B��B�B"�B49B^5B� B~�B}�B�DB�B�1B�Bq�BffBZBD�B,B+B  B�B��BÖB�3B��Bz�BhsBQ�B>wB+B\B
�B
�)B
��B
��B
�-B
��B
�{B
�7B
q�B
_;B
M�B
1'B
'�B
�B
�B

=B	�B	��B	��B	ffB	T�B	>wB	,B	'�B	�B	DB��B�B��B��B�B�mB��B�#B�B�B�#B�HB�HB�NB�B�yB�NB�B�ZB	B	�B	�B	A�B	_;B	�=B	��B	��B	��B	�RB	��B	�`B	�B	�B	�B	�yB	�ZB	�BB	�`B	�TB	�HB	�/B	�#B	�;B	�sB	�mB	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�sB	�B	�
B	�B	�#B	�HB	�fB	�mB	�mB	�fB	�`B	�NB	�NB	�NB	�HB	�NB	�ZB	�HB	�BB	�/B	�HB	�ZB	�fB	�`B	�TB	�sB	�B	�yB	�B	�yB	�ZB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B	��B	��B	��B	��B	��B
B
B
B
1B
1B
	7B
	7B
VB
VB
JB
DB
JB
JB

=B
JB
bB
hB
{B
oB
oB
uB
�B
�B
{B
{B
�B
�B
�B
�B
oB
hB
uB
bB
hB
uB
hB
JB
oB
{B
{B
{B
uB
oB
oB
bB
JB
	7B
B
%B
1B
B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
B
B
%B
B
B
B
B
B
%B
	7B
	7B
+B
	7B

=B
	7B
1B
B
B
DB
\B
\B
\B
VB
bB
hB
\B
VB
VB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
 �B
 �B
$�B
$�B
#�B
"�B
!�B
$�B
$�B
$�B
$�B
%�B
$�B
#�B
 �B
!�B
!�B
&�B
&�B
%�B
'�B
+B
,B
,B
,B
+B
)�B
(�B
.B
.B
-B
.B
/B
/B
/B
0!B
-B
1'B
33B
2-B
49B
5?B
5?B
5?B
5?B
5?B
49B
5?B
5?B
33B
2-B
2-B
33B
49B
6FB
7LB
7LB
7LB
7LB
8RB
8RB
7LB
8RB
8RB
8RB
7LB
7LB
7LB
8RB
8RB
9XB
:^B
9XB
8RB
8RB
:^B
:^B
:^B
:^B
<jB
;dB
:^B
9XB
<jB
=qB
;dB
:^B
;dB
=qB
>wB
>wB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
A�B
A�B
@�B
?}B
@�B
B�B
A�B
@�B
?}B
>wB
@�B
@�B
B�B
B�B
A�B
@�B
A�B
?}B
>wB
>wB
?}B
>wB
=qB
@�B
A�B
@�B
A�B
A�B
A�B
@�B
@�B
?}B
>wB
C�B
E�B
E�B
E�B
D�B
F�B
E�B
D�B
E�B
E�B
C�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
H�B
I�B
I�B
H�B
F�B
G�B
H�B
G�B
G�B
I�B
J�B
J�B
J�B
J�B
H�B
H�B
G�B
I�B
I�B
J�B
L�B
L�B
L�B
K�B
J�B
K�B
K�B
M�B
M�B
M�B
L�B
M�B
N�B
N�B
M�B
M�B
P�B
P�B
P�B
P�B
Q�B
R�B
Q�B
Q�B
Q�B
P�B
P�B
Q�B
R�B
T�B
VB
VB
VB
W
B
XB
W
B
VB
W
B
T�B
W
B
XB
XB
W
B
W
B
XB
YB
YB
ZB
[#B
[#B
ZB
ZB
[#B
[#B
YB
YB
[#B
[#B
]/B
\)B
]/B
]/B
]/B
\)B
\)B
[#B
]/B
^5B
]/B
\)B
\)B
^5B
`BB
`BB
`BB
`BB
aHB
`BB
`BB
_;B
_;B
`BB
_;B
aHB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
bNB
cTB
aHB
bNB
cTB
dZB
cTB
dZB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
dZB
dZB
dZB
bNB
dZB
ffB
ffB
e`B
dZB
dZB
dZB
cTB
e`B
ffB
gmB
ffB
ffB
ffB
hsB
hsB
gmB
ffB
gmB
gmB
gmB
hsB
iyB
hsB
hsB
jB
k�B
l�B
l�B
l�B
l�B
k�B
k�B
k�B
jB
k�B
k�B
k�B
m�B
n�B
n�B
n�B
m�B
k�B
k�B
k�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
q�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
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
q�B
q�B
q�B
q�B
p�B
p�B
p�B
q�B
r�B
s�B
s�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
v�B
u�B
v�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
w�B
w�B
v�B
x�B
x�B
x�B
w�B
w�B
x�B
y�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
{�B
{�B
{�B
{�B
z�B
{�B
|�B
}�B
}�B
}�B
}�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
~�B
� B
� B
� B
�B
�B
�B
�B
� B
� B
� B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�B
�%B
�+B
�+B
�+B
�+B
�1B
�+B
�1B
�1B
�1B
�1B
�+B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�1B
�+B
�+B
�1B
�7B
�=B
�7B
�=B
�=B
�=B
�=B
�=B
�JB
�JB
�JB
�DB
�DB
�DB
�JB
�PB
�JB
�JB
�PB
�JB
�JB
�JB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�PB
�PB
�PB
�\B
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�hB
�bB
�\B
�\B
�oB
�oB
�oB
�oB
�oB
�oB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�{B
��B
��B
��B
�{B
�{B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
?}B
?}B
?}B
@�B
A�B
C�B
C�B
C�B
C�B
D�B
D�B
C�B
E�B
D�B
C�B
G�B
P�B
S�B
W
B
XB
`BB
q�B
z�B
� B
� B
~�B
� B
� B
� B
~�B
}�B
x�B
��BJB^5B�=B��B�ZB�B��B�B"�B49B^5B� B~�B}�B�DB�B�1B�Bq�BffBZBD�B,B+B  B�B��BÖB�3B��Bz�BhsBQ�B>wB+B\B
�B
�)B
��B
��B
�-B
��B
�{B
�7B
q�B
_;B
M�B
1'B
'�B
�B
�B

=B	�B	��B	��B	ffB	T�B	>wB	,B	'�B	�B	DB��B�B��B��B�B�mB��B�#B�B�B�#B�HB�HB�NB�B�yB�NB�B�ZB	B	�B	�B	A�B	_;B	�=B	��B	��B	��B	�RB	��B	�`B	�B	�B	�B	�yB	�ZB	�BB	�`B	�TB	�HB	�/B	�#B	�;B	�sB	�mB	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�sB	�B	�
B	�B	�#B	�HB	�fB	�mB	�mB	�fB	�`B	�NB	�NB	�NB	�HB	�NB	�ZB	�HB	�BB	�/B	�HB	�ZB	�fB	�`B	�TB	�sB	�B	�yB	�B	�yB	�ZB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B	��B	��B	��B	��B	��B
B
B
B
1B
1B
	7B
	7B
VB
VB
JB
DB
JB
JB

=B
JB
bB
hB
{B
oB
oB
uB
�B
�B
{B
{B
�B
�B
�B
�B
oB
hB
uB
bB
hB
uB
hB
JB
oB
{B
{B
{B
uB
oB
oB
bB
JB
	7B
B
%B
1B
B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
B
B
%B
B
B
B
B
B
%B
	7B
	7B
+B
	7B

=B
	7B
1B
B
B
DB
\B
\B
\B
VB
bB
hB
\B
VB
VB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
 �B
 �B
$�B
$�B
#�B
"�B
!�B
$�B
$�B
$�B
$�B
%�B
$�B
#�B
 �B
!�B
!�B
&�B
&�B
%�B
'�B
+B
,B
,B
,B
+B
)�B
(�B
.B
.B
-B
.B
/B
/B
/B
0!B
-B
1'B
33B
2-B
49B
5?B
5?B
5?B
5?B
5?B
49B
5?B
5?B
33B
2-B
2-B
33B
49B
6FB
7LB
7LB
7LB
7LB
8RB
8RB
7LB
8RB
8RB
8RB
7LB
7LB
7LB
8RB
8RB
9XB
:^B
9XB
8RB
8RB
:^B
:^B
:^B
:^B
<jB
;dB
:^B
9XB
<jB
=qB
;dB
:^B
;dB
=qB
>wB
>wB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
A�B
A�B
@�B
?}B
@�B
B�B
A�B
@�B
?}B
>wB
@�B
@�B
B�B
B�B
A�B
@�B
A�B
?}B
>wB
>wB
?}B
>wB
=qB
@�B
A�B
@�B
A�B
A�B
A�B
@�B
@�B
?}B
>wB
C�B
E�B
E�B
E�B
D�B
F�B
E�B
D�B
E�B
E�B
C�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
H�B
I�B
I�B
H�B
F�B
G�B
H�B
G�B
G�B
I�B
J�B
J�B
J�B
J�B
H�B
H�B
G�B
I�B
I�B
J�B
L�B
L�B
L�B
K�B
J�B
K�B
K�B
M�B
M�B
M�B
L�B
M�B
N�B
N�B
M�B
M�B
P�B
P�B
P�B
P�B
Q�B
R�B
Q�B
Q�B
Q�B
P�B
P�B
Q�B
R�B
T�B
VB
VB
VB
W
B
XB
W
B
VB
W
B
T�B
W
B
XB
XB
W
B
W
B
XB
YB
YB
ZB
[#B
[#B
ZB
ZB
[#B
[#B
YB
YB
[#B
[#B
]/B
\)B
]/B
]/B
]/B
\)B
\)B
[#B
]/B
^5B
]/B
\)B
\)B
^5B
`BB
`BB
`BB
`BB
aHB
`BB
`BB
_;B
_;B
`BB
_;B
aHB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
bNB
cTB
aHB
bNB
cTB
dZB
cTB
dZB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
dZB
dZB
dZB
bNB
dZB
ffB
ffB
e`B
dZB
dZB
dZB
cTB
e`B
ffB
gmB
ffB
ffB
ffB
hsB
hsB
gmB
ffB
gmB
gmB
gmB
hsB
iyB
hsB
hsB
jB
k�B
l�B
l�B
l�B
l�B
k�B
k�B
k�B
jB
k�B
k�B
k�B
m�B
n�B
n�B
n�B
m�B
k�B
k�B
k�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
q�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
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
q�B
q�B
q�B
q�B
p�B
p�B
p�B
q�B
r�B
s�B
s�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
v�B
u�B
v�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
w�B
w�B
v�B
x�B
x�B
x�B
w�B
w�B
x�B
y�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
{�B
{�B
{�B
{�B
z�B
{�B
|�B
}�B
}�B
}�B
}�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
~�B
� B
� B
� B
�B
�B
�B
�B
� B
� B
� B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�B
�%B
�+B
�+B
�+B
�+B
�1B
�+B
�1B
�1B
�1B
�1B
�+B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�1B
�+B
�+B
�1B
�7B
�=B
�7B
�=B
�=B
�=B
�=B
�=B
�JB
�JB
�JB
�DB
�DB
�DB
�JB
�PB
�JB
�JB
�PB
�JB
�JB
�JB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�PB
�PB
�PB
�\B
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�hB
�bB
�\B
�\B
�oB
�oB
�oB
�oB
�oB
�oB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�{B
��B
��B
��B
�{B
�{B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.00 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20221122100213                              AO  ARCAADJP                                                                    20221122100213    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20221122100213  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20221122100213  QCF$                G�O�G�O�G�O�4000            