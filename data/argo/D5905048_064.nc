CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-12-25T00:29:23Z creation;2019-12-25T00:29:25Z conversion to V3.1;2019-12-26T01:01:36Z update;     
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
resolution        =���   axis      Z        x  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  `L   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ̜   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20191225002923  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               @A   JA  I2_0577_064                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��u�ax 1   @��v�� @3������d�(���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��fD��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��fD��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�1'A�1'A�/A�1'A�1'A�1'A�33A�7LA�9XA�9XA�9XA�5?A�7LA�7LA�9XA�;dA�;dA�;dA�=qA�=qA�?}A�?}A�A�A�C�A�E�A�G�A�O�A�Q�A�`BA�hsAԃAԝ�A�O�Aӕ�A���AϏ\Aΰ!A�7LA�jA�`BA��A�t�AÍPA�+A�9XA���A�I�A�9XA�A��RA��/A���A��DA���A���A��\A�  A��A�5?A��-A�XA���A��7A��A��DA��wA��A��/A��+A�Q�A���A�A�A�hsA�p�A��A��`A��A�ĜA���A��HA�JA�{A���A���A��/A��mA�=qA��A�S�A���A�-A��A�^5A��7A�/A�n�A���A�JA��A��\A�t�A�|�A��9A�Q�A��-A�$�A� �A��#A�bAz��AxVAv�uAt��Ar{Aol�An�DAm�hAj-Ag��Af��Ae�Ac/Aa7LA]S�A[��AY�^AW�TAV�yAU�ASC�AR-AP��AN�jAL��AK�AI�mAHr�AF�jAE��AC��A@~�A@(�A?�A?��A?x�A=�A;dZA:�`A9hsA8JA6�A5��A4��A3+A2�+A1O�A.=qA+�A)33A'��A&(�A%�A#��A"jA!x�A ~�A��A �AZAbA��A��AQ�A��A�A��A1'A5?AI�A�A��AjAA��A��A��A��AE�A�A
�DA	|�Av�A�A�A7LA�A��A�9AVAK�A"�A�AffA�Ax�A �!@�33@�n�@��7@�9X@�j@��@�z�@��@��!@�-@�V@��j@���@�j@�1'@��;@�~�@�/@�F@�5?@��^@�p�@�R@蛦@�l�@�!@�7@��H@�h@��@��@� �@��@߶F@���@�x�@۝�@ى7@�&�@�`B@�x�@��m@�X@���@�\)@�ȴ@ҏ\@��@с@� �@�;d@·+@̴9@̃@�j@�Z@�I�@� �@���@��#@ȣ�@��@���@�7L@���@�Z@�;d@��T@��@��@��@��\@�E�@�p�@��u@�dZ@�o@�E�@��@��-@��@���@�\)@�+@��@��@���@��@�hs@�V@���@���@�1'@��
@��@��#@�O�@��@��@���@��u@�(�@��w@�"�@�
=@�@��@��!@�M�@��T@�x�@�&�@��@�A�@�1@���@�@��R@���@�hs@�%@���@��j@��9@��D@� �@��@��@��m@���@�\)@�33@���@�n�@�^5@�E�@�5?@�$�@���@�?}@��@���@���@���@�z�@�j@�Q�@�(�@�ƨ@�dZ@�
=@���@�V@�J@��7@�O�@��@���@�(�@��
@��P@���@�V@��@��@��h@��@��u@�j@�I�@�bN@�b@��9@��H@���@��T@��@��@�v�@�V@�~�@�~�@�@�Z@���@��@�%@��/@�Z@��@��@��@�  @���@�\)@�\)@�C�@��@�-@���@�V@��@���@��D@�I�@�I�@��
@���@�C�@���@�~�@�^5@�J@��-@���@�p�@�X@�X@�O�@�7L@�7L@�G�@�G�@�?}@�`B@�`B@�X@�hs@�G�@��@��@���@�5?@��#@���@��@�`B@�7L@��`@��D@�9X@�(�@��@�1@��@�ƨ@��F@�l�@�;d@�K�@�S�@�K�@�"�@��H@�v�@�-@���@��@���@���@���@��9@��j@���@���@��+@�E�@�^5@�$�@�x�@�O�@�%@�bN@�ƨ@�l�@�@��R@���@��\@���@��+@�n�@�^5@�V@�M�@�M�@�-@��@�@��-@���@�7L@���@���@���@�z�@�A�@��m@��P@�33@�o@�ȴ@�-@��#@���@�hs@�?}@�&�@���@��@��u@��@�z�@�Z@�9X@�  @~�@~ff@~V@~5?@}�T@}O�@|�D@|�@{��@{ƨ@{�@{�@z=q@x��@x �@w\)@v��@v5?@u��@t��@t��@tI�@sƨ@sS�@r�H@r�\@rn�@rJ@q��@qG�@p��@p�u@p  @o��@n�@m�@m�h@lI�@kƨ@kS�@k@j��@j~�@j�@i�#@i�7@i7L@h�u@gK�@f�R@f�+@fff@fE�@e��@eV@d�D@c��@c��@b�!@b=q@a�7@a�@a%@`��@`��@`�u@`1'@_|�@_;d@^ȴ@]`B@]�@]V@\��@\�@\j@\Z@\I�@\(�@[��@[ƨ@[��@[��@[�@[C�@Z�@Z��@Z��@Z��@Z^5@Y��@X��@X�@Xb@W�@V5?@U�-@Up�@T��@TZ@S�m@S��@SC�@R�!@R^5@R-@Q�#@Qx�@Q&�@P��@P��@P��@P�u@PbN@O�@O�w@O�@O\)@N�y@N�R@N��@N��@Nff@N@M��@Mp�@MO�@L��@LZ@L1@K�m@K�
@K�
@Kƨ@K�F@K"�@J~�@J=q@I�@I��@I��@Ihs@H�`@HbN@H  @G�@G|�@Gl�@G
=@F@E��@E�@E?}@E/@D��@D�j@DZ@D(�@D�@C��@C�m@C�F@CS�@Co@B�!@Bn�@B=q@A��@A�@@r�@@  @?��@?;d@>ȴ@>��@>E�@=��@=��@=`B@=V@<�/@<��@<�j@<�@<Z@<1@;��@:�@:~�@9��@9��@9X@9G�@97L@9&�@9%@8�`@8��@8�9@8�@81'@7��@7�w@7�@7��@7�P@7|�@7\)@7
=@6��@6{@5�h@4��@4Z@4�@41@3��@3�@2��@2^5@2=q@2=q@2�@2J@1��@1�@1��@1��@1�7@1hs@1G�@17L@1&�@0��@0�@0 �@/��@/�P@/\)@/K�@/�@/
=@/
=@/
=@.�y@.v�@-�T@-p�@-�@,��@,�D@,I�@+�m@*�@*^5@*J@)��@)�7@)�7@)�7@)X@)7L@(�`@(��@(��@(��@(Ĝ@(��@(��@(�@(1'@'�w@'�@'|�@'\)@'�@&��@&�@&�+@&v�@&ff@&V@&E�@&$�@%��@%O�@$�@$��@$j@#��@#C�@"�H@"�!@"-@!x�@!%@ ��@ ��@ ��@ bN@�;@l�@;d@
=@��@�y@�y@�y@v�@V@$�@�@p�@/@V@V@�@�@Z@(�@1@ƨ@�@C�@o@�@�!@=q@�^@x�@G�@&�@��@��@�9@��@�u@�@r�@Q�@1'@�@��@��@|�@+@�@�R@�+@V@@�-@��@�h@�h@�h@�h@�h@�@�@p�@O�@?}@V@�@Z@(�@1@�m@�
@ƨ@��@��@��@t�@o@@�@��@��@n�@-@��@��@��@��@��@��@�7@x�@G�@�`@�@r�@r�@Q�@A�@1'@ �@  @�;@�;@�;@�;@�;@��@�P@;d@+@+@�@
=@��@�@ȴ@��@ff@V@5?@$�@@�T@��@��@`B@O�@?}@�@V@�/@��@�j@�@�@�@�@��@��@�D@�D@z�@z�@(�@ƨ@��@t�@S�@
��@
�\@
n�@
^5@
M�@
=q@
=q@
-@
-@
�@
J@	��@	�@	�#@	�^@	�7@	7L@��@��@r�@bN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�1'A�1'A�/A�1'A�1'A�1'A�33A�7LA�9XA�9XA�9XA�5?A�7LA�7LA�9XA�;dA�;dA�;dA�=qA�=qA�?}A�?}A�A�A�C�A�E�A�G�A�O�A�Q�A�`BA�hsAԃAԝ�A�O�Aӕ�A���AϏ\Aΰ!A�7LA�jA�`BA��A�t�AÍPA�+A�9XA���A�I�A�9XA�A��RA��/A���A��DA���A���A��\A�  A��A�5?A��-A�XA���A��7A��A��DA��wA��A��/A��+A�Q�A���A�A�A�hsA�p�A��A��`A��A�ĜA���A��HA�JA�{A���A���A��/A��mA�=qA��A�S�A���A�-A��A�^5A��7A�/A�n�A���A�JA��A��\A�t�A�|�A��9A�Q�A��-A�$�A� �A��#A�bAz��AxVAv�uAt��Ar{Aol�An�DAm�hAj-Ag��Af��Ae�Ac/Aa7LA]S�A[��AY�^AW�TAV�yAU�ASC�AR-AP��AN�jAL��AK�AI�mAHr�AF�jAE��AC��A@~�A@(�A?�A?��A?x�A=�A;dZA:�`A9hsA8JA6�A5��A4��A3+A2�+A1O�A.=qA+�A)33A'��A&(�A%�A#��A"jA!x�A ~�A��A �AZAbA��A��AQ�A��A�A��A1'A5?AI�A�A��AjAA��A��A��A��AE�A�A
�DA	|�Av�A�A�A7LA�A��A�9AVAK�A"�A�AffA�Ax�A �!@�33@�n�@��7@�9X@�j@��@�z�@��@��!@�-@�V@��j@���@�j@�1'@��;@�~�@�/@�F@�5?@��^@�p�@�R@蛦@�l�@�!@�7@��H@�h@��@��@� �@��@߶F@���@�x�@۝�@ى7@�&�@�`B@�x�@��m@�X@���@�\)@�ȴ@ҏ\@��@с@� �@�;d@·+@̴9@̃@�j@�Z@�I�@� �@���@��#@ȣ�@��@���@�7L@���@�Z@�;d@��T@��@��@��@��\@�E�@�p�@��u@�dZ@�o@�E�@��@��-@��@���@�\)@�+@��@��@���@��@�hs@�V@���@���@�1'@��
@��@��#@�O�@��@��@���@��u@�(�@��w@�"�@�
=@�@��@��!@�M�@��T@�x�@�&�@��@�A�@�1@���@�@��R@���@�hs@�%@���@��j@��9@��D@� �@��@��@��m@���@�\)@�33@���@�n�@�^5@�E�@�5?@�$�@���@�?}@��@���@���@���@�z�@�j@�Q�@�(�@�ƨ@�dZ@�
=@���@�V@�J@��7@�O�@��@���@�(�@��
@��P@���@�V@��@��@��h@��@��u@�j@�I�@�bN@�b@��9@��H@���@��T@��@��@�v�@�V@�~�@�~�@�@�Z@���@��@�%@��/@�Z@��@��@��@�  @���@�\)@�\)@�C�@��@�-@���@�V@��@���@��D@�I�@�I�@��
@���@�C�@���@�~�@�^5@�J@��-@���@�p�@�X@�X@�O�@�7L@�7L@�G�@�G�@�?}@�`B@�`B@�X@�hs@�G�@��@��@���@�5?@��#@���@��@�`B@�7L@��`@��D@�9X@�(�@��@�1@��@�ƨ@��F@�l�@�;d@�K�@�S�@�K�@�"�@��H@�v�@�-@���@��@���@���@���@��9@��j@���@���@��+@�E�@�^5@�$�@�x�@�O�@�%@�bN@�ƨ@�l�@�@��R@���@��\@���@��+@�n�@�^5@�V@�M�@�M�@�-@��@�@��-@���@�7L@���@���@���@�z�@�A�@��m@��P@�33@�o@�ȴ@�-@��#@���@�hs@�?}@�&�@���@��@��u@��@�z�@�Z@�9X@�  @~�@~ff@~V@~5?@}�T@}O�@|�D@|�@{��@{ƨ@{�@{�@z=q@x��@x �@w\)@v��@v5?@u��@t��@t��@tI�@sƨ@sS�@r�H@r�\@rn�@rJ@q��@qG�@p��@p�u@p  @o��@n�@m�@m�h@lI�@kƨ@kS�@k@j��@j~�@j�@i�#@i�7@i7L@h�u@gK�@f�R@f�+@fff@fE�@e��@eV@d�D@c��@c��@b�!@b=q@a�7@a�@a%@`��@`��@`�u@`1'@_|�@_;d@^ȴ@]`B@]�@]V@\��@\�@\j@\Z@\I�@\(�@[��@[ƨ@[��@[��@[�@[C�@Z�@Z��@Z��@Z��@Z^5@Y��@X��@X�@Xb@W�@V5?@U�-@Up�@T��@TZ@S�m@S��@SC�@R�!@R^5@R-@Q�#@Qx�@Q&�@P��@P��@P��@P�u@PbN@O�@O�w@O�@O\)@N�y@N�R@N��@N��@Nff@N@M��@Mp�@MO�@L��@LZ@L1@K�m@K�
@K�
@Kƨ@K�F@K"�@J~�@J=q@I�@I��@I��@Ihs@H�`@HbN@H  @G�@G|�@Gl�@G
=@F@E��@E�@E?}@E/@D��@D�j@DZ@D(�@D�@C��@C�m@C�F@CS�@Co@B�!@Bn�@B=q@A��@A�@@r�@@  @?��@?;d@>ȴ@>��@>E�@=��@=��@=`B@=V@<�/@<��@<�j@<�@<Z@<1@;��@:�@:~�@9��@9��@9X@9G�@97L@9&�@9%@8�`@8��@8�9@8�@81'@7��@7�w@7�@7��@7�P@7|�@7\)@7
=@6��@6{@5�h@4��@4Z@4�@41@3��@3�@2��@2^5@2=q@2=q@2�@2J@1��@1�@1��@1��@1�7@1hs@1G�@17L@1&�@0��@0�@0 �@/��@/�P@/\)@/K�@/�@/
=@/
=@/
=@.�y@.v�@-�T@-p�@-�@,��@,�D@,I�@+�m@*�@*^5@*J@)��@)�7@)�7@)�7@)X@)7L@(�`@(��@(��@(��@(Ĝ@(��@(��@(�@(1'@'�w@'�@'|�@'\)@'�@&��@&�@&�+@&v�@&ff@&V@&E�@&$�@%��@%O�@$�@$��@$j@#��@#C�@"�H@"�!@"-@!x�@!%@ ��@ ��@ ��@ bN@�;@l�@;d@
=@��@�y@�y@�y@v�@V@$�@�@p�@/@V@V@�@�@Z@(�@1@ƨ@�@C�@o@�@�!@=q@�^@x�@G�@&�@��@��@�9@��@�u@�@r�@Q�@1'@�@��@��@|�@+@�@�R@�+@V@@�-@��@�h@�h@�h@�h@�h@�@�@p�@O�@?}@V@�@Z@(�@1@�m@�
@ƨ@��@��@��@t�@o@@�@��@��@n�@-@��@��@��@��@��@��@�7@x�@G�@�`@�@r�@r�@Q�@A�@1'@ �@  @�;@�;@�;@�;@�;@��@�P@;d@+@+@�@
=@��@�@ȴ@��@ff@V@5?@$�@@�T@��@��@`B@O�@?}@�@V@�/@��@�j@�@�@�@�@��@��@�D@�D@z�@z�@(�@ƨ@��@t�@S�@
��@
�\@
n�@
^5@
M�@
=q@
=q@
-@
-@
�@
J@	��@	�@	�#@	�^@	�7@	7L@��@��@r�@bN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B=qB=qB=qB=qB>wB>wB>wB?}B@�B@�B?}B>wB>wB>wB?}B?}B?}B>wB>wB?}B?}B@�B@�B@�BB�BB�BC�BF�BM�BQ�BffB�B(�BA�B^5B{�B�1B��B�B�B�B�9B�9B�?B�jBÖBɺB��B��B��B�#B�B�B��B�B��B��B��B��B  BVB�B!�B"�B#�B �B�B�BuB
=B��B�B�fB�}B��B�bB�Bu�BffB\)BD�B:^B33B+BhB��B�ZB�TB�5B�BƨB�LB�DB|�Bv�Bm�B^5BS�BC�B �BPBB
�B
�B
�`B
�#B
��B
�B
�hB
hsB
S�B
G�B
9XB
+B
�B
hB
JB	��B	�NB	�B	��B	�^B	�B	�uB	�B	y�B	l�B	cTB	\)B	N�B	G�B	=qB	5?B	(�B	 �B	�B	bB	
=B	B��B�B�B�B�B�yB�mB�B�B��B��BǮBĜB�wB�dB�?B�3B�B��B��B��B��B�{B�bB�VB�DB�=B�+B�%B�B�B�B~�B~�B|�B{�Bz�By�Bw�Bt�Bq�Bp�Bo�Bn�Bm�Bl�BjBiyBiyBhsBiyBffBdZBbNBaHB`BBaHB^5B]/B]/B\)B[#B[#BZB[#B[#B]/B\)B]/B\)B_;BhsBp�Bp�Bn�Bp�BffBaHB`BB_;B^5B]/B\)B[#B\)B]/B]/B\)B\)B^5B[#B[#B]/B_;BdZBffBgmBiyBl�Bm�Bn�Bs�Bw�Bw�Bw�By�Bz�B|�B� B}�B�B�B�1B�DB�VB�hB�oB�hB�uB��B��B��B��B��B��B�B�B�B�9B�LB�RB�RB�XB�dB�jB�wB��BÖBĜBĜBȴB��B��B��B�
B�B�B�;B�TB�fB�mB�sB�sB�yB�B�B�B��B��B��B��B	B	1B	DB	JB	VB	\B	bB	uB	�B	�B	�B	�B	�B	�B	!�B	$�B	&�B	(�B	-B	0!B	2-B	6FB	9XB	;dB	B�B	G�B	J�B	K�B	K�B	L�B	L�B	N�B	P�B	P�B	P�B	Q�B	R�B	S�B	W
B	XB	YB	ZB	[#B	[#B	^5B	`BB	aHB	bNB	cTB	cTB	dZB	dZB	e`B	gmB	m�B	p�B	r�B	r�B	t�B	u�B	x�B	z�B	~�B	�B	�B	�%B	�1B	�JB	�VB	�VB	�\B	�hB	�{B	�{B	�{B	��B	��B	��B	�B	�dB	��B	��B	�wB	�wB	�dB	�jB	�wB	�}B	ÖB	��B	��B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�HB	�BB	�;B	�;B	�BB	�TB	�TB	�NB	�ZB	�TB	�NB	�HB	�BB	�HB	�HB	�BB	�5B	�5B	�BB	�NB	�TB	�`B	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
+B

=B
DB

=B
DB

=B

=B
DB
JB
JB
PB
VB
oB
uB
uB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
!�B
"�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
+B
,B
,B
,B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
.B
.B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
9XB
9XB
:^B
:^B
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
=qB
=qB
=qB
=qB
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
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
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
D�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
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
YB
YB
YB
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
\)B
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
_;B
_;B
_;B
_;B
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
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
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
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
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
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
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
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
z�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B=qB=qB=qB=qB>wB>wB>wB?}B@�B@�B?}B>wB>wB>wB?}B?}B?}B>wB>wB?}B?}B@�B@�B@�BB�BB�BC�BF�BM�BRBgmB�B+BE�BbhB~wB�~B�4B��B��B�|B��B�tB�B��B�MB�	B�xB�"BӏBߊB�AB�hB��B��B��B��B��B��BGB�B]B$&B$�B%`B!�B!BBmB�B�0B�/B�B��B�zB�B�?BxBi_B_�BF�B;�B6FB/B�BB�`B�FB�vBܒB�=B��B�jB~(Bx�Bo�B`\BW?BHKB#nB�B�B
��B
�5B
�RB
ބB
��B
��B
��B
kkB
VmB
JXB
<jB
-�B
#B
uB
B	��B	�B	�#B	�\B	�qB	�'B	��B	��B	|B	n/B	e�B	^OB	P�B	I�B	?�B	7fB	*�B	#:B	�B	�B	0B	�B��B�9B�B�)B�qB��B��B�WB�BյB�pB�lB�B�OB��B��B��B�5B�
B��B��B�1B�9B� B��B��B��B�7B��B��B��B��B�B�B}�B|�B|PB|jBzBv+BraBq'Bp�Bp!BoBmwBk6BjBk�BkBj�Bg�BeBb�BbNBa�BbhB^�B^B^OB\�B[�B[�B[	B[�B\]B^jB\�B]�B\�B_;BhXBqABq[Bp!Br�BgBa�B`vB_�B^�B]�B]/B\)B]/B^B]�B\�B^B_�B[�B[�B^OB`�Be,Bf�Bg�Bi�Bl�Bm�BoiBt�By$Bx�BxBy�B{JB~BB��B~�B��B��B��B��B��B�TB�&B� B��B��B��B��B�B�LB��B��B�B�;B�B��B��B��B�DB�jB�VB�HB�B��B�B�SB�lB̘B�4B҉B�YB�yB��B߾B�B�B�B�B��B��B�B��B��B��B�+B�>B��B	�B	�B	xB	~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	"B	%,B	'8B	)_B	-CB	0oB	2�B	6�B	9�B	;�B	B�B	G�B	J�B	K�B	K�B	MB	MB	OB	Q B	Q B	QB	R B	S&B	TaB	W?B	X+B	Y1B	Z7B	[WB	[qB	^�B	`\B	abB	b�B	cnB	c�B	dtB	dtB	e�B	g�B	m�B	p�B	r�B	r�B	uB	v+B	y	B	{0B	cB	�[B	�mB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�FB	�B	�B	�AB	� B	�B	�}B	�B	�PB	�]B	�B	��B	ϑB	��B	�+B	�KB	�kB	�WB	�)B	�OB	�VB	�pB	��B	�\B	�vB	�B	�B	�B	߾B	�pB	�BB	�B	�B	�hB	�B	�B	�B	�B	�vB	�bB	�|B	�vB	�OB	�OB	�\B	�NB	�nB	�zB	�fB	�mB	�sB	�sB	�yB	�B	��B	�B	��B	�ZB	�ZB	�TB	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	��B	��B	�B	�(B	�(B	�<B	�6B	�JB	�6B	�B	��B	�B	�B	�.B
 �B
oB
�B
GB
B
YB
�B

rB
�B

�B
�B

�B

�B
xB
dB
dB
PB
pB
�B
�B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
;B
!-B
!B
"B
"B
#B
$B
%B
&B
&B
&B
&B
&B
'B
'B
'B
'B
'B
(
B
($B
($B
)*B
)DB
)DB
*0B
*eB
+6B
+6B
,"B
,"B
,"B
,=B
,"B
,=B
,=B
,WB
,qB
-CB
-)B
-)B
-)B
-CB
.cB
.IB
/OB
/OB
/iB
0UB
0UB
1AB
1'B
1AB
1AB
1AB
1[B
1[B
2aB
2|B
3�B
5ZB
5ZB
5ZB
5ZB
5ZB
5?B
5ZB
5ZB
5ZB
6`B
6`B
6FB
6`B
6`B
6`B
6`B
6`B
6`B
6zB
6�B
7�B
7�B
7�B
7�B
9�B
9�B
:xB
:�B
:xB
:�B
;B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>wB
>�B
>�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
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
D�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
L�B
L�B
M�B
NB
NB
N"B
OB
OB
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
Q B
Q B
Q B
Q B
P�B
P�B
Q�B
Q�B
RB
RB
R B
R B
R B
S&B
S@B
TB
TB
TB
TB
T,B
U2B
VB
VB
VB
VB
VB
VB
VB
VB
W$B
W$B
W$B
W$B
W
B
W$B
W$B
W$B
X+B
X+B
X+B
Y1B
YB
Y1B
YB
YB
YB
Y1B
YKB
ZQB
ZQB
Z7B
[=B
[=B
[=B
[WB
\xB
]dB
]IB
]IB
^OB
^5B
^5B
^OB
^OB
^OB
_;B
_;B
_;B
_;B
_VB
_;B
_VB
_VB
_VB
`BB
`\B
`\B
`\B
`\B
`\B
`\B
aHB
aHB
aHB
abB
abB
a|B
a|B
b�B
bhB
bhB
c�B
c�B
c�B
dtB
d�B
e�B
e�B
ffB
ffB
f�B
f�B
f�B
g�B
g�B
g�B
gmB
gmB
gmB
g�B
g�B
h�B
h�B
h�B
h�B
i�B
i�B
iyB
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
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
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
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
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
zB
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
z�B
{�B
{�B
|B
|B
|B
}B
}B
}B
|�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201912260101372019122601013720191226010137201804050705592018040507055920180405070559JA  ARFMdecpA19c                                                                20161202093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191225002923  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191225002924  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191225002924  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191225002925  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191225002925  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191225002925  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191225002925  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191225002925  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191225002925                      G�O�G�O�G�O�                JA  ARUP                                                                        20191225011512                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161214014118  CV  JULD            G�O�G�O�F��                JM  ARSQOW  1.1 2017V1                                                          20180404220559  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040549  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                