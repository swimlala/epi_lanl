CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-07-29T21:35:11Z creation;2017-07-29T21:35:17Z conversion to V3.1;2019-12-19T08:01:18Z update;     
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
_FillValue                 �  IH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  px   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΄   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170729213511  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_144                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�s��m�1   @�tI���@3��rGE9�d��f�A�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�3D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�ɚD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�3D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�ɚD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�;dA�7LA�33A�1'A�+A� �A��A��A�{A��`Aܥ�A�=qAڬA�/AفA��#A��/A��`AғuA��A�jA�t�A�bNA�\)A��A�;dA�S�A�+AĲ-A�VA�"�A��yA�+A�  A���A���A�~�A���A��A��RA�C�A���A���A�{A�A���A�A�A���A�M�A�A�A��A��jA�bA���A���A�?}A��A��\A��A���A�A���A�~�A��A��FA�ȴA�VA�x�A��A�1'A���A�1A��A��/A�33A�=qA��HA�%A��jA��;A�$�A�VA�A�A���A���A�~�A��A�=qA���A�/A�jA�9XA�p�A��wA�p�A���A��PA���A�A�bA��mA�ȴA��A�A�ȴA�M�A��yA�hsA���A��A�A�A��RA�  A~�`A{�wAz��Ay��Aw7LAt-ArZAp�/AnbNAk�AkhsAj1'Ai�Ai`BAhz�Ae�7Adz�Ac��Ac"�Ab$�A`~�A_A^-A]�A[�;A[%AZ{AY`BAW��AVv�AR�DAN�AMoAL(�AKt�AJ=qAI�AH~�AFA�ABĜA@  A<�A89XA6VA5t�A4��A3��A1S�A0A�A/��A/oA-+A,��A,�A*�A)�;A)�PA(�A'�-A&A�A%K�A%
=A$A"�uA"�A!�A $�AbNA��A�jA�7A�^AC�A�uAp�A�A��A�+A9XA�A��A�9A(�A�A��A%AĜA�TA�A
�uA	��A�`A�AJA�^A�A�A��AA�A`BA��A �`@��
@��@�I�@�C�@���@��@�v�@��+@���@�x�@�O�@�1'@�@�=q@��#@�^@�@�G�@�Ĝ@�1@�\@�7@��;@�G�@�E�@���@�bN@���@�1'@���@ؓu@׶F@֗�@֏\@֧�@�^5@�n�@Ѓ@Ͼw@�\)@���@�{@̬@�
=@��@��@�  @��
@� �@Ǿw@Ɵ�@�hs@ēu@�(�@�"�@�J@���@�|�@���@�J@�G�@�V@�r�@� �@�|�@�n�@�@��@�&�@��@�r�@�1'@� �@�b@��@���@�;d@��@���@�
=@�v�@���@�&�@��m@�dZ@���@�ȴ@��H@�33@��P@�  @��
@�l�@�33@���@�M�@�ff@���@�O�@���@��;@�;d@��y@��#@��D@�  @�@�V@�5?@��@��h@�?}@�V@���@�Ĝ@��@�1'@��
@�\)@��@��y@���@�ff@�@��#@���@�x�@�G�@�/@��@��`@��9@�j@�1'@���@��m@�ƨ@���@�33@��H@���@���@���@��R@��+@�M�@�J@���@��-@���@�x�@�X@�O�@��`@�I�@���@��
@��
@���@�ƨ@��F@��P@��@���@�~�@�E�@��T@�`B@��`@��@���@��@�r�@�A�@�ƨ@��@�l�@�K�@�"�@���@��R@��\@��+@�M�@��T@���@��@�X@�?}@���@���@�j@��P@�K�@�+@�"�@�
=@���@��H@���@��!@���@���@�v�@�J@��^@�x�@�7L@��@�%@���@�I�@��@���@�l�@��@���@���@��+@�=q@��@��#@�@��-@���@�X@�?}@�V@��`@��@�Q�@��@���@��@��P@�K�@�
=@���@��\@�v�@�V@�-@�@���@��-@��@�x�@�X@�?}@�V@��u@�bN@��@��
@��w@���@�dZ@�+@�o@�
=@�@��y@��y@��@��!@���@�v�@�n�@�=q@�$�@�{@���@��@�@���@�7L@�&�@��@���@���@��/@��@���@��D@��@��@��
@��@�dZ@�C�@�33@�
=@�ȴ@�M�@��@��7@�X@�7L@�V@���@�Ĝ@��j@��9@���@��D@� �@�1@���@��@�C�@�+@��@�
=@��y@���@��!@���@�v�@�^5@�=q@��@���@�`B@���@���@�Ĝ@��9@���@�9X@�b@��@�@|�@+@~��@~��@~ff@}�T@}��@}p�@}O�@}O�@}`B@|�/@|1@{�
@{33@z�!@z=q@y��@y%@x�u@xA�@w��@w;d@w
=@v�y@v�y@vȴ@v��@vv�@v$�@u��@u�h@uO�@t�@t(�@sƨ@sdZ@s"�@r��@r~�@q��@q�7@p��@p�@p  @ol�@o
=@n��@n$�@n@m��@m�@m�@lZ@k��@j��@jM�@jJ@i��@i�7@ix�@iG�@h��@h�9@h �@g��@g��@g|�@g;d@g
=@f��@fE�@e��@e�h@eV@dj@cƨ@cdZ@cC�@b�@bn�@bJ@a�^@a&�@`�9@`A�@_l�@_�@^ȴ@^ff@]�@]�h@]O�@\�@\�@\I�@[��@[�m@[�F@[t�@[@Z��@Z~�@Z=q@Y�7@Y&�@Y%@X�@Xb@W�@Wl�@W�@V�+@VV@V@U@U�h@U?}@T�@T��@T�D@Tj@T1@S�F@S�@S"�@R�@R��@R��@R~�@RM�@Q�#@Q�7@Q%@P��@P�9@Pr�@PQ�@Pb@O�;@O��@O|�@OK�@O;d@Nȴ@NE�@N$�@N@M�@M�T@M��@M`B@MV@L�@L�@L�/@L��@LZ@K��@K��@KdZ@J�@J��@Jn�@J=q@J�@I�^@Ix�@I7L@H��@Hr�@HbN@H  @G��@G
=@F�R@Fff@FV@F5?@E�@E�@E`B@E/@D�j@Dz�@Dj@D1@C33@B�H@B~�@A�@A��@Ahs@A&�@@��@@�`@@�@@Q�@?�;@?��@?K�@?;d@?
=@>ȴ@>ff@>E�@>{@=p�@=/@=�@=V@<��@<�/@<Z@;�m@;ƨ@;��@;��@;��@;�@;dZ@;o@:��@:�@9��@9hs@9�@8Ĝ@8��@8�u@8bN@7�@7�w@7|�@7l�@7;d@7
=@6�@6�+@6v�@6V@6$�@5�@5�@5��@5p�@5V@4j@4(�@3�m@3��@3dZ@3S�@333@3@2�@2��@2�\@2-@1�@1��@1hs@17L@0�`@0r�@0A�@/�;@/l�@/+@.��@.�y@.ȴ@.��@.E�@-��@-`B@-?}@-V@,�@,I�@,(�@,1@+�F@+S�@+o@*�H@*��@*n�@*M�@*=q@)��@)��@)��@(�`@(�9@(��@(�u@(�@(�@(�@(r�@( �@(b@( �@'�@'��@'�@'l�@'+@&�@&�R@&��@&ff@&{@%��@%?}@%V@$��@$�@$�@$z�@$Z@#�m@#��@#t�@#"�@"�@"��@"~�@"^5@"=q@"-@"-@"�@!�#@!��@!x�@!hs@!7L@ �9@ bN@ Q�@  �@�;@��@��@�w@��@|�@\)@K�@�@�y@�@ȴ@�R@E�@{@@��@��@�h@�@�@�@�D@I�@9X@(�@�@1@��@�
@��@t�@C�@"�@�@��@�!@�\@n�@M�@J@�#@��@x�@X@7L@�@�@bN@1'@�;@��@K�@
=@�y@�R@�+@E�@$�@{@@�T@��@`B@�@V@�/@��@�@�D@1@ƨ@t�@C�@33@"�@"�@"�@@�!@~�@^5@-@J@�@�7@7L@�@%@��@��@��@�@r�@bN@Q�@Q�@1'@ �@  @�@�;@�;@��@�@|�@K�@+@
=@��@�R@�R@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�;dA�7LA�33A�1'A�+A� �A��A��A�{A��`Aܥ�A�=qAڬA�/AفA��#A��/A��`AғuA��A�jA�t�A�bNA�\)A��A�;dA�S�A�+AĲ-A�VA�"�A��yA�+A�  A���A���A�~�A���A��A��RA�C�A���A���A�{A�A���A�A�A���A�M�A�A�A��A��jA�bA���A���A�?}A��A��\A��A���A�A���A�~�A��A��FA�ȴA�VA�x�A��A�1'A���A�1A��A��/A�33A�=qA��HA�%A��jA��;A�$�A�VA�A�A���A���A�~�A��A�=qA���A�/A�jA�9XA�p�A��wA�p�A���A��PA���A�A�bA��mA�ȴA��A�A�ȴA�M�A��yA�hsA���A��A�A�A��RA�  A~�`A{�wAz��Ay��Aw7LAt-ArZAp�/AnbNAk�AkhsAj1'Ai�Ai`BAhz�Ae�7Adz�Ac��Ac"�Ab$�A`~�A_A^-A]�A[�;A[%AZ{AY`BAW��AVv�AR�DAN�AMoAL(�AKt�AJ=qAI�AH~�AFA�ABĜA@  A<�A89XA6VA5t�A4��A3��A1S�A0A�A/��A/oA-+A,��A,�A*�A)�;A)�PA(�A'�-A&A�A%K�A%
=A$A"�uA"�A!�A $�AbNA��A�jA�7A�^AC�A�uAp�A�A��A�+A9XA�A��A�9A(�A�A��A%AĜA�TA�A
�uA	��A�`A�AJA�^A�A�A��AA�A`BA��A �`@��
@��@�I�@�C�@���@��@�v�@��+@���@�x�@�O�@�1'@�@�=q@��#@�^@�@�G�@�Ĝ@�1@�\@�7@��;@�G�@�E�@���@�bN@���@�1'@���@ؓu@׶F@֗�@֏\@֧�@�^5@�n�@Ѓ@Ͼw@�\)@���@�{@̬@�
=@��@��@�  @��
@� �@Ǿw@Ɵ�@�hs@ēu@�(�@�"�@�J@���@�|�@���@�J@�G�@�V@�r�@� �@�|�@�n�@�@��@�&�@��@�r�@�1'@� �@�b@��@���@�;d@��@���@�
=@�v�@���@�&�@��m@�dZ@���@�ȴ@��H@�33@��P@�  @��
@�l�@�33@���@�M�@�ff@���@�O�@���@��;@�;d@��y@��#@��D@�  @�@�V@�5?@��@��h@�?}@�V@���@�Ĝ@��@�1'@��
@�\)@��@��y@���@�ff@�@��#@���@�x�@�G�@�/@��@��`@��9@�j@�1'@���@��m@�ƨ@���@�33@��H@���@���@���@��R@��+@�M�@�J@���@��-@���@�x�@�X@�O�@��`@�I�@���@��
@��
@���@�ƨ@��F@��P@��@���@�~�@�E�@��T@�`B@��`@��@���@��@�r�@�A�@�ƨ@��@�l�@�K�@�"�@���@��R@��\@��+@�M�@��T@���@��@�X@�?}@���@���@�j@��P@�K�@�+@�"�@�
=@���@��H@���@��!@���@���@�v�@�J@��^@�x�@�7L@��@�%@���@�I�@��@���@�l�@��@���@���@��+@�=q@��@��#@�@��-@���@�X@�?}@�V@��`@��@�Q�@��@���@��@��P@�K�@�
=@���@��\@�v�@�V@�-@�@���@��-@��@�x�@�X@�?}@�V@��u@�bN@��@��
@��w@���@�dZ@�+@�o@�
=@�@��y@��y@��@��!@���@�v�@�n�@�=q@�$�@�{@���@��@�@���@�7L@�&�@��@���@���@��/@��@���@��D@��@��@��
@��@�dZ@�C�@�33@�
=@�ȴ@�M�@��@��7@�X@�7L@�V@���@�Ĝ@��j@��9@���@��D@� �@�1@���@��@�C�@�+@��@�
=@��y@���@��!@���@�v�@�^5@�=q@��@���@�`B@���@���@�Ĝ@��9@���@�9X@�b@��@�@|�@+@~��@~��@~ff@}�T@}��@}p�@}O�@}O�@}`B@|�/@|1@{�
@{33@z�!@z=q@y��@y%@x�u@xA�@w��@w;d@w
=@v�y@v�y@vȴ@v��@vv�@v$�@u��@u�h@uO�@t�@t(�@sƨ@sdZ@s"�@r��@r~�@q��@q�7@p��@p�@p  @ol�@o
=@n��@n$�@n@m��@m�@m�@lZ@k��@j��@jM�@jJ@i��@i�7@ix�@iG�@h��@h�9@h �@g��@g��@g|�@g;d@g
=@f��@fE�@e��@e�h@eV@dj@cƨ@cdZ@cC�@b�@bn�@bJ@a�^@a&�@`�9@`A�@_l�@_�@^ȴ@^ff@]�@]�h@]O�@\�@\�@\I�@[��@[�m@[�F@[t�@[@Z��@Z~�@Z=q@Y�7@Y&�@Y%@X�@Xb@W�@Wl�@W�@V�+@VV@V@U@U�h@U?}@T�@T��@T�D@Tj@T1@S�F@S�@S"�@R�@R��@R��@R~�@RM�@Q�#@Q�7@Q%@P��@P�9@Pr�@PQ�@Pb@O�;@O��@O|�@OK�@O;d@Nȴ@NE�@N$�@N@M�@M�T@M��@M`B@MV@L�@L�@L�/@L��@LZ@K��@K��@KdZ@J�@J��@Jn�@J=q@J�@I�^@Ix�@I7L@H��@Hr�@HbN@H  @G��@G
=@F�R@Fff@FV@F5?@E�@E�@E`B@E/@D�j@Dz�@Dj@D1@C33@B�H@B~�@A�@A��@Ahs@A&�@@��@@�`@@�@@Q�@?�;@?��@?K�@?;d@?
=@>ȴ@>ff@>E�@>{@=p�@=/@=�@=V@<��@<�/@<Z@;�m@;ƨ@;��@;��@;��@;�@;dZ@;o@:��@:�@9��@9hs@9�@8Ĝ@8��@8�u@8bN@7�@7�w@7|�@7l�@7;d@7
=@6�@6�+@6v�@6V@6$�@5�@5�@5��@5p�@5V@4j@4(�@3�m@3��@3dZ@3S�@333@3@2�@2��@2�\@2-@1�@1��@1hs@17L@0�`@0r�@0A�@/�;@/l�@/+@.��@.�y@.ȴ@.��@.E�@-��@-`B@-?}@-V@,�@,I�@,(�@,1@+�F@+S�@+o@*�H@*��@*n�@*M�@*=q@)��@)��@)��@(�`@(�9@(��@(�u@(�@(�@(�@(r�@( �@(b@( �@'�@'��@'�@'l�@'+@&�@&�R@&��@&ff@&{@%��@%?}@%V@$��@$�@$�@$z�@$Z@#�m@#��@#t�@#"�@"�@"��@"~�@"^5@"=q@"-@"-@"�@!�#@!��@!x�@!hs@!7L@ �9@ bN@ Q�@  �@�;@��@��@�w@��@|�@\)@K�@�@�y@�@ȴ@�R@E�@{@@��@��@�h@�@�@�@�D@I�@9X@(�@�@1@��@�
@��@t�@C�@"�@�@��@�!@�\@n�@M�@J@�#@��@x�@X@7L@�@�@bN@1'@�;@��@K�@
=@�y@�R@�+@E�@$�@{@@�T@��@`B@�@V@�/@��@�@�D@1@ƨ@t�@C�@33@"�@"�@"�@@�!@~�@^5@-@J@�@�7@7L@�@%@��@��@��@�@r�@bN@Q�@Q�@1'@ �@  @�@�;@�;@��@�@|�@K�@+@
=@��@�R@�R@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B� B�B� B~�B{�Bv�Bo�B^5BYBXBQ�BE�B?}Be`BiyB^5By�By�B}�B{�Bu�B�B� B�By�B�bB�{B��B��B��B��B��B��B�B�B�B�B��B�B�B�B�9B�9B�-B��B�B�B��B�B��B��B��B��B�bBz�B~�B~�B�%B�B�B�%B�=B�%B{�Bx�Bs�BiyB\)BN�BH�B;dB)�B�B�B%B�B�`B�5B��BȴB�}B�FB�B��B��B�DB�B�oBv�B\)BQ�BJ�B?}B(�B�B�B�B\B
��B
�B
�BB
�B
B
�B
�{B
�DB
�B
y�B
p�B
W
B
D�B
A�B
;dB
0!B
"�B
�B
hB
B
B	��B	��B	�B	�mB	�B	��B	��B	ȴB	��B	�?B	�B	��B	��B	��B	��B	�bB	�1B	y�B	gmB	L�B	+B	"�B	"�B	�B	�B	�B	PB	B�mB�BŢB�?B�^B�XB�RB�3B��B�B�B�B��B�B��B��B��B��B��B��B��B��B��B��B�uB��B��B�VB�1B�7B�+B�+B�B�JB�7B�B� Bw�B{�B~�B~�B{�Bw�Bz�Bv�Br�Bu�By�Bv�Bu�Bw�Bs�Bn�BgmBn�Bp�Bp�Bn�Bl�Bk�Be`B[#B\)BZBZB`BBbNB_;BcTBe`Bt�B�%B�+B�1B�B�B�JB�JB�VB�VB�VB�VB�VB�bB��B�{B�PB�+B�%B�%B�Bw�Be`BiyBp�Br�Bt�B� B�Bv�Bv�B|�B}�B|�B{�B|�B}�B�B�=B�PB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�-B�'B�3B�XB�}B��BĜBɺB��B��B��B��B��B�
B�)B�;B�NB�fB�mB�yB�sB�B�B��B��B��B	B	B		7B	DB	JB	JB	\B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	,B	1'B	33B	9XB	:^B	;dB	@�B	C�B	E�B	G�B	H�B	I�B	K�B	O�B	VB	YB	\)B	^5B	`BB	e`B	gmB	gmB	jB	l�B	m�B	n�B	o�B	q�B	t�B	v�B	z�B	|�B	}�B	~�B	�B	�%B	�+B	�1B	�1B	�=B	�PB	�\B	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�3B	�?B	�LB	�dB	�qB	�qB	�qB	�qB	�wB	��B	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�;B	�;B	�5B	�;B	�HB	�NB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
DB
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
\B
\B
\B
\B
hB
hB
hB
bB
bB
bB
hB
hB
hB
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
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
!�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
'�B
(�B
(�B
)�B
,B
,B
,B
,B
,B
,B
,B
,B
-B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
5?B
5?B
5?B
6FB
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
9XB
9XB
9XB
9XB
9XB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
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
C�B
C�B
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
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
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
I�B
J�B
J�B
J�B
J�B
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
L�B
L�B
K�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
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
R�B
R�B
R�B
R�B
R�B
R�B
R�B
T�B
T�B
T�B
T�B
VB
VB
T�B
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
XB
XB
XB
XB
YB
YB
ZB
YB
YB
YB
ZB
ZB
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
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
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
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
ffB
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
k�B
k�B
k�B
k�B
k�B
l�B
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
n�B
n�B
n�B
n�B
n�B
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
q�B
q�B
q�B
q�B
q�B
r�B
q�B
r�B
q�B
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
s�B
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
v�B
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
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B� B�B�B�B.B|jBw�Bp�B`�BZQBY�BTFBJXBEBhsBmCBcB{�B|6B�iB~�ByrB�GB��B��B~�B��B��B�|B�B�$B��B� B��B�B� B�CB��B��B�WB�!B��B�tB�ZB��B�0B��B��B�B�UB��B��B��B�pB�B}B�OB�B��B�B�SB�1B��B��B}�By�Bt�Bk�B^�BQ�BJrB=�B,�B�B�B�B�LB��B�BҽB��B��B��B��B��B�B�B��B��B|B^�BS[BK�BA�B,�BB5BQBNB
��B
�B
�B
רB
�%B
�oB
�sB
�JB
��B
{�B
r|B
ZQB
FYB
CaB
>wB
3�B
%FB
 �B
aB
�B
�B	�"B	�lB	�B	�B	�B	�:B	��B	ɠB	��B	�LB	��B	�*B	�@B	�/B	��B	��B	��B	|B	i�B	Q�B	/B	$�B	$B	�B	OB	B	�B	SB��B�BʌB��B��B��B��B�B��B�cB��B�oB�*B��B�B��B��B��B�&B�;B�dB��B�jB�#B�2B�_B��B�HB�XB�rB��B��B�+B�B�rB��B�'Bz�B}"B}B}B|�By	B{�BxRBt9Bv�BzxBxBv�Bx�Bt�BpUBi�Bo�Bq'BqBoOBm]BlWBf�B]dB]dB[�B[�Ba-Bc:B`�Bd&BfLBt�B�tB�zB��B�B�%B��B��B��B��B��B��B�B��B�yB��B�(B�B�+B��B�-Bz�Bh�BjBqABsMBt�B�4B��By$BxB}�B~]B}qB|�B~BB��B��B�"B��B��B�5B��B��B�TB�NB��B��B��B��B��B�sB�B�iB��B��B��B��B��B��B�B�B��B��B��B��B�4B�:B�sB�]B�;B�B��B�
B��B�_B��B�B��B��B��B	 �B	B		lB	�B	�B	�B	�B	�B	�B	�B	/B	CB	5B	 'B	"�B	%�B	,�B	1�B	3�B	9�B	:�B	;�B	@�B	C�B	E�B	G�B	H�B	J	B	LB	P.B	V9B	YKB	\]B	^jB	`�B	ezB	g�B	g�B	j�B	l�B	m�B	n�B	o�B	q�B	t�B	v�B	z�B	}"B	~(B	HB	�AB	�?B	�+B	�KB	�KB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	�B	�>B	�WB	�UB	�[B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	ÖB	ĶB	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	��B	�B	�B	�BB	�vB	�B	�B	�$B	�$B	�$B	�$B	�+B	�+B	�1B	�1B	�EB	�_B	�QB	�]B	�dB	�VB	�VB	ބB	ߊB	�|B	�B	�B	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�$B	��B	�B	�<B	�B	�B	�(B	�.B
 B
B
'B
'B
B
'B
-B
3B
3B
9B
9B
9B
?B
?B
YB
YB
tB
YB
KB
KB
1B
	RB
	RB
	RB
	RB
	RB
	�B
^B
^B
xB
xB
JB
~B
~B
�B
�B
�B
�B
vB
vB
vB
�B
hB
�B
�B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#B
"B
$B
$B
#�B
$&B
#�B
$B
%B
%B
%B
&B
&B
'B
'8B
'B
(
B
(�B
)B
)*B
(>B
)DB
)DB
*0B
,"B
,"B
,"B
,"B
,"B
,"B
,"B
,=B
-CB
.B
./B
./B
./B
.IB
.IB
/OB
/5B
/5B
/iB
0UB
1[B
1AB
1[B
2aB
2GB
2aB
2aB
3hB
3MB
3�B
5ZB
5ZB
5ZB
6`B
6`B
6`B
6zB
7fB
7fB
7LB
8lB
8lB
8lB
8�B
8lB
9rB
9�B
9�B
9rB
9rB
8�B
9�B
9rB
9rB
9�B
9�B
:xB
:xB
:xB
;B
;B
;B
;B
;B
;B
;B
;B
;B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
?�B
?}B
?�B
?�B
@�B
@�B
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
C{B
C�B
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
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
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
IB
I�B
I�B
I�B
J�B
J�B
J�B
J�B
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
L�B
L�B
K�B
M�B
M�B
M�B
M�B
M�B
NB
NB
N�B
N�B
N�B
N�B
N�B
N�B
N�B
OB
OB
PB
O�B
O�B
QB
Q B
Q B
Q B
QB
RB
RB
R B
RB
RB
RB
RB
R�B
SB
SB
SB
R�B
SB
S&B
SB
S&B
UB
U2B
UB
UB
U�B
VB
UB
VB
VB
VB
VB
VB
W$B
W$B
W$B
W?B
XEB
X+B
XEB
XEB
Y1B
Y1B
Z7B
Y1B
YB
YKB
ZQB
ZQB
[=B
[=B
[=B
[=B
\CB
\CB
\CB
\CB
]/B
]IB
]IB
^OB
^OB
^OB
^OB
^5B
^OB
^jB
_pB
`BB
`BB
`BB
`BB
`BB
`vB
`\B
`BB
`BB
`\B
`\B
`BB
`\B
a|B
abB
abB
abB
bhB
bhB
b�B
cTB
cnB
cTB
c�B
cnB
dZB
dtB
d�B
dtB
dtB
e`B
ezB
ezB
ezB
f�B
f�B
ffB
ffB
f�B
f�B
f�B
f�B
gmB
f�B
g�B
gmB
g�B
g�B
g�B
hsB
hsB
hsB
h�B
h�B
h�B
h�B
h�B
i�B
iyB
i�B
i�B
i�B
i�B
j�B
j�B
j�B
jB
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
kkB
k�B
k�B
l�B
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
n�B
n�B
n�B
n�B
n�B
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
q�B
q�B
q�B
q�B
q�B
r�B
q�B
r�B
q�B
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
s�B
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
v�B
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
xB
w�B
w�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201708030033442017080300334420170803003344201806221316582018062213165820180622131658201804050719052018040507190520180405071905  JA  ARFMdecpA19c                                                                20170730063506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170729213511  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170729213515  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170729213515  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170729213516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170729213516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170729213516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170729213516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170729213517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170729213517                      G�O�G�O�G�O�                JA  ARUP                                                                        20170729215607                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170730153745  CV  JULD            G�O�G�O�F�ӝ                JM  ARCAJMQC2.0                                                                 20170802153344  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170802153344  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221905  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041658  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                