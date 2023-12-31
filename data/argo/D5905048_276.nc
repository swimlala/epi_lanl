CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-29T21:35:12Z creation;2018-08-29T21:35:17Z conversion to V3.1;2019-12-19T07:30:03Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180829213512  20200116231518  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_276                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�}s'@ڀ1   @�}tq� @4��oiDg�da��rG1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A^ffA�  A�  A�  A�  A���A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�  A   A   A@  A^ffA�  A�  A�  A�  A���A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�5?A�9XA�9XA�7LA�1'A�33A�33A�33A�33A�33A�5?A�5?A�7LA�7LA�9XA�;dA�;dA�=qA�=qA�C�A�G�A�XAݏ\Aݥ�AݶFAݾwAݲ-Aݙ�A݉7A�M�A� �A��HA�M�Aۗ�A�S�A��Aڏ\A�$�A��A�bNA�=qA��A��/A���A��A�`BA˼jA���A�A��A�p�A���A�/A�$�A��A�A���A�hsA���AhA�+A���A�dZA��A���A�%A��yA���A�E�A�O�A��hA��A�r�A�E�A��A���A�;dA�ĜA��A��A�1'A��/A�(�A�bNA�p�A�"�A�A��wA���A�^5A���A�ffA�XA�/A�n�A�A�A��+A�$�A���A�S�A�  A�p�A�7LA�I�A��A���A���A��A���A���A�"�A�A��A���A�{AS�A~�A}hsA{;dAy�Atr�Aq�mAm��Ak�Ag%Ae�Ac�wAb�yAa��A`ffA`�`AaS�Aa\)Aa&�A`�uA]A[hsAX��AX{AU��AT�AS�ASC�ARI�AO�TAN-AM�AK�^AI��AF�AD$�AB{A@��A?��A>~�A=?}A<��A:�A9hsA8^5A7|�A6bNA5��A5O�A4ĜA3�7A2~�A/"�A,��A*�A)�wA(�yA(VA%�
A$z�A"n�A!p�A�FA\)A1'A&�A��A��At�AoA{A��A��A�^A/A�yA-A�mA�uA=qAt�A�AoA�A
=A�/A�!A��AE�A�AbA
A�A
�A
 �A	��A	%A�FA�+A��A��AA�A9XAE�AƨA  AK�A^5A�hA �HA ~�A �uA r�A �@��;@�l�@��H@���@��-@��`@�`B@�  @�%@�7L@�R@�ȴ@��@��@�\@�j@�x�@�  @߅@�\)@ڗ�@ٲ-@� �@�  @���@��@��
@�ƨ@׮@�ȴ@�r�@�\)@�&�@�o@�5?@ͺ^@̼j@�
=@�^5@ɲ-@ǥ�@š�@�`B@Ĭ@�r�@���@�dZ@�
=@���@�M�@ģ�@�;d@�1'@��;@�ƨ@Ǿw@ǍP@�t�@�t�@Ǖ�@���@�l�@�o@\@�=q@��^@�z�@���@��@��@�l�@���@��@�Ĝ@��H@�E�@�O�@�1'@��R@��@�`B@�hs@��@��@��j@�j@�Z@�Z@� �@�(�@��@��;@��@���@��@�S�@�@�hs@�X@�p�@�hs@���@�r�@�1@���@�E�@�7L@��j@�z�@�bN@�1'@��@��w@��@�n�@�@��T@��^@���@���@�@�ff@�ȴ@��y@���@��@�V@��@��@�t�@�;d@��@���@�V@���@��#@���@�X@�/@��@���@��@�%@�V@�7L@���@��@�?}@�x�@�G�@��@�%@���@�r�@�(�@�  @���@���@��@�"�@��+@�@�G�@��@��/@��@�Z@�9X@�9X@�(�@�1@��@�l�@�K�@�C�@�dZ@�l�@�\)@�"�@���@��\@�$�@��@���@��h@�x�@�O�@�7L@��@��@���@�z�@��
@�\)@�\)@�S�@�C�@�+@��@���@�^5@�E�@�$�@�J@��7@�7L@�%@�%@�%@���@���@��@��/@��D@�Z@���@��P@�@���@�V@�$�@��#@���@�7L@��/@��u@�j@�1@��
@��F@���@�dZ@��@�@��@�ȴ@�^5@�{@���@���@�hs@���@�I�@��@���@��m@�ƨ@���@�t�@�dZ@���@�ff@�-@�{@�$�@���@��T@��#@�@���@��@�Ĝ@��@��P@�"�@��y@��!@�v�@�@���@�@���@�X@�/@�V@��@��@�Z@�  @��F@�t�@�+@���@���@��+@�^5@�V@�$�@�@���@��@�X@�7L@�7L@��@��@�/@��@��@�V@���@��j@���@�Q�@
=@}�@}V@|��@|�@}�@}�@}��@}��@}�@}/@|9X@y�@zJ@z�@y�^@xA�@xA�@x  @w�@vȴ@u?}@t��@t1@s��@s�@sdZ@s33@s33@sC�@st�@t1@t�/@t9X@sƨ@s�
@s�F@s��@sS�@s33@rn�@q�^@qx�@q�@p�9@p  @o+@o
=@nȴ@nff@n{@m�@m/@l�j@lZ@l(�@kt�@j�H@j��@j^5@i��@h�`@hbN@hA�@hb@g��@g��@g|�@f��@f�+@f5?@f{@f{@f@e�T@e�@d�D@d�@cƨ@cS�@b�!@b=q@a��@ax�@ahs@`��@`  @_�@_l�@_+@^ȴ@^��@]��@]/@\��@\�j@\Z@[�m@[ƨ@[�@[33@Z�!@ZJ@Y�#@Yx�@X�9@X�u@XQ�@W��@W�@Wl�@Vȴ@Vv�@V5?@V$�@V{@U�T@U��@U`B@U�@T�/@T��@TZ@TI�@TI�@T9X@S�
@S��@SdZ@SS�@So@R�@R�H@R��@R��@R��@Rn�@R�@RJ@Q��@Q&�@P�9@P�u@P1'@P  @O�@O�@O��@O�P@O\)@O�@N��@N�y@Nȴ@N{@Mp�@Mp�@L�/@L�j@LI�@K�F@J~�@Jn�@JM�@I��@I�#@I�7@Ix�@IX@I7L@HĜ@H�u@HA�@Hb@G�;@G�@G�;@G�w@G�P@G|�@G|�@G|�@G\)@G\)@Gl�@F��@F��@F@E��@E@EO�@D�/@D�j@D��@Dz�@D�@C�m@Cƨ@C�F@Co@B��@B~�@B^5@A��@@��@@Ĝ@@��@@�`@@��@@�`@@�@@bN@@1'@@  @?��@?�P@?�@>ȴ@>��@>v�@>V@>$�@=�T@=��@=��@=@=p�@=/@<��@<��@<j@<Z@<�@;�F@;dZ@;"�@:�!@:J@9��@9��@:J@:J@:J@:J@9��@9��@9hs@9&�@8��@8�9@8Q�@8A�@7�@7
=@6��@6��@6��@6�+@6E�@5O�@4��@41@3��@3�m@3��@3"�@2�@2��@2��@2M�@1��@1�^@1x�@0��@0A�@0b@0b@0  @/��@/l�@/K�@/
=@.�+@.v�@.v�@.V@.5?@.$�@-�T@-�@-?}@,�@,�/@,��@,z�@,(�@,1@+�
@+t�@+33@+@*�H@*��@*~�@*~�@)��@)�^@)G�@(��@(A�@( �@( �@(  @'�;@'��@'��@'�@'
=@&�@&�+@&V@&$�@%�T@%?}@$�@$(�@#�
@#��@#33@"��@"~�@"^5@"-@"J@!�@!��@!G�@!&�@!%@ ��@ bN@ 1'@   @   @   @�@��@+@ȴ@��@��@V@�@�-@�-@�-@��@��@�@�h@p�@V@��@�@�D@I�@9X@(�@�@��@�F@t�@�@��@�\@n�@^5@-@�@�#@��@��@G�@%@��@��@�u@�@Q�@A�@��@�@\)@�y@��@�+@v�@V@$�@�@��@�-@p�@/@/@V@��@��@�@�D@Z@I�@I�@9X@9X@9X@9X@(�@(�@(�@�@1@�
@��@�@t�@dZ@o@��@~�@^5@M�@=q@=q@-@�@�@��@�7@x�@hs@7L@7L@7L@7L@7L@7L@7L@7L@7L@�@Ĝ@�@Q�@1'@ �@�@�@\)@+@
=@ȴ@��@��@��@��@��@�+@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�5?A�9XA�9XA�7LA�1'A�33A�33A�33A�33A�33A�5?A�5?A�7LA�7LA�9XA�;dA�;dA�=qA�=qA�C�A�G�A�XAݏ\Aݥ�AݶFAݾwAݲ-Aݙ�A݉7A�M�A� �A��HA�M�Aۗ�A�S�A��Aڏ\A�$�A��A�bNA�=qA��A��/A���A��A�`BA˼jA���A�A��A�p�A���A�/A�$�A��A�A���A�hsA���AhA�+A���A�dZA��A���A�%A��yA���A�E�A�O�A��hA��A�r�A�E�A��A���A�;dA�ĜA��A��A�1'A��/A�(�A�bNA�p�A�"�A�A��wA���A�^5A���A�ffA�XA�/A�n�A�A�A��+A�$�A���G�O�G�O�A�p�A�7LA�I�A��A���A���A��A���A���A�"�A�A��A���A�{AS�A~�A}hsA{;dAy�Atr�Aq�mAm��Ak�Ag%Ae�Ac�wAb�yAa��A`ffA`�`AaS�Aa\)Aa&�A`�uA]A[hsAX��AX{AU��AT�AS�ASC�ARI�AO�TAN-AM�AK�^AI��AF�AD$�AB{A@��A?��A>~�A=?}A<��A:�A9hsA8^5A7|�A6bNA5��A5O�A4ĜA3�7A2~�A/"�A,��A*�A)�wA(�yA(VA%�
A$z�A"n�A!p�A�FA\)A1'A&�A��A��At�AoA{A��A��A�^A/A�yA-A�mA�uA=qAt�A�AoA�A
=A�/A�!A��AE�A�AbA
A�A
�A
 �A	��A	%A�FA�+A��A��AA�A9XAE�AƨA  AK�A^5A�hA �HA ~�A �uA r�A �@��;@�l�@��H@���@��-@��`@�`B@�  @�%@�7L@�R@�ȴ@��@��@�\@�j@�x�@�  @߅@�\)@ڗ�@ٲ-@� �@�  @���@��@��
@�ƨ@׮@�ȴ@�r�@�\)@�&�@�o@�5?@ͺ^@̼j@�
=@�^5@ɲ-@ǥ�@š�@�`B@Ĭ@�r�@���@�dZ@�
=@���@�M�@ģ�@�;d@�1'@��;@�ƨ@Ǿw@ǍP@�t�@�t�@Ǖ�@���@�l�@�o@\@�=q@��^@�z�@���@��@��@�l�@���@��@�Ĝ@��H@�E�@�O�@�1'@��R@��@�`B@�hs@��@��@��j@�j@�Z@�Z@� �@�(�@��@��;@��@���@��@�S�@�@�hs@�X@�p�@�hs@���@�r�@�1@���@�E�@�7L@��j@�z�@�bN@�1'@��@��w@��@�n�@�@��T@��^@���@���@�@�ff@�ȴ@��y@���@��@�V@��@��@�t�@�;d@��@���@�V@���@��#@���@�X@�/@��@���@��@�%@�V@�7L@���@��@�?}@�x�@�G�@��@�%@���@�r�@�(�@�  @���@���@��@�"�@��+@�@�G�@��@��/@��@�Z@�9X@�9X@�(�@�1@��@�l�@�K�@�C�@�dZ@�l�@�\)@�"�@���@��\@�$�@��@���@��h@�x�@�O�@�7L@��@��@���@�z�@��
@�\)@�\)@�S�@�C�@�+@��@���@�^5@�E�@�$�@�J@��7@�7L@�%@�%@�%@���@���@��@��/@��D@�Z@���@��P@�@���@�V@�$�@��#@���@�7L@��/@��u@�j@�1@��
@��F@���@�dZ@��@�@��@�ȴ@�^5@�{@���@���@�hs@���@�I�@��@���@��m@�ƨ@���@�t�@�dZ@���@�ff@�-@�{@�$�@���@��T@��#@�@���@��@�Ĝ@��@��P@�"�@��y@��!@�v�@�@���@�@���@�X@�/@�V@��@��@�Z@�  @��F@�t�@�+@���@���@��+@�^5@�V@�$�@�@���@��@�X@�7L@�7L@��@��@�/@��@��@�V@���@��j@���@�Q�@
=@}�@}V@|��@|�@}�@}�@}��@}��@}�@}/@|9X@y�@zJ@z�@y�^@xA�@xA�@x  @w�@vȴ@u?}@t��@t1@s��@s�@sdZ@s33@s33@sC�@st�@t1@t�/@t9X@sƨ@s�
@s�F@s��@sS�@s33@rn�@q�^@qx�@q�@p�9@p  @o+@o
=@nȴ@nff@n{@m�@m/@l�j@lZ@l(�@kt�@j�H@j��@j^5@i��@h�`@hbN@hA�@hb@g��@g��@g|�@f��@f�+@f5?@f{@f{@f@e�T@e�@d�D@d�@cƨ@cS�@b�!@b=q@a��@ax�@ahs@`��@`  @_�@_l�@_+@^ȴ@^��@]��@]/@\��@\�j@\Z@[�m@[ƨ@[�@[33@Z�!@ZJ@Y�#@Yx�@X�9@X�u@XQ�@W��@W�@Wl�@Vȴ@Vv�@V5?@V$�@V{@U�T@U��@U`B@U�@T�/@T��@TZ@TI�@TI�@T9X@S�
@S��@SdZ@SS�@So@R�@R�H@R��@R��@R��@Rn�@R�@RJ@Q��@Q&�@P�9@P�u@P1'@P  @O�@O�@O��@O�P@O\)@O�@N��@N�y@Nȴ@N{@Mp�@Mp�@L�/@L�j@LI�@K�F@J~�@Jn�@JM�@I��@I�#@I�7@Ix�@IX@I7L@HĜ@H�u@HA�@Hb@G�;@G�@G�;@G�w@G�P@G|�@G|�@G|�@G\)@G\)@Gl�@F��@F��@F@E��@E@EO�@D�/@D�j@D��@Dz�@D�@C�m@Cƨ@C�F@Co@B��@B~�@B^5@A��@@��@@Ĝ@@��@@�`@@��@@�`@@�@@bN@@1'@@  @?��@?�P@?�@>ȴ@>��@>v�@>V@>$�@=�T@=��@=��@=@=p�@=/@<��@<��@<j@<Z@<�@;�F@;dZ@;"�@:�!@:J@9��@9��@:J@:J@:J@:J@9��@9��@9hs@9&�@8��@8�9@8Q�@8A�@7�@7
=@6��@6��@6��@6�+@6E�@5O�@4��@41@3��@3�m@3��@3"�@2�@2��@2��@2M�@1��@1�^@1x�@0��@0A�@0b@0b@0  @/��@/l�@/K�@/
=@.�+@.v�@.v�@.V@.5?@.$�@-�T@-�@-?}@,�@,�/@,��@,z�@,(�@,1@+�
@+t�@+33@+@*�H@*��@*~�@*~�@)��@)�^@)G�@(��@(A�@( �@( �@(  @'�;@'��@'��@'�@'
=@&�@&�+@&V@&$�@%�T@%?}@$�@$(�@#�
@#��@#33@"��@"~�@"^5@"-@"J@!�@!��@!G�@!&�@!%@ ��@ bN@ 1'@   @   @   @�@��@+@ȴ@��@��@V@�@�-@�-@�-@��@��@�@�h@p�@V@��@�@�D@I�@9X@(�@�@��@�F@t�@�@��@�\@n�@^5@-@�@�#@��@��@G�@%@��@��@�u@�@Q�@A�@��@�@\)@�y@��@�+@v�@V@$�@�@��@�-@p�@/@/@V@��@��@�@�D@Z@I�@I�@9X@9X@9X@9X@(�@(�@(�@�@1@�
@��@�@t�@dZ@o@��@~�@^5@M�@=q@=q@-@�@�@��@�7@x�@hs@7L@7L@7L@7L@7L@7L@7L@7L@7L@�@Ĝ@�@Q�@1'@ �@�@�@\)@+@
=@ȴ@��@��@��@��@��@�+@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	}�B	}�B	}�B	}�B	~�B	~�B	~�B	� B	� B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�+B	�DB	�hB	��B	�dB	ĜB	��B	�5B	�fB	�B	�yB	�B	�B	�mB	�B
�B
&�B
7LB
)�B	�mB	�5B	�dB
B
�B
33B
D�B
jB
�B
��B
ȴB
��B
��B
��B
��B�B-BD�BJ�BL�BM�BffBs�B|�B�%B�B�oB�dB�B�B�B�sB�B�yB�sB�5B�`B��BDBhB�B�B�B�BB��B�HB��B��B�RB�DBhsB/BB+BB
��B
�yB
�B
ɺB
�B
�3B
��B
�B
�`B
�B
�oB
�/B
��B
�)B
��B
��B
�}B
��B
bNB
)�B
:^B
=qB
<jB
33B
�B	��B	��B	��B	z�B	YB	E�B	&�B	.B	/B	&�B	#�B	�B	6FB	Q�B	\)B	XB	P�B	6FB	$�B	�B	'�B	�B	uB	�B	{B	+B��B�B��B�ZB�B�qB�LB�^B��BƨB�XB�RB�dB��B��B�B�B�B�B�!B��B��B�uBw�Bt�B~�B�DB�=B�1Bq�By�Bs�Bz�Bt�BiyBz�B�B�%B�B�JB�=B�B�bB�VB�JB�\B�uB�=Bz�B�PB��B��B��B�B��B��B��B��B��B��B��B�B�B��B��B��B��B�{B�oB��B��B��B�!B�wB��BǮBɺBȴBÖBŢBŢB��B��B��B�#B�;B�NB�ZB�BB�/B�TB�BŢB�'B�'B��B�B��B��B��B�=B�=Bt�BgmB�VB�bB�PB��B��B��B��B��B��B�VB�B�PB�DB�\B��B��B��B��B��B��B�oB�hB��B��B�B�!B�9B�}BȴB��B�sB	B��B��B	  B	+B	%B	+B	B	B��B��B	DB	DB	JB	
=B	B��B	  B��B��B��B��B��B��B��B��B��B��B��B	B	
=B		7B	bB	oB	�B	�B	 �B	%�B	)�B	-B	0!B	2-B	2-B	1'B	/B	(�B	-B	8RB	:^B	9XB	5?B	33B	2-B	0!B	'�B	%�B	1'B	6FB	9XB	:^B	<jB	@�B	?}B	C�B	K�B	R�B	T�B	W
B	[#B	aHB	e`B	ffB	hsB	gmB	ffB	cTB	ffB	ffB	cTB	s�B	t�B	t�B	t�B	t�B	w�B	x�B	x�B	{�B	}�B	� B	�B	�%B	�7B	�=B	�\B	�JB	�DB	�VB	�bB	�bB	�bB	�\B	�VB	�VB	�hB	�oB	�uB	�uB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�-B	�'B	�'B	�-B	�-B	�FB	�XB	�RB	�^B	�^B	�^B	�^B	�^B	�qB	�dB	�XB	�jB	B	ÖB	ÖB	ÖB	ÖB	ĜB	ƨB	ȴB	ȴB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�B	�
B	�B	�#B	�)B	�#B	�#B	�5B	�5B	�)B	�#B	�/B	�)B	�BB	�5B	�)B	�/B	�NB	�ZB	�ZB	�ZB	�ZB	�`B	�sB	�`B	�ZB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�fB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
	7B
DB
PB
PB
JB
DB

=B
+B
B
DB
PB
JB
JB
\B
hB
bB
VB

=B
PB
VB
bB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
 �B
 �B
 �B
"�B
$�B
$�B
$�B
"�B
 �B
"�B
#�B
%�B
$�B
%�B
&�B
&�B
(�B
(�B
'�B
'�B
+B
+B
+B
+B
+B
+B
+B
-B
-B
,B
-B
.B
.B
-B
-B
-B
/B
.B
.B
0!B
0!B
/B
1'B
0!B
/B
1'B
1'B
2-B
2-B
1'B
1'B
1'B
1'B
2-B
1'B
2-B
33B
33B
33B
2-B
2-B
2-B
33B
33B
49B
49B
5?B
5?B
6FB
5?B
6FB
7LB
6FB
5?B
6FB
7LB
8RB
:^B
;dB
:^B
;dB
<jB
;dB
;dB
;dB
;dB
:^B
8RB
7LB
:^B
8RB
9XB
9XB
6FB
6FB
:^B
;dB
:^B
;dB
;dB
<jB
<jB
<jB
;dB
<jB
<jB
=qB
>wB
?}B
?}B
>wB
?}B
@�B
@�B
@�B
@�B
A�B
A�B
@�B
A�B
A�B
C�B
C�B
B�B
B�B
D�B
D�B
C�B
C�B
D�B
D�B
D�B
B�B
C�B
E�B
E�B
B�B
C�B
D�B
G�B
G�B
H�B
G�B
F�B
G�B
G�B
G�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
H�B
I�B
I�B
I�B
J�B
K�B
J�B
J�B
I�B
J�B
I�B
J�B
L�B
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
O�B
O�B
P�B
O�B
N�B
P�B
R�B
R�B
Q�B
P�B
M�B
O�B
Q�B
S�B
S�B
R�B
R�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
S�B
VB
XB
XB
XB
W
B
XB
YB
XB
XB
ZB
[#B
ZB
ZB
ZB
YB
YB
ZB
ZB
[#B
[#B
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
[#B
\)B
\)B
\)B
\)B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
`BB
`BB
_;B
`BB
`BB
_;B
^5B
_;B
`BB
aHB
aHB
aHB
bNB
bNB
dZB
cTB
cTB
dZB
cTB
cTB
e`B
dZB
dZB
e`B
e`B
ffB
gmB
ffB
ffB
e`B
dZB
e`B
gmB
gmB
ffB
ffB
gmB
iyB
iyB
iyB
iyB
hsB
hsB
hsB
gmB
hsB
iyB
iyB
iyB
jB
jB
iyB
iyB
hsB
iyB
hsB
iyB
jB
jB
k�B
jB
k�B
jB
jB
k�B
k�B
k�B
l�B
k�B
l�B
l�B
l�B
l�B
k�B
l�B
k�B
k�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
n�B
n�B
p�B
p�B
p�B
p�B
o�B
n�B
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
p�B
p�B
p�B
p�B
p�B
q�B
p�B
p�B
p�B
q�B
r�B
r�B
s�B
s�B
s�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
r�B
s�B
s�B
t�B
t�B
s�B
s�B
s�B
t�B
u�B
t�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	}�B	~B	~B	~B	~�B	B	B	� B	� B	� B	�B	�B	� B	�B	�B	�B	�B	�B	�+B	�DB	�hB	��B	�0B	ĜB	��B	�OB	�B	��B	��B	�B	�CB	��B	�B
B
(
B
8�B
-wB	�qB	��B	��B
_B
B
5B
F�B
lWB
��B
�HB
�B
��B
ңB
�$B �B!�B.�BD�BK^BM�BOBg8Bt�B}�B�EB�%B�B��B�iB�-B�B�B�WB�B�B��B�>B�BJB�B�B�B�B�B	B �B�zB�:B�[B��B�vBl"B3hB�B	�B�B
�B
��B
ۦB
��B
�AB
��B
ԕG�O�G�O�B
�xB
��B
ޞB
�$B
�jB
�9B
�uB
B
�B
iyB
0�B
<�B
>�B
=�B
4�B
/B	��B	�TB	��B	~�B	]�B	H�B	,WB	0UB	0�B	($B	%FB	B	5�B	Q�B	\CB	X�B	RTB	9�B	'�B	"�B	)DB	�B	�B	�B	�B	�B��B��B�0B�B��B� B��B��B�-B��B�dB��B�jB�WB��B�wB�UB�qB��B��B��B��B��B|BxB�;B�~B��B��Bt�B{�BvFB|�Bw2Bl�B|jB�aB��B�[B��B�B�mB�B��B��B�.B�,B��B}�B��B�hB��B�TB�B�B�*B�2B�@B� B�dB��B��B�9B� B�B�nB�B�SB�B��B��B�tB�;B��B� B��B��B��BĶB�tB�%B�B�FB�{B�qBߤB�B�B��B��B�B�qB��B��B�B�B��B�_B�B�WB�dB��Bw�Bi�B��B�B�pB��B��B��B��B��B��B�(B��B�<B��B��B�\B�ZB��B��B�VB�dB��B��B�KB�yB�cB��B��B��B��B��B��B	�B��B�6B	 4B	EB	YB	EB	SB	uB�`B��B	DB	�B	�B	
�B	B�BB	 �B��B�qB��B��B��B�B�wB��B��B��B�B	oB	
XB		�B	bB	�B	�B	�B	 �B	&B	)�B	-)B	0UB	2aB	2GB	1[B	/iB	)�B	-wB	8RB	:DB	9�B	5�B	3�B	2�B	0�B	(�B	&�B	1vB	6`B	9rB	:�B	<�B	@�B	@ B	DB	LB	SB	T�B	W$B	[	B	aB	e,B	f2B	h�B	g�B	f�B	c�B	f�B	gRB	d@B	s�B	uB	t�B	uB	uB	w�B	y$B	y	B	|B	~B	�B	�B	�%B	�7B	�#B	�(B	�~B	�xB	�VB	�}B	�}B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�*B	�>B	�*B	�B	�B	�!B	�GB	�GB	�[B	��B	�aB	�|B	�zB	�rB	��B	�xB	�xB	�xB	��B	��B	��B	��B	��B	��B	�uB	ðB	ÖB	ðB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	��B	�B	�B	�B	�.B	�HB	�NB	�@B	�2B	�9B	�9B	�SB	�aB	�MB	�9B	�EB	�YB	�B	�=B	�CB	�WB	�WB	�OB	�jB	�]B	�qB	�dB	�xB	�vB	�jB	ܒB	ݘB	�hB	�tB	�tB	�tB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�	B	�	B	�	B	��B	��B	�	B	�$B	��B	�B	�B	�B
B
 B
B
B
9B
9B
9B
3B
GB
SB
mB
�B
oB
�B
EB
	7B
)B
6B
PB
dB
^B

rB
�B
�B
DB
6B
~B
�B
vB
�B
}B
�B

�B
jB
�B
}B
TB
�B
�B
�B
�B
�B
qB
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
 �B
 �B
 �B
"�B
$�B
$�B
$�B
"�B
!B
#B
$B
%�B
%B
%�B
'B
'B
)B
)B
($B
(>B
+B
+B
+B
+6B
+B
+QB
+6B
-)B
-)B
,=B
-CB
./B
./B
-CB
-CB
-CB
/5B
.IB
.cB
0;B
0;B
/5B
1AB
0;B
/5B
1[B
1AB
2GB
2GB
1AB
1AB
1[B
1AB
2GB
1AB
2GB
33B
33B
3MB
2aB
2GB
2GB
3MB
3MB
4TB
4TB
5ZB
5?B
6FB
5tB
6`B
7fB
6zB
5tB
6`B
7fB
8lB
:xB
;JB
:xB
;dB
<jB
;B
;B
;�B
;B
:�B
8�B
7�B
:xB
8�B
9rB
9rB
6�B
6zB
:^B
;B
:xB
;B
;B
<jB
<jB
<�B
;�B
<�B
<�B
=�B
>�B
?}B
?}B
>�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
@�B
A�B
A�B
C�B
C�B
B�B
B�B
D�B
D�B
C�B
C�B
D�B
D�B
D�B
B�B
C�B
E�B
E�B
B�B
C�B
D�B
G�B
G�B
H�B
G�B
F�B
G�B
G�B
G�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
H�B
I�B
I�B
I�B
J�B
K�B
J�B
J�B
I�B
J�B
I�B
J�B
L�B
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
O�B
O�B
Q B
PB
O(B
P�B
R�B
R�B
RB
QB
N<B
PB
R:B
TB
TB
SB
S&B
UB
UB
UB
UB
UB
VB
U2B
TFB
VB
X+B
XB
X+B
W
B
X+B
Y1B
X+B
XEB
ZB
[#B
Z7B
ZB
Z7B
Y1B
YeB
ZQB
Z7B
[	B
[=B
Z7B
ZQB
[=B
[=B
[WB
\CB
\CB
\CB
\CB
]IB
]IB
[WB
\CB
\]B
\]B
\]B
_VB
_;B
_VB
_VB
_VB
_VB
^jB
`BB
`\B
_VB
`vB
`BB
_VB
^jB
_pB
`vB
aHB
abB
a|B
bhB
bhB
dZB
cnB
cnB
dtB
cnB
cnB
ezB
dZB
dtB
ezB
ezB
f�B
gmB
ffB
f�B
ezB
d�B
e�B
g�B
gmB
f�B
f�B
g�B
i_B
i�B
iyB
iyB
h�B
hsB
h�B
g�B
h�B
iyB
i�B
i�B
jB
jB
i�B
i�B
h�B
i�B
h�B
i�B
jB
j�B
k�B
j�B
k�B
j�B
j�B
k�B
k�B
k�B
l�B
k�B
l�B
l�B
l�B
l�B
k�B
l�B
k�B
k�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
n�B
n�B
p�B
p�B
p�B
p�B
o�B
n�B
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
p�B
p�B
p�B
p�B
p�B
q�B
p�B
p�B
p�B
q�B
r�B
r�B
s�B
s�B
s�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
r�B
s�B
s�B
t�B
t�B
s�B
s�B
s�B
t�B
u�B
t�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<'�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809030036552018090300365520180903003655201809030200242018090302002420180903020024201809040026242018090400262420180904002624  JA  ARFMdecpA19c                                                                20180830063510  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180829213512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180829213515  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180829213516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180829213516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180829213516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180829213516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180829213516  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180829213516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180829213516  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180829213517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180829213517                      G�O�G�O�G�O�                JA  ARUP                                                                        20180829215654                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180830154045  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20180831000000  CF  PSAL_ADJUSTED_QCCN  CN  G�O�                JM  ARSQJMQC2.0                                                                 20180831000000  CF  TEMP_ADJUSTED_QCCN  CN  G�O�                JM  ARCAJMQC2.0                                                                 20180902153655  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180902153655  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180902170024  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180903152624  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231518                      G�O�G�O�G�O�                