CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-15T00:35:13Z creation;2018-03-15T00:35:17Z conversion to V3.1;2019-12-19T07:47:08Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180315003513  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_219                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�SsT2 1   @�St�l @:*�~����df��@�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_y�D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D��3D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @@  @�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_y�D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D��3D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�1'A�33A�1'A�1'A�33A�33A�1'A�33A�5?A�5?A�5?A�1'A�5?A�7LA�+A�1'A�33A�33A�"�A�
=A�%A���A��A��A��/A���A��
A���A���A���A�A���A��-A��A�x�A���A���A�t�A���A���A��yA���A� �A�JA���A��A��
A��A�JA�^5A���A��DA�v�A�jA���A�A�+A�r�A���A���A�Q�A��A���A�?}A�Q�A�S�A�p�A��A���A�7LA��A�  A�~�A�bA��9A�p�AƨA~��A~-A{�wAzbNAx��Av�+ArbAq;dAp�`AoƨAmAl�+Ak��Ak�-AkAjQ�Aj{Ai�Ai��Ahz�Ag"�Ad�HAc;dAb{Aa�PA`5?A_XA^��A^I�A]�A]�7A]?}A\Q�A[G�AZQ�AY%AW��AW�AV�HATVAR��AR��ARQ�AR(�AR  AQ�PAP�RAPbAO��AN�AM�FAL~�AL�AK�wAK��AKdZAKoAJ�jAI��AG��AGVAF�DAFbNAE��ADjACƨACS�AB�AA�^A@��A?�mA>�A>Q�A>{A=��A=��A=x�A=S�A<��A<M�A;��A:��A9�mA9%A8��A7��A7C�A7VA6�A6�A5�^A5��A5&�A41A2��A1�#A/ƨA.M�A-G�A,�9A,^5A,$�A+
=A*1A)�^A)��A)��A)��A(�jA(5?A((�A'��A&�A%p�A$ȴA#�FA#oA"jA!��A!��A!�A   A33A(�A�yA�AdZAĜAjAQ�AI�A�
AbNA �Al�A�A1'A�AXA�`Av�A��Ap�A"�A��AS�A
I�A	%A�\A5?A�mA��AA�wA�^A�FA��A�PA�Al�A�A\)A��A �+@�=q@�t�@��@�V@�^5@�x�@�w@�!@�9@�E�@���@�"�@�$�@�x�@�p�@�G�@�%@�I�@�S�@�-@㝲@��@���@ڧ�@ّh@�G�@��@؋D@���@�=q@պ^@��@�b@��@�/@д9@�z�@��
@��@�x�@��@̬@���@�E�@��@ȣ�@�Q�@�(�@�o@�r�@��@�5?@��T@���@�X@��@���@��j@�A�@���@�{@�`B@��@�=q@�z�@���@��@���@���@��j@���@�(�@��
@�|�@�v�@���@��@��/@��@�Z@�I�@�1'@��
@�;d@���@���@�z�@��;@��H@�G�@�r�@���@�|�@�o@�@�&�@���@�1'@�t�@���@���@�v�@�J@��@���@�r�@�b@���@�\)@�
=@���@�@�V@��/@��u@���@�1'@���@��H@��@��H@��H@���@���@��R@��!@�n�@���@�/@�%@���@��@���@�bN@��
@�33@�^5@��h@��9@�Z@��@���@���@�ȴ@��!@���@�7L@�%@��/@�z�@�Q�@�1'@�9X@�Z@��@���@�E�@��@��@���@�@�/@���@���@��9@��@�(�@�ƨ@�;d@���@���@�@�G�@�G�@�O�@�&�@�%@�1'@�dZ@��@��y@��@��@���@�p�@�?}@���@���@�Ĝ@�Ĝ@��j@���@��u@�9X@��;@��w@��w@��F@��w@�o@�~�@�M�@�E�@�ff@�n�@�^5@�$�@�@��T@���@��-@�G�@���@��9@��D@�Z@�(�@�b@�w@|�@�@~��@~$�@|�/@{t�@{o@{@z��@zM�@yx�@y7L@x�`@xbN@x �@x1'@xb@w�@w�;@w��@w��@w��@w\)@v�@vE�@vV@vE�@u�@u�h@u?}@t�/@t�@sdZ@so@r�H@r=q@qX@q%@o�@o|�@o�@n�y@n5?@m��@m�-@m�h@m`B@mO�@m/@mV@l(�@k��@kC�@k@j��@j�!@j~�@j^5@jM�@j=q@j-@i�@i��@iX@iG�@i7L@i&�@i%@h�9@h1'@g��@g+@fȴ@f�R@f��@f��@f�R@f�R@fv�@ep�@d�@dz�@d9X@d�@cƨ@c33@b��@bM�@b-@a��@a��@a��@a�7@ahs@aG�@`�9@`A�@_��@^��@]��@]�h@]�@]?}@]?}@]/@\��@[�
@[��@[dZ@Z�!@Z�\@Z~�@Z~�@Zn�@Z^5@Z-@Y�@Y��@Yhs@Y�@X�u@XA�@W��@V�R@V��@V�R@V�R@V��@V5?@V@U�-@Up�@U?}@U�@T��@T��@Tj@T(�@S�m@S��@S��@St�@S"�@R��@Rn�@R=q@R�@RJ@Q�#@QG�@QG�@Q%@P�u@P�@P�@PQ�@Pb@O��@O�@N��@M@M�@MV@L��@Lz�@L�@K�m@K"�@I��@I�^@Ihs@H�`@H�9@Hr�@HQ�@H1'@H  @G�w@GK�@G�@G
=@F�@F��@FV@F$�@E�@E@E`B@D��@C�m@C�
@C�F@C��@C33@B�@B�!@B~�@Bn�@A�@@�`@@�u@@r�@@ �@?�@?�;@?l�@?;d@?;d@?�@>�@>��@>�+@>ff@>$�@>$�@>{@=�@=��@<��@<(�@;�m@;��@;"�@;@;@:�@:��@:~�@9��@9�^@9�7@9x�@9X@9G�@9&�@9%@8��@8Ĝ@8��@8A�@7�;@7�w@7+@6ȴ@6E�@5�h@5/@5V@4�@4j@4I�@4(�@4(�@41@3��@3�F@3C�@2�@2~�@2�@1�@1�#@1��@1G�@1�@0��@0Ĝ@0�9@0��@0�@0bN@0 �@/��@/��@/�P@/l�@/�@.��@.�y@.�@.�@.�R@.�+@.V@.$�@.{@.@-�-@-�h@-p�@-?}@,�/@,�@,�@,��@,j@+�F@+�@+S�@+@*�@*�H@*�!@*�\@*~�@*~�@*~�@*n�@*=q@*J@)�@)��@)�7@)x�@)G�@(��@(�u@(b@'l�@';d@&��@&��@&�+@&V@&{@%�@%�@$�/@$Z@#�F@#dZ@#C�@#o@#@"�@"�!@"�\@"~�@"^5@!��@!��@!��@!��@!��@!hs@ ��@ 1'@�@�w@�@��@�P@;d@��@ff@ff@V@E�@5?@{@{@@��@@@��@��@�h@/@V@�@z�@��@�F@t�@S�@�@��@��@=q@��@�@�^@x�@hs@X@%@Ĝ@��@bN@b@�;@�;@��@��@\)@�@�@ȴ@�R@�R@��@��@��@ff@E�@{@@�T@�@`B@`B@O�@�@V@�/@��@Z@(�@�@�@1@1@�F@t�@"�@�H@�H@��@�!@M�@J@�@��@hs@&�@%@�`@��@�@r�@ �@�;@��@�@��@+@�y@�@�+@E�@@@�h@�@?}@�/@�@�D@Z@�@1@1@1@1@�@�@�@�@1@�
@o@
��@
�!@
�\@
M�@
-@
�@
�@	�@	��@	��@	��@	��@	x�@	7L@Ĝ@�@Q�@1'@b@�@��@\)@+@��@��@V@$�@�@@�-@`B@�j@j@9X@(�@(�@��@�m@ƨ@�F@��@��@�@t�@C�@C�@C�@S�@o@�H@�H@��@��@~�@n�@^5@�@�#@�^@��@&�@�@ ��@ Ĝ@ ��@ ��@ �@ r�@ bN11111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�1'A�33A�1'A�1'A�33A�33A�1'A�33A�5?A�5?A�5?A�1'A�5?A�7LA�+A�1'A�33A�33A�"�A�
=A�%A���A��A��A��/A���A��
A���A���A���A�A���A��-A��A�x�A���A���A�t�A���A���A��yA���A� �A�JA���A��A��
A��A�JA�^5A���A��DA�v�A�jA���A�A�+A�r�A���A���A�Q�A��A���A�?}A�Q�A�S�A�p�A��A���A�7LA��A�  A�~�A�bA��9A�p�AƨA~��A~-A{�wAzbNAx��Av�+ArbAq;dAp�`AoƨAmAl�+Ak��Ak�-AkAjQ�Aj{Ai�Ai��Ahz�Ag"�Ad�HAc;dAb{Aa�PA`5?A_XA^��A^I�A]�A]�7A]?}A\Q�A[G�AZQ�AY%AW��AW�AV�HATVAR��AR��ARQ�AR(�AR  AQ�PAP�RAPbAO��AN�AM�FAL~�AL�AK�wAK��AKdZAKoAJ�jAI��AG��AGVAF�DAFbNAE��ADjACƨACS�AB�AA�^A@��A?�mA>�A>Q�A>{A=��A=��A=x�A=S�A<��A<M�A;��A:��A9�mA9%A8��A7��A7C�A7VA6�A6�A5�^A5��A5&�A41A2��A1�#A/ƨA.M�A-G�A,�9A,^5A,$�A+
=A*1A)�^A)��A)��A)��A(�jA(5?A((�A'��A&�A%p�A$ȴA#�FA#oA"jA!��A!��A!�A   A33A(�A�yA�AdZAĜAjAQ�AI�A�
AbNA �Al�A�A1'A�AXA�`Av�A��Ap�A"�A��AS�A
I�A	%A�\A5?A�mA��AA�wA�^A�FA��A�PA�Al�A�A\)A��A �+@�=q@�t�@��@�V@�^5@�x�@�w@�!@�9@�E�@���@�"�@�$�@�x�@�p�@�G�@�%@�I�@�S�@�-@㝲@��@���@ڧ�@ّh@�G�@��@؋D@���@�=q@պ^@��@�b@��@�/@д9@�z�@��
@��@�x�@��@̬@���@�E�@��@ȣ�@�Q�@�(�@�o@�r�@��@�5?@��T@���@�X@��@���@��j@�A�@���@�{@�`B@��@�=q@�z�@���@��@���@���@��j@���@�(�@��
@�|�@�v�@���@��@��/@��@�Z@�I�@�1'@��
@�;d@���@���@�z�@��;@��H@�G�@�r�@���@�|�@�o@�@�&�@���@�1'@�t�@���@���@�v�@�J@��@���@�r�@�b@���@�\)@�
=@���@�@�V@��/@��u@���@�1'@���@��H@��@��H@��H@���@���@��R@��!@�n�@���@�/@�%@���@��@���@�bN@��
@�33@�^5@��h@��9@�Z@��@���@���@�ȴ@��!@���@�7L@�%@��/@�z�@�Q�@�1'@�9X@�Z@��@���@�E�@��@��@���@�@�/@���@���@��9@��@�(�@�ƨ@�;d@���@���@�@�G�@�G�@�O�@�&�@�%@�1'@�dZ@��@��y@��@��@���@�p�@�?}@���@���@�Ĝ@�Ĝ@��j@���@��u@�9X@��;@��w@��w@��F@��w@�o@�~�@�M�@�E�@�ff@�n�@�^5@�$�@�@��T@���@��-@�G�@���@��9@��D@�Z@�(�@�b@�w@|�@�@~��@~$�@|�/@{t�@{o@{@z��@zM�@yx�@y7L@x�`@xbN@x �@x1'@xb@w�@w�;@w��@w��@w��@w\)@v�@vE�@vV@vE�@u�@u�h@u?}@t�/@t�@sdZ@so@r�H@r=q@qX@q%@o�@o|�@o�@n�y@n5?@m��@m�-@m�h@m`B@mO�@m/@mV@l(�@k��@kC�@k@j��@j�!@j~�@j^5@jM�@j=q@j-@i�@i��@iX@iG�@i7L@i&�@i%@h�9@h1'@g��@g+@fȴ@f�R@f��@f��@f�R@f�R@fv�@ep�@d�@dz�@d9X@d�@cƨ@c33@b��@bM�@b-@a��@a��@a��@a�7@ahs@aG�@`�9@`A�@_��@^��@]��@]�h@]�@]?}@]?}@]/@\��@[�
@[��@[dZ@Z�!@Z�\@Z~�@Z~�@Zn�@Z^5@Z-@Y�@Y��@Yhs@Y�@X�u@XA�@W��@V�R@V��@V�R@V�R@V��@V5?@V@U�-@Up�@U?}@U�@T��@T��@Tj@T(�@S�m@S��@S��@St�@S"�@R��@Rn�@R=q@R�@RJ@Q�#@QG�@QG�@Q%@P�u@P�@P�@PQ�@Pb@O��@O�@N��@M@M�@MV@L��@Lz�@L�@K�m@K"�@I��@I�^@Ihs@H�`@H�9@Hr�@HQ�@H1'@H  @G�w@GK�@G�@G
=@F�@F��@FV@F$�@E�@E@E`B@D��@C�m@C�
@C�F@C��@C33@B�@B�!@B~�@Bn�@A�@@�`@@�u@@r�@@ �@?�@?�;@?l�@?;d@?;d@?�@>�@>��@>�+@>ff@>$�@>$�@>{@=�@=��@<��@<(�@;�m@;��@;"�@;@;@:�@:��@:~�@9��@9�^@9�7@9x�@9X@9G�@9&�@9%@8��@8Ĝ@8��@8A�@7�;@7�w@7+@6ȴ@6E�@5�h@5/@5V@4�@4j@4I�@4(�@4(�@41@3��@3�F@3C�@2�@2~�@2�@1�@1�#@1��@1G�@1�@0��@0Ĝ@0�9@0��@0�@0bN@0 �@/��@/��@/�P@/l�@/�@.��@.�y@.�@.�@.�R@.�+@.V@.$�@.{@.@-�-@-�h@-p�@-?}@,�/@,�@,�@,��@,j@+�F@+�@+S�@+@*�@*�H@*�!@*�\@*~�@*~�@*~�@*n�@*=q@*J@)�@)��@)�7@)x�@)G�@(��@(�u@(b@'l�@';d@&��@&��@&�+@&V@&{@%�@%�@$�/@$Z@#�F@#dZ@#C�@#o@#@"�@"�!@"�\@"~�@"^5@!��@!��@!��@!��@!��@!hs@ ��@ 1'@�@�w@�@��@�P@;d@��@ff@ff@V@E�@5?@{@{@@��@@@��@��@�h@/@V@�@z�@��@�F@t�@S�@�@��@��@=q@��@�@�^@x�@hs@X@%@Ĝ@��@bN@b@�;@�;@��@��@\)@�@�@ȴ@�R@�R@��@��@��@ff@E�@{@@�T@�@`B@`B@O�@�@V@�/@��@Z@(�@�@�@1@1@�F@t�@"�@�H@�H@��@�!@M�@J@�@��@hs@&�@%@�`@��@�@r�@ �@�;@��@�@��@+@�y@�@�+@E�@@@�h@�@?}@�/@�@�D@Z@�@1@1@1@1@�@�@�@�@1@�
@o@
��@
�!@
�\@
M�@
-@
�@
�@	�@	��@	��@	��@	��@	x�@	7L@Ĝ@�@Q�@1'@b@�@��@\)@+@��@��@V@$�@�@@�-@`B@�j@j@9X@(�@(�@��@�m@ƨ@�F@��@��@�@t�@C�@C�@C�@S�@o@�H@�H@��@��@~�@n�@^5@�@�#@�^@��@&�@�@ ��@ Ĝ@ ��@ ��@ �@ r�@ bN11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B��B��B�B�B�B�B�B�B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�BhsB/B�B.B2-B0!B1'BXBR�BQ�BZBVBL�B8RB+B�RB�-B��Bq�B<jBD�BI�B?}B/B(�B�BuB{B%B
�B
�NB
�
B
��B
��B
��B
ȴB
��B
�B
��B
�=B
�\B
�DB
k�B
m�B
aHB
_;B
>wB
6FB
'�B
DB	�5B
B
B	�B	�`B	�;B	�B	�yB	�NB	�5B	�BB	�5B	��B	��B	�B	��B	��B	��B	��B	�PB	�\B	�uB	�PB	�PB	�=B	�B	x�B	p�B	iyB	e`B	]/B	bNB	S�B	7LB	A�B	Q�B	Q�B	P�B	M�B	E�B	9XB	9XB	7LB	.B	$�B	!�B	0!B	0!B	2-B	,B	%�B	�B	\B��B	hB	bB	bB	%B��B	  B	B��B�B�B�B�B�B��B��B��B��B��B�B�B�B�)B�5B�B�TB�B�/B�HB�BB�#B��B��B��B�dB�-B�!B��B��B��B��B�B�B��B��B��B�B�B��B��B��B��B��B�PB~�B�JB�B�+B�+B�1B�%B~�Bq�Br�Bk�Be`Bk�Bo�Bn�Br�Bs�Bo�BbNBO�B<jBS�BVBL�B<jB@�BO�BL�BG�BG�BD�B6FB"�B	7B&�B:^B@�B@�BD�BF�BF�BE�BC�BA�B>wB9XB0!B�B��B��B�sB+B1B{B�B
=B�BbBbBPBBbBbB�B�B$�B!�B�B�BoBPB  B��BB  BoB�B�B�BoB�B�B�B�B�BhB�B�B�BPB�B�B�B\B�B�B�B�B�BbBB�B"�B,B.B.B/B/B-B&�B �B"�B#�B�B �B�B.B-B,B49B<jB=qB:^B:^B;dB7LB;dB@�BD�BE�BG�BH�BF�BB�B>wB8RBC�BI�BG�BC�BA�BM�BS�BS�BT�BO�BYB_;BbNBcTBjBm�Bo�Bm�BjBq�Bv�Bv�Bx�B{�B|�B~�B}�B}�B�1B�1B�7B�%B}�B�PB�VB�\B�VB�PB�VB�VB�PB�DB�7B�PB�uB��B��B�{B�bB�bB�\B�VB�oB��B��B��B��B�B�B�B��B�B�XB�qB�jB�}B��BBÖB�}B�qB�jBĜB��B�B��B��B��B�B�B�B�B�B�B�)B�HB�HB�HB�B�B�B�B�B�B��B��B	  B��B	B		7B	DB	PB	uB	�B	�B	�B	�B	�B	�B	 �B	%�B	(�B	'�B	&�B	"�B	'�B	/B	2-B	49B	49B	33B	5?B	;dB	<jB	<jB	<jB	<jB	@�B	D�B	G�B	H�B	J�B	N�B	O�B	Q�B	Q�B	Q�B	Q�B	Q�B	S�B	_;B	aHB	`BB	aHB	bNB	hsB	iyB	jB	n�B	p�B	p�B	q�B	q�B	r�B	t�B	u�B	v�B	v�B	y�B	�B	�B	�B	�%B	�1B	�7B	�7B	�JB	�bB	�hB	�bB	�bB	��B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�?B	�LB	�jB	�wB	�}B	�}B	�}B	�wB	�}B	B	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�NB	�TB	�TB	�ZB	�TB	�HB	�BB	�`B	�fB	�`B	�yB	�B	�B	�B	�yB	�yB	�yB	�yB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B
B
B
  B
  B
+B
1B
1B

=B

=B
DB
DB
DB
DB
DB
PB
VB
PB
PB
PB
VB
\B
\B
VB
VB
\B
{B
uB
uB
oB
uB
{B
{B
�B
oB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
"�B
"�B
"�B
!�B
!�B
 �B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
%�B
$�B
%�B
&�B
$�B
%�B
%�B
&�B
(�B
+B
+B
)�B
-B
-B
.B
-B
-B
,B
+B
,B
,B
.B
/B
0!B
/B
/B
0!B
1'B
2-B
2-B
1'B
2-B
1'B
1'B
1'B
33B
33B
33B
2-B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
33B
5?B
5?B
5?B
5?B
6FB
7LB
6FB
5?B
33B
7LB
8RB
7LB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
9XB
:^B
:^B
;dB
:^B
;dB
:^B
:^B
9XB
:^B
9XB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
<jB
<jB
>wB
>wB
A�B
C�B
C�B
D�B
C�B
C�B
D�B
D�B
C�B
B�B
F�B
F�B
E�B
D�B
D�B
B�B
C�B
F�B
F�B
G�B
G�B
G�B
F�B
D�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
I�B
H�B
G�B
I�B
I�B
H�B
G�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
M�B
N�B
M�B
M�B
O�B
O�B
N�B
N�B
O�B
O�B
O�B
P�B
Q�B
Q�B
O�B
P�B
P�B
Q�B
R�B
R�B
S�B
S�B
S�B
R�B
R�B
R�B
R�B
S�B
S�B
R�B
T�B
VB
T�B
T�B
T�B
T�B
S�B
T�B
VB
W
B
W
B
W
B
VB
T�B
VB
VB
W
B
YB
XB
W
B
W
B
W
B
XB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
ZB
ZB
\)B
\)B
\)B
ZB
\)B
]/B
\)B
\)B
]/B
]/B
^5B
_;B
^5B
^5B
_;B
`BB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
aHB
_;B
]/B
aHB
bNB
cTB
bNB
dZB
dZB
dZB
cTB
dZB
dZB
dZB
dZB
cTB
cTB
bNB
dZB
dZB
e`B
e`B
e`B
dZB
e`B
ffB
e`B
e`B
ffB
gmB
gmB
hsB
hsB
gmB
ffB
iyB
jB
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
m�B
m�B
m�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
m�B
m�B
m�B
m�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�11111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B��B��B�B�B�B�B�B�B��B��B�B��B�B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��Bk6B4�BB/�B33B1[B2-BX�BS�BR�BZkBV�BNG�O�G�O�B�PB��B��Bu�BB�BF�BK^BA B1B*�B~B�B�BKB
�?B
�`B
�eB
�,B
̳B
��B
ɠB
��B
��B
�B
��B
�}B
��B
ncB
n�B
b�B
`BB
A;B
8B
*B
�B	�hB
�B
�B	�nB	�B	�B	�B	��B	�:B	��B	��B	ބB	՛B	�aB	�B	�ZB	��B	�B	��B	��B	�bB	��B	�"B	��B	��B	��B	z*B	q�B	j�B	f�B	^�B	cB	U�B	:^B	CB	R:B	RTB	Q4B	N"B	FYB	:xB	:*B	8B	/5B	&�B	# B	0�B	0�B	2aB	,qB	&�B	 vB	�B�qB	 B	B	�B	EB�XB	 �B	�B��B��B��B��B�B�|B�6B�DB�6B�0B�$B�TB�B�"B��B�pB�=B�B�eB��B�B��BۦB�B�gB͹B�B��B��B�#B��B�B��B��B��B�;B��B�DB�CB�)B�>B��B�KB�B�yB��B��B�jB��B�B�B��B��B�BsMBs�BmBgBl�BpoBoiBs3Bs�Bo�BcnBRB?HBT�BV�BN"B>�BBBPbBM�BH�BHKBE9B7fB%B�B(>B:�BAB@�BD�BF�BF�BE�BC�BA�B>�B9�B0�B�B�B��B�B�B	�B�BeB�B$B�BNB�B�BNB�B#BB$�B"B!B1B@BVB�B�B�B�B�B�B�B1BuB#B5BIBsB_B�BBB?B�B�BB1B�B$BSBB/B	BhB�BxB#nB,=B.cB.cB/OB/OB-]B'mB!�B#�B$�BB!�B!B.cB-�B,�B4�B<�B=�B:�B:�B;�B8B<B@�BD�BE�BG�BH�BF�BB�B?B9�BD3BJ#BHKBD�BB�BNpBTaBT{BU�BP�BY�B_�Bb�Bc�Bj�Bm�Bo�Bm�BkBrBv�Bw2By>B|6B}<B.B~�B~�B�fB�fB�RB��B~�B�jB�pB�\B�pB�jB�pB�pB��B��B��B��B��B��B��B��B��B��B��B�B�B�$B�!B�VB�RB�/B�5B�]B��B��B��B��B��B��B��BªBðB��B�B�VB��B��B��B� B�BB�.B�B�QB�QB�KB�BٚBܒB�B��B��B�}B�B��B��B�[B�GB�*B�B	 OB��B	[B		lB	xB	�B	�B	�B	�B	�B	�B	�B	�B	!B	%�B	(�B	(
B	'B	#nB	(XB	/5B	2GB	49B	49B	3hB	5tB	;B	<�B	<�B	<�B	<�B	@�B	D�B	G�B	H�B	J�B	N�B	P.B	RB	R:B	R B	R:B	RoB	T�B	_VB	abB	`�B	a|B	b�B	h�B	i�B	j�B	n�B	p�B	p�B	q�B	q�B	r�B	t�B	u�B	v�B	v�B	zB	�B	�3B	�GB	�YB	�fB	�lB	�lB	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�
B	�B	�*B	�B	�B	�8B	�=B	�AB	�MB	�ZB	�fB	��B	��B	�}B	��B	�}B	��B	��B	ªB	ĶB	ĶB	ĶB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�B	�B	�B	�B	�&B	�:B	�2B	�2B	�B	�KB	�1B	�7B	�7B	�7B	�B	�EB	�KB	�QB	�eB	چB	�hB	�nB	�nB	�ZB	�TB	�|B	�B	�zB	�B	�B	�yB	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�6B	�BB
B
'B
 4B
;B
'B
 OB
 iB
EB
KB
fB

XB

XB
^B
^B
^B
^B
xB
jB
VB
jB
PB
�B
VB
vB
vB
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
"�B
"�B
"�B
!�B
!�B
 �B
"�B
#�B
$�B
%B
$�B
$�B
%�B
%�B
&�B
%�B
%B
%�B
'B
%B
%�B
&B
'B
(�B
+B
+B
*0B
-B
-)B
.B
-)B
-)B
,B
+QB
,"B
,"B
./B
/5B
0!B
/5B
/5B
0;B
1AB
2-B
2B
1AB
2-B
1AB
1AB
1AB
3MB
33B
3MB
2GB
49B
4TB
49B
49B
4TB
4TB
4TB
4TB
5?B
5?B
3hB
5ZB
5ZB
5tB
5tB
6`B
7LB
6`B
5ZB
3�B
7fB
8lB
7fB
9XB
9rB
9rB
:�B
:^B
:^B
:^B
:^B
9rB
:xB
:xB
;B
:xB
;dB
:�B
:�B
9�B
:�B
9�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
<�B
<�B
>�B
>�B
A�B
C�B
C�B
D�B
C�B
C�B
D�B
D�B
C�B
B�B
F�B
F�B
E�B
D�B
D�B
B�B
C�B
F�B
F�B
G�B
G�B
G�B
F�B
D�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
I�B
H�B
G�B
I�B
I�B
H�B
G�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
M�B
N�B
M�B
M�B
O�B
O�B
N�B
N�B
PB
O�B
PB
Q B
RB
Q�B
O�B
Q B
Q B
RB
R�B
R�B
S�B
S�B
S�B
SB
S&B
S&B
SB
TB
TB
S@B
T�B
VB
UB
UB
UB
U2B
TB
UB
VB
W$B
W
B
W
B
VB
UB
VB
VB
W$B
YB
X+B
W$B
W$B
W$B
X+B
Y1B
Y1B
Y1B
Z7B
Z7B
ZB
[=B
[=B
Z7B
ZQB
\)B
\]B
\CB
ZQB
\CB
]/B
\]B
\CB
]IB
]IB
^OB
_VB
^OB
^OB
_VB
`\B
`\B
`\B
aHB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
abB
_VB
]~B
abB
bhB
c�B
bhB
dZB
dZB
dZB
cnB
dZB
dtB
dtB
dZB
cnB
cnB
b�B
dtB
dtB
ezB
ezB
e�B
dtB
ezB
f�B
ezB
ezB
f�B
g�B
gmB
h�B
h�B
g�B
f�B
i�B
jB
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
m�B
m�B
m�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
m�B
m�B
m�B
m�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�11111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803190035262018031900352620180319003526201806221239022018062212390220180622123902201804050436082018040504360820180405043608  JA  ARFMdecpA19c                                                                20180315093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180315003513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180315003516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180315003516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180315003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180315003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180315003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180315003517  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180315003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180315003517  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180315003517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180315003517                      G�O�G�O�G�O�                JA  ARUP                                                                        20180315005544                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180315153256  CV  JULD            G�O�G�O�F�                JM  ARSQJMQC2.0                                                                 20180316000000  CF  TEMP_ADJUSTED_QCB�  B�  G�O�                JM  ARCAJMQC2.0                                                                 20180318153526  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180318153526  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193608  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033902  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                