CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-13T00:35:18Z creation;2016-08-13T00:35:20Z conversion to V3.1;2019-12-19T08:29:28Z update;     
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
resolution        =���   axis      Z        h  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  sh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160813003518  20200116201516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_027                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�¶��k�1   @�·I���@4��m\��d�����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�C3DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D���D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�C3DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D���D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��/A��A��
A��
A��
A��
A��
A��A��A��A��#A��#A��#A��#A��#A��/A��/A��/A��/A��A��A���A�PAߡ�Aޡ�A�bNA�ĜA�7LAܴ9AۓuAٸRA���A׍PA���A�-A��A�ZA�t�A��TAЇ+A���A�n�A�  A���A�33A�C�A�I�A���A�jA���A�VA��
A�M�A�5?A��wA�1'A�M�A�O�A���A��A�^5A�A���A���A���A��A��A��;A��A�{A� �A�\)A� �A�^5A�z�A�|�A�x�A��A�C�A�hsA�bNA��
A��^A��\A�ZA�JA��jA�/A�ĜA�C�A��jA���A��wA�G�A��A�ƨA�n�A�bNA� �A�oA�ZA�-A�ZA�|�A��#A��FA���A��mA�x�A��A�jA�dZA��-A��RA��`A{�;Ax��Aw��AvbNAt�jAr�Ao�Ak�Ak/Aj�9Ag�
Af�Ac�Ab�A`�A^�uA]XA\v�A[hsAZAY��AYƨAXAV�RAU�;AS�^AR�`APjAL-AJ��AI�AFȴAEAC�#AB�AB�A@��A=S�A<�RA<E�A;�FA9O�A733A6z�A5��A5/A41'A3A1�mA/�A.�\A-�wA+��A*Q�A)oA'ƨA&��A&VA%VA#�#A#A"E�A!t�A �A A�A -A�mA��A�FAC�A��Ap�A �A��A1A33A�`A��A5?AƨA"�A(�A(�AC�AbNA;dA1'A��AXAoA�`AVA��A
��A
(�A	�
A	��A	�A	`BA��A-A�yA(�A"�AAdZA �@��7@��!@�@��h@��@�r�@��P@�%@�bN@�-@��@�`B@�F@���@�Q�@�ȴ@��@��@�j@�%@�p�@�1@��@��@���@�"�@�^@�9X@���@�-@��/@�(�@�5?@�&�@أ�@׾w@��@Ցh@��@ԓu@�C�@ҸR@�J@� �@���@θR@�v�@�-@�{@�{@�@��`@˶F@ˍP@ˍP@ʗ�@��#@ɲ-@��@��@�`B@��;@�"�@��H@�@�\)@�o@�5?@�`B@�V@�b@�p�@���@���@���@��T@��T@��`@�bN@�A�@�(�@�9X@�1@�v�@��@���@�G�@���@�p�@��/@�I�@��P@���@�ȴ@��R@��\@�V@���@�@��/@�9X@�l�@��y@�$�@��#@��#@���@���@��#@���@��@���@�r�@�b@� �@���@�"�@�5?@��@�@��^@��@�n�@���@�/@���@�S�@��F@�ƨ@�j@�33@��@��^@��@�-@���@��@�/@���@�&�@�z�@��@�l�@�o@���@�`B@�V@���@�p�@�I�@��@�;d@��R@�
=@�33@�33@��R@���@�G�@��@��@���@��@�Z@�(�@�A�@�z�@�A�@�b@��;@���@�|�@��@�|�@�@�{@���@��@���@��7@�p�@�hs@�`B@�X@�7L@��@�ƨ@���@�-@�J@��@��T@��#@��7@�O�@��@���@���@�z�@�Z@�9X@��@��;@�l�@�33@��y@���@���@���@�n�@�$�@��@�@���@�hs@�7L@��/@�bN@�  @��@�dZ@��y@���@��+@�v�@�V@��@��-@�V@�j@�  @��@��F@���@��F@���@�dZ@��H@���@���@�^5@�5?@���@���@���@�hs@�`B@��@��@�Ĝ@���@��@�1'@�ƨ@��@�|�@�;d@�o@��H@��R@�=q@��@��@��-@��7@�&�@��9@�Q�@��@�;d@��R@�v�@��T@�x�@�&�@���@�j@�1@��m@��
@��F@��F@��P@�\)@�"�@���@��\@�V@�@��-@��7@��7@��7@�p�@�&�@���@�Ĝ@��@�j@�9X@��w@��@�l�@�"�@��y@���@�=q@���@���@��h@�X@��@���@��`@��u@�9X@�b@�  @��@l�@~�@~ff@~{@}p�@|��@|z�@|I�@|9X@|(�@{��@{33@z�!@z^5@z�@y��@y%@x�`@x��@x�@x �@wl�@w+@v�y@vff@u�@uV@tz�@tI�@t(�@sƨ@s��@s�@sdZ@r��@r-@qx�@p��@pbN@o��@ol�@o\)@oK�@n�y@n��@nV@m�T@m@m/@l��@l(�@kt�@jM�@i�@i�#@i�7@i%@hĜ@hr�@h �@hb@g�;@g�@f�R@f��@f5?@e�@e@ep�@d�D@d1@c��@b�H@bJ@a�^@a7L@`Ĝ@`�@`r�@`r�@`bN@`Q�@`b@_�w@_|�@_\)@_;d@^�@^V@^5?@^{@]��@\��@\�j@\�D@\I�@\1@[��@[��@[C�@["�@[o@Z�\@Y��@Y�7@Y&�@XĜ@X�u@XbN@Xb@X  @W�@W\)@V��@V$�@U�-@UV@T��@Tj@TZ@TZ@T9X@T(�@T(�@S��@S��@S�@SS�@R��@R~�@R^5@RM�@RM�@R=q@R=q@R-@Q�^@Q�7@Qhs@Q&�@Q�@P��@P�`@P��@Pr�@O�@O�P@N�y@N��@N�+@Nff@M�-@M/@MV@L��@L�@K�
@K��@K�@Kt�@KS�@KC�@K33@J�@JM�@I��@Ix�@H�`@H�9@H �@G|�@F�y@F��@FE�@F{@E��@E@E�h@EO�@E/@D�@D��@DZ@D1@C�
@Cƨ@C��@C@A��@Ax�@AG�@A�@A%@@Ĝ@@�@@bN@@A�@@b@?�@?��@?�P@?l�@?
=@>�y@>5?@<��@<�j@<�D@<(�@;t�@;"�@;@:�!@:^5@:J@9��@9�7@9G�@8�`@8��@8�@8Q�@8A�@81'@8  @7�w@7|�@6��@6��@6��@6��@6�+@6E�@5�@5�T@5�-@4�@4�j@4��@4j@49X@41@3�@2�H@2�@1��@1��@1X@17L@17L@17L@1%@0Ĝ@0��@0�u@0�@0r�@0bN@0b@/�w@/�P@/|�@/l�@/
=@.�y@.��@.v�@.ff@.V@.E�@.E�@.5?@-�T@-`B@-?}@-/@-V@,�/@,9X@+�
@+�F@+�@+33@+o@*�@*�@*�\@*M�@)��@)��@)��@)hs@)G�@)G�@)7L@(��@(�@(b@'\)@'+@&�@&��@&�+@&v�@&V@&{@%�T@%��@%�@$�@$��@$z�@$z�@$I�@$1@#t�@#S�@#C�@#33@"�!@"=q@!�#@!�#@!�^@!&�@!%@ �`@ �u@ A�@ 1'@ 1'@��@�P@�@�+@v�@v�@v�@v�@V@E�@�T@�@�@j@1@�F@t�@C�@33@o@��@�\@J@��@��@��@X@&�@&�@�@��@�9@r�@Q�@1'@�@��@�w@��@K�@��@��@��@v�@V@$�@{@�@�-@�@`B@`B@O�@?}@�@��@�j@Z@I�@�@1@��@�F@S�@o@o@o@@@@��@^5@M�@-@�#@��@�7@&�@Ĝ@�9@�9@��@�@r�@r�@bN@Q�@1'@ �@  @�@�P@|�@+@�@��@V@$�@{@�@��@@p�@V@��@�@�@�@��@�j@�@��@z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��/A��A��
A��
A��
A��
A��
A��A��A��A��#A��#A��#A��#A��#A��/A��/A��/A��/A��A��A���A�PAߡ�Aޡ�A�bNA�ĜA�7LAܴ9AۓuAٸRA���A׍PA���A�-A��A�ZA�t�A��TAЇ+A���A�n�A�  A���A�33A�C�A�I�A���A�jA���A�VA��
A�M�A�5?A��wA�1'A�M�A�O�A���A��A�^5A�A���A���A���A��A��A��;A��A�{A� �A�\)A� �A�^5A�z�A�|�A�x�A��A�C�A�hsA�bNA��
A��^A��\A�ZA�JA��jA�/A�ĜA�C�A��jA���A��wA�G�A��A�ƨA�n�A�bNA� �A�oA�ZA�-A�ZA�|�A��#A��FA���A��mA�x�A��A�jA�dZA��-A��RA��`A{�;Ax��Aw��AvbNAt�jAr�Ao�Ak�Ak/Aj�9Ag�
Af�Ac�Ab�A`�A^�uA]XA\v�A[hsAZAY��AYƨAXAV�RAU�;AS�^AR�`APjAL-AJ��AI�AFȴAEAC�#AB�AB�A@��A=S�A<�RA<E�A;�FA9O�A733A6z�A5��A5/A41'A3A1�mA/�A.�\A-�wA+��A*Q�A)oA'ƨA&��A&VA%VA#�#A#A"E�A!t�A �A A�A -A�mA��A�FAC�A��Ap�A �A��A1A33A�`A��A5?AƨA"�A(�A(�AC�AbNA;dA1'A��AXAoA�`AVA��A
��A
(�A	�
A	��A	�A	`BA��A-A�yA(�A"�AAdZA �@��7@��!@�@��h@��@�r�@��P@�%@�bN@�-@��@�`B@�F@���@�Q�@�ȴ@��@��@�j@�%@�p�@�1@��@��@���@�"�@�^@�9X@���@�-@��/@�(�@�5?@�&�@أ�@׾w@��@Ցh@��@ԓu@�C�@ҸR@�J@� �@���@θR@�v�@�-@�{@�{@�@��`@˶F@ˍP@ˍP@ʗ�@��#@ɲ-@��@��@�`B@��;@�"�@��H@�@�\)@�o@�5?@�`B@�V@�b@�p�@���@���@���@��T@��T@��`@�bN@�A�@�(�@�9X@�1@�v�@��@���@�G�@���@�p�@��/@�I�@��P@���@�ȴ@��R@��\@�V@���@�@��/@�9X@�l�@��y@�$�@��#@��#@���@���@��#@���@��@���@�r�@�b@� �@���@�"�@�5?@��@�@��^@��@�n�@���@�/@���@�S�@��F@�ƨ@�j@�33@��@��^@��@�-@���@��@�/@���@�&�@�z�@��@�l�@�o@���@�`B@�V@���@�p�@�I�@��@�;d@��R@�
=@�33@�33@��R@���@�G�@��@��@���@��@�Z@�(�@�A�@�z�@�A�@�b@��;@���@�|�@��@�|�@�@�{@���@��@���@��7@�p�@�hs@�`B@�X@�7L@��@�ƨ@���@�-@�J@��@��T@��#@��7@�O�@��@���@���@�z�@�Z@�9X@��@��;@�l�@�33@��y@���@���@���@�n�@�$�@��@�@���@�hs@�7L@��/@�bN@�  @��@�dZ@��y@���@��+@�v�@�V@��@��-@�V@�j@�  @��@��F@���@��F@���@�dZ@��H@���@���@�^5@�5?@���@���@���@�hs@�`B@��@��@�Ĝ@���@��@�1'@�ƨ@��@�|�@�;d@�o@��H@��R@�=q@��@��@��-@��7@�&�@��9@�Q�@��@�;d@��R@�v�@��T@�x�@�&�@���@�j@�1@��m@��
@��F@��F@��P@�\)@�"�@���@��\@�V@�@��-@��7@��7@��7@�p�@�&�@���@�Ĝ@��@�j@�9X@��w@��@�l�@�"�@��y@���@�=q@���@���@��h@�X@��@���@��`@��u@�9X@�b@�  @��@l�@~�@~ff@~{@}p�@|��@|z�@|I�@|9X@|(�@{��@{33@z�!@z^5@z�@y��@y%@x�`@x��@x�@x �@wl�@w+@v�y@vff@u�@uV@tz�@tI�@t(�@sƨ@s��@s�@sdZ@r��@r-@qx�@p��@pbN@o��@ol�@o\)@oK�@n�y@n��@nV@m�T@m@m/@l��@l(�@kt�@jM�@i�@i�#@i�7@i%@hĜ@hr�@h �@hb@g�;@g�@f�R@f��@f5?@e�@e@ep�@d�D@d1@c��@b�H@bJ@a�^@a7L@`Ĝ@`�@`r�@`r�@`bN@`Q�@`b@_�w@_|�@_\)@_;d@^�@^V@^5?@^{@]��@\��@\�j@\�D@\I�@\1@[��@[��@[C�@["�@[o@Z�\@Y��@Y�7@Y&�@XĜ@X�u@XbN@Xb@X  @W�@W\)@V��@V$�@U�-@UV@T��@Tj@TZ@TZ@T9X@T(�@T(�@S��@S��@S�@SS�@R��@R~�@R^5@RM�@RM�@R=q@R=q@R-@Q�^@Q�7@Qhs@Q&�@Q�@P��@P�`@P��@Pr�@O�@O�P@N�y@N��@N�+@Nff@M�-@M/@MV@L��@L�@K�
@K��@K�@Kt�@KS�@KC�@K33@J�@JM�@I��@Ix�@H�`@H�9@H �@G|�@F�y@F��@FE�@F{@E��@E@E�h@EO�@E/@D�@D��@DZ@D1@C�
@Cƨ@C��@C@A��@Ax�@AG�@A�@A%@@Ĝ@@�@@bN@@A�@@b@?�@?��@?�P@?l�@?
=@>�y@>5?@<��@<�j@<�D@<(�@;t�@;"�@;@:�!@:^5@:J@9��@9�7@9G�@8�`@8��@8�@8Q�@8A�@81'@8  @7�w@7|�@6��@6��@6��@6��@6�+@6E�@5�@5�T@5�-@4�@4�j@4��@4j@49X@41@3�@2�H@2�@1��@1��@1X@17L@17L@17L@1%@0Ĝ@0��@0�u@0�@0r�@0bN@0b@/�w@/�P@/|�@/l�@/
=@.�y@.��@.v�@.ff@.V@.E�@.E�@.5?@-�T@-`B@-?}@-/@-V@,�/@,9X@+�
@+�F@+�@+33@+o@*�@*�@*�\@*M�@)��@)��@)��@)hs@)G�@)G�@)7L@(��@(�@(b@'\)@'+@&�@&��@&�+@&v�@&V@&{@%�T@%��@%�@$�@$��@$z�@$z�@$I�@$1@#t�@#S�@#C�@#33@"�!@"=q@!�#@!�#@!�^@!&�@!%@ �`@ �u@ A�@ 1'@ 1'@��@�P@�@�+@v�@v�@v�@v�@V@E�@�T@�@�@j@1@�F@t�@C�@33@o@��@�\@J@��@��@��@X@&�@&�@�@��@�9@r�@Q�@1'@�@��@�w@��@K�@��@��@��@v�@V@$�@{@�@�-@�@`B@`B@O�@?}@�@��@�j@Z@I�@�@1@��@�F@S�@o@o@o@@@@��@^5@M�@-@�#@��@�7@&�@Ĝ@�9@�9@��@�@r�@r�@bN@Q�@1'@ �@  @�@�P@|�@+@�@��@V@$�@{@�@��@@p�@V@��@�@�@�@��@�j@�@��@z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�HB
�HB
�BB
�BB
�BB
�NB
�B
�B
�B
�TB
�5B
�;B
�;B
�/B
�#B
��B
�
B
��B
�
B
ĜB
�wB
��B
�5B �B�1Bn�By�B� B�oB�B�B�LBĜB��B��B�B��B'�BuBuB(�B2-Bm�B��B��B%B+B%B��B��B��B�B�ZB�)B�
B��B��B�}B�FB�B��B��B|�B|�B�+B{�BaHBJ�B49B�BuBVBB�B�BȴB�!B��B��B��Bw�BI�B6FB=qB33B(�B�BuBB
��B
�B
�sB
�/B
�B
��B
�!B
��B
�DB
aHB
D�B
:^B
1'B
%�B
�B
1B	�B	�B	�sB	�
B	��B	�wB	�'B	��B	��B	�\B	�=B	�B	� B	~�B	|�B	t�B	hsB	dZB	S�B	J�B	E�B	/B	"�B	�B	{B	DB	1B	B	  B��B�B�B�B�B�fB�5B�B��B��B��B��BƨBÖB�qB�jB�^B�LB�FB�9B�?B�FB�wBBÖBĜBŢBŢBĜBĜBÖBŢB��B��B�}B�qB�}BB��BĜBŢBĜBŢBĜBÖBƨBǮBĜBB�wB�^B�LB�?B�3B�3B�B�B��B�B�B�B�B�B�B��B��B��B��B�PB�=B�=B�B}�B}�B|�B{�Bz�By�Bz�Bz�B�B�B�B�B�B�%B�=B�=B�JB��B��B��B�B�B��B��B��B��B��B��B��B�B�B�!B�B�B�'B�'B�FB�FB�XB�jB�qB�qBƨB��B��B��B�B�/B�;B�HB�`B�fB�B��B��B��B	  B	B	B	%B	JB	DB	JB	bB	�B	�B	�B	�B	�B	�B	�B	oB	uB	{B	�B	�B	!�B	#�B	#�B	$�B	%�B	&�B	%�B	"�B	"�B	)�B	7LB	:^B	=qB	>wB	@�B	A�B	A�B	C�B	C�B	D�B	E�B	H�B	L�B	K�B	K�B	K�B	L�B	O�B	R�B	S�B	S�B	T�B	T�B	VB	W
B	XB	ZB	^5B	_;B	ffB	dZB	e`B	k�B	m�B	o�B	y�B	y�B	x�B	|�B	�=B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�FB	�dB	�^B	�XB	�RB	�LB	�FB	�RB	�XB	�}B	�}B	�qB	�dB	�dB	�jB	��B	ƨB	ɺB	ȴB	ŢB	ŢB	ƨB	ȴB	ȴB	��B	��B	��B	�
B	�5B	�5B	�5B	�;B	�HB	�`B	�mB	�mB	�mB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�mB	�mB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
B
%B
%B
%B
%B
%B
%B
1B
	7B

=B

=B
JB
JB
PB
VB
bB
oB
hB
hB
hB
hB
hB
hB
bB
\B
VB
JB
DB

=B
	7B
1B
1B
+B
+B
+B
+B
+B
	7B
PB
bB
bB
bB
bB
bB
hB
hB
oB
oB
oB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
!�B
!�B
"�B
"�B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
-B
-B
-B
.B
-B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
1'B
1'B
2-B
2-B
2-B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
5?B
6FB
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
9XB
:^B
:^B
:^B
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
=qB
=qB
=qB
>wB
>wB
>wB
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
?}B
?}B
@�B
@�B
@�B
A�B
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
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
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
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
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
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
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
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
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
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
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
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
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
bNB
bNB
bNB
bNB
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
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
gmB
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
iyB
iyB
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
n�B
n�B
n�B
o�B
o�B
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
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�\B
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�HB
�bB
�vB
��B
�B
�B
�hB
�OB
��B
�B
��B
��B
�B
�VB
ݲB
�$B
�B
רB
��B
�YB
�}B
�DB
�5B!�B�BqAB{B��B��B��B�oB��B��B��B�2B�B iB*�B�B�B*KB4nBoiB�eB��BfB	�B�B�cB�6B�VB��B�fB��B�+B�sB՛B��B��B�vB�DB�#B}B~�B�=B�iBd�BO\B6�BBMB�BYB�!BܬB��B��B�*B��B�NB|jBMB:B@ B5?B+B�BB�B
�rB
��B
�0B
ބB
��B
�bB
��B
��B
�B
d�B
FYB
<jB
3�B
)_B
;B
�B	�OB	��B	�kB	�1B	�B	��B	��B	�eB	�B	��B	��B	�tB	�iB	�B	~�B	v`B	jB	f�B	U�B	NVB	I�B	1AB	$�B	 �B	�B	�B		�B	3B	[B	;B�B�}B��B�]B�B�;B�B�B�aBϑB͟B�7B�B�B��B�6B�	B��B��B�B�B� BðBĶBżBƨB�B��B�SB��B�B�uBªB�;B�.B�B��BB�B�%B�SB�tBżB�mB�B��B��B�B��B�B��B��B��B�B�B�WB��B��B�]B�]B�wB��B�WB��B�:B��B��B��B��B��B��B~�B~BB}qB|�B{�B{dB{�B|6B�oB��B�GB�[B�-B�+B��B��B�JB��B��B�B�wB��B��B��B�B�B��B��B��B��B�OB��B��B��B��B�-B��B��B�*B��B�(B��B�_B��B�B�&B�1B�IBߊB�B��B�B��B�`B�VB�.B	 iB	-B	GB	B	�B	xB	JB	bB	�B	=B	CB	#B	�B	#B	�B	&B	B	�B	�B	IB	"B	#�B	#�B	$�B	&fB	'�B	&2B	#B	#:B	)�B	7�B	:�B	=�B	?B	@�B	A�B	A�B	C�B	C�B	D�B	FB	IRB	MPB	LdB	L0B	L0B	MB	O�B	SB	S�B	TB	U2B	UgB	V9B	WsB	X_B	Z7B	^�B	_�B	f�B	d�B	ezB	k�B	m]B	o�B	z*B	zB	xlB	|B	�	B	�mB	��B	��B	�nB	��B	�jB	�RB	�KB	�*B	�6B	�B	��B	�B	��B	��B	��B	��B	��B	�lB	�>B	��B	�4B	��B	��B	��B	�PB	��B	��B	�#B	�RB	��B	��B	��B	�B	��B	��B	��B	��B	�$B	�OB	�jB	�jB	�VB	�|B	�`B	�B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	�]B	�"B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�	B	��B	��B	��B	�*B	�B	�B	��B	�B	�B	�B	�"B	�VB	�]B	�HB
 OB
 OB
;B
 B
 B
;B
;B
oB
uB
uB
UB
 B
 B
'B
B
-B
3B
mB
SB
SB
mB
SB
SB
?B
SB
?B
%B
YB
tB
?B
YB
fB
	�B

�B

rB
dB
~B
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B
	�B
�B
�B
zB
EB
_B
EB
EB
	lB
jB
�B
�B
�B
�B
�B
�B
�B
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
 �B
 �B
 �B
!B
!�B
!�B
"�B
"�B
!�B
#B
"�B
$B
$B
$&B
%B
%,B
%�B
%�B
%�B
%�B
%�B
%�B
&LB
'B
'8B
'B
(
B
(
B
)B
)B
*B
*B
)�B
*B
+6B
+B
+6B
,=B
,=B
-]B
-wB
-)B
./B
-CB
.IB
./B
/5B
/5B
0!B
0;B
0;B
0oB
0;B
1AB
1AB
2GB
1[B
1vB
2aB
2GB
2|B
3�B
4TB
4�B
4nB
5tB
5?B
5?B
5?B
5ZB
5ZB
5ZB
5?B
6`B
5?B
6zB
6zB
6`B
6`B
6zB
7�B
7fB
7fB
8lB
8lB
8lB
8�B
9�B
9rB
9rB
9�B
9�B
8lB
9rB
9rB
9rB
9rB
9rB
:xB
:^B
:�B
9�B
:�B
:�B
:�B
;�B
<�B
<jB
<jB
<�B
<jB
<jB
<�B
=qB
=�B
=�B
=�B
=�B
>�B
>wB
>wB
>wB
>wB
>�B
>�B
>�B
?�B
?�B
?}B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
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
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
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
G�B
G�B
H�B
H�B
H�B
H�B
H�B
IB
IB
I�B
J�B
J�B
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
LB
MB
L�B
L�B
MB
NB
M�B
N�B
N�B
N�B
PB
O�B
O�B
O�B
Q B
Q B
Q B
Q B
P�B
Q B
Q B
Q B
RB
R B
Q�B
R�B
SB
SB
TB
SB
S�B
TB
T,B
UB
UB
UB
UB
UB
U2B
UMB
V9B
W?B
W$B
W$B
W$B
XB
XB
Y1B
Y1B
Y1B
YB
YB
ZB
Z7B
Z7B
Z7B
Z7B
[#B
ZQB
Z7B
[=B
[=B
[=B
[#B
\)B
\)B
\)B
\CB
\CB
\]B
\CB
\CB
]/B
]IB
]dB
]IB
\CB
\CB
\CB
]IB
]IB
]IB
]IB
]IB
]dB
]/B
]IB
]IB
]IB
]/B
]dB
]dB
]dB
^OB
_pB
`\B
`\B
`\B
`\B
`\B
`BB
`\B
abB
abB
a|B
bhB
bhB
bhB
b4B
bhB
bhB
b�B
c�B
cnB
cnB
c�B
cnB
dtB
dZB
d�B
d�B
dtB
dZB
dtB
ezB
e`B
ezB
e�B
ezB
e�B
f�B
gmB
gmB
g�B
gmB
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
jB
jB
j�B
j�B
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
n�B
n�B
n�B
o�B
o�B
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
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608170038102016081700381020160817003810201806221300422018062213004220180622130042201804050659392018040506593920180405065939  JA  ARFMdecpA19c                                                                20160813093504  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160813003518  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160813003518  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160813003519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160813003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160813003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160813003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160813003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160813003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160813003520                      G�O�G�O�G�O�                JA  ARUP                                                                        20160813012019                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160813153533  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20160813153533  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20160813153533  CV  LATITUDE        G�O�G�O�A���                JM  ARCAJMQC2.0                                                                 20160816153810  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160816153810  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404215939  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040042  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201516                      G�O�G�O�G�O�                