CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-10-07T00:35:19Z creation;2017-10-07T00:35:23Z conversion to V3.1;2019-12-19T07:55:40Z update;     
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
resolution        =���   axis      Z        @  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     @  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  n�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  r�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     @  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  �<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ټ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20171007003519  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_167                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�+��y] 1   @�+�����@4۲��m]�d�S&�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���A   A   A@  A`  A�  A���A�  A�  A�33A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���A   A   A@  A`  A�  A���A�  A�  A�33A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�oA�JA�1A�
=A�1A�
=A�JA�JA�JA�VA�VA�JA�bA�VA�VA�bA�VA�VA�bA�oA�bA�oA�oA�oA�{A�{A�oA�{A�{A��A��A���A�ZAѶFAк^AμjA�$�A��A̝�A�$�A��A��A�\)Aư!A�ȴA��AËDA��PA���A�A�A�Q�A��jA�`BA��+A�+A�^5A�1A��uA��A�bA�Q�A���A�l�A�
=A���A��FA�^5A��A��A�-A�A� �A��PA��DA�
=A��A��jA��hA���A��uA���A��^A���A�~�A�JA�^5A���A�;dA�hsA�XA�bNA��-A�5?A�1'A�/A�bA��PA���A�oA�7LA�~�A��+A��PA�{A��!A���A���A�
=A��7A�A�?}A��!A��#A��A��FA�?}A|ZAx��Au�Ar��Aq��Ap��An�HAi&�Ah��Ag��Af�jAcO�A`r�A`-A_XA^{A]`BA\�A\v�A[|�AZ��AX��AT�9ASdZAR�\AP�DAO��AOXAN�9ALI�AJ�AJA�AIp�AH�`AFM�ADz�AB�ABE�AA`BA>^5A<~�A:^5A9+A8JA7K�A6JA4��A21A1;dA0VA.��A.9XA-7LA,{A+t�A)oA'�7A&�A%��A%33A#oA!�PA (�A;dA��A�\AbAG�AoAVA��A�RAbA��A�\A�`A �A|�A�`A�#A|�A��A��A�jAC�A�Ap�A
��A
��A
^5A	dZAVA�;A;dAĜAffA^5A(�A�A�^AVAI�A�AO�A �jA ~�A n�A M�A  �@���@���@�1@��-@�K�@�^5@��@��@�I�@�ƨ@���@��@���@�O�@�n�@�@ᙚ@߾w@�j@�1'@�z�@�j@� �@�@�~�@��@ᙚ@�7L@���@���@�V@���@�%@�I�@ۅ@��@��/@�9X@�@�v�@�?}@�A�@�1'@�(�@ӍP@��@�l�@��@͉7@�ƨ@�@�x�@�?}@��/@�A�@�|�@���@��#@�X@���@��@�&�@��!@�M�@�@�&�@��@�(�@��w@�dZ@���@�ȴ@�~�@���@��7@�?}@��@��@�r�@�j@�I�@�  @��@���@��P@�l�@�\)@�C�@�o@�v�@���@���@��D@� �@��m@���@���@���@�~�@�^5@��@�p�@��/@�I�@�ƨ@�t�@��@���@���@���@�G�@���@�Ĝ@��u@�(�@�+@��!@�@�G�@��@��u@�(�@��
@�S�@���@��H@���@��+@�V@�E�@�$�@��@��T@�hs@��D@��F@�dZ@�;d@�ȴ@�^5@�-@���@�`B@���@��@��`@���@�z�@�b@��m@���@���@�;d@�@��H@���@��R@���@�ff@���@�hs@�`B@�hs@�?}@�/@��@�V@��/@���@�z�@�Z@�9X@� �@���@���@���@���@���@���@���@�\)@�@��@���@��+@�M�@��@��#@���@�G�@���@��`@���@��@�9X@�1@��
@���@�|�@�dZ@�;d@�33@�33@�33@��@�ȴ@�n�@�=q@�5?@�5?@��@���@�hs@�X@�?}@�&�@��/@��j@���@���@�j@��@�dZ@�o@�@��@��R@�n�@�M�@�@��#@��-@��7@�x�@�p�@�`B@�X@�G�@�7L@���@�Q�@�(�@�  @��
@��w@��F@��F@�\)@��y@���@���@�n�@�{@���@��^@��-@���@���@�X@�%@���@���@�1@�|�@�dZ@��@��\@�5?@��@�X@�7L@��@��@�1'@��;@��w@��;@��m@��@�\)@�"�@�
=@��\@�v�@�n�@�ff@�^5@�ff@�ff@�E�@��T@��@�%@��u@�b@��
@��P@�K�@�+@��@���@�n�@�ff@�V@�E�@�=q@��@���@���@��7@�?}@���@���@��j@���@��D@�z�@�r�@�bN@�1'@��@��@~�@~5?@}�-@}�@}?}@}V@|�@|��@|9X@{�@{"�@z�H@zn�@y��@y7L@x �@w�;@w�w@w��@w\)@vȴ@v$�@u@u�@uV@t�/@t��@s�F@s�@sdZ@sC�@sC�@sC�@so@s@r��@q��@q&�@pA�@o+@n�R@nv�@nV@n5?@n{@m�h@l�@l��@l�@k��@kC�@j�@j�\@j^5@jJ@i�#@i��@ihs@i�@i%@h�`@hb@g\)@f��@f�y@f�y@f�@fv�@f5?@fE�@f{@e`B@d�@d�/@d�j@d��@dZ@d�@c��@b��@b=q@a�@a��@a7L@`��@`�u@`1'@_�@_�w@_\)@^�@^��@^V@]��@]�h@]�@\�@[ƨ@[S�@[C�@[@Z��@Z�\@Z=q@ZJ@Y��@Y��@Y�7@YG�@Y7L@Y%@X�`@Xr�@Xb@W��@W\)@V{@U�@U/@T��@T�@Tj@Tj@T�@S��@SS�@S@R��@R�\@RM�@Q�^@Qhs@Q%@P��@P�@P  @O��@O�P@O\)@N��@M�-@MO�@MV@LZ@LI�@L1@KC�@K"�@J�H@J=q@IG�@I�@I�@I�@H�9@Hb@G�w@G��@G|�@G\)@G�@F��@F�@F�R@Fv�@E@Ep�@EO�@E/@D�/@D��@D�D@DZ@D1@C��@C�m@C�
@Cƨ@C�F@C�F@C��@C��@C�@CC�@C33@B��@A�^@A�#@A��@Ax�@Ahs@AX@A7L@A&�@@�`@@bN@?�@?\)@?
=@>�R@>V@>5?@>$�@=�@=�h@=�h@=�h@=��@=��@=��@=�-@=��@=��@=�-@=�-@=�-@=��@=��@=�h@=�@=?}@<��@;��@;�@;dZ@;@:��@:��@:M�@:J@9�@9�#@9��@9��@9�^@9��@9��@9hs@8�`@8�u@8r�@8A�@7�;@7�@7�@7��@7�@7�@7|�@7l�@7+@7�@6�@6E�@5p�@4j@3�
@3�F@3�@3�@3t�@3C�@3"�@3@2��@1�@1��@1��@1�7@1�7@1x�@1X@17L@0��@0�`@0�9@0A�@/�@/�@/��@/K�@.�@.ȴ@.��@.�+@.v�@.v�@.V@-�@-�-@-�h@-`B@-V@,��@,�@,�j@,�D@,z�@,9X@,�@,1@+��@+�m@+�F@+�@+dZ@+"�@*��@*M�@*^5@*^5@*-@*�@)��@)��@)��@)X@)7L@)7L@)&�@(��@(�9@(�@(A�@(b@'��@'�@'�P@'\)@'K�@'+@&�R@&E�@&@%�T@%�-@%�h@%p�@%�@%V@$�@$I�@#�@#"�@#@"�@"�H@"�!@"-@"�@!��@!�@!��@!��@!7L@ Ĝ@ �9@ ��@ Q�@  �@�;@�w@�w@�w@�w@�@��@�P@l�@l�@�@�y@��@$�@�T@@�-@�h@�@O�@/@�@��@z�@Z@9X@�@1@�
@�
@��@t�@S�@33@@�H@��@��@�!@��@��@�\@n�@J@J@�#@��@��@x�@x�@hs@G�@7L@�@%@��@�`@��@Ĝ@�9@��@�u@r�@1'@�@�;@��@��@�w1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�oA�JA�1A�
=A�1A�
=A�JA�JA�JA�VA�VA�JA�bA�VA�VA�bA�VA�VA�bA�oA�bA�oA�oA�oA�{A�{A�oA�{A�{A��A��A���A�ZAѶFAк^AμjA�$�A��A̝�A�$�A��A��A�\)Aư!A�ȴA��AËDA��PA���A�A�A�Q�A��jA�`BA��+A�+A�^5A�1A��uA��A�bA�Q�A���A�l�A�
=A���A��FA�^5A��A��A�-A�A� �A��PA��DA�
=A��A��jA��hA���A��uA���A��^A���A�~�A�JA�^5A���A�;dA�hsA�XA�bNA��-A�5?A�1'A�/A�bA��PA���A�oA�7LA�~�A��+A��PA�{A��!A���A���A�
=A��7A�A�?}A��!A��#A��A��FA�?}A|ZAx��Au�Ar��Aq��Ap��An�HAi&�Ah��Ag��Af�jAcO�A`r�A`-A_XA^{A]`BA\�A\v�A[|�AZ��AX��AT�9ASdZAR�\AP�DAO��AOXAN�9ALI�AJ�AJA�AIp�AH�`AFM�ADz�AB�ABE�AA`BA>^5A<~�A:^5A9+A8JA7K�A6JA4��A21A1;dA0VA.��A.9XA-7LA,{A+t�A)oA'�7A&�A%��A%33A#oA!�PA (�A;dA��A�\AbAG�AoAVA��A�RAbA��A�\A�`A �A|�A�`A�#A|�A��A��A�jAC�A�Ap�A
��A
��A
^5A	dZAVA�;A;dAĜAffA^5A(�A�A�^AVAI�A�AO�A �jA ~�A n�A M�A  �@���@���@�1@��-@�K�@�^5@��@��@�I�@�ƨ@���@��@���@�O�@�n�@�@ᙚ@߾w@�j@�1'@�z�@�j@� �@�@�~�@��@ᙚ@�7L@���@���@�V@���@�%@�I�@ۅ@��@��/@�9X@�@�v�@�?}@�A�@�1'@�(�@ӍP@��@�l�@��@͉7@�ƨ@�@�x�@�?}@��/@�A�@�|�@���@��#@�X@���@��@�&�@��!@�M�@�@�&�@��@�(�@��w@�dZ@���@�ȴ@�~�@���@��7@�?}@��@��@�r�@�j@�I�@�  @��@���@��P@�l�@�\)@�C�@�o@�v�@���@���@��D@� �@��m@���@���@���@�~�@�^5@��@�p�@��/@�I�@�ƨ@�t�@��@���@���@���@�G�@���@�Ĝ@��u@�(�@�+@��!@�@�G�@��@��u@�(�@��
@�S�@���@��H@���@��+@�V@�E�@�$�@��@��T@�hs@��D@��F@�dZ@�;d@�ȴ@�^5@�-@���@�`B@���@��@��`@���@�z�@�b@��m@���@���@�;d@�@��H@���@��R@���@�ff@���@�hs@�`B@�hs@�?}@�/@��@�V@��/@���@�z�@�Z@�9X@� �@���@���@���@���@���@���@���@�\)@�@��@���@��+@�M�@��@��#@���@�G�@���@��`@���@��@�9X@�1@��
@���@�|�@�dZ@�;d@�33@�33@�33@��@�ȴ@�n�@�=q@�5?@�5?@��@���@�hs@�X@�?}@�&�@��/@��j@���@���@�j@��@�dZ@�o@�@��@��R@�n�@�M�@�@��#@��-@��7@�x�@�p�@�`B@�X@�G�@�7L@���@�Q�@�(�@�  @��
@��w@��F@��F@�\)@��y@���@���@�n�@�{@���@��^@��-@���@���@�X@�%@���@���@�1@�|�@�dZ@��@��\@�5?@��@�X@�7L@��@��@�1'@��;@��w@��;@��m@��@�\)@�"�@�
=@��\@�v�@�n�@�ff@�^5@�ff@�ff@�E�@��T@��@�%@��u@�b@��
@��P@�K�@�+@��@���@�n�@�ff@�V@�E�@�=q@��@���@���@��7@�?}@���@���@��j@���@��D@�z�@�r�@�bN@�1'@��@��@~�@~5?@}�-@}�@}?}@}V@|�@|��@|9X@{�@{"�@z�H@zn�@y��@y7L@x �@w�;@w�w@w��@w\)@vȴ@v$�@u@u�@uV@t�/@t��@s�F@s�@sdZ@sC�@sC�@sC�@so@s@r��@q��@q&�@pA�@o+@n�R@nv�@nV@n5?@n{@m�h@l�@l��@l�@k��@kC�@j�@j�\@j^5@jJ@i�#@i��@ihs@i�@i%@h�`@hb@g\)@f��@f�y@f�y@f�@fv�@f5?@fE�@f{@e`B@d�@d�/@d�j@d��@dZ@d�@c��@b��@b=q@a�@a��@a7L@`��@`�u@`1'@_�@_�w@_\)@^�@^��@^V@]��@]�h@]�@\�@[ƨ@[S�@[C�@[@Z��@Z�\@Z=q@ZJ@Y��@Y��@Y�7@YG�@Y7L@Y%@X�`@Xr�@Xb@W��@W\)@V{@U�@U/@T��@T�@Tj@Tj@T�@S��@SS�@S@R��@R�\@RM�@Q�^@Qhs@Q%@P��@P�@P  @O��@O�P@O\)@N��@M�-@MO�@MV@LZ@LI�@L1@KC�@K"�@J�H@J=q@IG�@I�@I�@I�@H�9@Hb@G�w@G��@G|�@G\)@G�@F��@F�@F�R@Fv�@E@Ep�@EO�@E/@D�/@D��@D�D@DZ@D1@C��@C�m@C�
@Cƨ@C�F@C�F@C��@C��@C�@CC�@C33@B��@A�^@A�#@A��@Ax�@Ahs@AX@A7L@A&�@@�`@@bN@?�@?\)@?
=@>�R@>V@>5?@>$�@=�@=�h@=�h@=�h@=��@=��@=��@=�-@=��@=��@=�-@=�-@=�-@=��@=��@=�h@=�@=?}@<��@;��@;�@;dZ@;@:��@:��@:M�@:J@9�@9�#@9��@9��@9�^@9��@9��@9hs@8�`@8�u@8r�@8A�@7�;@7�@7�@7��@7�@7�@7|�@7l�@7+@7�@6�@6E�@5p�@4j@3�
@3�F@3�@3�@3t�@3C�@3"�@3@2��@1�@1��@1��@1�7@1�7@1x�@1X@17L@0��@0�`@0�9@0A�@/�@/�@/��@/K�@.�@.ȴ@.��@.�+@.v�@.v�@.V@-�@-�-@-�h@-`B@-V@,��@,�@,�j@,�D@,z�@,9X@,�@,1@+��@+�m@+�F@+�@+dZ@+"�@*��@*M�@*^5@*^5@*-@*�@)��@)��@)��@)X@)7L@)7L@)&�@(��@(�9@(�@(A�@(b@'��@'�@'�P@'\)@'K�@'+@&�R@&E�@&@%�T@%�-@%�h@%p�@%�@%V@$�@$I�@#�@#"�@#@"�@"�H@"�!@"-@"�@!��@!�@!��@!��@!7L@ Ĝ@ �9@ ��@ Q�@  �@�;@�w@�w@�w@�w@�@��@�P@l�@l�@�@�y@��@$�@�T@@�-@�h@�@O�@/@�@��@z�@Z@9X@�@1@�
@�
@��@t�@S�@33@@�H@��@��@�!@��@��@�\@n�@J@J@�#@��@��@x�@x�@hs@G�@7L@�@%@��@�`@��@Ĝ@�9@��@�u@r�@1'@�@�;@��@��@�w1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bq�Bq�Br�Br�Br�Br�Bs�Bs�Bs�Bs�Bs�Bs�Br�Br�Br�Br�Br�Br�Bs�Br�Br�Br�Br�Br�Br�Br�Bq�Bp�Bn�BiyBR�B!�B�B\)BjB�hB�}B��B��BȴB��B�B�B+BDBVB%B�B�B#�B0!B8RB:^BG�BF�BS�BW
BW
B[#BgmBm�Bk�Bl�Bn�Bo�BhsBw�B�B{�Bs�Bq�Bp�BhsBYBXBaHB]/BT�BE�BE�BF�B@�BI�BB�B?}BA�B=qB'�B�B%B�/B�dB��B�DBiyBG�B)�BuB
��B
��B
�B
�;B
ǮB
��B
�B
ȴB
��B
��B
��B
��B
��B
�DB
p�B
_;B
C�B
'�B
+B	��B	�sB	�B	�HB	��B	��B	�?B	�B	��B	�DB	y�B	�7B	�B	w�B	w�B	t�B	o�B	dZB	\)B	K�B	(�B	0!B	/B	"�B	%�B	$�B	�B	PB		7B	JB	B	  B�B�B�TB�fB�;B��BĜB�qB�wB�jB�LB�!B�B��B��B��B��B��B�oB�VB�bB�B�7B�JB�DB�Bp�Bo�Bm�Bp�Bl�Bm�Bs�Bt�By�B}�B� B~�By�Bz�Br�BgmBjBiyBe`B^5BQ�B^5B_;BZBYBYBaHB`BBe`BbNB\)B^5BhsBffBgmBiyBk�BgmB^5B`BBl�Bs�Bw�B|�B|�B�B�B�B�B}�B}�B�B}�B}�B�B�By�B�B�JB�JB�{B��B��B�\B�DB�B�%B��B�!B�9B�?B�LB�jB�jB�dB�}B�wB�qB�wB��B��B�qB�wB�}B��BĜBȴBȴB��B��B��B��B��B��BǮB��BɺB��BǮBB��B��B��B��B��B��B��B��B��B��BȴBƨB��B�B��B�B��B�B�/B�;B�fB�yB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	B	
=B	VB	hB	uB	{B	�B	 �B	!�B	"�B	$�B	.B	33B	5?B	7LB	8RB	=qB	?}B	C�B	E�B	H�B	J�B	K�B	J�B	I�B	Q�B	Q�B	VB	ZB	[#B	_;B	cTB	dZB	hsB	jB	jB	jB	l�B	n�B	o�B	o�B	p�B	o�B	q�B	t�B	{�B	� B	� B	�B	�+B	�1B	�DB	�VB	�uB	�{B	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�3B	�9B	�FB	�FB	�RB	�dB	�dB	�dB	�dB	�^B	�^B	�dB	�wB	�}B	��B	��B	B	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�
B	�B	�#B	�/B	�/B	�)B	�#B	�;B	�HB	�HB	�HB	�NB	�TB	�ZB	�`B	�ZB	�TB	�`B	�sB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B
B
B
B
B
B
B
B
B
1B
	7B
	7B

=B

=B
	7B
1B
+B
%B
+B
1B
1B
DB
JB
PB
VB
VB
\B
bB
hB
hB
hB
hB
bB
\B
bB
hB
bB
hB
oB
uB
uB
{B
{B
{B
{B
uB
{B
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
!�B
!�B
!�B
!�B
 �B
�B
!�B
!�B
!�B
"�B
#�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
#�B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
'�B
(�B
(�B
(�B
(�B
'�B
'�B
'�B
(�B
)�B
+B
)�B
+B
+B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
.B
-B
/B
0!B
0!B
/B
0!B
0!B
1'B
1'B
1'B
0!B
1'B
2-B
1'B
1'B
1'B
2-B
2-B
2-B
0!B
33B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
7LB
8RB
9XB
9XB
9XB
8RB
9XB
;dB
:^B
:^B
9XB
=qB
=qB
<jB
?}B
>wB
=qB
?}B
?}B
>wB
>wB
A�B
B�B
A�B
@�B
@�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
D�B
E�B
E�B
E�B
E�B
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
G�B
F�B
F�B
F�B
E�B
E�B
I�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
H�B
J�B
J�B
K�B
K�B
L�B
M�B
L�B
L�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
M�B
M�B
M�B
M�B
M�B
L�B
K�B
K�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
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
R�B
R�B
Q�B
R�B
Q�B
P�B
P�B
Q�B
S�B
VB
VB
W
B
VB
VB
W
B
VB
VB
T�B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
W
B
XB
YB
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
ZB
[#B
[#B
[#B
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
]/B
]/B
]/B
]/B
\)B
^5B
_;B
_;B
^5B
_;B
_;B
_;B
^5B
_;B
_;B
`BB
`BB
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
`BB
aHB
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
bNB
`BB
aHB
cTB
dZB
dZB
e`B
dZB
cTB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
hsB
hsB
hsB
gmB
gmB
gmB
gmB
gmB
ffB
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
hsB
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
l�B
k�B
k�B
l�B
l�B
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
o�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bq�Bq�Br�Br�Br�Br�Bs�Bs�Bs�Bs�Bs�Bs�Br�Br�Br�Br�Br�Br�Bs�Br�Br�Br�Br�Br�Br�Br�Bq�Bp�BoBkQBYKB,WB"B^OBn}B��B��BΥB�PBˬBևB�B�%B	B�B�B
=B�B vB%�B1[B9XB<BH�BHKBT�BX+BX�B\�Bh�BncBl�Bm�Bo�Bq�BkBx�B��B}qBv+Bs�BraBj�B\CBZBa�B^BV�BH�BG�BH�BB[BJrBC�BABB�B?�B,B!�B)B�:B��B��B�vBl�BK�B.IBsB
��B �B
�3B
�4B
�JB
��B
ٴB
��B
�QB
��B
��B
�B
�?B
��B
s�B
a�B
G�B
,�B
�B	��B	�6B	�B	�B	�@B	��B	��B	��B	��B	�(B	}B	��B	�GB	y>B	x�B	utB	poB	e�B	]�B	NpB	-�B	1�B	0�B	%B	&�B	%�B	!B	B	
�B	6B	YB	oB��B��B�FB�B�B�pB�+B� B�B��B��B��B��B��B��B�B�?B��B��B��B��B��B�)B��B�JB��BshBq�BoiBrBnIBo5BtnBu�Bz*B~(B�OB}Bz�B{�Bt�Bi�Bk�BjBf�B`BT�B_pB`vB[�B[	BZ�Bb4BaBe�BcB]�B_pBi*BgRBh$Bi�Bk�Bh$B`Ba�Bm�Bt�Bx�B}�B}�B�[B�SB�gB��B.BB��B�B�B��B�MB|B�B��B�B�B�B��B��B��B�B��B��B�IB�B�tB��B��B�"B�B��B��B�]B�HB��B��B�(B�B�4B��B�SB�RBɠB�<B͟B�pB�B�:B̈́B��B�ABʦB�PB�BðB��B�"B�6B�JB�PB�\BΥB�oB҉B�6B�	B�1B�aB�mBՁB�SBԕB�eB�~BߤB�B��B��B��B��B��B��B��B��B�B�B��B�B��B�B�B�"B�<B�jB	 �B	�B	
�B	�B	�B	�B	�B	�B	 �B	!�B	# B	%zB	.�B	3�B	5�B	7�B	8�B	=�B	?�B	C�B	E�B	H�B	J�B	LB	K)B	J�B	RTB	R�B	V9B	ZQB	[�B	_�B	c�B	d�B	h�B	j�B	j�B	j�B	l�B	n�B	o�B	o�B	p�B	p!B	rGB	u?B	|B	�4B	�iB	�aB	�_B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	� B	�NB	��B	�B	�B	�)B	�/B	�5B	�5B	�IB	�OB	�GB	�MB	�TB	�`B	�zB	�lB	�dB	�dB	�dB	�dB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�&B	�B	�B	�9B	�B	�B	�+B	�?B	�YB	�_B	�=B	�B	�IB	�]B	�qB	�pB	�bB	�bB	�bB	�B	�nB	�tB	�zB	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�	B	�	B	�B	��B	�B	�(B	�B	�B	�0B	�B	�B	�JB	�JB	�"B	�6B	�JB	�6B	�6B	�JB
 B
 B	�.B	�jB	�(B
 B
�B
B
AB
AB
GB
9B
mB
KB
	7B
	lB

=B

=B
	RB
fB
zB
tB
�B
�B
�B
xB
~B
�B
pB
�B
�B
}B
hB
�B
�B
�B
}B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
�B
B
B
�B
!�B
!�B
!�B
!�B
 �B
�B
!�B
!�B
!�B
"�B
#�B
#B
#�B
#�B
$B
#�B
#�B
#�B
$�B
$B
!�B
!�B
"�B
#�B
#�B
#�B
$B
%B
%�B
%�B
&2B
(
B
(�B
)B
)B
)B
($B
($B
(>B
)*B
*B
+B
*0B
+B
+6B
,"B
,"B
-)B
-CB
-)B
./B
./B
.IB
./B
.IB
.IB
-]B
/5B
0;B
0;B
/5B
0;B
0;B
1AB
1'B
1'B
0;B
1AB
2-B
1AB
1AB
1[B
2GB
2aB
2aB
0�B
3MB
5ZB
5ZB
6`B
6FB
6`B
6`B
6zB
7�B
8lB
8lB
8lB
8lB
7�B
8lB
9rB
9�B
9�B
8�B
9rB
;B
:xB
:�B
9�B
=�B
=�B
<�B
?}B
>�B
=�B
?�B
?�B
>�B
>�B
A�B
B�B
A�B
@�B
@�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
D�B
E�B
E�B
E�B
E�B
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
G�B
F�B
F�B
F�B
E�B
E�B
I�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
H�B
J�B
J�B
K�B
K�B
L�B
M�B
L�B
L�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
M�B
M�B
M�B
M�B
M�B
L�B
K�B
L0B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q B
P�B
Q B
Q B
Q4B
Q B
RB
RB
RB
SB
R�B
R�B
S�B
R�B
SB
SB
RB
S&B
R B
Q4B
Q4B
R:B
T,B
VB
VB
W
B
VB
VB
W$B
VB
V9B
UMB
X+B
XB
XEB
XB
XB
X+B
X+B
X+B
XEB
X+B
W?B
X+B
YKB
Y1B
Y1B
Y1B
ZB
Z7B
Z7B
[#B
[#B
Z7B
Z7B
Z7B
[=B
[=B
[=B
\B
\CB
\CB
\CB
\CB
\CB
]IB
]/B
]/B
]IB
]IB
]IB
]IB
]IB
\]B
^OB
_;B
_;B
^OB
_;B
_;B
_VB
^OB
_VB
_VB
`BB
`BB
_;B
_VB
_pB
`\B
`\B
`\B
`\B
abB
`\B
abB
`\B
`vB
`vB
a|B
bhB
bhB
b�B
bhB
bhB
bhB
bhB
`�B
a�B
cnB
d�B
dtB
e`B
dtB
c�B
e`B
ezB
e`B
ezB
ezB
ezB
e�B
ffB
f�B
f�B
f�B
ffB
gmB
hsB
hsB
hsB
gmB
gmB
gmB
g�B
gmB
f�B
g�B
g�B
g�B
h�B
h�B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
j�B
j�B
jB
jB
jB
jB
j�B
k�B
k�B
k�B
k�B
k�B
l�B
k�B
k�B
l�B
l�B
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n}B
n�B
o�B
o�B
o�1111111111111111111111111111113111111111111111111111111111111111111111111111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(�U<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710101047562017101010475620171010104756201806221320132018062213201320180622132013201804050722502018040507225020180405072250  JA  ARFMdecpA19c                                                                20171007093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171007003519  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171007003521  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171007003521  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171007003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171007003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171007003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171007003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171007003522  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171007003523                      G�O�G�O�G�O�                JA  ARUP                                                                        20171007005546                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20171010014656  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20171010014637  CV  JULD            G�O�G�O�F�]�                JM  ARCAJMQC2.0                                                                 20171010014756  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171010014756  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20171011000000  CF  PSAL_ADJUSTED_QCB�  Dހ G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222250  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042013  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                