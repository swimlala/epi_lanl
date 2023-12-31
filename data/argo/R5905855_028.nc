CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:15:39Z creation;2022-06-04T19:15:40Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
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
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
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
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191539  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @���\Y�T1   @��ȼM^p@/����o�d��+1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�33A�  A���A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bz��B~  B�  B�  B�  B�  B�  B�33B�33B�33B�  B���B���B�  B�  B�  B�  B�  B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd33Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DxfDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D��3D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@�  @�  A   A   A@  A`  A�  A�  A�33A�  A���A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bz��B~  B�  B�  B�  B�  B�  B�33B�33B�33B�  B���B���B�  B�  B�  B�  B�  B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd33Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DxfDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D��3D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�,�A�+6A�.IA�,qA�(�A���A�A��A�c A�,A��A�kA��A�\A�PA��A��A�PA�xA��?A��A��A��pA�m�A�v�A�̘A�:�A��A�AA�3�AֶFA�_A�e�A�L�A�F�A�FA̟VA�.AɆ�A�=�AȊ�AƳ�AŬqA�NA�/A�m�A� �A��~A�"hA��A��A�&�A��RA���A���A��A�9�A�"�A��FA��GA�:�A�� A���A�� A���A���A�uZA��A�,qA���A��IA�~(A���A�,A�m�A�`A���A���A�s�A��;A� �A��A�1'A�X�A�WsA�"�A�?HA�x�A��A�_A�P�A�0!A~9XA}�A|H�Az��Ay� AxzAv�At��Ap��Ak�KAizAh�Af��Ae�Aa�A^%�AZ+�AW$�AS�AP1�ANPHAH��ACO�A@F�A=�
A;t�A:��A9�MA8XyA7��A6�HA63�A4��A2��A1�TA0�	A0��A/�A/C�A.v�A-��A-P�A,�LA,+A+��A+y>A*�{A*@A)��A)iDA)'�A(S�A(qA'�{A&33A%~A$A A#�hA ffA�tAe,A�A�A iA�A<6A)_A&�AhsA��AیA�tAV�A��A�5A��A�XAm]A;A-wA��A+kA� A�A�A}�AR�A�A��Au�A}VAx�AA�A��A�3AdZA�AݘA��A�VA��A��Ag�Ad�A_A�oA�A
.�A	&A�'A�zA6A2aAA iA��A�A��A�}A��A��AM�A"�A�/Ag�AGEA>BA4A��A�A~(A!A�A�ZA�8A�A�)Ao�A�}AS�A��A�KA�RA��AA�AƨAW�A!A ��A ��A j�A :*@��@�c�@�x@�� @��@���@��.@���@���@�/@��u@�\�@��	@�8�@��*@�҉@�s�@�PH@�D�@�:�@��@���@�S@�ԕ@�C@�}@�H@�@�4@�c�@�<6@��X@�W�@���@�U�@��@�e�@��B@�1@�@�j@�e�@�r@�/�@�5�@旍@��5@�:�@�R@�*�@��#@��f@�h�@��@�Y�@�H�@�@�:�@࿱@�H�@ߪ�@���@�Ft@ݞ�@�l�@۶F@�%@�C�@ٲ-@�a�@�J#@�=�@���@ؑ @� �@��@�4@���@բ�@�7L@��@���@ԇ+@�'R@��)@ӥ@ә�@��]@�1�@�4n@���@�p�@Ю}@�c@ε�@�<�@��@ͣn@�e,@�J�@�33@̨�@�4n@���@��@ʬ@�d�@�=q@�  @��@�H�@�G@�n/@���@Ƅ�@�Q�@��@Ţ�@��@���@ħ@č�@�A�@���@Õ�@�Y�@�9�@��@��@�@��P@�:�@�ی@���@�V@�ƨ@�=@�h�@� �@���@�Mj@��p@�?@�X�@�V@��P@��j@�Ta@��)@�b�@��@�V@��}@��@��6@�x@��@�U2@�4@��#@�~�@�֡@���@�`�@�&�@�Y�@���@���@��O@�~(@�Q@��+@�_p@�ں@��X@���@�S�@�	@��@�C�@�v�@�M@�'R@��]@���@���@�5�@��@��H@���@���@�V@��@��@��@�h�@�~@��@�J@���@��[@�j�@�W?@�[W@�%F@��v@�d�@��j@���@�33@��@��y@���@�$@�G@��z@�Dg@�!�@���@���@���@��h@�}V@�a|@�:�@��#@�<6@��O@�l"@�!@��g@�u�@�m�@��0@� \@���@���@�A�@��9@�|�@�<6@��@��@��9@�D�@�˒@�hs@�%F@�(@��@��	@��/@��\@�-�@���@�C�@��,@��O@���@���@�xl@�oi@�K^@���@��*@���@��7@�_p@��2@���@�_�@�x@���@�s�@�T�@�(�@���@���@�	@�u�@�IR@��@��@��@�ȴ@���@�u%@�@�@��]@���@��7@�O�@�0�@�;@��u@�V�@�5?@���@�Y�@��@�o@��)@��9@���@�+k@��@��z@���@�~�@�=�@� \@��c@���@��2@���@��5@��/@���@�d�@��#@�Q�@��@��?@���@�s�@�?�@��m@���@�1�@��@���@�{�@�c�@�W�@�E�@�'R@�J@��&@��	@�o @�Y�@�C@��@���@�[�@�@�@�e@���@��@���@��@�x�@�9�@��@��@�~�@�Z�@��o@���@�b�@���@��4@�kQ@�@��a@�qv@�L�@�=@��@��B@���@��u@�H�@��@_p@~��@~ �@}��@}k�@}�@|�@|�[@|�U@|��@|��@|�.@|h�@|A�@{��@{S�@z��@z}V@z($@y�@y��@y�X@y�@x��@xV�@x�@w�@w˒@w��@w�@v�B@v}V@u��@u�@tĜ@t�Y@tC-@s��@s�P@r�X@r�r@ri�@rGE@r#:@q�D@q�@qDg@q%@p�@p�@p��@p�o@pQ�@o˒@o�P@oW?@n��@n��@n3�@m�~@m�@l�@l��@l*�@k|�@j�M@jxl@i��@i�~@i@@h�j@h�u@he�@g�@gs@g,�@fYK@e�@ee,@e(�@d��@c��@c��@c=@b�@bQ@a@a�@`��@`��@`g8@`	�@_��@_Y@^�,@^:*@]�t@]k�@\��@\��@[��@[H�@Z�@Z��@Z� @Y��@Y�@XɆ@X6@W��@W�K@W�@@W��@W��@W��@W�P@W@V�L@V{@U�C@UO�@U�@T�Y@T7@S��@Ss@Rȴ@R.�@Q��@Q��@Qu�@Q%F@P��@P  @O�@O�}@O�@OC�@N�M@NYK@N
�@N@M��@M�@L�.@LZ@L~@K�*@K{J@Ko@J�L@J?@I��@I`B@H�I@H9X@H�@G�@@GK�@F�@F�,@F��@F�}@F�L@F��@Fa|@E��@E�-@EY�@Eq@E;@DɆ@De�@D�@C�Q@C��@CdZ@C�@BZ�@B6�@A�>@AG�@@��@@r�@@1@?�@?K�@>��@>Ta@=��@=u�@= \@<�)@<��@<u�@<bN@<1'@<x@;�m@;�@;��@:�y@:~�@:YK@9�D@9��@9�@9/@8�@8'R@7�@7b�@7=@6��@6h
@6_@5�o@5�@5��@55�@5�@4��@4�@4Ft@4"h@4M@3�+@3y�@333@3"�@3o@2��@2C�@1��@1��@0�K@0�`@0�o@0H@0*�@0�@/�:@/t�@/o�@/C�@.v�@.0U@.@-�-@-:�@,��@,�E@,��@,[�@,(�@,q@,�@,M@+��@+��@+Z�@+U�@+F�@+@*��@*��@*��@*�'@*�F@*_�@*5?@)�@)�h@)G�@(�5@(�@(`�@'�]@'��@']�@';d@' i@&͟@&�1@&	@%�@%��@%��@%k�@$��@$�@$�9@$��@$c�@$'R@#�m@#ݘ@#��@#�@#�@"�@":*@"�@"O@"	@!�>@!�d@!�t@!��@!B�@!:�@!<6@!=�@!=�@!�@ �p@ �4@ U2@  �@ b@ @� @@O@@�!@��@z@W�@5?@�j@��@u�@Vm@<6@�@֡@�Y@7�@G@��@�@��@j�@F�@&@�@�"@�@�@��@\�@)�@ �@�>@��@F@8�@-w@�@�P@�U@��@��@u�@H@$@�@��@� @��@\)@��@u%@3�@.�@�@�z@��@j@O�@B�@4@�@Ɇ@|�@h�@bN@C-@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�,�A�+6A�.IA�,qA�(�A���A�A��A�c A�,A��A�kA��A�\A�PA��A��A�PA�xA��?A��A��A��pA�m�A�v�A�̘A�:�A��A�AA�3�AֶFA�_A�e�A�L�A�F�A�FA̟VA�.AɆ�A�=�AȊ�AƳ�AŬqA�NA�/A�m�A� �A��~A�"hA��A��A�&�A��RA���A���A��A�9�A�"�A��FA��GA�:�A�� A���A�� A���A���A�uZA��A�,qA���A��IA�~(A���A�,A�m�A�`A���A���A�s�A��;A� �A��A�1'A�X�A�WsA�"�A�?HA�x�A��A�_A�P�A�0!A~9XA}�A|H�Az��Ay� AxzAv�At��Ap��Ak�KAizAh�Af��Ae�Aa�A^%�AZ+�AW$�AS�AP1�ANPHAH��ACO�A@F�A=�
A;t�A:��A9�MA8XyA7��A6�HA63�A4��A2��A1�TA0�	A0��A/�A/C�A.v�A-��A-P�A,�LA,+A+��A+y>A*�{A*@A)��A)iDA)'�A(S�A(qA'�{A&33A%~A$A A#�hA ffA�tAe,A�A�A iA�A<6A)_A&�AhsA��AیA�tAV�A��A�5A��A�XAm]A;A-wA��A+kA� A�A�A}�AR�A�A��Au�A}VAx�AA�A��A�3AdZA�AݘA��A�VA��A��Ag�Ad�A_A�oA�A
.�A	&A�'A�zA6A2aAA iA��A�A��A�}A��A��AM�A"�A�/Ag�AGEA>BA4A��A�A~(A!A�A�ZA�8A�A�)Ao�A�}AS�A��A�KA�RA��AA�AƨAW�A!A ��A ��A j�A :*@��@�c�@�x@�� @��@���@��.@���@���@�/@��u@�\�@��	@�8�@��*@�҉@�s�@�PH@�D�@�:�@��@���@�S@�ԕ@�C@�}@�H@�@�4@�c�@�<6@��X@�W�@���@�U�@��@�e�@��B@�1@�@�j@�e�@�r@�/�@�5�@旍@��5@�:�@�R@�*�@��#@��f@�h�@��@�Y�@�H�@�@�:�@࿱@�H�@ߪ�@���@�Ft@ݞ�@�l�@۶F@�%@�C�@ٲ-@�a�@�J#@�=�@���@ؑ @� �@��@�4@���@բ�@�7L@��@���@ԇ+@�'R@��)@ӥ@ә�@��]@�1�@�4n@���@�p�@Ю}@�c@ε�@�<�@��@ͣn@�e,@�J�@�33@̨�@�4n@���@��@ʬ@�d�@�=q@�  @��@�H�@�G@�n/@���@Ƅ�@�Q�@��@Ţ�@��@���@ħ@č�@�A�@���@Õ�@�Y�@�9�@��@��@�@��P@�:�@�ی@���@�V@�ƨ@�=@�h�@� �@���@�Mj@��p@�?@�X�@�V@��P@��j@�Ta@��)@�b�@��@�V@��}@��@��6@�x@��@�U2@�4@��#@�~�@�֡@���@�`�@�&�@�Y�@���@���@��O@�~(@�Q@��+@�_p@�ں@��X@���@�S�@�	@��@�C�@�v�@�M@�'R@��]@���@���@�5�@��@��H@���@���@�V@��@��@��@�h�@�~@��@�J@���@��[@�j�@�W?@�[W@�%F@��v@�d�@��j@���@�33@��@��y@���@�$@�G@��z@�Dg@�!�@���@���@���@��h@�}V@�a|@�:�@��#@�<6@��O@�l"@�!@��g@�u�@�m�@��0@� \@���@���@�A�@��9@�|�@�<6@��@��@��9@�D�@�˒@�hs@�%F@�(@��@��	@��/@��\@�-�@���@�C�@��,@��O@���@���@�xl@�oi@�K^@���@��*@���@��7@�_p@��2@���@�_�@�x@���@�s�@�T�@�(�@���@���@�	@�u�@�IR@��@��@��@�ȴ@���@�u%@�@�@��]@���@��7@�O�@�0�@�;@��u@�V�@�5?@���@�Y�@��@�o@��)@��9@���@�+k@��@��z@���@�~�@�=�@� \@��c@���@��2@���@��5@��/@���@�d�@��#@�Q�@��@��?@���@�s�@�?�@��m@���@�1�@��@���@�{�@�c�@�W�@�E�@�'R@�J@��&@��	@�o @�Y�@�C@��@���@�[�@�@�@�e@���@��@���@��@�x�@�9�@��@��@�~�@�Z�@��o@���@�b�@���@��4@�kQ@�@��a@�qv@�L�@�=@��@��B@���@��u@�H�@��@_p@~��@~ �@}��@}k�@}�@|�@|�[@|�U@|��@|��@|�.@|h�@|A�@{��@{S�@z��@z}V@z($@y�@y��@y�X@y�@x��@xV�@x�@w�@w˒@w��@w�@v�B@v}V@u��@u�@tĜ@t�Y@tC-@s��@s�P@r�X@r�r@ri�@rGE@r#:@q�D@q�@qDg@q%@p�@p�@p��@p�o@pQ�@o˒@o�P@oW?@n��@n��@n3�@m�~@m�@l�@l��@l*�@k|�@j�M@jxl@i��@i�~@i@@h�j@h�u@he�@g�@gs@g,�@fYK@e�@ee,@e(�@d��@c��@c��@c=@b�@bQ@a@a�@`��@`��@`g8@`	�@_��@_Y@^�,@^:*@]�t@]k�@\��@\��@[��@[H�@Z�@Z��@Z� @Y��@Y�@XɆ@X6@W��@W�K@W�@@W��@W��@W��@W�P@W@V�L@V{@U�C@UO�@U�@T�Y@T7@S��@Ss@Rȴ@R.�@Q��@Q��@Qu�@Q%F@P��@P  @O�@O�}@O�@OC�@N�M@NYK@N
�@N@M��@M�@L�.@LZ@L~@K�*@K{J@Ko@J�L@J?@I��@I`B@H�I@H9X@H�@G�@@GK�@F�@F�,@F��@F�}@F�L@F��@Fa|@E��@E�-@EY�@Eq@E;@DɆ@De�@D�@C�Q@C��@CdZ@C�@BZ�@B6�@A�>@AG�@@��@@r�@@1@?�@?K�@>��@>Ta@=��@=u�@= \@<�)@<��@<u�@<bN@<1'@<x@;�m@;�@;��@:�y@:~�@:YK@9�D@9��@9�@9/@8�@8'R@7�@7b�@7=@6��@6h
@6_@5�o@5�@5��@55�@5�@4��@4�@4Ft@4"h@4M@3�+@3y�@333@3"�@3o@2��@2C�@1��@1��@0�K@0�`@0�o@0H@0*�@0�@/�:@/t�@/o�@/C�@.v�@.0U@.@-�-@-:�@,��@,�E@,��@,[�@,(�@,q@,�@,M@+��@+��@+Z�@+U�@+F�@+@*��@*��@*��@*�'@*�F@*_�@*5?@)�@)�h@)G�@(�5@(�@(`�@'�]@'��@']�@';d@' i@&͟@&�1@&	@%�@%��@%��@%k�@$��@$�@$�9@$��@$c�@$'R@#�m@#ݘ@#��@#�@#�@"�@":*@"�@"O@"	@!�>@!�d@!�t@!��@!B�@!:�@!<6@!=�@!=�@!�@ �p@ �4@ U2@  �@ b@ @� @@O@@�!@��@z@W�@5?@�j@��@u�@Vm@<6@�@֡@�Y@7�@G@��@�@��@j�@F�@&@�@�"@�@�@��@\�@)�@ �@�>@��@F@8�@-w@�@�P@�U@��@��@u�@H@$@�@��@� @��@\)@��@u%@3�@.�@�@�z@��@j@O�@B�@4@�@Ɇ@|�@h�@bN@C-@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
� B
�4B
� B
� B
�iB
��B
��B
�B
}�B
{�B
z�B
{�B
{�B
|B
|6B
|jB
|�B
|�B
�4B
�B
x�B
bB
V�B
S�B
[	B
L�B
B�B
R�B
dB
u?B
z�B
��B
ևB
��B
ߊB
�B
�%B
�B&�B8�B3�B7�BC�BBB�BaBsBR:B��B��B�jB	B�BSBmB9B"hB33B>�B>�B"�B��B�8B��B�NB�_B�B��B�sB�B��B�|B�7B��B��B��B��Bi�BZkBFtB:�B./BTB�B
�6B
�%B
��B
�2B
��B
c�B
J�B
7�B
+�B
%�B
$�B
 �B
 B
�B
�B	�fB	��B	�/B	��B	�fB	y$B	kB	S�B	5?B	gB	-B��B�AB��B�B��B�B��B�AB�gB��B�]B�-B�B	�B		�B	@B	QB	,"B	2-B	7�B	?�B	LJB	W�B	^OB	e`B	l�B	o�B	v�B	��B	�lB	��B	��B	��B	�|B	� B	��B	�^B	�jB	��B	��B	��B	��B	��B	�B	��B	�B	ңB	ϑB	��B	��B	��B	��B	��B	��B	��B	ʌB	�.B	�:B	�'B	�fB	�lB
 OB	�wB	�BB
�B
�B
^B
~B
�B
�B
.B
�B
�B
B
�B
 B
MB
B
{B
gB
�B
�B
�B
�B
sB
�B
�B
�B	��B	��B	�B	�KB	��B	�hB	�RB	��B	��B	��B	��B
  B
 B
 4B
B
3B
3B
�B
DB

XB

XB

�B
)B
xB

�B

�B

XB
�B
�B
vB
.B
�B
B

=B
_B
YB
�B
B
3B
B
�B
�B
3B
{B
�B
zB
1B
�B
�B
9B
3B
�B
;B
�B
�B
B
�B
�B
 �B
 �B
�B
 B
B
;B
 �B
�B
-B
�B	��B	�XB	�lB	��B	��B	��B	�ZB	�zB	�B	��B	�LB	��B	�B	�?B	��B	�B	��B	�fB	�B	�$B	�0B	��B	��B	�rB	��B
�B
�B
AB
�B
�B
 �B
 B	�.B	��B	�wB	�B	��B	��B	��B	��B	�rB	�	B	��B	�B	��B	�B	�aB	�B	�MB	�TB	��B	�^B	��B	��B	�LB	��B	�>B	��B	�lB	�RB	��B	�lB	�B	�8B	��B	��B	�zB	�xB	�"B	��B	��B	��B	��B	��B	�%B	�ZB	�zB	��B	�$B	�B	�0B	��B	�VB	�<B	��B	��B	��B	��B	��B	�"B	�VB	��B	��B	��B	�B	�(B	��B	��B	�wB	�BB	�wB	�BB	�]B	�BB	�BB	�BB	��B	�B	��B	�]B	��B	��B	�wB	�B	�B	��B	��B	��B	��B	��B	�(B	��B	��B	��B	��B
 �B
 �B
[B
�B
�B
�B
[B
'B
uB
-B
-B
GB
 iB
GB
�B
�B
�B
�B
{B
B
�B
�B
�B
�B
�B
-B
B
�B
�B
�B
[B
�B
AB
B
B
B
�B
�B
B
�B
-B
-B
GB
aB
B
aB
aB
B
B
�B
�B
�B
-B
-B
GB
GB
B
�B
�B
[B
�B
�B
B
GB
�B
MB
SB
�B
?B
�B
B
EB
�B
	�B

�B
0B
B
JB
~B
�B
0B
�B
�B
�B
B
xB

XB
	RB
	B
�B
	B
	B
	�B
	�B

	B

	B

	B

XB

�B
�B
�B
�B
�B
�B
�B
JB
�B
�B
PB
"B
<B
VB
VB
pB
pB
�B
BB
�B
�B
�B
�B
�B
 B
NB
�B
 B
 B
B
 B
TB
:B
:B
�B
�B
hB
NB
4B
 B
�B
}B
 B
&B
�B
�B
B
�B
gB
�B
{B
�B
B
:B
B
TB
B
FB
�B
MB
�B
B
mB
�B
mB
�B
?B
+B
B
B
�B
�B
)B
xB
�B
/B
/B
�B
�B
jB
�B
;B
�B
�B
�B
 �B
!B
!HB
!-B
!HB
!bB
!|B
!�B
!�B
!�B
"�B
# B
#:B
#�B
#�B
#�B
$@B
$�B
%B
%,B
%,B
%zB
%�B
%�B
&�B
&�B
&�B
'B
'B
'�B
($B
(sB
(>B
(�B
(�B
(�B
)yB
*0B
)�B
*B
*�B
*�B
+QB
,"B
,"B
,�B
-wB
-wB
-�B
./B
.IB
.cB
.}B
.cB
.cB
.}B
.�B
.�B
/�B
/�B
0B
0UB
0�B
0�B
0�B
0�B
1AB
1�B
1�B
2B
2B
2-B
2-B
2�B
2�B
2|B
3MB
3hB
3�B
3�B
4TB
4�B
4�B
5�B
5�B
6FB
6`B
6`B
6zB
6�B
7LB
7fB
7�B
7�B
7�B
7�B
7�B
8�B
8lB
8�B
8�B
8�B
8lB
8�B
8lB
8�B
8�B
9>B
:xB
:DB
:�B
;JB
:�B
;dB
;�B
<B
<6B
<�B
<�B
<�B
>BB
>wB
>wB
>wB
?B
?�B
?�B
@4B
@iB
@�B
@�B
AoB
AB
AB
AB
AB
A B
BB
B[B
B�B
B�B
C-B
CaB
C�B
DB
D�B
D�B
D�B
D�B
EB
EB
E�B
FB
F?B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
IB
I7B
IB
H�B
H�B
I7B
I�B
I�B
J	B
J=B
J�B
KDB
KDB
KDB
K�B
K�B
L0B
L~B
L�B
L�B
L�B
MPB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
NVB
OB
N�B
O�B
OvB
OvB
O�B
O�B
O�B
O�B
PHB
PHB
PHB
P.B
P.B
O�B
PB
PHB
P�B
P}B
P�B
QB
QhB
Q�B
Q�B
Q�B
R:B
R�B
R�B
S&B
S&B
S[B
S�B
S�B
S�B
TaB
T�B
T�B
U�B
U�B
U�B
VB
VB
VSB
VSB
VmB
VmB
V�B
V�B
V�B
W$B
WsB
WYB
W�B
W�B
X+B
X+B
Y1B
YKB
YeB
Y�B
Y�B
Z7B
Z�B
Z�B
Z�B
Z�B
[	B
[WB
[WB
[�B
[�B
\)B
\)B
\)B
\CB
\�B
\�B
\�B
\�B
\�B
\�B
]IB
]/B
\)B
]�B
]�B
]�B
]�B
^B
^OB
^5B
^jB
^jB
^B
]�B
]�B
]IB
\�B
\�B
]IB
]�B
^OB
^B
^�B
`B
`\B
`\B
`vB
`�B
a-B
a�B
a�B
bB
b4B
b4B
bB
bNB
b�B
b�B
b�B
c:B
cTB
cTB
c B
c:B
cnB
c�B
c�B
c�B
dB
c�B
d&B
dB
dB
dZB
d�B
d�B
eFB
e�B
e�B
f2B
fLB
f�B
f�B
f�B
f�B
f�B
fLB
f�B
f�B
f�B
f�B
gB
gB
g8B
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h
B
hsB
hsB
h�B
iDB
i�B
j�B
kB
k�B
k�B
k�B
k�B
k�B
lB
k�B
k�B
k�B
k�B
l=B
lWB
l�B
l�B
mB
m�B
nB
n/B
n/B
n}B
n�B
n�B
n�B
oB
o5B
oiB
oOB
oOB
o�B
o�B
pB
o�B
p;B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
q�B
rB
r-B
rGB
rB
r-B
r-B
raB
r�B
s3B
s�B
tB
s�B
tB
t9B
tnB
t�B
t�B
t�B
t�B
t�B
u%B
uZB
uZB
u?B
uZB
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
� B
�4B
� B
� B
�iB
��B
��B
�B
}�B
{�B
z�B
{�B
{�B
|B
|6B
|jB
|�B
|�B
�4B
�B
x�B
bB
V�B
S�B
[	B
L�B
B�B
R�B
dB
u?B
z�B
��B
ևB
��B
ߊB
�B
�%B
�B&�B8�B3�B7�BC�BBB�BaBsBR:B��B��B�jB	B�BSBmB9B"hB33B>�B>�B"�B��B�8B��B�NB�_B�B��B�sB�B��B�|B�7B��B��B��B��Bi�BZkBFtB:�B./BTB�B
�6B
�%B
��B
�2B
��B
c�B
J�B
7�B
+�B
%�B
$�B
 �B
 B
�B
�B	�fB	��B	�/B	��B	�fB	y$B	kB	S�B	5?B	gB	-B��B�AB��B�B��B�B��B�AB�gB��B�]B�-B�B	�B		�B	@B	QB	,"B	2-B	7�B	?�B	LJB	W�B	^OB	e`B	l�B	o�B	v�B	��B	�lB	��B	��B	��B	�|B	� B	��B	�^B	�jB	��B	��B	��B	��B	��B	�B	��B	�B	ңB	ϑB	��B	��B	��B	��B	��B	��B	��B	ʌB	�.B	�:B	�'B	�fB	�lB
 OB	�wB	�BB
�B
�B
^B
~B
�B
�B
.B
�B
�B
B
�B
 B
MB
B
{B
gB
�B
�B
�B
�B
sB
�B
�B
�B	��B	��B	�B	�KB	��B	�hB	�RB	��B	��B	��B	��B
  B
 B
 4B
B
3B
3B
�B
DB

XB

XB

�B
)B
xB

�B

�B

XB
�B
�B
vB
.B
�B
B

=B
_B
YB
�B
B
3B
B
�B
�B
3B
{B
�B
zB
1B
�B
�B
9B
3B
�B
;B
�B
�B
B
�B
�B
 �B
 �B
�B
 B
B
;B
 �B
�B
-B
�B	��B	�XB	�lB	��B	��B	��B	�ZB	�zB	�B	��B	�LB	��B	�B	�?B	��B	�B	��B	�fB	�B	�$B	�0B	��B	��B	�rB	��B
�B
�B
AB
�B
�B
 �B
 B	�.B	��B	�wB	�B	��B	��B	��B	��B	�rB	�	B	��B	�B	��B	�B	�aB	�B	�MB	�TB	��B	�^B	��B	��B	�LB	��B	�>B	��B	�lB	�RB	��B	�lB	�B	�8B	��B	��B	�zB	�xB	�"B	��B	��B	��B	��B	��B	�%B	�ZB	�zB	��B	�$B	�B	�0B	��B	�VB	�<B	��B	��B	��B	��B	��B	�"B	�VB	��B	��B	��B	�B	�(B	��B	��B	�wB	�BB	�wB	�BB	�]B	�BB	�BB	�BB	��B	�B	��B	�]B	��B	��B	�wB	�B	�B	��B	��B	��B	��B	��B	�(B	��B	��B	��B	��B
 �B
 �B
[B
�B
�B
�B
[B
'B
uB
-B
-B
GB
 iB
GB
�B
�B
�B
�B
{B
B
�B
�B
�B
�B
�B
-B
B
�B
�B
�B
[B
�B
AB
B
B
B
�B
�B
B
�B
-B
-B
GB
aB
B
aB
aB
B
B
�B
�B
�B
-B
-B
GB
GB
B
�B
�B
[B
�B
�B
B
GB
�B
MB
SB
�B
?B
�B
B
EB
�B
	�B

�B
0B
B
JB
~B
�B
0B
�B
�B
�B
B
xB

XB
	RB
	B
�B
	B
	B
	�B
	�B

	B

	B

	B

XB

�B
�B
�B
�B
�B
�B
�B
JB
�B
�B
PB
"B
<B
VB
VB
pB
pB
�B
BB
�B
�B
�B
�B
�B
 B
NB
�B
 B
 B
B
 B
TB
:B
:B
�B
�B
hB
NB
4B
 B
�B
}B
 B
&B
�B
�B
B
�B
gB
�B
{B
�B
B
:B
B
TB
B
FB
�B
MB
�B
B
mB
�B
mB
�B
?B
+B
B
B
�B
�B
)B
xB
�B
/B
/B
�B
�B
jB
�B
;B
�B
�B
�B
 �B
!B
!HB
!-B
!HB
!bB
!|B
!�B
!�B
!�B
"�B
# B
#:B
#�B
#�B
#�B
$@B
$�B
%B
%,B
%,B
%zB
%�B
%�B
&�B
&�B
&�B
'B
'B
'�B
($B
(sB
(>B
(�B
(�B
(�B
)yB
*0B
)�B
*B
*�B
*�B
+QB
,"B
,"B
,�B
-wB
-wB
-�B
./B
.IB
.cB
.}B
.cB
.cB
.}B
.�B
.�B
/�B
/�B
0B
0UB
0�B
0�B
0�B
0�B
1AB
1�B
1�B
2B
2B
2-B
2-B
2�B
2�B
2|B
3MB
3hB
3�B
3�B
4TB
4�B
4�B
5�B
5�B
6FB
6`B
6`B
6zB
6�B
7LB
7fB
7�B
7�B
7�B
7�B
7�B
8�B
8lB
8�B
8�B
8�B
8lB
8�B
8lB
8�B
8�B
9>B
:xB
:DB
:�B
;JB
:�B
;dB
;�B
<B
<6B
<�B
<�B
<�B
>BB
>wB
>wB
>wB
?B
?�B
?�B
@4B
@iB
@�B
@�B
AoB
AB
AB
AB
AB
A B
BB
B[B
B�B
B�B
C-B
CaB
C�B
DB
D�B
D�B
D�B
D�B
EB
EB
E�B
FB
F?B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
IB
I7B
IB
H�B
H�B
I7B
I�B
I�B
J	B
J=B
J�B
KDB
KDB
KDB
K�B
K�B
L0B
L~B
L�B
L�B
L�B
MPB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
NVB
OB
N�B
O�B
OvB
OvB
O�B
O�B
O�B
O�B
PHB
PHB
PHB
P.B
P.B
O�B
PB
PHB
P�B
P}B
P�B
QB
QhB
Q�B
Q�B
Q�B
R:B
R�B
R�B
S&B
S&B
S[B
S�B
S�B
S�B
TaB
T�B
T�B
U�B
U�B
U�B
VB
VB
VSB
VSB
VmB
VmB
V�B
V�B
V�B
W$B
WsB
WYB
W�B
W�B
X+B
X+B
Y1B
YKB
YeB
Y�B
Y�B
Z7B
Z�B
Z�B
Z�B
Z�B
[	B
[WB
[WB
[�B
[�B
\)B
\)B
\)B
\CB
\�B
\�B
\�B
\�B
\�B
\�B
]IB
]/B
\)B
]�B
]�B
]�B
]�B
^B
^OB
^5B
^jB
^jB
^B
]�B
]�B
]IB
\�B
\�B
]IB
]�B
^OB
^B
^�B
`B
`\B
`\B
`vB
`�B
a-B
a�B
a�B
bB
b4B
b4B
bB
bNB
b�B
b�B
b�B
c:B
cTB
cTB
c B
c:B
cnB
c�B
c�B
c�B
dB
c�B
d&B
dB
dB
dZB
d�B
d�B
eFB
e�B
e�B
f2B
fLB
f�B
f�B
f�B
f�B
f�B
fLB
f�B
f�B
f�B
f�B
gB
gB
g8B
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h
B
hsB
hsB
h�B
iDB
i�B
j�B
kB
k�B
k�B
k�B
k�B
k�B
lB
k�B
k�B
k�B
k�B
l=B
lWB
l�B
l�B
mB
m�B
nB
n/B
n/B
n}B
n�B
n�B
n�B
oB
o5B
oiB
oOB
oOB
o�B
o�B
pB
o�B
p;B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
q�B
rB
r-B
rGB
rB
r-B
r-B
raB
r�B
s3B
s�B
tB
s�B
tB
t9B
tnB
t�B
t�B
t�B
t�B
t�B
u%B
uZB
uZB
u?B
uZB
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105232  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191539  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191539  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191540                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041547  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041547  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                